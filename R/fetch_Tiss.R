#' Download da TISS anual por Muncípio
#'
#'
#' Faz o download e extração dos dados da base TISS do FTP da ANS e retorna dados do município escolhido, procedimentos realizados, quantidade e valor no ano de interesse
#'
#' tipo: AMBULATORIAL / HOSPITALAR
#'
#' uf: unidade federativa
#'
#' cod_IBGE: vetor de códigos IBGE de 6 dígitos para os municipios de interesse
#'
#' ano: a partir de 2015
#'
#' mes: vetor dos meses de interesse
#'
#' @name fetch_Tiss
#' @examples
#' #Loading package
#' library(TissLCA)
#' data <- fetch_Tiss(tipo = AMBULATORIAL, mes = c("12"), ano = 2020, cod_IBGE = c("411520"), uf = PR)
#' @import utils dplyr XML
#' @importFrom magrittr %>%
#' @export
fetch_Tiss <- function(tipo,mes,ano,uf,cod_IBGE) {

  tipo <- substitute(tipo)
  uf <- substitute(uf)
  wd <- getwd()
  base_url <- "http://ftp.dadosabertos.ans.gov.br/FTP/PDA/TISS"
  endpoint <- paste(base_url,
                    tipo,
                    ano,
                    uf, sep = "/")
  link <- htmlParse(endpoint)
  links <- xpathSApply(link, "//a/@href")
  links <- subset(links, grepl("zip", links))


  date <- paste(ano,
                mes,
                sep = "")

  links <- subset(links, grepl(paste(date, collapse = "|"), links))

  endpointcomlete <- paste(endpoint,
                           links,
                           sep = "/")

  arquivo <- c()
  zip <- data.frame()
  dados <- list()

  impar <- seq(1, length(endpointcomlete), 2)
  par <- seq(2, length(endpointcomlete),2)

  for (i in 1:length(endpointcomlete)) {

    temp <- tempfile()

    zip <- c()

    utils::download.file(endpointcomlete[i],temp)

    zip <- utils::unzip(temp, list = TRUE)

    arquivo[i] <- zip$Name

    dados[[i]] <- read.csv(unz(temp,arquivo[i]), sep = ";", dec = "," )

    if(i %in% impar) {dados[[i]] <- dados[[i]] %>%
      dplyr::filter(CD_MUNICIPIO_PRESTADOR %in% cod_IBGE)}

    if(i %in% par) {dados[[i]] <- dados[[i]] %>%
      dplyr::filter(ID_EVENTO_ATENCAO_SAUDE %in% dados[[i-1]]$ID_EVENTO_ATENCAO_SAUDE)}

  }


  tiss_uf <- Reduce(dplyr::full_join, dados)

  return(tiss_uf)
}
