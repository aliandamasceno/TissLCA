#' Download da TISS mensal por estado
#'
#'
#' Faz o download e extração dos dados da base TISS do FTP da ANS e retorna dados do estado escolhido, procedimentos realizados, quantidade e valor no ano de interesse
#'
#' tipo: string para 'AMBULATORIAL' ou 'HOSPITALAR'
#'
#' mes: string para o mês de interesse
#'
#' uf: string para unidade federativa
#'
#' @name getmonth
#' @examples
#' # Loading package
#' library(TissLCA)
#' data <- getmonth('AMBULATORIAL', '12', 2019, 'AC')
#' @importFrom  magrittr %>%
#' @export
getmonth <- function(tipo,mes,ano,uf) {
  temp <- tempfile()
  base_url <- "http://ftp.dadosabertos.ans.gov.br/FTP/PDA/TISS"
  uf.zip<- paste(uf, ".zip",sep = "")
  endpoint <- paste(base_url,
                    tipo,
                    ano,
                    uf.zip, sep = "/")
  utils::download.file(endpoint,temp)
  zip<-utils::unzip(temp, list = TRUE)
  arquivos<-zip$Name
  arquivos <- as.data.frame(arquivos) %>%
    tidyr::separate(arquivos, into = c("a", "b"), sep = 7) %>%
    tidyr::separate(b, into = c("c","d"), sep = 2) %>%
    dplyr::filter(c == mes) %>%
    tidyr::unite(a, c("a","c","d"), sep = "")
  dados<-list()
  for (i in 1:length(arquivos$a)) {
    dados[[i]]<-read.csv(unz(temp,arquivos$a[i]))
  }
  tiss_uf<-Reduce(dplyr::full_join, dados)
  tiss_uf$UF<-uf
  return(tiss_uf)
}
