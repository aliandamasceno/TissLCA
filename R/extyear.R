#' Extração da TISS anual por município
#'
#'
#' Faz a leitura dos dados anuais descompactados e juntados numa mesma pasta pelo usuário e retorna dados do município escolhido, procedimentos realizados, quantidade e valor no ano de interesse
#'
#' tipo: AMBULATORIAL / HOSPITALAR
#'
#' cod_IBGE: Inteiro para código IBGE do município de 6 dígitos
#'
#' wd: Pasta de trabalho onde estão localizados os dados anuais descompactados
#'
#' Juntar numa mesma pasta dados completos do ano de interesse
#' @name extyear
#' @examples
#' # Loading package
#' library(TissLCA)
#' data <- extyear(AMBULATORIAL,280030,"D:/Dados/TISS/AMBULATORIAL/2021/AC")
#' @import dplyr
#' @importFrom  magrittr %>%
#' @export
extyear <- function(tipo, cod_IBGE,wd) {
  tipo <- substitute(tipo)
  arquivos<-list.files(wd, pattern = "csv", full.names = TRUE)
  dados<-lapply(arquivos, read.csv, sep = ";",dec = "," , encoding = "UTF-8", header = TRUE)
  impar <- seq(1, length(dados), 2)
  par <- seq(2, length(dados),2)
  if(tipo == "HOSPITALAR") {
    for (i in impar) {
      dados[[i]] <- dados[[i]] %>%
        dplyr::filter(CD_MUNICIPIO_PRESTADOR == cod_IBGE)
    }
    df1 <- dplyr::full_join(dados[[1]], dados[[2]], by = "ID_EVENTO_ATENCAO_SAUDE")
    df2 <- dplyr::full_join(dados[[3]], dados[[4]], by = "ID_EVENTO_ATENCAO_SAUDE")
    df3 <- dplyr::full_join(dados[[5]], dados[[6]], by = "ID_EVENTO_ATENCAO_SAUDE")
    df4 <- dplyr::full_join(dados[[7]], dados[[8]], by = "ID_EVENTO_ATENCAO_SAUDE")
    df5 <- dplyr::full_join(dados[[9]], dados[[10]], by = "ID_EVENTO_ATENCAO_SAUDE")
    df6 <- dplyr::full_join(dados[[11]], dados[[12]], by = "ID_EVENTO_ATENCAO_SAUDE")
    df7 <- dplyr::full_join(dados[[13]], dados[[14]], by = "ID_EVENTO_ATENCAO_SAUDE")
    df8 <- dplyr::full_join(dados[[15]], dados[[16]], by = "ID_EVENTO_ATENCAO_SAUDE")
    df9 <- dplyr::full_join(dados[[17]], dados[[18]], by = "ID_EVENTO_ATENCAO_SAUDE")
    df10 <- dplyr::full_join(dados[[19]], dados[[20]], by = "ID_EVENTO_ATENCAO_SAUDE")
    df11 <- dplyr::full_join(dados[[21]], dados[[22]], by = "ID_EVENTO_ATENCAO_SAUDE")
    df12 <- dplyr::full_join(dados[[23]], dados[[24]], by = "ID_EVENTO_ATENCAO_SAUDE")
    df <- rbind(df1, df2, df3, df4, df5, df6,df7, df8, df9,
                df10, df11, df12)
    df <- df %>%
      dplyr::filter(CD_MUNICIPIO_PRESTADOR == cod_IBGE) %>%
      dplyr::mutate(VL_PROCEDIMENTO = as.numeric(as.character(VL_ITEM_EVENTO_INFORMADO)),
                    QT_PROCEDIMENTO = as.numeric(as.character(QT_ITEM_EVENTO_INFORMADO)),
                    VL_PROCEDIMENTO = tidyr::replace_na(VL_ITEM_EVENTO_INFORMADO, 0),
                    QT_PROCEDIMENTO = tidyr::replace_na(QT_ITEM_EVENTO_INFORMADO, 0)) %>%
      dplyr::filter(!is.na(VL_ITEM_EVENTO_INFORMADO), !is.na(QT_ITEM_EVENTO_INFORMADO))
  } else{
    for (i in impar) {
      dados[[i]] <- dados[[i]] %>%
        dplyr::filter(CD_MUNICIPIO_PRESTADOR == cod_IBGE) %>%
        dplyr::select(ID_EVENTO_ATENCAO_SAUDE, CD_MUNICIPIO_PRESTADOR)
    }
    for (i in par) {
      dados[[i]] <- dados[[i]] %>%
        dplyr::select(ID_EVENTO_ATENCAO_SAUDE,VL_ITEM_EVENTO_INFORMADO,CD_PROCEDIMENTO,
                      QT_ITEM_EVENTO_INFORMADO)
    }
    df1 <- dplyr::full_join(dados[[1]], dados[[2]], by = "ID_EVENTO_ATENCAO_SAUDE")
    df2 <- dplyr::full_join(dados[[3]], dados[[4]], by = "ID_EVENTO_ATENCAO_SAUDE")
    df3 <- dplyr::full_join(dados[[5]], dados[[6]], by = "ID_EVENTO_ATENCAO_SAUDE")
    df4 <- dplyr::full_join(dados[[7]], dados[[8]], by = "ID_EVENTO_ATENCAO_SAUDE")
    df5 <- dplyr::full_join(dados[[9]], dados[[10]], by = "ID_EVENTO_ATENCAO_SAUDE")
    df6 <- dplyr::full_join(dados[[11]], dados[[12]], by = "ID_EVENTO_ATENCAO_SAUDE")
    df7 <- dplyr::full_join(dados[[13]], dados[[14]], by = "ID_EVENTO_ATENCAO_SAUDE")
    df8 <- dplyr::full_join(dados[[15]], dados[[16]], by = "ID_EVENTO_ATENCAO_SAUDE")
    df9 <- dplyr::full_join(dados[[17]], dados[[18]], by = "ID_EVENTO_ATENCAO_SAUDE")
    df10 <- dplyr::full_join(dados[[19]], dados[[20]], by = "ID_EVENTO_ATENCAO_SAUDE")
    df11 <- dplyr::full_join(dados[[21]], dados[[22]], by = "ID_EVENTO_ATENCAO_SAUDE")
    df12 <- dplyr::full_join(dados[[23]], dados[[24]], by = "ID_EVENTO_ATENCAO_SAUDE")
    df <- rbind(df1, df2, df3, df4, df5, df6,df7, df8, df9,
                df10, df11, df12)
    df <- df %>%
      dplyr::filter(CD_MUNICIPIO_PRESTADOR == cod_IBGE) %>%
      dplyr::select(CD_MUNICIPIO_PRESTADOR,CD_PROCEDIMENTO,
                    QT_ITEM_EVENTO_INFORMADO, VL_ITEM_EVENTO_INFORMADO) %>%
      dplyr::mutate(VL_ITEM_EVENTO_INFORMADO = as.numeric(as.character(VL_ITEM_EVENTO_INFORMADO)),
                    QT_ITEM_EVENTO_INFORMADO = as.numeric(as.character(QT_ITEM_EVENTO_INFORMADO)),
                    VL_ITEM_EVENTO_INFORMADO = tidyr::replace_na(VL_ITEM_EVENTO_INFORMADO, 0),
                    QT_ITEM_EVENTO_INFORMADO = tidyr::replace_na(QT_ITEM_EVENTO_INFORMADO, 0)) %>%
      dplyr::filter(!is.na(VL_ITEM_EVENTO_INFORMADO), !is.na(QT_ITEM_EVENTO_INFORMADO))
  }
  return(df)
}
