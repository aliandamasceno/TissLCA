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
#' data <- extyear(AMBULATORIAL,280030,"D:/Dados/TISS/AMBULATORIAL/2019/SE")
#' data <- extyear(HOSPITALAR, 120040, "D:/Dados/TISS/HOSPITALAR/2019")
#' @import dplyr
#' @importFrom  magrittr %>%
#' @export
extyear <- function(tipo, cod_IBGE,wd) {
  tipo <- substitute(tipo)
  arquivos<-list.files(wd, pattern = "csv", full.names = TRUE)
  dados<-lapply(arquivos, read.csv, sep = ",")
  impar <- seq(1, length(dados), 2)
  par <- seq(2, length(dados),2)
  if(tipo == "HOSPITALAR") {
    for (i in impar) {
      dados[[i]] <- dados[[i]] %>%
        dplyr::filter(CD_MUNIC_PRESTADOR == cod_IBGE)
    }
    df1 <- dplyr::full_join(dados[[1]], dados[[2]], by = "X.ID_EVENTO")
    df2 <- dplyr::full_join(dados[[3]], dados[[4]], by = "X.ID_EVENTO")
    df3 <- dplyr::full_join(dados[[5]], dados[[6]], by = "X.ID_EVENTO")
    df4 <- dplyr::full_join(dados[[7]], dados[[8]], by = "X.ID_EVENTO")
    df5 <- dplyr::full_join(dados[[9]], dados[[10]], by = "X.ID_EVENTO")
    df6 <- dplyr::full_join(dados[[11]], dados[[12]], by = "X.ID_EVENTO")
    df7 <- dplyr::full_join(dados[[13]], dados[[14]], by = "X.ID_EVENTO")
    df8 <- dplyr::full_join(dados[[15]], dados[[16]], by = "X.ID_EVENTO")
    df9 <- dplyr::full_join(dados[[17]], dados[[18]], by = "X.ID_EVENTO")
    df10 <- dplyr::full_join(dados[[19]], dados[[20]], by = "X.ID_EVENTO")
    df11 <- dplyr::full_join(dados[[21]], dados[[22]], by = "X.ID_EVENTO")
    df12 <- dplyr::full_join(dados[[23]], dados[[24]], by = "X.ID_EVENTO")
    df <- rbind(df1, df2, df3, df4, df5, df6,df7, df8, df9,
                df10, df11, df12)
    df <- df %>%
      dplyr::filter(CD_MUNIC_PRESTADOR == cod_IBGE) %>%
      dplyr::mutate(VL_PROCEDIMENTO = as.numeric(as.character(VL_PROCEDIMENTO)),
                    QT_PROCEDIMENTO = as.numeric(as.character(QT_PROCEDIMENTO)),
                    VL_PROCEDIMENTO = tidyr::replace_na(VL_PROCEDIMENTO, 0),
                    QT_PROCEDIMENTO = tidyr::replace_na(QT_PROCEDIMENTO, 0)) %>%
      dplyr::filter(!is.na(VL_PROCEDIMENTO), !is.na(QT_PROCEDIMENTO))
  } else{
    for (i in impar) {
      dados[[i]] <- dados[[i]] %>%
        dplyr::filter(CD_MUNIC_PRESTADOR == cod_IBGE) %>%
        dplyr::select(X.ID_EVENTO, CD_MUNIC_PRESTADOR)
    }
    for (i in par) {
      dados[[i]] <- dados[[i]] %>%
        dplyr::select(X.ID_EVENTO,VL_PROCEDIMENTO,CD_TUSS_PROCEDIMENTO,
                      QT_PROCEDIMENTO)
    }
    df1 <- dplyr::full_join(dados[[1]], dados[[2]], by = "X.ID_EVENTO")
    df2 <- dplyr::full_join(dados[[3]], dados[[4]], by = "X.ID_EVENTO")
    df3 <- dplyr::full_join(dados[[5]], dados[[6]], by = "X.ID_EVENTO")
    df4 <- dplyr::full_join(dados[[7]], dados[[8]], by = "X.ID_EVENTO")
    df5 <- dplyr::full_join(dados[[9]], dados[[10]], by = "X.ID_EVENTO")
    df6 <- dplyr::full_join(dados[[11]], dados[[12]], by = "X.ID_EVENTO")
    df7 <- dplyr::full_join(dados[[13]], dados[[14]], by = "X.ID_EVENTO")
    df8 <- dplyr::full_join(dados[[15]], dados[[16]], by = "X.ID_EVENTO")
    df9 <- dplyr::full_join(dados[[17]], dados[[18]], by = "X.ID_EVENTO")
    df10 <- dplyr::full_join(dados[[19]], dados[[20]], by = "X.ID_EVENTO")
    df11 <- dplyr::full_join(dados[[21]], dados[[22]], by = "X.ID_EVENTO")
    df12 <- dplyr::full_join(dados[[23]], dados[[24]], by = "X.ID_EVENTO")
    df <- rbind(df1, df2, df3, df4, df5, df6,df7, df8, df9,
                df10, df11, df12)
    df <- df %>%
      dplyr::filter(CD_MUNIC_PRESTADOR == cod_IBGE) %>%
      dplyr::select(CD_MUNIC_PRESTADOR,CD_TUSS_PROCEDIMENTO,
                    QT_PROCEDIMENTO, VL_PROCEDIMENTO) %>%
      dplyr::mutate(VL_PROCEDIMENTO = as.numeric(as.character(VL_PROCEDIMENTO)),
                    QT_PROCEDIMENTO = as.numeric(as.character(QT_PROCEDIMENTO)),
                    VL_PROCEDIMENTO = tidyr::replace_na(VL_PROCEDIMENTO, 0),
                    QT_PROCEDIMENTO = tidyr::replace_na(QT_PROCEDIMENTO, 0)) %>%
      dplyr::filter(!is.na(VL_PROCEDIMENTO), !is.na(QT_PROCEDIMENTO))
  }
  return(df)
}
