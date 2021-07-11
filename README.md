# TissLCA
Download and extract ANS TISS data

O PACOTE É COMPOSTO POR TRÊS FUNÇÕES: getmonth, getyear e extyear

1. getmonth: Faz o download e extração dos dados da base TISS do FTP da ANS e retorna dados do estado escolhido, procedimentos realizados, quantidade e valor no ano de interesse

usage(tipo, mes, ano, uf)

tipo: AMBULATORIAL / HOSPITALAR; mes: inteiro para o mês de interesse; uf: unidade federativa; ano: a partir de 2015

2. getyear: Faz o download e extração dos dados da base TISS do FTP da ANS e retorna dados do município escolhido, procedimentos realizados, quantidade e valor no ano de interesse

usage(tipo, ano, uf, cod_IBGE)

tipo: AMBULATORIAL / HOSPITALAR; uf: unidade federativa; cod_IBGE: inteiro para código IBGE do município de 6 dígitos; ano: a partir de 2015

3. extyear: Faz a leitura dos dados anuais descompactados e juntados numa mesma pasta pelo usuário e retorna dados do município escolhido, procedimentos realizados, quantidade e valor no ano de interesse

usage(tipo, cod_IBGE, wd)

tipo: AMBULATORIAL / HOSPITALAR; cod_IBGE: inteiro para código IBGE do município de 6 dígitos; wd: Pasta de trabalho onde estão localizados os dados anuais descompactados
