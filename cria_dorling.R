# 0. Setup inicial
## Carrega bibliotecas
library(readr)
library(dplyr)
library(cartogram)
library(geobr)
library(stringi)
library(stringr)
library(sf)

## Função para padronizar nomes
padroniza_nomes <- \(str) {
  str |> 
    stringi::stri_trans_general('latin-ascii;upper') |> 
    stringr::str_remove_all('([:space:]|[:punct:])')
}

## Carrega o banco de dados com o encoding correto
rawData <- readr::read_csv2('filiacao-2024_pais.csv',
                            locale = locale(encoding = 'ISO-8859-1'))

## Seleciona as colunas de interesse
rawData <- rawData |> 
  dplyr::select(UF:Quantitativo)

## Elimina linhas com partidos que não mais existem
rawData <- rawData |> 
  dplyr::filter(!Partido %in% c('DEM','PATRIOTA','PRP','PTB','PSC'))

## Mantém apenas municípios BR
rawData <- rawData |> 
  dplyr::filter(UF != 'ZZ')

# 1. Identifica as maiores forças partidárias ####
## Calcula a qtd de filiados por partido e município
workData <- rawData |> 
  dplyr::summarise(qtd = sum(Quantitativo),
                   .by = c(UF, Município, Partido))

## Lista os partidos com mais filiados em mais municípios
topPartidos <- workData |> 
  dplyr::slice_max(order_by = qtd, by = Município) |> 
  dplyr::count(Partido, sort = T) |> 
  dplyr::mutate(pct = 100*n/sum(n),
                cum = cumsum(pct)) |> 
  dplyr::filter(lag(cum, default = 0) <= 80) |> 
  dplyr::pull(Partido)
topPartidos <- c(topPartidos, 'Outros')

## Associa cores aos partidos
names(topPartidos) <- topPartidos
topPartidos[1:7] <- c('#dac853','#5cdde6','#70d660',
                      '#6b9bd8','#e7716e','#4c59c8','#c7cdcc')
readr::write_rds(topPartidos, 'topPartidos.rds')

## Identifica o maior partido de cada município
workData <- workData |> 
  dplyr::slice_max(order_by = qtd, by = Município) |> 
  dplyr::mutate(Partido = ifelse(Partido %in% names(topPartidos),
                                 Partido, 'Outros'),
                Partido = factor(Partido, names(topPartidos))) |> 
  dplyr::arrange(UF, Município, Partido) |> 
  dplyr::slice(1, .by = Município)

# 2. Cria o cartograma ####
## Cria uma chave para identificar os municípios
workData <- workData |> 
  dplyr::mutate(chave = padroniza_nomes(Município),
                chave = stringr::str_glue('{UF}_{chave}'))

## Corrige a cahve de alguns municípios
muniAjuste <- c(
  'MG_DONAEUSEBIA' = 'MG_DONAEUZEBIA',
  'BA_CAMACA' = 'BA_CAMACAN',
  'BA_CAMACANRI' = 'BA_CAMACARI',
  'MG_SAOTHOMEDASLETRAS' = 'MG_SAOTOMEDASLETRAS',
  'MT_SANTOANTONIODOLEVERGER' = 'MT_SANTOANTONIODELEVERGER',
  'PA_ELDORADODOSCARAJAS' = 'PA_ELDORADODOCARAJAS',
  'PR_MUNHOZDEMELLO' = 'PR_MUNHOZDEMELO',
  'RN_AREZ' = 'RN_ARES',
  'RN_ASSU' = 'RN_ACU',
  'RO_ALVORADADOOESTE' = 'RO_ALVORADADOESTE',
  'RO_ESPIGAODOOESTE' = 'RO_ESPIGAODOESTE',
  'SE_AMPARODESAOFRANCISCO' = 'SE_AMPARODOSAOFRANCISCO',
  'SE_GRACCHOCARDOSO' = 'SE_GRACHOCARDOSO',
  'SP_SAOLUISDOPARAITINGA' = 'SP_SAOLUIZDOPARAITINGA'
)
workData <- workData |> 
  dplyr::mutate(chave = stringr::str_replace_all(chave, muniAjuste))

## Obtém os polígonos dos municípios BR
muni_shp <- geobr::read_municipality(year = 2022)
muni_shp <- muni_shp |> 
  dplyr::mutate(chave = padroniza_nomes(name_muni),
                chave = stringr::str_glue('{abbrev_state}_{chave}')) |> 
  dplyr::select(code_muni, name_muni, chave)

## Cria o cartograma
muni_carto <- muni_shp |> 
  dplyr::right_join(workData |> dplyr::select(-Município, -UF)) |> 
  dplyr::filter(!is.na(name_muni)) |> 
  sf::st_transform('EPSG:31983') |> 
  cartogram_dorling(weight = 'qtd', m_weight = 0.2, k = 0.5)
readr::write_rds(muni_carto, 'dorling_muniBR.rds')
