# 0. Setup inicial
## Carrega bibliotecas
library(dplyr)
library(readr)
library(ggplot2)

## Carrega o dorling
muni_carto <- readr::read_rds('dorling_muniBR.rds')

## Carrega a paleta de cores
topPartidos <- readr::read_rds('topPartidos.rds')

## Destaca as capitais
codeCapitais <- c(1200401, 2704302, 1600303, 1302603, 2927408,
                  2304400, 5300108, 3205309, 5208707, 2111300,
                  5103403, 5002704, 3106200, 1501402, 2507507,
                  4106902, 2611606, 2211001, 2408102, 4314902,
                  4205407, 3304557, 1100205, 1400100, 3550308,
                  2800308, 1721000)
muni_carto <- muni_carto |> 
  dplyr::mutate(lwd = ifelse(code_muni %in% codeCapitais, 0.8, 0.3))

# 3. Cria o mapa ####
muni_carto |> 
  ggplot() +
  geom_sf(aes(fill = Partido, linewidth = I(lwd)), color = 'gray20') +
  scale_fill_manual(values = topPartidos) +
  theme_void() + 
  theme(plot.background = element_rect(fill = '#EBEDD3', colour = NA))
