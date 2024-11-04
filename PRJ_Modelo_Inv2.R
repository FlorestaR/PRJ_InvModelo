# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PRJ_Modelo_Inv2: Inventário da Fazenda Modelo   ~~~~~~~~~~~~~~~~~~~~~
#
# Autor: Luiz Carlos Estraviz Rodriguez
#        Otávio Magalhães Silva Souza
#        Departamento de Ciências Florestais
#        ESALQ/USP - 03/Nov/2024
#
#   - Estimativas de inventário com amostragem em duas fases
#        Amostragem Dupla Simples (ADS)                             
#        Amostragem Dupla Estratificada (ADE)                             
#
# Linguagem de programação:
#       R (v 4.4.1)
#       package: forestinventory (v 1.0.0 2021-01-08 - Andreas Hill)
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list=ls(all=TRUE))                                   # Limpa memória
gc()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Lê pacote forestinventory
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if(!require(tidyverse))
  install.packages(tidyverse)
library(tidyverse)

# Leitura da versão mais atual do pacote rio 
#           para importação de planilhas Excel
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (!require("remotes")){install.packages("remotes")}
if (!require(rio))      {remotes::install_github("gesistsa/rio")}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Lê pacote forestinventory
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if(!require(forestinventory))  
  install.packages("forestinventory")
library(forestinventory)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Amostragem Dupla - parâmetros do pacote
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Função:
# twophase(
#         formula  = Y ~ Z + W, Y é a variável medida no campo. Z e W são variáveis auxiliares,
#         data     = dataframe ou vetor com a variável Y,
#         phase_id = list (
#                         phase.col   = nome coluna id da fase,
#                         terrgrid.id = valor numérico do id da fase),
#         area     = list(
#                        sa.col = nome coluna de estratificação,
#                        areas  = vetor c("", "", "") de estratos), ubiased = TRUE para viés e FALSE para sem viés,
#         cluster  = nome da coluna, se houver cluster sampling)
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define local e nome da planilha para leitura dos dados
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
prjNome  <- "PRJ_InvModelo"
dirNome  <- paste0('C:/GitHub/', prjNome, '/DADOS/')
arqNome_metricas  <- paste0(dirNome, prjNome, '_Metricas.xlsx')
arqNome_grid <- paste0(dirNome, prjNome, '.xlsx')
metricas     <- import(arqNome_metricas)
grid     <- import(arqNome_grid, which = "grid")
talhoes <- import(arqNome_grid, which = "talhoes")

# Mescla as métricas LiDAR com o Grid através da coluna "gridcell". Células do
# grid cuja área é menor do que 1m² são descartadas.
grid_laz <- merge(grid, metricas, by = "gridcell") %>%
  filter_at(vars(areacell), all_vars(. >= 1))

# Seleciona colunas de interesse
X <- tibble(grid_laz) %>% select(fase, areacell, idade, zmean, 
                                 zsd, pzabovezmean, zq30, zq75, zq95, MHDOM, VTCC)

X$fase <- as.numeric(X$fase) # Converte a coluna "fase" para o tipo numérico
X$boundaryweights <- X$areacell / 400 # Cria a coluna "boundaryweights"
X <- as.data.frame(X) # Tibble não é uma função nativa do R, é uma função chamada pelo tidyverse. 
                      # Dessa forma, o formato da tabela que se gera não é reconhecido pela função 
                      # twophase() do package forestinventory. O forestinventory reconhece data frames, 
                      # já que são nativos do R.

# Análise de regressão linear para verificar a correlação
# entre o p95 e a idade do inventário com o VTCC
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
m <- lm(VTCC ~ zq95 + idade, data = X)    # Análise de Regressão Linear
summary(m)                          # Mostra os resultados da regressão
VTCCparcelas <- X$VTCC[!is.na(X$VTCC)] # VTCCparcelas recebe os valores não nulos de VTCC contidos em X, ou seja, recebe os valores de VTCC das parcelas de campo
plot(VTCCparcelas, predict(m))              # Gráfico de observado vs predito
abline(0,1)

# Dupla Amostragem Casual
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
reg2p_nex <- twophase(formula = VTCC ~ zq95 + idade, # formula relaciona os valores de VTCC com zq95 e IDINV (análise de regressão)
                      data = X, #  Base de dados utilizada
                      boundary_weights = "boundaryweights",
                      phase_id = list(phase.col = "fase", terrgrid.id = 2)) # phase_id recebe uma lista em que a coluna a ser analisada é a "Inventario" e o identificador da segunda fase é 2
summary(reg2p_nex) # Dá os resultados da Dupla Amostragem
confint(reg2p_nex) # Estatística de confiança

# Dupla Amostragem Estratificada
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
reg2p_nex_est = twophase(
  formula = VTCC ~ zq95 + idade,
  data = X,
  phase_id =list(phase.col = "fase", terrgrid.id = 2),
  boundary_weights = "boundaryweights",
  small_area = list(sa.col = "idade", areas = c("3.7", "5.2"), unbiased = FALSE)) 
summary(reg2p_nex_est)
confint(reg2p_nex_est)

y = c(2, 4, 6, 8, 10)
x = c(1, 2, 3, 4, 5)

y <- c(2, 4, 6, 8, 10)
x <- c(1, 2, 3, 4, 5)

plot(x, y, xlim = c(0, max(x)), ylim = c(0, max(y)))
abline(0,2)
