# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PRJ_Modelo_Dados: Inventário da Fazenda Modelo  ~~~~~~~~~~~~~~~~~~~~~
#
# Autor: Luiz Carlos Estraviz Rodriguez
#        Otávio Magalhães Silva Souza
#        Departamento de Ciências Florestais
#        ESALQ/USP - 30/Set/2024
#
#   - download dos dados mantidos em um repositório github público
#      - shape files dos talhões e das parcelas de inventário
#   - sugestão de pasta para armazenamento local:
#        C:/LiDAR/PRJ_Modelo/DADOS
#
# Linguagem de programação:
#       R (v 4.3)
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list=ls(all=TRUE))                                   # Limpa memória
gc()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. Leitura e organização de dados
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----
if(!require(tidyverse))  # Para melhor manipulação de dados e funções
  install.packages("tidyverse")
library(tidyverse)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Download do shape da Fazenda Modelo (2 layers: talhoes e parcelas)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
gitOnde <- "https://github.com/FlorestaR/dados/blob/main/5_LIDARF/Modelo/SHAPES"
gitNome <- "fazmodelo.zip"
gitArqv <- file.path(gitOnde, gitNome) %>% paste0("?raw=true")

tmpd <- tempdir(check = TRUE)                    # diretório temporário
zipf <- file.path(tmpd, "shapes.zip")              # arquivo temporário

options(timeout=1000) # Reset timeout oferecendo mais tempo de download
if(!file.exists(zipf))  # garante download de dados binários (wb)
  download.file(gitArqv, mode="wb", destfile = zipf) 

unzip(zipf, exdir = tmpd)     # shape é unziped no diretório temporário
unlink(zipf)                                  # deleta o arquivo zipado

if(!require(sf))                           # Para manipulação de shapes
  install.packages("sf")
library(sf)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Lê atributos dos talhoes
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shpArq <- paste0(tmpd, "/Modelo_talhoes.shp")       # shape com talhões
talhoesComGeo <- read_sf(shpArq) %>%                # completo com geom
rename(fazenda = FAZENDA, bloco = TALHAO, 
       talhao = SUBTALHAO, area = AREA)                       # s/ geom
talhoesSemGeo <- tibble(sf::st_drop_geometry(talhoesComGeo))
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Lê atributos das parcelas
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shpArq <- paste0(tmpd, "/Modelo_parcelas.shp")     # shape com parcelas
parcelasComGeo <- read_sf(shpArq) %>%               # completo com geom
select("Fazenda", "ID_Talhao", "SUBTALHAO", "NUMPARCELA", "AREAPARCEL",
       "NUMMEDICAO", "ANOREF", "DATAREALIZ", "IDINV",
       "MHDOM", "VTCC", "geometry") %>%
  rename(fazenda = Fazenda, bloco = ID_Talhao, talhao= SUBTALHAO,
         parcela= NUMPARCELA, areapar= AREAPARCEL, medicao= NUMMEDICAO,
         anoinv= ANOREF, datinv= DATAREALIZ, idade= IDINV)
parcelasSemGeo <- tibble(sf::st_drop_geometry(parcelasComGeo)) # s/geom
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Reorganiza colunas da tabela "parcelas" e acrescenta coluna fase
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parcelas <- parcelasSemGeo %>% arrange(parcela)
parcelas$areapar <- round(parcelas$areapar, 0)
n        <- count(parcelas)
fase     <- rep(1:1, n)
parcelas <- cbind(fase, parcelas)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cria coluna idade na tabela talhoes, extraída da tabela parcelas
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
talhoes <-
  parcelasSemGeo %>%
  group_by(talhao) %>%
  summarise(idade = unique(idade)) %>%
  left_join(talhoesSemGeo) %>%
  select("talhao", "idade", "area") %>%
  arrange(talhao) %>% as.data.frame
talhoesComGeo <- inner_join(talhoesComGeo, talhoes, by="talhao") %>%
  select("OBJECTID", "talhao", "idade", "geometry")
rm(talhoesSemGeo, parcelasSemGeo)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Lê atributos do grid
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shpArq <- paste0(tmpd, "/Modelo_grid.shp")           # shape com o grid
gridComGeo <- read_sf(shpArq)                       # completo com geom
gridSemGeo <- tibble(sf::st_drop_geometry(gridComGeo))        # s/ geom

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Reorganiza colunas da tabela "parcelas"
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
grid <-
  gridSemGeo %>%
  select(gridcell, rowcell, colcell, areacell, talhao, fase, parcela,
         medicao, anoinv, idade, MHDOM, VTCC) %>% 
  arrange(gridcell) %>% as.data.frame
rm(gridSemGeo)

# Cria diretórios e pastas para onde os tiles LiDAR serão copiados
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
prjNome <- 'PRJ_InvModelo'
dirNome <- paste0('C:/GitRepo/', prjNome)
dir.create(dirNome, showWarnings = F)

# Salva e imprime dados reorganizados
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dirNome <- paste0(dirNome, '/DADOS/')
dir.create(dirNome, showWarnings = F)

# Leitura da mais atual versão rio para facilmente ler e salvar Excel
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (!require("remotes")){install.packages("remotes")}
if (!require(rio))      {remotes::install_github("gesistsa/rio")}
library(rio)
# Salva dados em planilha Excel
arqNome <- paste0(dirNome, prjNome, '.xlsx')
export(list(talhoes = talhoes,
            parcelas  = parcelas,
            grid = grid),
       file = arqNome)

# Mostra tabelas no Viewer
# Use o "Export" do Viewer para salvar em diferentes formatos
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if(!require(kableExtra))
  install.packages("kableExtra")
library(kableExtra)
# Mostra talhões
AreaTotal <- talhoes$AREA %>% sum
NotaDeRodape <- paste0(": ", AreaTotal)
talhoes %>%
  kbl(caption = "Talhões da Fazenda Modelo", align = "r") %>%
  kable_classic(full_width = F) %>%
  footnote(general = NotaDeRodape, 
           general_title = "Área total",
           footnote_as_chunk = T)
# Mostra parcelas
parcelas %>%
  kbl(caption = "Parcelas da Fazenda Modelo", align = "r") %>%
  kable_classic(full_width = F)

# Cria diretório para salvar mapas
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dirNome <- paste0('C:/GitRepo/', prjNome)
dir.create(dirNome, showWarnings = F)
dirNome <- paste0(dirNome, '/MAPAS/')
dir.create(dirNome, showWarnings = F)

# Cria mapa dos talhões com localização das parcelas (EPSG: 31983)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
arqNome <- paste0(dirNome, prjNome, '.png')
png(arqNome, 30, 20, 'cm', res = 200)                # abre "impressão"

ggplot() +        #         plot dos talhões e parcelas (cor por idade)
  geom_sf(data = talhoesComGeo, colour = "black", fill="white") +
  geom_sf(data = parcelasComGeo, aes(fill = factor(idade))) +
  scale_fill_discrete(name = "Idade") +
  guides(fill = guide_legend(reverse=F)) +
  coord_sf(datum=st_crs(31983)) +        # Especifica sistema de coord.
  scale_y_continuous(breaks = seq(from=7356500,to=7359000, by=200)) +
  scale_x_continuous(breaks = seq(from=206200, to=207600,  by=200))

dev.off()                                # fecha "impressão" do aquivo
# ----