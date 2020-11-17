#Webinar PAIFIT

#Packages
library(ggplot2)
library(zoo)
library(tidyverse)
library(readxl)
library(GGally)
library(scales)
library(corrplot)
library(ggplot2)
library(zoo)
library(tidyverse)
library(EpiEstim)
library(incidence)
library(readxl)

##### TRENDS 2020 #####

#Carregar dados
Mobil_covidbr <- read_excel("trends_covidbr.xlsx", sheet = "Mobil1")
Popularity_covidbr <- read_excel("trends_covidbr.xlsx", sheet = "Popularity")

#Média móvel para dados de popularidade google trends
Popularity_covidbr <- Popularity_covidbr %>%
  dplyr::arrange(desc(var)) %>% 
  dplyr::group_by(var) %>% 
  dplyr::mutate(popularity_rm = zoo::rollmean(popularity, k = 7, fill = NA)) %>% 
  dplyr::ungroup()

#Média móvel para dados de mobilidade google
Mobil_covidbr <- Mobil_covidbr %>%
  dplyr::arrange(desc(var)) %>% 
  dplyr::group_by(var) %>% 
  dplyr::mutate(popularity_rm = zoo::rollmean(mobility, k = 7, fill = NA)) %>% 
  dplyr::ungroup()

pop_covid<-rbind(subset(Popularity_covidbr, select= -c(popularity)), subset(Mobil_covidbr, select= -c(mobility)))

#Gerar figura temporal por variável
ggplot(data=pop_covid,aes(x=as.Date(date), y = popularity_rm)) +
  geom_line(color="black", size=1.0) +    
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  theme(strip.text.x = element_text(size = 15)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  facet_wrap(~var,ncol = 2 ,scales = "free_y") + 
  annotate("rect", xmin=as.Date("2020-03-17"), 
           xmax=as.Date(Inf), ymin=-Inf, ymax=Inf, alpha=0.3) + 
  labs(x = "Date", y = "Popularity (%)") +
  theme_bw()

##### Correlações ######

pop_covid2<-pop_covid

# converte os dados linhas para colunas por variável 
P<-
  pop_covid %>% pivot_wider(
    names_from = c(var),
    values_from = popularity_rm
  )

#Matriz de correlação
corCov<-cor(na.omit(subset(P, select= -c(date))))
corrplot(corCov, method="color", 
         tl.col="black", 
         tl.srt=45, 
         addCoef.col = "white",
         diag = F)

##### Mapas ####


#Pacotes
library(sf) 
library(maps)
library(maptools)
library(ggmap)
library(mapdata)
library(gridExtra)
library(raster)


#Carregar arquivos
dataset1 <- read_excel("corona_map.xlsx")
#Arquivo de mapas IGBE
mapbr <- sf::st_read("Estados_do_Brasil/Estados_do_Brasil/Brasil.shp")


#Agrupar dados com base em uma coluna
m1 <- merge(mapbr,dataset1, by='UF')

head(m1)

pg1<-ggplot(data = m1) +
  geom_sf(aes(fill = variavel)) + 
  xlab("Longitude") + ylab("Latitude") 

#preparo das cores dos mapas em uma graduação
p3g<- pg1 + scale_fill_gradient2(low="gray99", mid= "gray90", high="turquoise4",
                                 limits=c(0, 100),
                                 breaks=c(0,20,40,60,80,100),
                                 na.value = "gray90")

#configura as datas
variable_names1 <- list(
  "1" = "25/02/2020",
  "2" = "17/03/2020",
  "3" = "18/03/2020",
  "4" = "19/03/2020",
  "5" = "20/03/2020",
  "6" = "21/03/2020",
  "7" = "22/03/2020",
  "8" = "01/05/2020"
)

variable_labeller1 <- function(variable,value){
  return(variable_names1[value])
}

#Gerar figura com mapas em um layout de acordo com a data
mapaCorona<-p3g + facet_wrap(~m1$d, ncol = 2, labeller = variable_labeller1) +
  theme(strip.background = element_blank(),
        panel.background = element_blank(),
        axis.title = element_blank()
        , axis.text = element_blank()
        , axis.ticks = element_blank(),
        legend.key.size = unit(0.2, "cm"),
        legend.key.width = unit(0.9,"cm")) +
  theme(legend.direction = "horizontal", legend.position= "bottom", legend.box = "vertical") +
  labs(fill = "(%)") +
  theme(plot.title = element_text(color="Black", size=16, face="bold") ) +
  ggtitle("Popularidade - 'Coronavírus' ")

mapaCorona

##### Dados de óbitos por estado #####

#Carregando o arquivo xls
covid_data<-read_excel("HIST_PAINEL_COVIDBR_11ago2020.xlsx")

#explorando os dados
str(covid_data)

#calculando a média móvel por estado
covid_data <- covid_data %>%
  dplyr::arrange(desc(estado)) %>% 
  dplyr::group_by(estado) %>% 
  dplyr::mutate(obitosNovos_mm = zoo::rollmean(obitosNovos, k = 7, fill = NA)) %>% 
  dplyr::ungroup()

#gerando o gráfico com os dados novos (média móvel por estado)

ggplot(data=covid_data,           #banco de dados
       aes(x=data,                #eixo x
           y = obitosNovos_mm)) + #eixo y 
  geom_line() +                   #tipo de gráfico (linha)
  facet_wrap(~estado,             #dividir o gráfico por estado (1 figura por estado) em um layout
             scales = "free_y")   #deixa a escala em y Livre (cada gráfico terá uma escala)




