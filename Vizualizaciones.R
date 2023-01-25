
library(tidyverse)
library(readr)
library(ggplot2)


clarin <- read.csv("links_clarin_1.csv", encoding = "UTF-8")
texto_clarin <- read.csv("texto_clarin.csv", encoding = "UTF-8")
df_clarin <- read.csv("df_clarin_count.csv")
df_pag12 <- read.csv("df_count_pag12.csv")
df_pag12_lda <- read.csv("df_pag12_lda.csv")
df_telam <- read.csv("df_count_telam.csv")
df_ambito <- read.csv("df_count_ambito.csv")





df <- data.frame(
  medio = c("Télam", "Clarín", "Pagina 12", "Ambito Financiero", "Perfil"),
  n_doc = c(2497, 5590, 6650, 14440, 19958)
)

graf_n_doc <- ggplot(data = df, aes(y = n_doc, x = medio, fill = medio))+
  geom_col()+
  labs(title = "Cantidad de notas publicadas por medio",
       subtitle = "01/09/22 - 04/01/23",
       x = " ",
       y = " ",
       caption = "Fuente: Elaboracion propia")+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_manual(values=c('#fbb4ae',
                             '#b3cde3',
                             '#ccebc5',
                             '#decbe4',
                             '#fed9a6')) 


df_clarin <- df_clarin%>%
  mutate(Dominant_Topic = case_when(Dominant_Topic == 0 ~ "Espectaculo/Misc",
                                    Dominant_Topic == 1 ~ "Copa Mundial/Futbol",
                                    Dominant_Topic == 2 ~ "Politica/Justicia",
                                    Dominant_Topic == 3 ~ "Economia"))



df_pag12 <- df_pag12%>%
  mutate(Dominant_Topic = case_when(Dominant_Topic == 0 ~ "Justicia/Seguridad",
                                    Dominant_Topic == 1 ~ "Copa Mundial/Deportes",
                                    Dominant_Topic == 2 ~ "Politica/Economia",
                                    Dominant_Topic == 3 ~ "Cultura general/Espectaculo"))




df_telam <- df_telam%>%
  mutate(Dominant_Topic = case_when(Dominant_Topic == 0 ~ "Misc",
                                    Dominant_Topic == 1 ~ "Trabajo/Produccion",
                                    Dominant_Topic == 2 ~ "Justicia/Investigacion",
                                    Dominant_Topic == 3 ~ "Politica Nacional",
                                    Dominant_Topic == 4 ~ "Artes/Literatura"))


df_ambito <- df_ambito%>%
  mutate(Dominant_Topic = case_when(Dominant_Topic == 0 ~ "Pol. publicas/Produccion",
                                    Dominant_Topic == 1 ~ "Justicia/Investigacion",
                                    Dominant_Topic == 2 ~ "Economia Financiera",
                                    Dominant_Topic == 3 ~ "Copa Mundial",
                                    Dominant_Topic == 4 ~ "Politica Nacional"))



####

  ggplot(data = df_clarin, aes(y = count, x = Dominant_Topic, fill = Dominant_Topic))+
  geom_col()+
  labs(title = "Cantidad de notas publicadas por Topico dominante",
       subtitle = "Diario Clarin  01/09/22 - 04/01/23",
       x = " ",
       y = " ",
       caption = "Fuente: Elaboracion propia")+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_manual(values=c('#fbb4ae',
                             '#b3cde3',
                             '#ccebc5',
                             '#decbe4'))



ggplot(data = df_pag12, aes(y = count, x = Dominant_Topic, fill = Dominant_Topic))+
  geom_col()+
  labs(title = "Cantidad de notas publicadas por Topico dominante",
       subtitle = "Diario Pagina12  01/09/22 - 04/01/23",
       x = " ",
       y = " ",
       caption = "Fuente: Elaboracion propia")+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_manual(values=c('#fbb4ae',
                             '#b3cde3',
                             '#ccebc5',
                             '#decbe4'))



ggplot(data = df_telam, aes(y = count, x = Dominant_Topic, fill = Dominant_Topic))+
  geom_col()+
  labs(title = "Cantidad de notas publicadas por Topico dominante",
       subtitle = "Diario Telam  01/09/22 - 04/01/23",
       x = " ",
       y = " ",
       caption = "Fuente: Elaboracion propia")+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_manual(values=c('#fbb4ae',
                             '#b3cde3',
                             '#ccebc5',
                             '#decbe4',
                             '#fed9a6'))



  ggplot(data = df_ambito, aes(y = count, x = Dominant_Topic, fill = Dominant_Topic))+
  geom_col()+
  labs(title = "Cantidad de notas publicadas por Topico dominante",
       subtitle = "Diario Telam  01/09/22 - 04/01/23",
       x = " ",
       y = " ",
       caption = "Fuente: Elaboracion propia")+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_manual(values=c('#fbb4ae',
                             '#b3cde3',
                             '#ccebc5',
                             '#decbe4',
                             '#fed9a6'))



#########
tabla_clarin <- clarin%>%
  head(5)%>%
  gt()%>%
  tab_header(title = "Base de datos extraida de GDELT",
             subtitle = "Diario Clarin  01/09/22")%>%
  gt_theme_pff()

tabla_clarin <- texto_clarin%>%
  head(1)%>%
  gt()%>%
  tab_header(title = "Base de datos de textos extraidos",
             subtitle = "Diario Clarin")%>%
  gt_theme_pff()

#####
d<-df_pag12_lda%>%
  group_by(T_dominante)%>%
  slice(which.max(Por_top_dom))

d_2<-df_pag12_lda%>%
  group_by(T_dominante)%>%
  slice(which.min(Por_top_dom))

#####

d <- d%>%
  mutate(T_dominante = case_when(T_dominante == 0 ~ "Justicia/Seguridad",
                                    T_dominante == 1 ~ "Copa Mundial/Deportes",
                                    T_dominante == 2 ~ "Politica/Economia",
                                    T_dominante == 3 ~ "Cultura general"))


d%>%
  gt()%>%
  tab_header(title = "Noticias por tópico mas representativas",
             subtitle = "Diario Pagina12")%>%
  gt_theme_pff()

