## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------
#knitr::opts_chunk$set(echo = TRUE)
#knitr::opts_chunk$set(comment=NA)


## --------------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(rvest)
library(PxWebApiData)
library(dbplyr)


## --------------------------------------------------------------------------------------------------------------------------------
webpage <- read_html("https://www.ssb.no/a/histstat/aarbok/ht-0901-bnp.html")
tabell <- html_table(html_nodes(webpage, "table")[[2]])


## --------------------------------------------------------------------------------------------------------------------------------
head(tabell)
tail(tabell)
str(tabell)
names(tabell)


## --------------------------------------------------------------------------------------------------------------------------------
tabell <- tabell %>% drop_na()


## --------------------------------------------------------------------------------------------------------------------------------
names(tabell) <- c("År", "BNP", "BNP_endring",
                   "BNP_percap", "BNP_percap_endring")

tabell <- as_tibble(tabell)

tabell


## --------------------------------------------------------------------------------------------------------------------------------
tabell <-
  tabell %>% 
  mutate(BNP=str_replace_all(BNP, " ", ""), #erstatter vi mellomrom som tusenskillingtegn
         BNP_endring=na_if(BNP_endring, ""), #tomme tegn ("") for endringene med manglende observasjon (NA)
         BNP_percap_endring=na_if(BNP_percap_endring, ""),
         BNP_endring=str_replace(BNP_endring, ",","."), #erstatte komma med punktum
         BNP_percap_endring=str_replace(BNP_percap_endring, ",",".")) %>% 
  mutate_if(is.character, as.numeric) #gjør alle bokstaver om til tall

tabell

#Oppgave 1
#Lag et plot med BNP per innbygger i perioden.

tabell %>% 
  filter(År >=1865) %>% 
  ggplot(aes(x=År, y=BNP_percap)) + 
  geom_line(color="dark red") +
  scale_y_continuous(labels = scales::comma) +
  labs(title="BNP pr innbygger fra 1865",
       x =" ",
       y = "BNP pr capita") +
  theme_bw()


#Oppgave 2
#Gitt disse dataene, er det en annen viktig variabel som vi kan beregne ut fra dem, 
#i så fall hvilken? Bruk dplyr::mutate() funksjonen til å beregn denne størrelsen.

tabell %>% 
  mutate(capita=(BNP/BNP_percap)) %>% 
  ggplot(aes(x=År,y=capita)) +
  geom_line(color="Blue") + 
  labs(ttitle = "Innbyggere fra 1865",
       x="År fra 1865",
       y="Innbyggere")


 #Oppgave 3
#Denne tabellen inneholder årlige BNP data frem til 2011. 
#I det forrige caset så vi på nyere månedlige BNP tall. 
#I denne oppgaven skal du spleise de to BNP seriene til en lang tabell, 
#per år. Vi trenger ikke justere BNP-tallene ettersom begge tabellene har 2005 som basisår.

#Benytt funksjonen dplyr::bind_rows().
## -----------------------------------------------------------------------------------------------------------------------------------------------
tabellny <- ApiData("https://data.ssb.no/api/v0/no/table/09842/",
                    Tid = paste(1970:2019),
                    ContentsCode = "BNP")


## -----------------------------------------------------------------------------------------------------------------------------------------------
head(tabellny[[1]])


## -----------------------------------------------------------------------------------------------------------------------------------------------
head(tabellny[[2]])


## -----------------------------------------------------------------------------------------------------------------------------------------------
bnp <- tabellny[[1]]

bnp <- bnp %>%
  filter(år>=2012) %>%
  rename(År=år, BNP=value) %>% 
  select(År, BNP) %>% 
  mutate_if(is.character, as.numeric)
head(bnp)



nytabell <-
  bind_rows(tabell,bnp)

nytabell %>% 
  slice(140:155)
