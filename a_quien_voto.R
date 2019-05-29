######################
#### A QUIÉN VOTO ####
######################

library(tidyverse)
library(magrittr)
library(jsonlite)

datos <- readr::read_csv("Csv/aquienvoto.csv")
candidatos <- jsonlite::read_json(path = "Csv/candidatos.json", simplifyVector = TRUE) %>% as_tibble() %>% unnest() %>% arrange(id)
datos <- left_join(datos, candidatos, by = c("candidatoId" = "id")) %>% select(id, candidatoId, party, name, everything())

#####################
#### END OF CODE ####
#####################