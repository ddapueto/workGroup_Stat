######################
#### A QUIÉN VOTO ####
######################

library(tidyverse)
library(magrittr)
library(jsonlite)
library(rvest)
library(xml2)

datos <- readr::read_csv("Csv/aquienvoto.csv")
candidatos <- jsonlite::read_json(path = "Csv/candidatos.json", simplifyVector = TRUE) %>% as_tibble() %>% unnest() %>% arrange(id)
datos <- left_join(datos, candidatos, by = c("candidatoId" = "id")) %>% select(id, candidatoId, party, name, everything())

url_preguntas <- "https://github.com/johnblanco/predictor_electoral/blob/master/predictor_pol/preguntas.json"
preguntas <- xml2::read_html(url_preguntas, encoding = "UTF-8")
preguntas <- html_nodes(preguntas, "table") %>% 
   html_table(b, fill = TRUE) %>% 
   .[[1]] %>% 
   as_tibble() %>% 
   filter(str_detect(string = X2, pattern = "[[:punct:]][A-z]")) %>% 
   filter(!str_detect(string = X2, pattern = "questions")) %>% 
   mutate(X1 = if_else(str_detect(string = X2, pattern = "subject"), X2, NA_character_)) %>% 
   tidyr::fill(X1, .direction = "down" ) %>% 
   filter(X1 != X2) %>% 
   mutate(X1 = str_replace_all(string = X1, pattern = "[[:punct:]]", replacement = ""),
          X1 = str_replace(string = X1, pattern = "subject ", replacement = ""),
          X1 = str_to_title(X1),
          X2 = str_replace_all(string = X2, pattern = "[[:punct:]]", replacement = "")) %>% 
   rename(Subject = X1, Question = X2)




#####################
#### END OF CODE ####
#####################