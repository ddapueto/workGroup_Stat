############################
#### COMPRAS ESTATALES #####
############################

library(tidyverse)
library(lubridate)
library(stringr)
library(rvest)

#### Descarga y le la base de monedas ####
url_monedas <- "https://www.comprasestatales.gub.uy/comprasenlinea/jboss/reporteMonedas.do"
monedas <- read_lines(url_monedas, locale = locale(encoding = "Latin1"))[3] %>% 
   str_replace(pattern = "^(<monedas>)", replacement = "") %>% 
   str_replace(pattern = "(</monedas>)$", replacement = "") %>% 
   str_replace_all(pattern = "/>", replacement = "/>\\\n") %>% 
   str_split(pattern = "\\n") %>% 
   .[[1]] %>% 
   tibble(x = .) %>% 
   mutate(x = str_replace(string = x, pattern = "<moneda ", replacement = ""),
          x = str_replace_all(string = x, pattern = "(\")([[:space:]])([a-z])", replacement = "\\1@\\3")) %>% 
   separate(x, sep = "@",
            into = c("id_moneda", "desc_moneda", "sigla_moneda", "id_moneda_arbitraje")) %>% 
   mutate(id_moneda = as.numeric(str_replace_all(string = id_moneda, pattern = "[^0-9]", replacement = "")),
          desc_moneda = str_replace_all(string = desc_moneda, pattern = "(^desc-moneda=\")|(\")$", replacement = ""),
          sigla_moneda = str_replace_all(string = sigla_moneda, pattern = "(^sigla-moneda=\")|(\")", replacement = ""),
          id_moneda_arbitraje = as.numeric(str_replace_all(string = id_moneda_arbitraje, pattern = "[^0-9]", replacement = ""))) %>% 
   filter(!is.na(id_moneda))


compras <- readr::read_csv("Csv/comprasEstatalesrefactor.csv")
# all.equal(compras$X1, seq(0, dim(compras)[1]-1, 1))
# comentario: revisar parseo....hay cosas raras (por ejemplo `@id_compra` es character)

# id_compra <- as.numeric(compras$`@id_compra`)
# pudo_parsear <- NULL
# for (i in 1:dim(compras)[1]) {
#    if (is.na(id_compra)[i] == TRUE) {
#       pudo_parsear[i] <- "No"
#    }
#    else {
#       pudo_parsear[i] <- "Yes"
#    }
# }
# table(pudo_parsear)


#### Por ahora sigo como si todo estuviera bien ####
names(compras) <- str_replace_all(string = names(compras), pattern = "[`|@]", replacement = "")
compras <- compras %>% 
   mutate(id = `X1`,
          fecha_compra = lubridate::dmy(fecha_compra),
          fecha_pub_adj = lubridate::dmy_hm(fecha_pub_adj))

# Variable classes
tibble(variables = names(sapply(compras, class)),
       classes = sapply(compras, class)) %>%
   unnest() %>% 
   count(classes)


#################################
##### FIN DE LA PROGRAMACIÓN ####
#################################