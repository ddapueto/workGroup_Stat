############################
#### COMPRAS ESTATALES #####
############################

library(tidyverse)
library(lubridate)
library(stringr)
library(rvest)

compras <- readr::read_csv("Csv/comprasEstatales.csv")
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

url_monedas <- "https://www.comprasestatales.gub.uy/comprasenlinea/jboss/reporteMonedas.do"
read_lines(url_monedas)[3] %>% 
   str_replace(pattern = "^(<monedas>)", replacement = "") %>% 
   str_replace(pattern = "(</monedas>)$", replacement = "") %>% 
   str_replace_all(pattern = "/>", replacement = "/>\\\n") %>% 
   str_split(pattern = "\\n") %>% 
   .[[1]] %>% 
   tibble(x = .) %>% 
   mutate(x = str_replace(string = x, pattern = "<moneda ", replacement = "")) 


# Variable classes
tibble(variables = names(sapply(compras, class)),
       classes = sapply(compras, class)) %>%
   unnest() %>% 
   count(classes)


#################################
##### FIN DE LA PROGRAMACIÓN ####
#################################