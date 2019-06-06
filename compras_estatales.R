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
          desc_moneda = str_to_title(str_replace_all(string = desc_moneda, pattern = "(^desc-moneda=\")|(\")$", replacement = "")),
          sigla_moneda = str_replace_all(string = sigla_moneda, pattern = "(^sigla-moneda=\")|(\")", replacement = ""),
          id_moneda_arbitraje = as.numeric(str_replace_all(string = id_moneda_arbitraje, pattern = "[^0-9]", replacement = ""))) %>% 
   filter(!is.na(id_moneda))


compras <- readr::read_csv("Csv/comprasEstatalesrefactor.csv")
compras <- compras %>% 
   mutate(apel = fct_recode(apel, "No" = "N", "Yes" = "S"),
          es_reiteracion = fct_recode(es_reiteracion, "No" = "N", "Yes" = "S"),
          estado_compra = as.factor(estado_compra),
          fecha_compra = lubridate::dmy(fecha_compra),
          fecha_pub_adj = lubridate::dmy_hm(fecha_pub_adj),
          fondos_rotatorios = fct_recode(fondos_rotatorios, "No" = "N", "Yes" = "S"),
          id_inciso = as.factor(id_inciso),
          id_moneda_monto_adj = factor(id_moneda_monto_adj, levels = monedas$desc_moneda),
          id_tipo_resol = as.factor(id_tipo_resol),
          id_tipocompra = as.factor(id_tipocompra),
          id_ue = as.factor(id_ue),
          nro_ampliacion = as.factor(nro_ampliacion),
          subtipo_compra = as.factor(subtipo_compra))

# Variable classes
tibble(variables = names(sapply(compras, class)),
       classes = sapply(compras, class)) %>%
   unnest() %>% 
   count(classes)

# adjudicaciones -> detalle de la compra (de la factura)
# oferentes -> todos los que participaron 


#################################
##### FIN DE LA PROGRAMACIÓN ####
#################################