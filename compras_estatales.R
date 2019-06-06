############################
#### COMPRAS ESTATALES #####
############################

library(tidyverse)
library(lubridate)
library(stringr)
library(rvest)

## Estados de compras ##
url_estados_compras <- "https://www.comprasestatales.gub.uy/comprasenlinea/jboss/reporteEstadosCompra.do"
estados_compra <- read_lines(url_estados_compras, locale = locale(encoding = "Latin1"))[3] %>% 
   str_replace(pattern = "^(<estados-compra>)", replacement = "") %>% 
   str_replace(pattern = "(</estados-compra>)$", replacement = "") %>% 
   str_replace_all(pattern = "/>", replacement = "/>\\\n") %>% 
   str_split(pattern = "\\n") %>% 
   .[[1]] %>% 
   tibble(x = .) %>% 
   mutate(x = str_replace(string = x, pattern = "<estado-compra ", replacement = ""),
          x = str_replace_all(string = x, pattern = "(\")([[:space:]])([a-z])", replacement = "\\1@\\3")) %>% 
   separate(x, sep = "@",
            into = c("id_estado_compra", "descripcion")) %>% 
   mutate(id_estado_compra = as.numeric(str_replace_all(string = id_estado_compra, pattern = "[^0-9]", replacement = "")),
          descripcion = str_to_title(str_replace_all(string = descripcion, pattern = "(^descripcion=\")|(\"\\s/>)$", replacement = ""))) %>% 
   filter(!is.na(id_estado_compra))

## Estados proveedor ##
url_estados_proveedor <- "https://www.comprasestatales.gub.uy/comprasenlinea/jboss/reporteEstadosProveedor.do"
estados_proveedor <- read_lines(url_estados_proveedor, locale = locale(encoding = "Latin1"))[3] %>% 
   str_replace(pattern = "^(<estados-proveedor>)", replacement = "") %>% 
   str_replace(pattern = "(</estados-proveedor>)$", replacement = "") %>% 
   str_replace_all(pattern = "/>", replacement = "/>\\\n") %>% 
   str_split(pattern = "\\n") %>% 
   .[[1]] %>% 
   tibble(x = .) %>% 
   mutate(x = str_replace(string = x, pattern = "<estado-proveedor ", replacement = ""),
          x = str_replace_all(string = x, pattern = "(\")([[:space:]])([a-z])", replacement = "\\1@\\3")) %>% 
   separate(x, sep = "@",
            into = c("estado", "desc_estado", "val_adjs", "val_amps")) %>% 
   mutate(estado = str_to_title(str_replace_all(string = estado, pattern = "^(estado=\")|(\")$", replacement = "")),
          desc_estado = str_replace_all(string = desc_estado, pattern = "(^desc-estado=\")|(\")$", replacement = ""),
          val_adjs = str_replace(string = val_adjs, pattern = "^.*(S|N).*$", replacement = "\\1"),
          val_amps = str_replace(string = val_amps, pattern = "^.*(S|N).*$", replacement = "\\1")) %>% 
   filter(!is.na(desc_estado))

## Incisos ##
url_incisos <- "https://www.comprasestatales.gub.uy/comprasenlinea/jboss/reporteIncisos.do"
incisos <- read_lines(url_incisos, locale = locale(encoding = "Latin1"))[3] %>% 
   str_replace(pattern = "^(<incisos>)", replacement = "") %>% 
   str_replace(pattern = "(</incisos>)$", replacement = "") %>% 
   str_replace_all(pattern = "/>", replacement = "/>\\\n") %>% 
   str_split(pattern = "\\n") %>% 
   .[[1]] %>% 
   tibble(x = .) %>% 
   mutate(x = str_replace(string = x, pattern = "<inciso ", replacement = ""),
          x = str_replace_all(string = x, pattern = "(\")([[:space:]])([a-z])", replacement = "\\1@\\3")) %>% 
   separate(x, sep = "@",
            into = c("inciso", "nom_inciso")) %>% 
   mutate(inciso = as.numeric(str_replace_all(string = inciso, pattern = "[^0-9]", replacement = "")),
          nom_inciso = str_replace_all(string = nom_inciso, pattern = "^(nom-inciso=\")|(\"\\s/>)$", replacement = "")) %>% 
   filter(!is.na(inciso))




## Monedas ##
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
          fecha_compra = lubridate::dmy(fecha_compra), # comienzo licitación
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
   mutate(classes = if_else(classes == "POSIXct" | classes == "POSIXt", "Datetime", classes)) %>%
   count(classes)

# adjudicaciones -> detalle de la compra (de la factura)
# oferentes -> todos los que participaron 


#################################
##### FIN DE LA PROGRAMACIÓN ####
#################################