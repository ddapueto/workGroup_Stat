############################
#### COMPRAS ESTATALES #####
############################

library(tidyverse)
library(lubridate)
library(stringr)
library(rvest)

#### PARSERS METADATA ####

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

## Objetos gasto ##
url_objetos_gasto <- "https://www.comprasestatales.gub.uy/comprasenlinea/jboss/reporteObjetosGasto.do"
objetos_gastos <- read_lines(url_objetos_gasto, locale = locale(encoding = "Latin1"))[3] %>% 
   str_replace(pattern = "^(<objetos-gastos>)", replacement = "") %>% 
   str_replace(pattern = "(</objetos-gastos>)$", replacement = "") %>% 
   str_replace_all(pattern = "/>", replacement = "/>\\\n") %>% 
   str_split(pattern = "\\n") %>% 
   .[[1]] %>% 
   tibble(x = .) %>% 
   mutate(x = str_replace(string = x, pattern = "<objeto-gasto ", replacement = ""),
          x = str_replace_all(string = x, pattern = "(\")([[:space:]])([a-z])", replacement = "\\1@\\3")) %>% 
   separate(x, sep = "@",
            into = c("odg", "descripcion")) %>% 
   mutate(odg = as.numeric(str_replace_all(string = odg, pattern = "[^0-9]", replacement = "")),
          descripcion = str_to_title(str_replace_all(string = descripcion, pattern = "^(descripcion=\")|(\"\\s/>)$", replacement = ""))) %>% 
   filter(!is.na(odg))

## Porcentaje suprograma PCPD ##
url_pcpd <- "https://www.comprasestatales.gub.uy/comprasenlinea/jboss/reportePorcentajesSubprogramasPCPD.do"
porcentaje_subprograma_pcpd <- read_lines(url_pcpd, locale = locale(encoding = "Latin1"))[3] %>% 
   str_replace(pattern = "^(<porcentajes-subprograma-pcpd>)", replacement = "") %>% 
   str_replace(pattern = "(</porcentajes-subprograma-pcpd>)$", replacement = "") %>% 
   str_replace_all(pattern = "/>", replacement = "/>\\\n") %>% 
   str_split(pattern = "\\n") %>% 
   .[[1]] %>% 
   tibble(x = .) %>% 
   mutate(x = str_replace(string = x, pattern = "<porcentaje-subprograma-pcpd ", replacement = ""),
          x = str_replace_all(string = x, pattern = "(\")([[:space:]])([a-z])", replacement = "\\1@\\3")) %>% 
   separate(x, sep = "@",
            into = c("codigo_subprograma", "fecha_vigencia", "porcentaje")) %>% 
   mutate(codigo_subprograma = as.numeric(str_replace_all(string = codigo_subprograma, pattern = "[^0-9]", replacement = "")),
          fecha_vigencia = lubridate::dmy(str_replace_all(string = fecha_vigencia, pattern = "^(fecha-vigencia=\")|(\")", replacement = "")),
          porcentaje = as.numeric(str_replace_all(string = porcentaje, pattern = "[^0-9]", replacement = ""))) %>% 
   filter(!is.na(codigo_subprograma))

## Suprogramas PCPD ##
url_subprogramas_pcpd <- "https://www.comprasestatales.gub.uy/comprasenlinea/jboss/reporteSubprogramasPCPD.do"
subprogramas_pcpd <- read_lines(url_subprogramas_pcpd, locale = locale(encoding = "Latin1"))[3] %>% 
   str_replace(pattern = "^(<subprogramas-pcpd>)", replacement = "") %>% 
   str_replace(pattern = "(</subprogramas-pcpd>)$", replacement = "") %>% 
   str_replace_all(pattern = "/>", replacement = "/>\\\n") %>% 
   str_split(pattern = "\\n") %>% 
   .[[1]] %>% 
   tibble(x = .) %>% 
   mutate(x = str_replace(string = x, pattern = "<subprograma-pcpd ", replacement = ""),
          x = str_replace_all(string = x, pattern = "(\")([[:space:]])([a-z])", replacement = "\\1@\\3")) %>% 
   separate(x, sep = "@",
            into = c("codigo", "descripcion", "fecha_desde", "fecha_hasta")) %>% 
   mutate(codigo = as.numeric(str_replace_all(string = codigo, pattern = "[^0-9]", replacement = "")),
          descripcion = str_replace_all(string = descripcion, pattern = "^(descripcion=\")|(\")", replacement = ""),
          fecha_desde = lubridate::dmy(str_replace_all(string = fecha_desde, pattern = "^(fecha-desde=\")|(\")", replacement = "")),
          fecha_hasta = str_replace_all(string = fecha_hasta, pattern = "^(fecha-hasta=\")|(\"\\s/>)", replacement = "")) %>% 
   filter(!is.na(codigo))

## Subtipos compra ##
url_subtipos_compra <- "https://www.comprasestatales.gub.uy/comprasenlinea/jboss/reporteSubTiposCompra.do"
subtipos_compras <- read_lines(url_subtipos_compra, locale = locale(encoding = "Latin1"))[3] %>% 
   str_replace(pattern = "^(<subtipos-compra>)", replacement = "") %>% 
   str_replace(pattern = "(</subtipos-compra>)$", replacement = "") %>% 
   str_replace_all(pattern = "/>", replacement = "/>\\\n") %>% 
   str_split(pattern = "\\n") %>% 
   .[[1]] %>% 
   tibble(x = .) %>% 
   mutate(x = str_replace(string = x, pattern = "<subtipo-compra ", replacement = ""),
          x = str_replace_all(string = x, pattern = "(\")([[:space:]])([a-z])", replacement = "\\1@\\3"),
          x = if_else(str_detect(string = x, pattern = "pub-llamado") == FALSE,
                      str_replace(string = x, pattern = "@cond", replacement = "@@cond"), x),
          x = if_else(str_detect(string = x, pattern = "pub-adj") == FALSE,
                      str_replace(string = x, pattern = "@cant-adj", replacement = "@@cant-adj"), x),
          x = if_else(str_detect(string = x, pattern = "prov-rupe") == FALSE,
                      str_replace(string = x, pattern = "@pub-adj", replacement = "@@pub-adj"), x)) %>% 
   separate(x, sep = "@",
            into = c("id", "id_tipocompra", "resumen", "pub_llamado", "cond_precios_ofertas", "fecha_baja", "prov_rupe", 
                     "pub_adj", "cant_adj")) %>% 
   mutate(id = str_replace_all(string = id, pattern = "^(id=\")|(\")$", replacement = ""),
          id_tipocompra = str_replace_all(string = id_tipocompra, pattern = "^(id-tipocompra=\")|(\")", replacement = ""),
          resumen = str_replace_all(string = resumen, pattern = "^(resumen=\")|(\")", replacement = ""),
          pub_llamado = str_replace_all(string = pub_llamado, pattern = "^(pub-llamado=\")|(\")$", replacement = ""),
          cond_precios_ofertas = str_replace_all(string = cond_precios_ofertas, pattern = "^(cond-precios-ofertas=\")|(\")$", replacement = ""),
          cond_precios_ofertas = str_replace_all(string = cond_precios_ofertas, pattern = "\\&gt;", replacement = ">"),
          fecha_baja = lubridate::dmy(str_replace_all(string = fecha_baja, pattern = "^(fecha-baja=\")|(\")", replacement = "")),
          prov_rupe = str_replace_all(string = prov_rupe, pattern = "^(prov-rupe=\")|(\")$", replacement = ""),
          pub_adj = str_replace_all(string = pub_adj, pattern = "^(pub-adj=\")|(\")$", replacement = ""),
          cant_adj = str_replace_all(string = cant_adj, pattern = "^(cant-adj=\")|(\")$", replacement = "")) %>% 
   filter(!is.na(id_tipocompra))



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