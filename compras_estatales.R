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
write_rds(estados_compra, path = "Csv/meta_estados_de_compras.rds")

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
write_rds(estados_proveedor, path = "Csv/meta_estados_proveedor.rds")

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
write_rds(incisos, path = "Csv/meta_incisos.rds")

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
write_rds(monedas, path = "Csv/meta_monedas.rds")

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
write_rds(objetos_gastos, path = "Csv/meta_objetos_gastos.rds")

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
write_rds(porcentaje_subprograma_pcpd, path = "Csv/meta_porcentaje_subprograma_pcpd.rds")

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
write_rds(subprogramas_pcpd, path = "Csv/meta_subprogramas_pcpd.rds")

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
          cant_adj = str_replace_all(string = cant_adj, pattern = "^(cant-adj=\")|(\"\\s/>)$", replacement = "")) %>% 
   filter(!is.na(id_tipocompra))
write_rds(subtipos_compras, path = "Csv/meta_subtipos_compras.rds")

## Tipos de ajustes de adjuudiicacion ##
url_tipos_ajustes_adj <- "https://www.comprasestatales.gub.uy/comprasenlinea/jboss/reporteTiposAjusteAdj.do"
tipos_ajustes_adj <- read_lines(url_tipos_ajustes_adj, locale = locale(encoding = "Latin1"))[3] %>% 
   str_replace(pattern = "^(<tipos-ajuste-adj>)", replacement = "") %>% 
   str_replace(pattern = "(</tipos-ajuste-adj>)$", replacement = "") %>% 
   str_replace_all(pattern = "/>", replacement = "/>\\\n") %>% 
   str_split(pattern = "\\n") %>% 
   .[[1]] %>% 
   tibble(x = .) %>% 
   mutate(x = str_replace(string = x, pattern = "<tipo-ajuste-adj ", replacement = ""),
          x = str_replace_all(string = x, pattern = "(\")([[:space:]])([a-z])", replacement = "\\1@\\3"),
          x = if_else(str_detect(string = x, pattern = "nuevo-item-ofe") == FALSE,
                      str_replace(string = x, pattern = "@modif-item-adj", replacement = "@@modif-item-adj"), x)) %>% 
   separate(x, sep = "@",
            into = c("id", "descripcion", "reiteracion", "resolucion", "pub_llamado", "nuevo_item_ofe", "modif_item_adj", 
                     "nuevo_item_adj")) %>% 
   mutate(id = as.numeric(str_replace_all(string = id, pattern = "[^0-9]", replacement = "")),
          descripcion = str_replace_all(string = descripcion, pattern = "^(descripcion=\")|(\")", replacement = ""),
          reiteracion = as.factor(str_replace_all(string = reiteracion, pattern = "^(reiteracion=\")|(\")", replacement = "")),
          resolucion = as.factor(str_replace_all(string = resolucion, pattern = "^(resolucion=\")|(\")$", replacement = "")),
          pub_llamado = as.factor(str_replace_all(string = pub_llamado, pattern = "^(pub-llamado=\")|(\")$", replacement = "")),
          nuevo_item_ofe = as.factor(str_replace_all(string = nuevo_item_ofe, pattern = "^(nuevo-item-ofe=\")|(\")$", replacement = "")),
          modif_item_adj = as.factor(str_replace_all(string = modif_item_adj, pattern = "^(modif-item-adj=\")|(\")$", replacement = "")),
          nuevo_item_adj = as.factor(str_replace_all(string = nuevo_item_adj, pattern = "^(nuevo-item-adj=\")|(\"\\s/>)$", replacement = ""))) %>% 
   filter(!is.na(id))
write_rds(tipos_ajustes_adj, path = "Csv/meta_tipos_ajustes_adj.rds")

## Tipos dee compra ##
url_tipos_compra <- "https://www.comprasestatales.gub.uy/comprasenlinea/jboss/reporteTiposCompra.do"
tipos_compra <- read_lines(url_tipos_compra, locale = locale(encoding = "Latin1"))[3] %>% 
   str_replace(pattern = "^(<tipos-compra>)", replacement = "") %>% 
   str_replace(pattern = "(</tipos-compra>)$", replacement = "") %>% 
   str_replace_all(pattern = "/>", replacement = "/>\\\n") %>% 
   str_split(pattern = "\\n") %>% 
   .[[1]] %>% 
   tibble(x = .) %>% 
   mutate(x = str_replace(string = x, pattern = "<tipo-compra ", replacement = ""),
          x = str_replace_all(string = x, pattern = "(\")([[:space:]])([a-z])", replacement = "\\1@\\3"),
          x = if_else(str_detect(string = x, pattern = "acto-apertura") == FALSE,
                      str_replace(string = x, pattern = "@plazo-min-oferta", replacement = "@@plazo-min-oferta"), x),
          x = if_else(str_detect(string = x, pattern = "plazo-min-oferta") == FALSE,
                      str_replace(string = x, pattern = "@resolucion-obligatoria", replacement = "@@resolucion-obligatoria"), x),
          x = if_else(str_detect(string = x, pattern = "solics-llamado") == FALSE,
                      str_replace(string = x, pattern = "@ampliaciones", replacement = "@@ampliaciones"), x),
          x = if_else(str_detect(string = x, pattern = "tope-legal") == FALSE,
                      str_replace(string = x, pattern = "@pcpd", replacement = "@@pcpd"), x)) %>% 
   separate(x, sep = "@",
            into = c("id", "descripcion", "oferta_economica", "acto_apertura", "plazo_min_oferta", "resolucion_obligatoria", 
                     "solics_llamado",  "ampliaciones", "tope_legal", "pcpd")) %>% 
   mutate(id = str_replace_all(string = id, pattern = "^(id=\")|(\")", replacement = ""),
          descripcion = str_replace_all(string = descripcion, pattern = "^(descripcion=\")|(\")", replacement = ""),
          oferta_economica = str_replace_all(string = oferta_economica, pattern = "^(oferta-economica=\")|(\")", replacement = ""),
          acto_apertura = str_replace_all(string = acto_apertura, pattern = "^(acto-apertura=\")|(\")$", replacement = ""),
          plazo_min_oferta = str_replace_all(string = plazo_min_oferta, pattern = "^(plazo-min-oferta=\")|(\")$", replacement = ""),
          resolucion_obligatoria = str_replace_all(string = resolucion_obligatoria, pattern = "^(resolucion-obligatoria=\")|(\")$", replacement = ""),
          solics_llamado = str_replace_all(string = solics_llamado, pattern = "^(solics-llamado=\")|(\")$", replacement = ""),
          ampliaciones = str_replace_all(string = ampliaciones, pattern = "^(ampliaciones=\")|(\")$", replacement = ""),
          tope_legal = str_replace_all(string = tope_legal, pattern = "^(tope-legal=\")|(\")$", replacement = ""),
          pcpd = str_replace_all(string = pcpd, pattern = "^(pcpd=\")|(\"\\s/>)$", replacement = "")) %>% 
   filter(!is.na(descripcion))
write_rds(tipos_compra, path = "Csv/meta_tipos_compra.rds")

## Tipo de documentos de proveedor ##
url_tipos_doc_proveedor <- "https://www.comprasestatales.gub.uy/comprasenlinea/jboss/reporteTiposDocumento.do"
tipos_doc_proveedor <- read_lines(url_tipos_doc_proveedor, locale = locale(encoding = "Latin1"))[3] %>% 
   str_replace(pattern = "^(<tipos-doc>)", replacement = "") %>% 
   str_replace(pattern = "(</tipos-doc>)$", replacement = "") %>% 
   str_replace_all(pattern = "/>", replacement = "/>\\\n") %>% 
   str_split(pattern = "\\n") %>% 
   .[[1]] %>% 
   tibble(x = .) %>% 
   mutate(x = str_replace(string = x, pattern = "<tipo-doc ", replacement = ""),
          x = str_replace_all(string = x, pattern = "(\")([[:space:]])([a-z])", replacement = "\\1@\\3")) %>% 
   separate(x, sep = "@",
            into = c("tipo", "descripcion", "prov_rupe", "pcpd")) %>% 
   mutate(tipo = str_replace_all(string = tipo, pattern = "^(tipo=\")|(\")", replacement = ""),
          descripcion = str_replace_all(string = descripcion, pattern = "^(descripcion=\")|(\")", replacement = ""),
          prov_rupe = str_replace_all(string = prov_rupe, pattern = "^(prov-rupe=\")|(\")", replacement = ""),
          pcpd = str_replace_all(string = pcpd, pattern = "^(pcpd=\")|(\"\\s/>)$", replacement = "")) %>% 
   filter(!is.na(descripcion))
write_rds(tipos_doc_proveedor, path = "Csv/meta_tipos_doc_proveedor.rds")

## Tipos de resolucion ##
url_tipos_resolucion <- "https://www.comprasestatales.gub.uy/comprasenlinea/jboss/reporteTiposResolucion.do"
tipos_resolucion <- read_lines(url_tipos_resolucion, locale = locale(encoding = "Latin1"))[3] %>% 
   str_replace(pattern = "^(<tipos-res>)", replacement = "") %>% 
   str_replace(pattern = "(</tipos-res>)$", replacement = "") %>% 
   str_replace_all(pattern = "/>", replacement = "/>\\\n") %>% 
   str_split(pattern = "\\n") %>% 
   .[[1]] %>% 
   tibble(x = .) %>% 
   mutate(x = str_replace(string = x, pattern = "<tipo-res ", replacement = ""),
          x = str_replace_all(string = x, pattern = "(\")([[:space:]])([a-z])", replacement = "\\1@\\3")) %>% 
   separate(x, sep = "@",
            into = c("id", "descripcion")) %>% 
   mutate(id = as.numeric(str_replace_all(string = id, pattern = "[^0-9]", replacement = "")),
          descripcion = str_replace_all(string = descripcion, pattern = "^(descripcion=\")|(\")", replacement = "")) %>% 
   filter(!is.na(id))
write_rds(tipos_resolucion, path = "Csv/meta_tipos_resolucion.rds")

## Tipos resolucion tipo adjustes adjudicacion ##
url_tipos_resolucion_tipoajusteadj <- "https://www.comprasestatales.gub.uy/comprasenlinea/jboss/reporteTiposResolucionTipoAjusteAdj.do"
tipos_resolucion_tipoajusteadj <- read_lines(url_tipos_resolucion_tipoajusteadj, locale = locale(encoding = "Latin1"))[3] %>% 
   str_replace(pattern = "^(<tipos-resolucion-tipoajusteadj>)", replacement = "") %>% 
   str_replace(pattern = "(</tipos-resolucion-tipoajusteadj>)$", replacement = "") %>% 
   str_replace_all(pattern = "/>", replacement = "/>\\\n") %>% 
   str_split(pattern = "\\n") %>% 
   .[[1]] %>% 
   tibble(x = .) %>% 
   mutate(x = str_replace(string = x, pattern = "<tipo-resolucion-tipoajusteadj ", replacement = ""),
          x = str_replace_all(string = x, pattern = "(\")([[:space:]])([a-z])", replacement = "\\1@\\3")) %>% 
   separate(x, sep = "@",
            into = c("id_tipoajusteadj", "id_tiporesol")) %>% 
   mutate(id_tipoajusteadj = as.numeric(str_replace_all(string = id_tipoajusteadj, pattern = "[^0-9]", replacement = "")),
          id_tiporesol = as.numeric(str_replace_all(string = id_tiporesol, pattern = "[^0-9]", replacement = ""))) %>% 
   filter(!is.na(id_tipoajusteadj))
write_rds(tipos_resolucion_tipoajusteadj, path = "Csv/meta_tipos_resolucion_tipoajusteadj.rds")

## Tipos resolucion tipo compra ##
url_tipos_resolucion_tipocompra <- "https://www.comprasestatales.gub.uy/comprasenlinea/jboss/reporteTiposResolucionCompra.do"
tipos_resolucion_tipocompra <- read_lines(url_tipos_resolucion_tipoajusteadj, locale = locale(encoding = "Latin1"))[3] %>% 
   str_replace(pattern = "^(<tipos-resolucion-compra>)", replacement = "") %>% 
   str_replace(pattern = "(</tipos-resolucion-compra>)$", replacement = "") %>% 
   str_replace_all(pattern = "/>", replacement = "/>\\\n") %>% 
   str_split(pattern = "\\n") %>% 
   .[[1]] %>% 
   tibble(x = .) %>% 
   mutate(x = str_replace(string = x, pattern = "<tipo-resolucion-compra ", replacement = ""),
          x = str_replace_all(string = x, pattern = "(\")([[:space:]])([a-z])", replacement = "\\1@\\3")) %>% 
   separate(x, sep = "@",
            into = c("id_tipo_resolucion", "id_tipo_compra")) %>% 
   mutate(id_tipo_resolucion = as.numeric(str_replace_all(string = id_tipo_resolucion, pattern = "[^0-9]", replacement = "")),
          id_tipo_compra = as.numeric(str_replace_all(string = id_tipo_compra, pattern = "[^0-9]", replacement = ""))) %>% 
   filter(!is.na(id_tipo_resolucion))
write_rds(tipos_resolucion_tipocompra, path = "Csv/meta_tipos_resolucion_tipocompra.rds")

## Topes legales ##
url_topes_legales <- "https://www.comprasestatales.gub.uy/comprasenlinea/jboss/reporteTopesLegales.do"
topes_legales <- read_lines(url_topes_legales, locale = locale(encoding = "Latin1"))[3] %>% 
   str_replace(pattern = "^(<topes-legales>)", replacement = "") %>% 
   str_replace(pattern = "(</topes-legales>)$", replacement = "") %>% 
   str_replace_all(pattern = "/>", replacement = "/>\\\n") %>% 
   str_split(pattern = "\\n") %>% 
   .[[1]] %>% 
   tibble(x = .) %>% 
   mutate(x = str_replace(string = x, pattern = "<tope-legal ", replacement = ""),
          x = str_replace_all(string = x, pattern = "(\")([[:space:]])([a-z])", replacement = "\\1@\\3")) %>% 
   separate(x, sep = "@",
            into = c("id_tipo_compra", "fecha_desde", "comun", "ampliado")) %>% 
   mutate(id_tipo_compra = str_replace_all(string = id_tipo_compra, pattern = "^(id-tipo-compra=\")|(\")", replacement = ""),
          fecha_desde = lubridate::dmy(str_replace_all(string = fecha_desde, pattern = "^(fecha-desde=\")|(\")", replacement = "")),
          comun = str_replace_all(string = comun, pattern = "^(comun=\")|(\")", replacement = ""),
          ampliado = as.numeric(str_replace_all(string = ampliado, pattern = "[^0-9]", replacement = ""))) %>% 
   filter(!is.na(fecha_desde))
write_rds(topes_legales, path = "Csv/meta_topes_legales.rds")


## Base de compras ##
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

# https://www.comprasestatales.gub.uy/comprasenlinea/codigueras


#################################
##### FIN DE LA PROGRAMACIÓN ####
#################################