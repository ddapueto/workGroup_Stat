############################
#### COMPRAS ESTATALES #####
############################

library(tidyverse)

compras <- readr::read_csv("Csv/comprasEstatales.csv")
# all.equal(compras$X1, seq(0, dim(compras)[1]-1, 1))
# comentario: revisar parseo....hay cosas raras (por ejemplo `@id_compra` es character)

compras <- compras %>% 
   mutate(`@fecha_compra` = lubridate::dmy(`@fecha_compra`))

# Variable classes
tibble(variables = names(sapply(compras, class)),
       classes = sapply(compras, class)) %>%
   count(classes)

id_compra <- as.numeric(compras$`@id_compra`)
pudo_parsear <- NULL
for (i in 1:dim(compras)[1]) {
   if (is.na(id_compra)[i] == TRUE) {
      pudo_parsear[i] <- "No"
   }
   else {
      pudo_parsear[i] <- "Yes"
   }
}
table(pudo_parsear)





#################################
##### FIN DE LA PROGRAMACIÓN ####
#################################