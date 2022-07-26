## Problem Set 3 - Spatial Data
# Isa: setwd('C:/Users/USER/OneDrive - Universidad de los Andes/Escritorio/BigData/PS1/PS3_Gomez-Ortiz-Vanegas-/stores')
# Sofi:
setwd('C:/Users/Sofia/Documents/2022-2/BigData/PS3_Gomez-Ortiz-Vanegas-/stores')
rm(list=ls())
# importar librerías
## Llamar pacman (contiene la función p_load)
require(pacman)
install.packages("car")

## Llama/instala-llama las librerías listadas
p_load(tidyverse,rio,
       sf, # Leer/escribir/manipular datos espaciales
       leaflet, # Visualizaciones dinámicas
       tmaptools, # geocode_OSM()
       osmdata, # Get OSM's data
       skimr)

# Base de datos de entrenamiento y de prueba
house_train = import("train.Rds")
house_train = st_as_sf(house_train, coords=c("lon", "lat"), crs=4326)

house_test = import("test.Rds")
house_test = st_as_sf(house_test, coords=c("lon", "lat"), crs=4326)


house_train_price <- house_train %>% select(price)
house_train <- house_train %>% select(-price)


house_test <- house_test %>% mutate(base = 'TEST')
house_train <- house_train %>% mutate(base = 'TRAIN')

house_union <- union_all(house_test, house_train)

#------------------------ Imputación de datos -----------------------------#

# Imputacion superficie:

# Contar missings por columna
colSums(is.na(house_union)) # surface_total tiene 88936 NAs

# Crear una sola variable de superficie
house_union <- house_union %>% mutate(
  surface_total = 
    ifelse(is.na(surface_total),surface_covered, surface_total))
table(is.na(house_union$surface_total))
# Ahora surface_total tiene 79364 NAs

# Pasar descripcion a minuscula
house_union <- house_union %>% 
  mutate(description = str_to_lower(string = house_union$description))
house_union <- house_union %>% 
  mutate(title = str_to_lower(string = house_union$title))

# Encontrar patrones para completar variable de superficie
#house_union$description

p1 = "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+m2"
p2 = "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+mts2"
p3 = "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+mts 2"
p4 = "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+metros"
p5 = "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+mt2"
p6 = "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+mt"
p7 = "[:space:]+[:digit:]+[:space:]+m2"
p8 = "[:space:]+[:digit:]+[:space:]+mts2"
p9 = "[:space:]+[:digit:]+[:space:]+mts 2"
p10 = "[:space:]+[:digit:]+[:space:]+metros"
p11 = "[:space:]+[:digit:]+[:space:]+mt2"
p12 = "[:space:]+[:digit:]+[:space:]+mt"
p13 = "[:space:]+[:digit:]+m2"
p14 = "[:space:]+[:digit:]+mts2"
p15 = "[:space:]+[:digit:]+mts 2"
p16 = "[:space:]+[:digit:]+metros"
p17 = "[:space:]+[:digit:]+mt2"
p18 = "[:space:]+[:digit:]+mt"
p19 = "[:space:]+[:digit:]+mts"
p20 = "[:space:]+rea+[:space:]+[:digit:]+mt2"
p21 = "[:space:]+rea+[:space:]+[:digit:]+mts2"
p22 = "[:space:]+rea+[:space:]+[:digit:]+mt 2"
p23 = "[:space:]+rea+[:space:]+[:digit:]+mts 2"
p24 = "[:space:]+rea+[:space:]+[:digit:]+metros"
p25 = "[:space:]+[:digit:]+[:space:]+mts"
p26 = "[:space:]+[:digit:]+m+²"
p27 = "[:digit:]+m+²"
p28 = "[:space:]+[:digit:]+[:space:]+m+²"
p29 = "[:punct:]+[:digit:]+[:space:]+metros"
p30 = "[:sp_ace:]+[:digit:]+[:punct:]+[:digit:]++m2"
p31 = "[:space:]+[:digit:]+[:punct:]+[:digit:]+mts2"
p32= "[:space:]+[:digit:]+[:punct:]+[:digit:]+mts 2"
p33 = "[:space:]+[:digit:]+[:punct:]+[:digit:]+metros"
p34 = "[:space:]+[:digit:]+[:punct:]+[:digit:]+mt2"
p35 = "[:space:]+[:digit:]+[:punct:]+[:digit:]+mt"

house_union <- house_union %>% 
  mutate(surface_total = ifelse(is.na(surface_total)== T,
                                str_extract(string = house_union$description,
                                            pattern = p1),
                                surface_total)
  )
house_union <- house_union %>% 
  mutate(surface_total = ifelse(is.na(surface_total)== T,
                                str_extract(string = house_union$description,
                                            pattern = p2),
                                surface_total)
  )
house_union <- house_union %>% 
  mutate(surface_total = ifelse(is.na(surface_total)== T,
                                str_extract(string = house_union$description,
                                            pattern = p3),
                                surface_total)
  )
house_union <- house_union %>% 
  mutate(surface_total = ifelse(is.na(surface_total)== T,
                                str_extract(string = house_union$description,
                                            pattern = p4),
                                surface_total)
  )
house_union <- house_union %>% 
  mutate(surface_total = ifelse(is.na(surface_total)== T,
                                str_extract(string = house_union$description,
                                            pattern = p4),
                                surface_total)
  )
house_union <- house_union %>% 
  mutate(surface_total = ifelse(is.na(surface_total)== T,
                                str_extract(string = house_union$description,
                                            pattern = p5),
                                surface_total)
  )
house_union <- house_union %>% 
  mutate(surface_total = ifelse(is.na(surface_total)== T,
                                str_extract(string = house_union$description,
                                            pattern = p6),
                                surface_total)
  )
house_union <- house_union %>% 
  mutate(surface_total = ifelse(is.na(surface_total)== T,
                                str_extract(string = house_union$description,
                                            pattern = p7),
                                surface_total)
  )
house_union <- house_union %>% 
  mutate(surface_total = ifelse(is.na(surface_total)== T,
                                str_extract(string = house_union$description,
                                            pattern = p8),
                                surface_total)
  )
house_union <- house_union %>% 
  mutate(surface_total = ifelse(is.na(surface_total)== T,
                                str_extract(string = house_union$description,
                                            pattern = p9),
                                surface_total)
  )
house_union <- house_union %>% 
  mutate(surface_total = ifelse(is.na(surface_total)== T,
                                str_extract(string = house_union$description,
                                            pattern = p10),
                                surface_total)
  )
house_union <- house_union %>% 
  mutate(surface_total = ifelse(is.na(surface_total)== T,
                                str_extract(string = house_union$description,
                                            pattern = p11),
                                surface_total)
  )
house_union <- house_union %>% 
  mutate(surface_total = ifelse(is.na(surface_total)== T,
                                str_extract(string = house_union$description,
                                            pattern = p12),
                                surface_total)
  )
house_union <- house_union %>% 
  mutate(surface_total = ifelse(is.na(surface_total)== T,
                                str_extract(string = house_union$description,
                                            pattern = p13),
                                surface_total)
  )
house_union <- house_union %>% 
  mutate(surface_total = ifelse(is.na(surface_total)== T,
                                str_extract(string = house_union$description,
                                            pattern = p14),
                                surface_total)
  )
house_union <- house_union %>% 
  mutate(surface_total = ifelse(is.na(surface_total)== T,
                                str_extract(string = house_union$description,
                                            pattern = p15),
                                surface_total)
  )

house_union <- house_union %>% 
  mutate(surface_total = ifelse(is.na(surface_total)== T,
                                str_extract(string = house_union$description,
                                            pattern = p16),
                                surface_total)
  )
house_union <- house_union %>% 
  mutate(surface_total = ifelse(is.na(surface_total)== T,
                                str_extract(string = house_union$description,
                                            pattern = p17),
                                surface_total)
  )
house_union <- house_union %>% 
  mutate(surface_total = ifelse(is.na(surface_total)== T,
                                str_extract(string = house_union$description,
                                            pattern = p18),
                                surface_total)
  )
house_union <- house_union %>% 
  mutate(surface_total = ifelse(is.na(surface_total)== T,
                                str_extract(string = house_union$description,
                                            pattern = p19),
                                surface_total)
  )
house_union <- house_union %>% 
  mutate(surface_total = ifelse(is.na(surface_total)== T,
                                str_extract(string = house_union$description,
                                            pattern = p20),
                                surface_total)
  )
house_union <- house_union %>% 
  mutate(surface_total = ifelse(is.na(surface_total)== T,
                                str_extract(string = house_union$description,
                                            pattern = p21),
                                surface_total)
  )
house_union <- house_union %>% 
  mutate(surface_total = ifelse(is.na(surface_total)== T,
                                str_extract(string = house_union$description,
                                            pattern = p22),
                                surface_total)
  )
house_union <- house_union %>% 
  mutate(surface_total = ifelse(is.na(surface_total)== T,
                                str_extract(string = house_union$description,
                                            pattern = p23),
                                surface_total)
  )
house_union <- house_union %>% 
  mutate(surface_total = ifelse(is.na(surface_total)== T,
                                str_extract(string = house_union$description,
                                            pattern = p24),
                                surface_total)
  )
house_union <- house_union %>% 
  mutate(surface_total = ifelse(is.na(surface_total)== T,
                                str_extract(string = house_union$description,
                                            pattern = p25),
                                surface_total)
  )
house_union <- house_union %>% 
  mutate(surface_total = ifelse(is.na(surface_total)== T,
                                str_extract(string = house_union$description,
                                            pattern = p26),
                                surface_total)
  )
house_union <- house_union %>% 
  mutate(surface_total = ifelse(is.na(surface_total)== T,
                                str_extract(string = house_union$description,
                                            pattern = p27),
                                surface_total)
  )
house_union <- house_union %>% 
  mutate(surface_total = ifelse(is.na(surface_total)== T,
                                str_extract(string = house_union$description,
                                            pattern = p28),
                                surface_total)
  )
house_union <- house_union %>% 
  mutate(surface_total = ifelse(is.na(surface_total)== T,
                                str_extract(string = house_union$description,
                                            pattern = p29),
                                surface_total)
  )
house_union <- house_union %>% 
  mutate(surface_total = ifelse(is.na(surface_total)== T,
                                str_extract(string = house_union$description,
                                            pattern = p30),
                                surface_total)
  )
house_union <- house_union %>% 
  mutate(surface_total = ifelse(is.na(surface_total)== T,
                                str_extract(string = house_union$description,
                                            pattern = p31),
                                surface_total)
  )
house_union <- house_union %>% 
  mutate(surface_total = ifelse(is.na(surface_total)== T,
                                str_extract(string = house_union$description,
                                            pattern = p32),
                                surface_total)
  )
house_union <- house_union %>% 
  mutate(surface_total = ifelse(is.na(surface_total)== T,
                                str_extract(string = house_union$description,
                                            pattern = p33),
                                surface_total)
  )
house_union <- house_union %>% 
  mutate(surface_total = ifelse(is.na(surface_total)== T,
                                str_extract(string = house_union$description,
                                            pattern = p34),
                                surface_total)
  )
house_union <- house_union %>% 
  mutate(surface_total = ifelse(is.na(surface_total)== T,
                                str_extract(string = house_union$description,
                                            pattern = p35),
                                surface_total)
  )

table(is.na(house_union$surface_total)) ##42415   NAs (Se restacaron 47K)

# Imputacion baños:
table(is.na(house_union$bathrooms)) #34343  NAS
p1 = "[:space:]+[:digit:]+[:space:]+baños"
p2 = "[:space:]+[:digit:]+[:space:]+baños"
p3 = "[:space:]+[:digit:]+baños"
p4 = "[:space:]+[:digit:]+banos"
p5 = "[:space:]+con baño+[:space:]"
p6 = "[:space:]+un baño+[:space:]"
p7 = "[:space:]+dos baños+[:space:]"
p8 = "[:space:]+dos baños+[:punct:]"
p9 = "[:space:]+[:digit:]+[:space:]+baos"
house_union <- house_union %>% 
  mutate(bathrooms = ifelse(is.na(bathrooms)== T,
                            str_extract(string = house_union$description,
                                        pattern = p1),
                            bathrooms)
  )
house_union <- house_union %>% 
  mutate(bathrooms = ifelse(is.na(bathrooms)== T,
                            str_extract(string = house_union$description,
                                        pattern = p2),
                            bathrooms)
  )
house_union <- house_union %>% 
  mutate(bathrooms = ifelse(is.na(bathrooms)== T,
                            str_extract(string = house_union$description,
                                        pattern = p3),
                            bathrooms)
  )
house_union <- house_union %>% 
  mutate(bathrooms = ifelse(is.na(bathrooms)== T,
                            str_extract(string = house_union$description,
                                        pattern = p4),
                            bathrooms)
  )
house_union <- house_union %>% 
  mutate(bathrooms = ifelse(is.na(bathrooms)== T,
                            str_extract(string = house_union$description,
                                        pattern = p5),
                            bathrooms)
  )
house_union <- house_union %>% 
  mutate(bathrooms = ifelse(is.na(bathrooms)== T,
                            str_extract(string = house_union$description,
                                        pattern = p6),
                            bathrooms)
  )
house_union <- house_union %>% 
  mutate(bathrooms = ifelse(is.na(bathrooms)== T,
                            str_extract(string = house_union$description,
                                        pattern = p7),
                            bathrooms)
  )
house_union <- house_union %>% 
  mutate(bathrooms = ifelse(is.na(bathrooms)== T,
                            str_extract(string = house_union$description,
                                        pattern = p8),
                            bathrooms)
  )
house_union <- house_union %>% 
  mutate(bathrooms = ifelse(is.na(bathrooms)== T,
                            str_extract(string = house_union$description,
                                        pattern = p9),
                            bathrooms)
  )
table(is.na(house_union$bathrooms)) #16498 NAS

#-------------------------------- Creación de variables ----------------------#

# Crear variable tiene_terraza a partir de descripcion
p1 = "[:space:]+terraza+[:space:]"
p2 = "[:space:]+tiene terraza+[:space:]"
p3 = "[:space:]+terraza+[:punct:]"
p4 = "[:space:]+balcón+[:space:]"
p5 = "[:space:]+balcón terraza+[:space:]"
p6 = "[:space:]+balcón+[:punct:]"
p7 = "[:space:]+balcon+[:space:]"
p8 = "[:space:]+tiene balcon+[:space:]"
p9 = "[:space:]+balcon+[:punct:]"
p10 = "[:space:]+balcn+[:space:]"
p11 = "[:space:]+tiene balcn+[:space:]"
p12 = "[:space:]+balcn+[:punct:]"


house_union$tiene_terraza <- NA

house_union <- house_union %>% 
  mutate(tiene_terraza = ifelse(is.na(tiene_terraza)== T,
                                str_extract(string = house_union$description,
                                            pattern = p1),
                                tiene_terraza)
  )
house_union <- house_union %>% 
  mutate(tiene_terraza = ifelse(is.na(tiene_terraza)== T,
                                str_extract(string = house_union$description,
                                            pattern = p2),
                                tiene_terraza)
  )
house_union <- house_union %>% 
  mutate(tiene_terraza = ifelse(is.na(tiene_terraza)== T,
                                str_extract(string = house_union$description,
                                            pattern = p3),
                                tiene_terraza)
  )
house_union <- house_union %>% 
  mutate(tiene_terraza = ifelse(is.na(tiene_terraza)== T,
                                str_extract(string = house_union$description,
                                            pattern = p4),
                                tiene_terraza)
  )
house_union <- house_union %>% 
  mutate(tiene_terraza = ifelse(is.na(tiene_terraza)== T,
                                str_extract(string = house_union$description,
                                            pattern = p5),
                                tiene_terraza)
  )
house_union <- house_union %>% 
  mutate(tiene_terraza = ifelse(is.na(tiene_terraza)== T,
                                str_extract(string = house_union$description,
                                            pattern = p6),
                                tiene_terraza)
  )
house_union <- house_union %>% 
  mutate(tiene_terraza = ifelse(is.na(tiene_terraza)== T,
                                str_extract(string = house_union$description,
                                            pattern = p7),
                                tiene_terraza)
  )
house_union <- house_union %>% 
  mutate(tiene_terraza = ifelse(is.na(tiene_terraza)== T,
                                str_extract(string = house_union$description,
                                            pattern = p8),
                                tiene_terraza)
  )
house_union <- house_union %>% 
  mutate(tiene_terraza = ifelse(is.na(tiene_terraza)== T,
                                str_extract(string = house_union$description,
                                            pattern = p9),
                                tiene_terraza)
  )
house_union <- house_union %>% 
  mutate(tiene_terraza = ifelse(is.na(tiene_terraza)== T,
                                str_extract(string = house_union$description,
                                            pattern = p10),
                                tiene_terraza)
  )
house_union <- house_union %>% 
  mutate(tiene_terraza = ifelse(is.na(tiene_terraza)== T,
                                str_extract(string = house_union$description,
                                            pattern = p11),
                                tiene_terraza)
  )
house_union <- house_union %>% 
  mutate(tiene_terraza = ifelse(is.na(tiene_terraza)== T,
                                str_extract(string = house_union$description,
                                            pattern = p12),
                                tiene_terraza)
  )
table(is.na(house_union$tiene_terraza))
house_union$tiene_terraza[is.na(house_union$tiene_terraza)] <- 0

house_union <- house_union %>% 
  mutate(tiene_terraza = ifelse(tiene_terraza!=0,
                                1,
                                tiene_terraza)
  )

# Crear variable garaje a partir de descripcion
p1 = "[:space:]+garaje+[:space:]"
p2 = "[:punct:]+garaje"
p3 = "[:punct:]+parqueadero[:space:]"
p4 = "[:space:]+parqueadero+[:space:]"
p5 = "[:punct:]+parqueadero"
p6 = "[:punct:]+deposito+[:space:]"
p7 = "[:space:]+deposito+[:space:]"
p8 = "[:punct:]+deposito"
p9 = "[:punct:]+parqueaderos[:space:]"
p10 = "[:space:]+parqueaderos+[:space:]"
p11 = "[:punct:]+parqueaderos"
p12 = "[:space:]+garajes+[:space:]"
p12 = "[:punct:]+garajes"

house_union$garaje <- NA

house_union <- house_union %>% 
  mutate(garaje = ifelse(is.na(garaje)== T,
                         str_extract(string = house_union$description,
                                     pattern = p1),
                         garaje)
  )
house_union <- house_union %>% 
  mutate(garaje = ifelse(is.na(garaje)== T,
                         str_extract(string = house_union$description,
                                     pattern = p2),
                         garaje)
  )
house_union <- house_union %>% 
  mutate(garaje = ifelse(is.na(garaje)== T,
                         str_extract(string = house_union$description,
                                     pattern = p3),
                         garaje)
  )
house_union <- house_union %>% 
  mutate(garaje = ifelse(is.na(garaje)== T,
                         str_extract(string = house_union$description,
                                     pattern = p4),
                         garaje)
  )
house_union <- house_union %>% 
  mutate(garaje = ifelse(is.na(garaje)== T,
                         str_extract(string = house_union$description,
                                     pattern = p5),
                         garaje)
  )
house_union <- house_union %>% 
  mutate(garaje = ifelse(is.na(garaje)== T,
                         str_extract(string = house_union$description,
                                     pattern = p6),
                         garaje)
  )
house_union <- house_union %>% 
  mutate(garaje = ifelse(is.na(garaje)== T,
                         str_extract(string = house_union$description,
                                     pattern = p6),
                         garaje)
  )
house_union <- house_union %>% 
  mutate(garaje = ifelse(is.na(garaje)== T,
                         str_extract(string = house_union$description,
                                     pattern = p7),
                         garaje)
  )
house_union <- house_union %>% 
  mutate(garaje = ifelse(is.na(garaje)== T,
                         str_extract(string = house_union$description,
                                     pattern = p8),
                         garaje)
  )
house_union <- house_union %>% 
  mutate(garaje = ifelse(is.na(garaje)== T,
                         str_extract(string = house_union$description,
                                     pattern = p9),
                         garaje)
  )
house_union <- house_union %>% 
  mutate(garaje = ifelse(is.na(garaje)== T,
                         str_extract(string = house_union$description,
                                     pattern = p10),
                         garaje)
  )
house_union <- house_union %>% 
  mutate(garaje = ifelse(is.na(garaje)== T,
                         str_extract(string = house_union$description,
                                     pattern = p11),
                         garaje)
  )
house_union <- house_union %>% 
  mutate(garaje = ifelse(is.na(garaje)== T,
                         str_extract(string = house_union$description,
                                     pattern = p12),
                         garaje)
  )
house_union <- house_union %>% 
  mutate(garaje = ifelse(is.na(garaje)== T,
                         str_extract(string = house_union$description,
                                     pattern = p13),
                         garaje)
  )

# Crear variable tiene_garaje
table(is.na(house_union$garaje))
house_union$garaje[is.na(house_union$garaje)] <- 0

house_union <- house_union %>% 
  mutate(garaje = ifelse(garaje!=0,
                         1,
                         garaje)
  ) %>%
  mutate(tiene_garaje=garaje)
house_union <- house_union %>% select(-ad_type, -start_date, -end_date,
                                      -created_on, -l1, -l2, -surface_covered
                                      ,-currency,-title, -description
                                      ,-operation_type,-garaje)

# Arreglar formatos de variables imputadas
# Surface
house_union <- house_union %>% 
  mutate(surface_total = gsub('m2', '', house_union$surface_total))
house_union <- house_union %>% 
  mutate(surface_total = gsub('mts2', '', house_union$surface_total))
house_union <- house_union %>% 
  mutate(surface_total = gsub('mts 2', '', house_union$surface_total))
house_union <- house_union %>% 
  mutate(surface_total = gsub('metros', '', house_union$surface_total))
house_union <- house_union %>% 
  mutate(surface_total = gsub('mt2', '', house_union$surface_total))
house_union <- house_union %>% 
  mutate(surface_total = gsub('mt', '', house_union$surface_total))
house_union <- house_union %>% 
  mutate(surface_total = gsub(',', '.', house_union$surface_total))

surface_total_2 <- regmatches(house_union$surface_total, 
                              gregexpr(paste0("[[:digit:]]+","|","[[:digit:]]+[[:punct:]]+[[:digit:]]+"), house_union$surface_total))
house_union$surface <- surface_total_2

house_union <- house_union %>% 
  mutate(surface = ifelse(surface == 'character(0)',
                          NA,
                          surface)
  )
house_union <- house_union %>% select(-surface_total)
house_union$surface <- as.numeric(house_union$surface)

table(is.na(house_union$surface))

# Baños
house_union <- house_union %>% 
  mutate(bathrooms = gsub('con baño', '1', house_union$bathrooms))
house_union <- house_union %>% 
  mutate(bathrooms = gsub('un baño', '1', house_union$bathrooms))
house_union <- house_union %>% 
  mutate(bathrooms = gsub('dos baños', '2', house_union$bathrooms))

bathrooms_2 <- regmatches(house_union$bathrooms, 
                          gregexpr("[[:digit:]]+", house_union$bathrooms))

house_union$bathrooms_2 <- bathrooms_2

house_union <- house_union %>% 
  mutate(n_bathrooms = ifelse(bathrooms_2 == 'character(0)',
                              NA,
                              bathrooms_2)
  )
house_union <- house_union %>% select(-bathrooms_2, -bathrooms)
house_union$n_bathrooms <- as.numeric(house_union$n_bathrooms)

#cambiar NAs en baños por 1 (asumiendo que todos los apartamentos tienen al menos 1 baño)

pob_house <- pob_house%>% 
  mutate(n_bathrooms = ifelse(is.na(n_bathrooms )== T,
                              1,
                              n_bathrooms )
  )

house_med <- pob_house%>% 
  mutate(n_bathrooms = ifelse(is.na(n_bathrooms )== T,
                              1,
                              n_bathrooms )
  )

pob_house_mnz <- pob_house_mnz%>% 
  mutate(n_bathrooms = ifelse(is.na(n_bathrooms )== T,
                              1,
                              n_bathrooms )
  )

pob_house_mnz <- pob_house_mnz%>% 
  mutate(n_bathrooms = ifelse(is.na(n_bathrooms )== T,
                              1,
                              n_bathrooms )
  )

# Dicotomizar tipo de propiedad
house_union <- house_union %>% 
  mutate(es_casa = ifelse(property_type == 'Casa',
                          1,
                          0)
  )
house_union <- house_union %>% select(-property_type)

# Separar bogotá y medellín

house_bog <- house_union %>% filter(l3 == 'Bogotá D.C')
house_med <- house_union %>% filter(l3 == 'Medellín')

# Separar test y train
house_test <- house_union %>% filter(base == 'TEST')
house_train <- house_union %>% filter(base == 'TRAIN')

house_test <- house_test %>% select(-base)
house_train <- house_train %>% select(-base)

house_train$price <- house_train_price

house_test_bog <- house_test %>% filter(l3 == 'Bogotá D.C')
house_test_med <- house_test %>% filter(l3 == 'Medellín')

house_train_bog <- house_train %>% filter(l3 == 'Bogotá D.C')
house_train_med <- house_train %>% filter(l3 == 'Medellín')

house_test_bog <- house_test_bog %>% select(-l3)
house_test_med <- house_test_med %>% select(-l3)
house_train_bog <- house_train_bog %>% select(-l3)
house_train_med <- house_train_med %>% select(-l3)


# Exportar bog y med train y test
write.csv(house_test_bog ,"house_test_bog")
write.csv(house_test_med ,"house_test_med")
write.csv(house_train_bog ,"house_train_bog")
write.csv(house_train_med ,"house_train_med")

# Exportar train y test
write.csv(house_test ,"house_test")
write.csv(house_train ,"house_train")

#------ Datos externos ---------

#Importar datos de manzanas en Bogotá y Medellín

#mnz_bog = st_read("MGN_URB_MANZANA.shp") #importar archivo con capa de manzanas

#setwd('C:/Users/Sofia/Documents/2022-2/BigData/PS3_Gomez-Ortiz-Vanegas-/stores') #direccionar a carpeta descargada
#saveRDS(mnz_bog, "mnz_bog.rds")

#* Se borró el archivo pesado y se dejó el archivo rds

mnz_bog = import("C:/Users/Sofia/Documents/2022-2/BigData/PS3_Gomez-Ortiz-Vanegas-/stores/mnz_bog.rds")
colnames(mnz_bog)

#setwd('C:/Users/Sofia/Documents/2022-2/BigData/PS3_Gomez-Ortiz-Vanegas-/stores/05_ANTIOQUIA/URBANO') #direccionar a carpeta descargada

#mnz_med = st_read("MGN_URB_MANZANA.shp") #importar archivo con capa de manzanas (ya no se usa este sino su verión liviana)


##exportar manzanas medellín en formato más liviano - usamos ese de ahí en adelante

#setwd('C:/Users/Sofia/Documents/2022-2/BigData/PS3_Gomez-Ortiz-Vanegas-/stores') #direccionar a carpeta descargada
#saveRDS(mnz_med, "mnz_med.rds")

#* Se borró el archivo pesado y se dejó el archivo rds

mnz_med = import("C:/Users/Sofia/Documents/2022-2/BigData/PS3_Gomez-Ortiz-Vanegas-/stores/mnz_med.rds")
colnames(mnz_med)

#definir polígono de chapinero
chapinero <- getbb(place_name = "UPZ Chapinero, Bogota", 
                   featuretype = "boundary:administrative", 
                   format_out = "sf_polygon") %>% .$multipolygon

#definir polígono de El Poblado
poblado <- getbb(place_name = "Comuna 14 - El Poblado",
                 featuretype = "boundary:administrative",
                 format_out = "sf_polygon")

#definir polígono de Medellín
medellin <- getbb(place_name = "Medellín",
                 featuretype = "boundary:administrative",
                 format_out = "sf_polygon")

# limitar datos de manzanas al área de interés

#uniformar

house_train_bog = st_transform(house_train_bog, crs=4326)
house_test_bog = st_transform(house_test_bog, crs=4326)
house_train_med = st_transform(house_train_med, crs=4326)
house_test_med = st_transform(house_test_med, crs=4326)


#Manzanas y apartamentos limitados a chapinero y el poblado:

#chapinero

chapi_train <- house_train_bog[chapinero,]
chapi_test <- house_test_bog[chapinero,]
chapi_mnz <- mnz_bog[chapinero,]

leaflet() %>% addTiles() %>% addPolygons (data = chapinero, color="red")  %>% addCircles (data = chapi_test, color="green") %>% addCircles (data = chapi_train, color="purple")

#El Poblado - para medellín casi todos los datos de train se encuentran fuera del poblado, así que se usarán los datos de toda la ciudad

## Primera variable para bogotá y medellín: Crear variable "distancia mínima a las estaciones de transporte masivo"
## a partir de información externa (OSM)

# Importar datos de estaciones de transporte masivo en el área de interés 

bus_bog = opq(bbox = st_bbox(chapi_mnz)) %>%
  add_osm_feature(key = "amenity", value = "bus_station") %>%
  osmdata_sf() %>% .$osm_points %>% select(osm_id,name)

bus_ant = opq(bbox = st_bbox(mnz_med)) %>%
  add_osm_feature(key = "amenity", value = "bus_station") %>%
  osmdata_sf() %>% .$osm_points %>% select(osm_id,name)

bus_med <- bus_ant[medellin,]

# Medir distancia de los apartamentos a las estaciones de transporte

dist_med_bus_train = st_distance(x = house_train_med, y = bus_med)
dist_med_bus_test = st_distance(x = house_test_med, y = bus_med)

dist_chapi_bus_train = st_distance(x = chapi_train, y = bus_bog)
dist_chapi_bus_test = st_distance(x = chapi_test, y = bus_bog)

# Medir la mínima distancia de los apartamentos a las estaciones de transporte

min_med_bus_train = apply(dist_med_bus_train , 1 , min)

min_med_bus_test = apply(dist_med_bus_test , 1 , min)

min__chapi_bus_train = apply(dist_chapi_bus_train , 1 , min)

min__chapi_bus_test = apply(dist_chapi_bus_test , 1 , min)

house_train_med$dist_med_bus = min_med_bus_train #juntar a bases disponibles

house_test_med$dist_med_bus = min_med_bus_test

chapi_train$dist_chapi_bus = min__chapi_bus_train

chapi_test$dist_chapi_bus = min__chapi_bus_test

## Segunda variable para bogotá: distancia mínima a los cerros orientales

osm1 = opq(bbox = getbb("Bogota")) %>%
  add_osm_feature(key="natural" , value="peak")  
class(osm1)
osm1_sf = osm1 %>% osmdata_sf()
osm1_sf

chapi_east = osm1_sf$osm_points %>% select(osm_id)

#distancia a los cerros

dist_east_train = st_distance(x = chapi_train, y = chapi_east)

dist_east_test = st_distance(x = chapi_test, y = chapi_east)

#distancia mínima

min_east_train = apply(dist_east_train , 1 , min)

min_east_test = apply(dist_east_test , 1 , min)

#juntar a bases disponibles (aptos y aptos + manzanas)

chapi_train$dist_east = min_east_train

chapi_test$dist_east = min_east_test

## Segunda variable para medellín: distancia mínima al campo de golf

osm2 = opq(bbox = getbb("Medellín")) %>%
  add_osm_feature(key="leisure" , value="golf_course") 
class(osm2)
osm2_sf = osm2 %>% osmdata_sf()
osm2_sf

pob_golf = osm2_sf$osm_polygons %>% select(osm_id)

dist_golf_train = st_distance(x = house_train_med, y = pob_golf)

dist_golf_test = st_distance(x = house_test_med, y = pob_golf)

#distancia mínima

min_golf_train = apply(dist_golf_train , 1 , min)

min_golf_test = apply(dist_golf_test , 1 , min)

#juntar a bases disponibles

house_train_med$dist_golf = min_golf_train

house_test_med$dist_golf = min_golf_test

#GRÁFICAS PARA DOCUMENTO (MOSTRANDO VARIABLES ESPACIALES DISPONIBLES)

#Bogotá

library (sp)

column1 <- cbind(c(4.6947,4.6201,4.6174,4.6805, 4.6947))
column2 <- cbind(c(-74.0664,-74.0803,-74.0058,-73.9917, -74.0664))

coords <- cbind(column2,column1)

oriente_bogota <- st_polygon(list(coords))

leaflet() %>% addTiles() %>% addPolygons (data = oriente_bogota, color="red")  %>% addPolygons (data = chapinero, color="green") 

leaflet() %>% addTiles() %>% addPolygons (data = chapinero, color="red")  %>% addCircles (data = house_bog, color="green") 


pob_cerros <- chapi_east[oriente_bogota,]

ggplot()+
  geom_sf(data=pob_house, col = "black")+
  geom_sf(data=bus_pob, col ="blue")+
  geom_sf(data=pob_cerros, color="red")+
  theme_bw()+
  theme(axis.title=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size=6))

#Medellín
ggplot()+
  geom_sf(data=house_med, col = "black")+
  geom_sf(data=bus_med, col ="blue")+
  geom_sf(data=pob_golf, color="red")+
  theme_bw()+
  theme(axis.title=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size=6))

#-------------------- Modelo de predicción ------------------------------------

# ajustar formato price

chapi_east <- as.data.frame(chapi_train)

#st_geometry(chapi_train) = NULL 
#st_geometry(house_train_med) = NULL

#st_geometry(chapi_train$price) = NULL 
#st_geometry(house_train_med$price) = NULL 

# Se van a analizar dos formas funcionales:

chapi_train <- as.data.frame(rasterToPoints(chapi_train,spatial = TRUE))

# Forma lineal

chapi_1 <- chapi_train$price ~ chapi_train$dist_east+chapi_train$dist_chapi_bus + chapi_train$bedrooms + chapi_train$tiene_terraza + chapi_train$tiene_garaje+ chapi_train$surface
med_1 <- house_train_med$price ~ house_train_med$dist_golf + house_train_med$dist_med_bus + house_train_med$bedrooms + house_train_med$tiene_terraza + house_train_med$tiene_garaje+ house_train_med$surface

# Forma cuadrática

chapi_train$dist_chapi_bus2 <- chapi_train$dist_east + chapi_train$dist_chapi_bus*chapi_train$dist_chapi_bus

chapi_train$dist_east2 <- chapi_train$dist_east*chapi_train$dist_east

chapi_train$surface2 <- chapi_train$surface*chapi_train$surface


house_train_med$dist_med_bus2 <- house_train_med$dist_med_bus*house_train_med$dist_med_bus

house_train_med$dist_golf2 <- house_train_med$dist_golf*house_train_med$dist_golf

house_train_med$surface2 <- house_train_med$surface*house_train_med$surface


chapi_2 <- chapi_train$price ~ chapi_train$dist_east2 + chapi_train$dist_chapi_bus2 + chapi_train$bedrooms + chapi_train$tiene_terraza + chapi_train$tiene_garaje+ chapi_train$surface2
med_2 <- house_train_med$price ~ house_train_med$dist_golf2 + house_train_med$dist_med_bus2 + house_train_med$bedrooms + house_train_med$tiene_terraza + house_train_med$tiene_garaje+ house_train_med$surface2

## chapinero

# Lineal

chapi_train$price <-unlist(chapi_train$price)

lm_chapi1 <- lm(chapi_1, data=chapi_train)
pred_ols_chapi1 <- predict(lm_chapi1)


lm_chapi2 <- lm(chapi_2, data=chapi_train)
pred_ols_chapi2 <- predict(lm_chapi2)

# Random Forest - se utilizó rpart function para lidiar con los NAs

library(rpart)

rf_chapi1 <- rpart(chapi_1, data=chapi_train)
summary(rf_chapi1)

rf_chapi2 <- rpart(chapi_2, data=chapi_train)
summary(rf_chapi2)

# XGBoost

install.packages('xgboost')
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)

chapi_matriz_xg <- chapi_train[,c(1,3,4,5,6,9,10,11,12,13,14)]

chapi_matriz_xg$identificador <- 1:13473

chapi_matriz_xg$terraza = chapi_matriz_xg[,"tiene_terraza"] == "1"
chapi_matriz_xg$hay_terraza <- 0
chapi_matriz_xg$hay_terraza[chapi_matriz_xg$terraza == "TRUE"] <- 1

chapi_matriz_xg$garaje = chapi_matriz_xg[,"tiene_garaje"] == "1"
chapi_matriz_xg$hay_garaje <- 0
chapi_matriz_xg$hay_garaje[chapi_matriz_xg$garaje == "TRUE"] <- 1

chapi_matriz1_xg <- chapi_matriz_xg[,c(2,5,6,7,8,12,14,16)]

chapi_matriz1_xg <- as.matrix(chapi_matriz1_xg)

#numeric_matrix_chapi <- chapi_matriz1_xg
#write.csv(numeric_matrix_chapi,"numeric_matrix_chapi")

chapi_matriz2_xg <- chapi_matriz_xg[,c(2,5,6,9,10,11,12,14,16)]
chapi_matriz2_xg <- as.matrix(chapi_matriz2_xg)

require(Matrix)
require(data.table)


xgb_chapi1 <- xgboost(data = chapi_matriz1_xg[,c(1,2,3,4,5,7,8)], label = chapi_train$price, max.depth = 6, eta = 0.5, nrounds = 100, objective = "reg:squarederror")
xgb_chapi2 <- xgboost(data = chapi_matriz2_xg[,c(1,2,3,4,5,6,8)], label = chapi_train$price, max.depth = 6, eta = 0.5, nrounds = 100, objective = "reg:squarederror")

## Poblado

# Lineal

house_train_med$price <-unlist(house_train_med$price)

lm_med1 <- lm(med_1, data=house_train_med)
pred_ols_med1 <- predict(lm_med1)

lm_med2 <- lm(med_2, data=house_train_med)
pred_ols_med2 <- predict(lm_med2)

# Random Forest

rf_med1 <- rpart(med_1, data=house_train_med)
summary(rf_med1)

rf_med2 <- rpart(med_2, data=house_train_med)
summary(rf_med2)

# XGBoost

med_matriz_xg <- house_train_med[,c(1,3,4,5,6,9,10,11,12,13,14)]

med_matriz_xg$identificador <- 1:21356

med_matriz_xg$terraza = med_matriz_xg[,"tiene_terraza"] == "1"
med_matriz_xg$hay_terraza <- 0
med_matriz_xg$hay_terraza[med_matriz_xg$terraza == "TRUE"] <- 1

med_matriz_xg$garaje = med_matriz_xg[,"tiene_garaje"] == "1"
med_matriz_xg$hay_garaje <- 0
med_matriz_xg$hay_garaje[med_matriz_xg$garaje == "TRUE"] <- 1

med_matriz1_xg <- med_matriz_xg[,c(2,5,6,7,8,12,14,16)]
med_matriz1_xg <- as.matrix(med_matriz1_xg)

med_matriz2_xg <- med_matriz_xg[,c(2,5,6,9,10,11,12,14,16)]
med_matriz2_xg <- as.matrix(med_matriz2_xg)

xgb_pob1 <- xgboost(data = med_matriz1_xg[,c(1,2,3,4,5,7,8)], label = house_train_med$price, max.depth = 6, eta = 0.5, nrounds = 100, objective = "reg:squarederror")
xgb_pob2 <- xgboost(data = med_matriz2_xg[,c(1,2,3,4,5,6,8)], label = house_train_med$price, max.depth = 6, eta = 0.5, nrounds = 100, objective = "reg:squarederror")

#falta hacer lo que toca hacer con test

#-------- Superlearners --------------

install.packages("SuperLearner")
require("SuperLearner")

base1_capi <- as.data.frame(chapi_matriz_xg)

fitprice_chapi <- SuperLearner(Y=base1_capi$price, X= data.frame(base1_capi$dist_east+base1_capi$dist_chapi_bus+base1_capi$hay_terraza+base1_capi$hay_garaje+base1_capi$bedrooms+base1_capi$surface), #tal vez acá son comas
                  method = "method.NNLS", SL.library = c("SL.lm","SL.rpart","SL.xgboost"))

