## Problem Set 3 - Spatial Data
setwd('C:/Users/USER/OneDrive - Universidad de los Andes/Escritorio/BigData/PS1/PS3_Gomez-Ortiz-Vanegas-/stores')
rm(list=ls())
# importar librerías
## Llamar pacman (contiene la función p_load)
require(pacman)

## Llama/instala-llama las librerías listadas
p_load(tidyverse,rio,
       sf, # Leer/escribir/manipular datos espaciales
       leaflet, # Visualizaciones dinámicas
       tmaptools, # geocode_OSM()
       osmdata, # Get OSM's data
       skimr)

# Base de datos de entrenamiento y de prueba
house_train = import("train.Rds")
house_train = st_as_sf(house_train, coords=c("lon", "lat"), crs=4236)

house_test = import("test.Rds")
house_test = st_as_sf(house_test, coords=c("lon", "lat"), crs=4236)


house_train_price <- house_train %>% select(price)
house_train <- house_train %>% select(-price)


house_test <- house_test %>% mutate(base = 'TEST')
house_train <- house_train %>% mutate(base = 'TRAIN')

house_union <- union_all(house_test, house_train)

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

# Dicotomizar tipo de propiedad
house_union <- house_union %>% 
  mutate(es_casa = ifelse(property_type == 'Casa',
                          1,
                          0)
  )
house_union <- house_union %>% select(-property_type)

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
