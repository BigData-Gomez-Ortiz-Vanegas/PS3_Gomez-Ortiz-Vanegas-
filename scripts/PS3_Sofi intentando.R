## Problem Set 3 - Spatial Data
# Isa: setwd('C:/Users/USER/OneDrive - Universidad de los Andes/Escritorio/BigData/PS1/PS3_Gomez-Ortiz-Vanegas-/stores')
# Sofi:
setwd('C:/Users/Sofia/Documents/2022-2/BigData/PS3_Gomez-Ortiz-Vanegas-/stores')

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


house_train_price <- house_train %>% select(price, property_id)
house_train <- house_train %>% select(-price)


house_test <- house_test %>% mutate(base = 'TEST')
house_train <- house_train %>% mutate(base = 'TRAIN')

house_union <- union_all(house_test, house_train)

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

# Encontrar patrones para completar variable de superficie
#house_union$description

p1 = "[:sp_ace:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+m2"
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

table(is.na(house_union$surface_total)) ##42981   NAs (Se restacaron 47K)

#importar capas de manzanas para bogotá y medellín -> se descargó y de exportó una versión liviana con la que se trabaja de ahora en adelante (disponibles en stores)

#importar manzanas bogotá y exportar como .rds
#setwd('C:/Users/Sofia/Documents/2022-2/BigData/PS3_Gomez-Ortiz-Vanegas-/stores/11_BOGOTA/URBANO') #direccionar a carpeta descargada

#mnz_bog = st_read("MGN_URB_MANZANA.shp") #importar archivo con capa de manzanas

#setwd('C:/Users/Sofia/Documents/2022-2/BigData/PS3_Gomez-Ortiz-Vanegas-/stores') #direccionar a carpeta descargada
#saveRDS(mnz_bog, "mnz_bog.rds")

#* Se borró el archivo pesado y se dejó el archivo rds

mnz_bog = import("C:/Users/Sofia/Documents/2022-2/BigData/PS3_Gomez-Ortiz-Vanegas-/stores/mnz_bog.rds")
colnames(mnz_bog)
#leaflet() %>% addTiles() %>% addPolygons(data=mnz_bog , color="red") #mostrar capa de manzanas


#importar manzanas medellín y exportar como .rds

#setwd('C:/Users/Sofia/Documents/2022-2/BigData/PS3_Gomez-Ortiz-Vanegas-/stores/05_ANTIOQUIA/URBANO') #direccionar a carpeta descargada

#mnz_med = st_read("MGN_URB_MANZANA.shp") #importar archivo con capa de manzanas (ya no se usa este sino su verión liviana)


##exportar manzanas medellín en formato más liviano - usamos ese de ahí en adelante
#setwd('C:/Users/Sofia/Documents/2022-2/BigData/PS3_Gomez-Ortiz-Vanegas-/stores') #direccionar a carpeta descargada
#saveRDS(mnz_med, "mnz_med.rds")

#* Se borró el archivo pesado y se dejó el archivo rds

mnz_med = import("C:/Users/Sofia/Documents/2022-2/BigData/PS3_Gomez-Ortiz-Vanegas-/stores/mnz_med.rds")
colnames(mnz_med)
#leaflet() %>% addTiles() %>% addPolygons(data=mnz_med , color="red")#mostrar capa de manzanas

#uniformar

house_union_bog = st_transform(house_union, crs=st_crs(mnz_bog))
house_union_med = st_transform(house_union, crs=st_crs(mnz_med))

#Aquí me parece mejor dejarlas separadas para juntar medellin y bogotá

house_mnz_bog = st_join(x = house_union_bog,y = mnz_bog)

sf_use_s2(FALSE)
house_mnz_med = st_join(x = house_union_med,y = mnz_med)

#definir polígono de chapinero
chapinero <- getbb(place_name = "UPZ Chapinero, Bogota", 
                   featuretype = "boundary:administrative", 
                   format_out = "sf_polygon") %>% .$multipolygon
leaflet() %>% addTiles() %>% addPolygons(data=chapinero)

#gráfica bogotá
leaflet() %>% addTiles() %>% addPolygons(data=mnz_bog , color="red")%>% addCircles(data=house_union)

#solo chapinero
chapi_house_mnz <- house_mnz_bog[chapinero,]

chapi_mnz <- mnz_bog[chapinero,]

chapi_house <- house_union_bog[chapinero,]

leaflet() %>% addTiles() %>% addPolygons(data = chapi_mnz , color="red")%>% addCircles(data = chapi_house)

#definir polígono del poblado
poblado <- getbb(place_name = "Comuna 14 - El Poblado",
                 featuretype = "boundary:administrative",
                 format_out = "sf_polygon")
leaflet() %>% addTiles() %>% addPolygons(data=poblado)

pob_house_mnz <- house_mnz_med[poblado,]

pob_mnz <- mnz_med[poblado,]

pob_house <- house_union_med[poblado,]

leaflet() %>% addTiles() %>% addPolygons(data = pob_mnz , color="red")%>% addCircles(data = pob_house)

