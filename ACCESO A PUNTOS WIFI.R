#LIBRERIAS (instalación y carga)
libraries <- c("tidyverse",
               "readr", 
               "sf")

installed_packages <- rownames(installed.packages())
libraries_to_install <- setdiff(libraries, installed_packages)

if (length(libraries_to_install) > 0) {
  install.packages(libraries_to_install)}

lapply(libraries, library, character.only = TRUE)

#COLONIAS----
#https://datos.cdmx.gob.mx/dataset/catalogo-de-colonias-datos-abiertos
url <- "https://datos.cdmx.gob.mx/dataset/d8f83ce7-163d-4c2a-96e0-ae38d304c4a0/resource/e3bbadb4-f3de-4c52-b3f4-a4ffea4466a3/download/colonias_iecm_2022.zip"

options(timeout = 800)
temp_zip <- tempfile(fileext = ".zip")                                                 
temp_dir <- tempdir()                                                                   

download.file(url, temp_zip, mode = "wb")                                              
  

unzip(temp_zip, files = related_files, exdir = temp_dir)    

related_files <- paste0("colonias_iecm_2022/colonias_iecm2022_", c(".shp", ".shx", ".dbf", ".prj"))   

shp_file_path <- file.path(temp_dir, related_files[1])                                  

colonias <- st_read(shp_file_path, quiet = TRUE)|>                                     
   st_transform(crs = 32614)

total_colonias <- nrow(colonias)
#PUNTOS WIFI----
#https://datos.cdmx.gob.mx/dataset/puntos-de-acceso-wifi-en-la-ciudad-de-mexico
puntos_wifi <- read.csv("https://datos.cdmx.gob.mx/dataset/aa2ff336-b4aa-44f3-b38a-f303ef0f7673/resource/98f51fe2-18cb-4f50-a989-b9f81a2b5a76/download/2024-06-30-puntos_de_acceso_wifi.csv", 
                        header = TRUE, 
                        sep = ",", 
                        encoding = "UTF-8")|>
  mutate(validas = case_when(longitud > 0 |longitud > -90 | longitud < -100 | is.na(longitud) ~ "no valida", TRUE ~ "valida"), 
         longitud = case_when(longitud < 0 & longitud > -100 ~ longitud, 
                              longitud > 20 ~  longitud*-1,                   
                              longitud < 0 & longitud < -100 ~ longitud / 1000000))

puntos_no_validos<-puntos_wifi|>
  filter(validas == "no valida")

puntos_validos<-puntos_wifi|>
  filter(validas == "valida")|>
  st_as_sf(coords = c("longitud", "latitud"), crs = 4326)|>
  st_transform(32614)|>
  mutate(id_punto = row_number())|>
  st_intersection(colonias)|>
  group_by(id_punto)|>  
  slice(1)|>
  ungroup()|>
  st_drop_geometry()|>
  as_tibble()

puntos_colonias <-puntos_validos|>
  group_by(CVEUT)|>
  summarise(cuenta = n())

total_colonias_con_punto <- nrow(puntos_colonias)


porcentaje_colonias_wifi <-  total_colonias_con_punto*100/total_colonias

porcentaje_colonias_wifi               





