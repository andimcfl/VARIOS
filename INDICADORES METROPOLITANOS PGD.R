##########INDICADORES METROPOLITANOS PLAN GENERAL DE DESARROLLO DE LA CIUDAD DE MÉXICO############
##LIBRERIAS----
libraries <- c("tidyverse",
               "sf", 
               "readr")
installed_packages <- rownames(installed.packages())
libraries_to_install <- setdiff(libraries, installed_packages)

if (length(libraries_to_install) > 0) {
  install.packages(libraries_to_install)
}

lapply(libraries, library, character.only = TRUE)

##LISTA DE MUNICIPIOS QUE INTEGRAN LA ZONA METROPOLITANA DEL VALLE DE MÉXICO
##SISTEMA URBANO NACIONAL, 2018 DELIMITACIÓN VALLE DE MÉXICO
sun_2018<- read.csv("http://www.conapo.gob.mx/work/models/CONAPO/Marginacion/Datos_Abiertos/SUN/Base_SUN_2018.csv", header = TRUE, sep = ",",
                    fileEncoding = "", encoding = "latin1")|>
  filter(NOM_SUN == "Valle de México")|>
  mutate(CVEGEO = str_pad(CVE_MUN, width = 5, side = "left", pad = "0"), 
         CVE_ENT = str_pad(CVE_ENT, width = 2, side = "left", pad = "0"))|>
  select(CVE_ENT, CVEGEO,NOM_SUN)

##SISTEMA URBANO NACIONAL, 2020 DELIMITACIÓN CIUDAD DE MÉXICO
url <- "https://conapo.segob.gob.mx/work/models/CONAPO/Datos_Abiertos/DT/Metropolis.rar"
temp_zip <- tempfile(fileext = "Metropolis.rar")
temp_dir <- tempdir()
options(timeout = 800)
download.file(url, temp_zip, mode = "wb")


related_files <- paste0("Metropolis/Metropolis_2020", c(".shp", ".shx", ".dbf", ".prj"))

unzip(temp_zip, files = related_files, exdir = temp_dir)

shp_file_path <- file.path(temp_dir, related_files[1])

sun_2020<- st_read(shp_file_path, quiet = TRUE)|>
  st_transform(crs = 32614)|>
  filter(NOM_MET == "Ciudad de Moxico")|>
  select(CVEGEO, NOM_MET)|>
  st_drop_geometry()|>
  as.data.frame()

mun_zm<-sun_2018|>
  full_join(sun_2020, by = "CVEGEO")|>
  mutate(NOM_MET = "Ciudad de México")


##CENSO DE GOBIERNOS PARA ANALISIS METROPOLITANO (2011-2023)




