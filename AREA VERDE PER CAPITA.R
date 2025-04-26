#LIBRERIAS----
libraries <- c("tidyverse",
               "sf",  
               "DBI", 
               "RSQLite")

installed_packages <- rownames(installed.packages())
libraries_to_install <- setdiff(libraries, installed_packages)

if (length(libraries_to_install) > 0) {
  install.packages(libraries_to_install)
}

lapply(libraries, library, character.only = TRUE)


#DIRECTORIO PARA DATOS BÁSICOS ----
directorio_base <-"D:/BASES Y DATOS GENERALES"
#DIRECTORIO PARA ESPACIO PUBLICO----
directorio <- "C:/Users/brenp/Desktop/PGOTDU_CDMX/ESPACIO PUBLICO"
setwd(directorio)

##Áreas verdes 2017-2018 de la Ciudad de México----
url <- "http://www.conabio.gob.mx/informacion/gis/maps/ccl/avcdmxgw_c.zip"
options(timeout = 5000)
download.file(url, 'avcdmxgw_c.zip', mode = "wb")

zip::unzip("avcdmxgw_c.zip", exdir = "avcdmx_1718")

acvdmx_gdbs<-st_read("avcdmx_1718/avcdmx_1718.gdb")|>
  select(cobertura %in% c("Arbolado", "Arbustivo", "Herbáceo"))|>
  mutate(area_tipo= "area verde",)|>
  group_by(area_tipo)|>
  summarise(area_total = sum(Shape_Area))|>
  mutate(CVEGEO == "09")

RESAGEBURB_09XLSX20<- readxl::read_excel("https://www.inegi.org.mx/contenidos/programas/ccpv/2020/tabulados/cpv2020_b_eum_01_poblacion.xlsx"))|>
  mutate(CVEGEO = paste0(ENTIDAD),
         CVE_MUNI = paste0(ENTIDAD, MUN))|>
  mutate_all(~ gsub("\\*", "", .)) |> 
  mutate(across(9:230, ~ as.numeric(., na.rm = TRUE)))|>
  filter(NOM_MUN == "Total de la entidad Ciudad de México")|>
  select(CVEGEO, POBTOT)

limite<-limite|>
  left_join(RESAGEBURB_09XLSX20, by = "CVEGEO")


