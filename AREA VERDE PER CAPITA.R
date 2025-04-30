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

##################################### CONSIDERACIONES GENERALES DEL ÁREA VERDE DEFINIDA ###########################################
#         La definición del área verde, parte la Ley ambiental de la Ciudad de México publicada en la gaceta oficial de           # 
#         la Ciudad de México el 18 de julio del 2024                                                                             #
#         que en su articulo 4, fraccion VIII, establece como Áreas Verdes a "Toda superficie cubierta de vegetación,             # 
#         natural o inducida que se localice en la Ciudad de México"                                                              #
#         Fuente: CONABIO, (2024). 'Áreas verdes 2017-2018 de la Ciudad de México', escala: 1:4000. edición: 1.                   # 
#         Comisión Nacional para el Conocimiento y Uso de la Biodiversidad. Proyecto Biodiversidad Urbana. Ciudad de México.      # 
#         Vigencia: 2020-2023                                                                                                     # 
#         Mantenimiento: No                                                                                                       # 
###################################################################################################################################

##Áreas verdes de la Ciudad de México (2017-2018)----
url <- "http://www.conabio.gob.mx/informacion/gis/maps/ccl/avcdmxgw_c.zip"
options(timeout = 5000)
download.file(url, 'avcdmxgw_c.zip', mode = "wb")

zip::unzip("avcdmxgw_c.zip", exdir = "avcdmx_1718")

acvdmx_gdbs<-st_read("avcdmx_1718/avcdmx_1718.gdb")|>
  mutate(CVEGEO = CVE_ENT)|>
  st_drop_geometry()|>
  as.data.frame()|>
  group_by(CVEGEO)|>
  summarise(area_total = sum(Shape_Area))

# ##DATOS DEL CENSO DE POBLACIÓN Y VIVIENDA (PRINCIPALES RESULTADOS POR MANZANA)----
# url <- ""
# options(timeout = 500)
# download.file(url, 'RESAGEBURB_09_2020_xlsx.zip', mode = "wb")

#RESAGEBURB_09_2020_xlsx <- unzip('RESAGEBURB_09_2020_xlsx.zip', files = "RESAGEBURB_09XLSX20.xlsx")
##POBLACIÓN TOTAL DE LA CIUDAD DE MÉXICO, 2020
RESAGEBURB_09XLSX20<- readxl::read_excel(file.path(directorio_base, "RESAGEBURB_09XLSX20.xlsx"))|>
  mutate(CVEGEO = paste0(ENTIDAD),
         CVE_MUNI = paste0(ENTIDAD, MUN))|>
  mutate_all(~ gsub("\\*", "", .)) |> 
  mutate(across(9:230, ~ as.numeric(., na.rm = TRUE)))|>
  filter(NOM_MUN == "Total de la entidad Ciudad de México")|>
  select(CVEGEO, POBTOT)

##ÁREA VERDE PER CAPITA 2020? ----
av_pc_2018<-acvdmx_gdbs|>
  left_join(RESAGEBURB_09XLSX20, by = "CVEGEO")|>
  mutate(AV_PC_18 = round(area_total/POBTOT,2))


##ÁREA URBANA TOTAL (INFORMACIÓN DEL MGN2020)----
# url <-"https://www.inegi.org.mx/contenidos/productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marcogeo/889463807469/09_ciudaddemexico.zip"
# options(timeout = 5000)
# download.file(url, '09_ciudaddemexico.zip', mode = "wb")

# zip::unzip("09_ciudaddemexico.zip", exdir = "09_ciudaddemexico_2020")

zona_urbana_ageb_2020<- st_read(file.path(directorio_base,"09_ciudaddemexico_2020/conjunto_de_datos/09a.shp"))|>
  st_transform(crs = 32614)|>
  mutate(area = st_area(geometry))|>
  group_by(CVE_ENT)|>
  summarise(area_urbana = sum(area))

##ÁREA VERDE URBANA----
acvdmx_gdbs<-st_read("avcdmx_1718/avcdmx_1718.gdb")|>
  mutate(CVEGEO = CVE_ENT)|>
  st_intersection(zona_urbana_ageb_2020)

acvdmx_gdbs_2<-acvdmx_gdbs|>
  st_drop_geometry()|>
  as.data.frame()|>
  group_by(CVEGEO)|>
  summarise(area_total = sum(Shape_Area))

##ÁREA VERDE URBANA PER CAPITA 2020? ----
av_pc_2018<-acvdmx_gdbs_2|>
  left_join(RESAGEBURB_09XLSX20, by = "CVEGEO")|>
  mutate(AV_PC_18 = round(area_total/POBTOT,2))



  
  