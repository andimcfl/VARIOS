##########INDICADORES METROPOLITANOS PLAN GENERAL DE DESARROLLO DE LA CIUDAD DE MÉXICO############
##LIBRERIAS----
libraries <- c("tidyverse",
               "sf")
installed_packages <- rownames(installed.packages())
libraries_to_install <- setdiff(libraries, installed_packages)

if (length(libraries_to_install) > 0) {
  install.packages(libraries_to_install)
}

lapply(libraries, library, character.only = TRUE)

##LISTA DE MUNICIPIOS QUE INTEGRAN LA ZONA METROPOLITANA DEL VALLE DE MÉXICO----
sun_2018_delimitacion<- st_read("C:/Users/brenp/Desktop/MAPA SUN_2018/SUN_2018.shp")|>
  filter(NOM_SUN == "Valle de México")|>
  select(CVE_MUN)|>
  st_drop_geometry()|>
  as.data.frame()

##DIRECTORIO----
setwd("C:/Users/brenp/Desktop/PGOTDU_CDMX")


##MARCO GEOESTADISTICO 2020 (INTEGRADO)----
url <- "https://www.inegi.org.mx/contenidos/productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marcogeo/889463807469/mg_2020_integrado.zip"
options(timeout = 1000)

download.file(url, 'mg_2020_integrado.zip', mode = "wb")

zip::unzip("mg_2020_integrado.zip", exdir = "mg_2020_integrado")

mun_mgn_2020 <- st_read("mg_2020_integrado/conjunto_de_datos/00mun.shp")|>
  st_transform(crs = 32614)



