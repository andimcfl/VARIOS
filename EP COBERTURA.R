#LIBRERIAS----
libraries <- c("tidyverse",
               "sf",
               "sfnetworks", 
               "tidygraph", 
               "units", 
               "tmap", 
               "igraph")

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

limite<- st_read(file.path(directorio_base,"09_ciudaddemexico_2020/conjunto_de_datos/09mun.shp"))|>
  st_transform(32614)|>
  filter(NOMGEO == "Tlalpan")|>
  st_bbox()|>
  st_as_sfc()


# ##DATOS DEL CENSO DE POBLACIÓN Y VIVIENDA (PRINCIPALES RESULTADOS POR MANZANA)----
# url <- "https://www.inegi.org.mx/contenidos/programas/ccpv/2020/microdatos/ageb_manzana/RESAGEBURB_09_2020_xlsx.zip"
# options(timeout = 500)
# download.file(url, 'RESAGEBURB_09_2020_xlsx.zip', mode = "wb")

#RESAGEBURB_09_2020_xlsx <- unzip('RESAGEBURB_09_2020_xlsx.zip', files = "RESAGEBURB_09XLSX20.xlsx")

RESAGEBURB_09XLSX20<- readxl::read_excel(file.path(directorio_base, "RESAGEBURB_09XLSX20.xlsx"))|>
  mutate(CVEGEO = paste0(ENTIDAD, MUN, LOC, AGEB, MZA),
         CVE_MUNI = paste0(ENTIDAD, MUN))|>
  mutate_all(~ gsub("\\*", "", .)) |> 
  mutate(across(9:230, ~ as.numeric(., na.rm = TRUE)))|>
  filter(!MZA == "000")|>
  select(CVEGEO,POBTOT)

# ##MARCO GEOESTADÍSTICO NACIONAL 2020----
# url <-"https://www.inegi.org.mx/contenidos/productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marcogeo/889463807469/09_ciudaddemexico.zip"
# options(timeout = 5000)
# download.file(url, '09_ciudaddemexico.zip', mode = "wb")

# zip::unzip("09_ciudaddemexico.zip", exdir = "09_ciudaddemexico_2020")

#DATOS DE LAS MANZANAS PARA AGREGAR LOS DATOS DEL CENSO DE POBLACIÓN Y VIVIENDA PRINCIPALES RESULTADOS POR MANZANA
#MANZANAS Y CASERÍOS POLIGONALES Y PUNTOS
manzanas <- st_read(file.path(directorio_base,"09_ciudaddemexico_2020/conjunto_de_datos/09m.shp"))|>
   st_transform(crs = 32614)|>
   select(CVEGEO)

caserio_pol<- st_read(file.path(directorio_base,"09_ciudaddemexico_2020/conjunto_de_datos/09pem.shp"))|>
   st_transform(crs = 32614)|>
   select(CVEGEO)

undades_geoest<-manzanas|>
 bind_rows(caserio_pol)

#AGREGAR LOS DATOS DEL CENSO DE POBLACIÓN Y VIVIENDA A LOS POLÍGONOS
mza_ur_mgn_2020<-undades_geoest|>
  left_join(RESAGEBURB_09XLSX20, by ="CVEGEO")


##ESPACIO PÚBLICO----
ep <- st_read("https://datos.cdmx.gob.mx/dataset/33f7efb2-540a-41e5-a7b2-f5c83e030b54/resource/e443a870-08d2-42ad-843f-ac4e0eb5541e/download/inventario-de-reas-verdes-en-la-ciudad-de-mxico..json") |>
  st_transform(crs = 32614) |>
  filter(categoria_ %in% c('Parques, arboledas y alamedas', 'Plazas y jardines'))|>
  st_centroid()|>
  st_intersection(limite)


st_write(ep, "ep.shp")

##RED VIAL----
# url <-"https://www.inegi.org.mx/contenidos/productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/caminos/2024/794551132166_s.zip"
# options(timeout = 5000)
# download.file(url, '794551132166_s.zip', mode = "wb")

# zip::unzip("794551132166_s.zip", exdir = "red_nacional_de_caminos_2024")

rnc<-st_read(file.path(directorio_base,"red_nacional_de_caminos_2024/conjunto_de_datos/red_vial.shp"))|>
  st_transform(32614)|>
  st_intersection(limite)|>
  select(TIPO_VIAL,CIRCULA,VELOCIDAD)|> 
  mutate(long = st_length(geometry))|>
  st_cast("LINESTRING")

# Crear red desde línea (rnc)
red <- as_sfnetwork(rnc, directed = FALSE) |> 
  activate("edges") |> 
  mutate(weight = long)


st_write(rnc, "rnc.shp",  delete_dsn = TRUE)

# Función para establecer límite de distancia según categoría
limites <- function(categoria) {
  if (categoria %in% c('Parques, arboledas y alamedas')) {
    return(set_units(600, "m"))
  } else {
    return(set_units(400, "m"))
  }
}

# Lista de subredes accesibles por punto
iso_buffers <- lapply(1:nrow(ep), function(i) {
  punto <- ep[i, ]
  cat <- punto$categoria_
  max_dist <- limites(cat)
  max_dist <- set_units(max_dist, "m")  # Asegura que tiene unidades
  
  # Nodo más cercano
  nodo_cercano <- st_nearest_feature(
    punto,
    red |> activate("nodes") |> st_as_sf()
  )
  
  # Extraer pesos (en metros)
  pesos <- red |> activate("edges") |> pull(weight)
  
  # Calcular distancias desde el nodo de origen
  distancias <- igraph::distances(graph = red, v = nodo_cercano, weights = pesos)
  
  # Vector de distancias con unidades
  distancias_vec <- set_units(as.numeric(distancias), "m")
  
  # Asignar distancias a nodos
  red_con_dist <- red |>
    activate("nodes") |>
    mutate(dist = distancias_vec)
  
  # Índices de nodos dentro del rango
  nodos_cercanos_idx <- which(distancias_vec <= max_dist)
  
  # Nodos dentro del rango
  nodos_cercanos <- red_con_dist |>
    activate("nodes") |>
    slice(nodos_cercanos_idx)
  
  # Filtrar aristas que conectan solo esos nodos
  subred_edges <- red_con_dist |>
    activate("edges") |>
    filter(.N()$dist[from] <= max_dist & .N()$dist[to] <= max_dist)
  
  # Convertir a sf
  st_as_sf(subred_edges)
})

# Combinar las isodistancias en una sola capa
iso_merged <- do.call(rbind, iso_buffers)


# Visualización rápida
tmap_mode("view")
tm_shape(rnc) + tm_lines(col = "gray") +
  tm_shape(iso_merged) + tm_lines(col = "darkblue") +
  tm_shape(ep) + tm_dots(col = "blue", "purple", size = 0.5)
         
  
               

st_write(iso_merged, "iso_merged.shp", delete_dsn = TRUE)




