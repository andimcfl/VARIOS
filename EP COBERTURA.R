#LIBRERIAS----
libraries <- c("tidyverse",
               "sf",
               "sfnetworks", 
               "tidygraph", 
               "units", 
               "tmap", 
               "igraph", 
               "concaveman")

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
  filter(categoria_ %in% c("Parques, arboledas y alamedas", "Plazas y jardines"))|>
  st_centroid()|>
  st_intersection(limite)

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


# Función para establecer límite de distancia según categoría
limites <- function(categoria) {
  if (categoria == "Parques, arboledas y alamedas") {
    return(set_units(600, "m"))
  } else {
    return(set_units(400, "m"))
  }
}

# Función auxiliar para construir un sf uniforme
crear_poligono_sf <- function(geom, cat, id, metodo) {
  st_sf(
    geometry = st_sfc(geom),
    categoria_ = cat,
    id = id,
    metodo = metodo,
    crs = 32614
  )
}

# Generación de polígonos de cobertura
iso_poligonos <- lapply(1:nrow(ep), function(i) {
  punto <- ep[i, ]
  cat <- punto$categoria_
  max_dist <- limites(cat)
  
  nodo_cercano <- st_nearest_feature(punto, red |> activate("nodes") |> st_as_sf())
  pesos <- red |> activate("edges") |> pull(weight)
  distancias <- igraph::distances(graph = red, v = nodo_cercano, weights = pesos)
  distancias_vec <- set_units(as.numeric(distancias), "m")
  dist_nodo <- distancias_vec[nodo_cercano]
  
  red_con_dist <- red |> activate("nodes") |> mutate(dist = distancias_vec)
  nodos_cercanos_idx <- which(distancias_vec <= max_dist)
  nodos_cercanos <- red_con_dist |> 
    activate("nodes") |> 
    slice(nodos_cercanos_idx) |> 
    st_as_sf()
  
  if (dist_nodo > set_units(100, "m")) {
    # Si el punto está lejos de la red, se usa buffer de 100 m
    poligono_geom <- st_geometry(st_buffer(punto, dist = set_units(100, "m")))
    return(crear_poligono_sf(poligono_geom, cat, i, "buffer_100m"))
  } else {
    if (nrow(nodos_cercanos) >= 3) {
      poligono_geom <- st_geometry(concaveman(nodos_cercanos))
      return(crear_poligono_sf(poligono_geom, cat, i, "concaveman"))
    } else {
      poligono_geom <- st_geometry(st_buffer(punto, dist = set_units(100, "m")))
      return(crear_poligono_sf(poligono_geom, cat, i, "buffer_100m"))
    }
  }
})

# Combinar polígonos válidos
iso_poligonos_sf <- do.call(rbind, iso_poligonos[!sapply(iso_poligonos, is.null)])

# Visualización rápida
tmap_mode("view")

tm_shape(rnc) + tm_lines(col = "gray") +
  tm_shape(iso_poligonos_sf) + tm_lines(col = "darkblue") +
  tm_shape(ep) + tm_dots(col = "blue", "purple", size = 0.5)
         
st_write(iso_poligonos_sf, "iso_poligonos_sf.shp", delete_dsn = TRUE)




