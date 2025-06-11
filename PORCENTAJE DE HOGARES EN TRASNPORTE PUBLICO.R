#LIBRERIAS----
libraries <- c("tidyverse",
               "readr", 
               "srvyr",
               "ggplot2",
               "showtext", 
               "modeest")

installed_packages <- rownames(installed.packages())
libraries_to_install <- setdiff(libraries, installed_packages)

if (length(libraries_to_install) > 0) {
  install.packages(libraries_to_install)}

lapply(libraries, library, character.only = TRUE)

#CONSIDERACIONES----
#Tiempo en linea
options(timeout = 8000)
# Fuente
font_add_google(name = "Roboto", family = "Roboto")
showtext_auto()
#Ajustes para las UPM
options(survey.lonely.psu = "certainty")  

#ENCUESTA DE INGRESO Y GASTO EN LOS HOGARES----
#Función para leer los datos de la Encuesta Nacional de Ingresos y Gastos de los Hogares (ENIGH) 
leer_enigh <- function(anio)
{ 
  #URL de descarga los datos para los años 2018, 2020 y 2022
  urls <- list("2016" = "https://www.inegi.org.mx/contenidos/programas/enigh/nc/2016/microdatos/enigh2016_ns_concentradohogar_csv.zip",
               "2018" = "https://www.inegi.org.mx/contenidos/programas/enigh/nc/2018/microdatos/enigh2018_ns_concentradohogar_csv.zip",
               "2020" = "https://www.inegi.org.mx/contenidos/programas/enigh/nc/2020/microdatos/enigh2020_ns_concentradohogar_csv.zip",
               "2022" = "https://www.inegi.org.mx/contenidos/programas/enigh/nc/2022/microdatos/enigh2022_ns_concentradohogar_csv.zip"
  )
  
  # Archivo y directorio temporales
  temp_zip <- tempfile(fileext = ".zip")
  temp_dir <- tempdir()
  options(timeout = 800)
  
  # Descargar y descomprimir
  download.file(urls[[as.character(anio)]], temp_zip, mode = "wb")
  unzip(temp_zip, exdir = temp_dir)
  
  # Buscar el archivo de datos
  enigh_datos <- list.files(temp_dir, pattern = "concentradohogar.csv", 
                            full.names = TRUE, recursive = TRUE)
  
  # Leer el archivo como caracteres por defecto
  datos <- read_csv(enigh_datos, col_types = NULL)
  
  unlink(temp_zip)
  unlink(list.files(temp_dir, full.names = TRUE), recursive = TRUE)
  
  return(datos)
}

#ENCUESTA
e2016<-leer_enigh(2016)
e2018<-leer_enigh(2018)
e2020<-leer_enigh(2020)
e2022<-leer_enigh(2022)

##Función para leer los datos de la Encuesta Nacional de Ingresos y Gastos de los Hogares (ENIGH) GASTOSPERSONA----
leer_enigh <- function(anio)
{ 
  #URL de descarga los datos para los años 2018, 2020 y 2022
  urls <- list("2016" = "https://www.inegi.org.mx/contenidos/programas/enigh/nc/2016/microdatos/enigh2016_ns_gastospersona_csv.zip",
               "2018" = "https://www.inegi.org.mx/contenidos/programas/enigh/nc/2018/microdatos/enigh2018_ns_gastospersona_csv.zip",
               "2020" = "https://www.inegi.org.mx/contenidos/programas/enigh/nc/2020/microdatos/enigh2020_ns_gastospersona_csv.zip",
               "2022" = "https://www.inegi.org.mx/contenidos/programas/enigh/nc/2022/microdatos/enigh2022_ns_gastospersona_csv.zip"
  )
  
  # Archivo y directorio temporales
  temp_zip <- tempfile(fileext = ".zip")
  temp_dir <- tempdir()
  options(timeout = 800)
  
  # Descargar y descomprimir
  download.file(urls[[as.character(anio)]], temp_zip, mode = "wb")
  unzip(temp_zip, exdir = temp_dir)
  
  # Buscar el archivo de datos
  enigh_datos <- list.files(temp_dir, pattern = "gastospersona.csv", 
                            full.names = TRUE, recursive = TRUE)
  
  # Leer el archivo como caracteres por defecto
  datos <- read_csv(enigh_datos, col_types = NULL)
  
  unlink(temp_zip)
  unlink(list.files(temp_dir, full.names = TRUE), recursive = TRUE)
  
  return(datos)
}

#CARGAR ENCUESTA GASTOSPERSONA----
gastoper2016<-leer_enigh(2016)
gastoper2018<-leer_enigh(2018)
gastoper2020<-leer_enigh(2020)
gastoper2022<-leer_enigh(2022)

gastoper2016<-gastoper2016|>
  select(folioviv, foliohog, numren, clave)
gastoper2018<-gastoper2018|>
  select(folioviv, foliohog, numren, clave)
gastoper2020<-gastoper2020|>
  select(folioviv, foliohog, numren, clave)
gastoper2022<-gastoper2022|>
  select(folioviv, foliohog, numren, clave)


#PORCENTAJE DE HOGARES QUE USAN METRO, METROBUS O CABLEBUS Y TROLEBUS COMO PRINCIPAL MEDIO DE TRANSPORTE----
# Transporte público
# B001 Metro o tren ligero
# B002 Autobús
# B003 Trolebús o metrobús
# B004 Colectivo, combi o microbús
#2016
gasto_trans_pub_2016 <- gastoper2016|>
  mutate(cve_ent = substr(folioviv, 1,2))|>
  filter(cve_ent == "09")|>
  mutate(tipo_gasto = case_when(clave %in% c("B001","B002","B003") ~ "transporte publico masivo",
                                clave %in% c("B004", "B005", "B006", "B007") ~ "otros transportes publicos", 
                                clave %in% c("M001", "M002", "M003", "M004", "M005", "M006") ~ "transporte foraneo",
                                clave %in% c("M007", "M008", "M009", "M010", "M011") ~ "adquisición de vehículos", 
                                clave %in% c("M012", "M013", "M014", "M015", "M016", "M017", "M018") ~ "Refacciones para vehiculos", 
                                clave %in% c("F007", "F008", "F014") ~ "Combustibles para vehículos",
                                clave %in% c("F001", "F002", "F003", "F004", "F005", "F006","R005", "R006", "R007", "R008", "R010", "R011") ~ "comunicaciones", 
                                TRUE ~ "OTROS GASTOS"), 
         transporte = case_when(tipo_gasto == "transporte publico masivo" ~ "transporte publico masivo", 
                                tipo_gasto %in% c("transporte foraneo","otros transportes publicos","adquisición de vehículos",
                                                  "Refacciones para vehiculos","Combustibles para vehículos",
                                                  "comunicaciones") ~ "otro transporte"))|>
  filter(tipo_gasto !="OTROS GASTOS")|>
  group_by(folioviv, foliohog)|>
  summarise(tipo = mfv(transporte)[1])



gastohog_trans_pub_2016<-e2016|>
  mutate(cve_ent = substr(folioviv, 1,2))|>
  filter(cve_ent == "09")|>
  select(folioviv, foliohog, est_dis, upm, factor)|>
  left_join(gasto_trans_pub_2016, by = c("folioviv", "foliohog"))|>
  filter(!is.na(tipo))


#2018
gasto_trans_pub_2018 <- gastoper2018|>
  mutate(cve_ent = substr(folioviv, 1,2))|>
  filter(cve_ent == "09")|>
  mutate(tipo_gasto = case_when(clave %in% c("B001","B002","B003") ~ "transporte publico masivo",
                                clave %in% c("B004", "B005", "B006", "B007") ~ "otros transportes publicos", 
                                clave %in% c("M001", "M002", "M003", "M004", "M005", "M006") ~ "transporte foraneo",
                                clave %in% c("M007", "M008", "M009", "M010", "M011") ~ "adquisición de vehículos", 
                                clave %in% c("M012", "M013", "M014", "M015", "M016", "M017", "M018") ~ "Refacciones para vehiculos", 
                                clave %in% c("F007", "F008", "F014") ~ "Combustibles para vehículos",
                                clave %in% c("F001", "F002", "F003", "F004", "F005", "F006","R005", "R006", "R007", "R008", "R010", "R011") ~ "comunicaciones", 
                                TRUE ~ "OTROS GASTOS"), 
         transporte = case_when(tipo_gasto == "transporte publico masivo" ~ "transporte publico masivo", 
                                tipo_gasto %in% c("transporte foraneo","otros transportes publicos","adquisición de vehículos",
                                                  "Refacciones para vehiculos","Combustibles para vehículos",
                                                  "comunicaciones") ~ "otro transporte"))|>
  filter(tipo_gasto !="OTROS GASTOS")|>
  group_by(folioviv, foliohog)|>
  summarise(tipo = mfv(transporte)[1])


gastohog_trans_pub_2018<-e2018|>
  mutate(cve_ent = substr(folioviv, 1,2))|>
  filter(cve_ent == "09")|>
  select(folioviv, foliohog, est_dis, upm, factor)|>
  left_join(gasto_trans_pub_2018, by = c("folioviv", "foliohog"))|>
  filter(!is.na(tipo))

#2020
gasto_trans_pub_2020 <- gastoper2020|>
  mutate(cve_ent = substr(folioviv, 1,2))|>
  filter(cve_ent == "09")|>
  mutate(tipo_gasto = case_when(clave %in% c("B001","B002","B003") ~ "transporte publico masivo",
                                clave %in% c("B004", "B005", "B006", "B007") ~ "otros transportes publicos", 
                                clave %in% c("M001", "M002", "M003", "M004", "M005", "M006") ~ "transporte foraneo",
                                clave %in% c("M007", "M008", "M009", "M010", "M011") ~ "adquisición de vehículos", 
                                clave %in% c("M012", "M013", "M014", "M015", "M016", "M017", "M018") ~ "Refacciones para vehiculos", 
                                clave %in% c("F007", "F008", "F014") ~ "Combustibles para vehículos",
                                clave %in% c("F001", "F002", "F003", "F004", "F005", "F006","R005", "R006", "R007", "R008", "R010", "R011") ~ "comunicaciones", 
                                TRUE ~ "OTROS GASTOS"), 
         transporte = case_when(tipo_gasto == "transporte publico masivo" ~ "transporte publico masivo", 
                                tipo_gasto %in% c("transporte foraneo","otros transportes publicos","adquisición de vehículos",
                                                  "Refacciones para vehiculos","Combustibles para vehículos",
                                                  "comunicaciones") ~ "otro transporte"))|>
  filter(tipo_gasto !="OTROS GASTOS")|>
  group_by(folioviv, foliohog)|>
  summarise(tipo = mfv(transporte)[1])


gastohog_trans_pub_2020<-e2020|>
  mutate(cve_ent = substr(folioviv, 1,2))|>
  filter(cve_ent == "09")|>
  select(folioviv, foliohog, est_dis, upm, factor)|>
  left_join(gasto_trans_pub_2020, by = c("folioviv", "foliohog"))|>
  filter(!is.na(tipo))

#2022
gasto_trans_pub_2022 <- gastoper2022|>
  mutate(cve_ent = substr(folioviv, 1,2))|>
  filter(cve_ent == "09")|>
  mutate(tipo_gasto = case_when(clave %in% c("B001","B002","B003") ~ "transporte publico masivo",
                                clave %in% c("B004", "B005", "B006", "B007") ~ "otros transportes publicos", 
                                clave %in% c("M001", "M002", "M003", "M004", "M005", "M006") ~ "transporte foraneo",
                                clave %in% c("M007", "M008", "M009", "M010", "M011") ~ "adquisición de vehículos", 
                                clave %in% c("M012", "M013", "M014", "M015", "M016", "M017", "M018") ~ "Refacciones para vehiculos", 
                                clave %in% c("F007", "F008", "F014") ~ "Combustibles para vehículos",
                                clave %in% c("F001", "F002", "F003", "F004", "F005", "F006","R005", "R006", "R007", "R008", "R010", "R011") ~ "comunicaciones", 
                                TRUE ~ "OTROS GASTOS"), 
         transporte = case_when(tipo_gasto == "transporte publico masivo" ~ "transporte publico masivo", 
                                tipo_gasto %in% c("transporte foraneo","otros transportes publicos","adquisición de vehículos",
                                                  "Refacciones para vehiculos","Combustibles para vehículos",
                                                  "comunicaciones") ~ "otro transporte"))|>
  filter(tipo_gasto !="OTROS GASTOS")|>
  group_by(folioviv, foliohog)|>
  summarise(tipo = mfv(transporte)[1])


gastohog_trans_pub_2022<-e2022|>
  mutate(cve_ent = substr(folioviv, 1,2))|>
  filter(cve_ent == "09")|>
  select(folioviv, foliohog, est_dis, upm, factor)|>
  left_join(gasto_trans_pub_2022, by = c("folioviv", "foliohog"))|>
  filter(!is.na(tipo))


#Función para definir el diseño muestral
diseño_muestral <- function(anio) {
  
  objeto_datos <- get(paste0("gastohog_trans_pub_", anio))
  
  as_survey_design(objeto_datos,
                   strata = est_dis,
                   weights = factor,
                   ids = upm,
                   nest = TRUE)
}


dm_2016 <- diseño_muestral(2016)
dm_2018 <- diseño_muestral(2018)
dm_2020 <- diseño_muestral(2020)
dm_2022 <- diseño_muestral(2022)

##PORCENTAJE DE HOGARES QUE TIENEN Metro o tren ligero, Autobús, Trolebús o metrobús o Colectivo, combi o microbús como principal medio de transporte----
prc_hog_trans_pub_2016 <- dm_2016 |>
  group_by(tipo)|>
  summarise(tot_hog__tpublico = survey_total(vartype = "cv"),
            porcentaje = survey_prop(vartype = "cv")*100)|>
  filter(tipo == "transporte publico masivo")|>
  mutate(año = 2016)

prc_hog_trans_pub_2018 <- dm_2018 |>
  group_by(tipo)|>
  summarise(tot_hog__tpublico = survey_total(vartype = "cv"),
            porcentaje = survey_prop(vartype = "cv")*100)|>
  filter(tipo == "transporte publico masivo")|>
  mutate(año = 2018)

prc_hog_trans_pub_2020 <- dm_2020 |>
  group_by(tipo)|>
  summarise(tot_hog__tpublico = survey_total(vartype = "cv"),
            porcentaje = survey_prop(vartype = "cv")*100)|>
  filter(tipo == "transporte publico masivo")|>
  mutate(año = 2020)

prc_hog_trans_pub_2022 <- dm_2022 |>
  group_by(tipo)|>
  summarise(tot_hog__tpublico = survey_total(vartype = "cv"),
            porcentaje = survey_prop(vartype = "cv")*100)|>
  filter(tipo == "transporte publico masivo")|>
  mutate(año = 2022)


prc_hog_tp<-prc_hog_trans_pub_2016|>
  bind_rows(prc_hog_trans_pub_2018, prc_hog_trans_pub_2020, prc_hog_trans_pub_2022)|>
  select(año,porcentaje)|>
  mutate(tipo = "Porcentaje de hogares")




tcma<-((prc_hog_tp$porcentaje[prc_hog_tp$año==2022]/prc_hog_tp$porcentaje[prc_hog_tp$año==2016])^(1/(2022-2016))-1)
tcma



porcentaje_base <- prc_hog_tp |>
  filter(año == 2022) |>
  pull(porcentaje)

# Años a proyectar
años <- c(2030, 2040, 2045)
año_base <- 2022

tendencia_base <- data.frame(año = años,
                             porcentaje = round(porcentaje_base * (1 + tcma ) ^ (años - año_base), 2),
                             tipo = "Tendencia base")


tendecia_deseable<-data.frame(año = años,
                              porcentaje = round(porcentaje_base * (1 + tcma*1.1 ) ^ (años - año_base), 2),
                              tipo = "Tendencia deseable")


prc_hog_tp_tendencias <- prc_hog_tp|>
  bind_rows(tendencia_base,tendecia_deseable)

prc_hog_tp_tendencias

lineas <- bind_rows(data.frame(año = c(2022, 2030),
                               porcentaje = c(prc_hog_tp_tendencias$porcentaje[prc_hog_tp_tendencias$año == 2022 & prc_hog_tp_tendencias$tipo == "Porcentaje de hogares"],
                                              prc_hog_tp_tendencias$porcentaje[prc_hog_tp_tendencias$año == 2030 & prc_hog_tp_tendencias$tipo == "Tendencia base"]),
                               tipo = "Tendencia base"),
                    data.frame(año = c(2022, 2030),
                               porcentaje = c(prc_hog_tp_tendencias$porcentaje[prc_hog_tp_tendencias$año == 2022 & prc_hog_tp_tendencias$tipo == "Porcentaje de hogares"], 
                                              prc_hog_tp_tendencias$porcentaje[prc_hog_tp_tendencias$año == 2030 & prc_hog_tp_tendencias$tipo == "Tendencia deseable"]),
                               tipo = "Tendencia deseable"))


paleta_colores <- c("Porcentaje de hogares" = "#9F2241",
                    "Tendencia base" = "#027A35",
                    "Tendencia deseable" = "#cfcfcf")

linetypes <- c("Porcentaje de hogares" = "solid",
               "Tendencia base" = "dotdash",
               "Tendencia deseable" = "dotdash")

# Gráfico con etiquetas y eje Y personalizado
ggplot(prc_hog_tp_tendencias, aes(x = año, y = porcentaje, group = tipo, color = tipo, linetype = tipo)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.5) +
  geom_line(data = lineas, aes(x = año, y = porcentaje, group = tipo), linewidth = 1.2) +
  geom_text(aes(label = paste0(round(porcentaje, 1), "%")),
            vjust = -1, size = 4, family = "Roboto", show.legend = FALSE) +
  scale_color_manual(values = paleta_colores) +
  scale_linetype_manual(values = linetypes) +
  scale_y_continuous(breaks = seq(0, max(prc_hog_tp_tendencias$porcentaje, na.rm = TRUE) + 5, by = 5),
                     labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Porcentaje de hogares que usan el transporte público* como principal medio de transporte",
       x = "Año",
       y = "Porcentaje de gasto",
       color = "",
       linetype = "",
       caption = "*Se consideraron:Metro, tren ligero, Autobús, Trolebús y metrobús.
       Se considera la tasa de crecimiento media anual (TCMA) observada entre 2016 y 2022.
       La línea de tendencia deseable representa un escenario hipotético de mejora"
  ) +
  theme_bw(base_size = 10, base_family = "Roboto") +
  theme( panel.grid.minor.y = element_line(color = "gray80", linetype = "dotted"),
         panel.grid.minor.x = element_blank(),
         panel.grid.major.y = element_blank(),
         panel.border = element_blank(),
         axis.line = element_line(color = "gray80"),
         legend.position = "top",
         plot.caption = element_text(hjust = 0))





































##ESCENARIOS TENDENCIALES 2030,2035,2045
# Tasa de variación
prc_hog_tp <- prc_hog_tp |>
  mutate(valor_lag = lag(porcentaje),
         delta = (porcentaje / valor_lag) - 1)

delta_prom <- mean(prc_hog_tp$delta, na.rm = TRUE)

# Valor base 2022
valor_2022 <- prc_hog_tp|>
  filter(año == 2022)|>
  pull(porcentaje)

# Años a proyectar
anios_proy <- c(2030, 2035, 2045)
n_periodos <- anios_proy - 2022

# Proyecciones (tendencia base)
proyeccion_base <- data.frame(año = anios_proy,
                              porcentaje = valor_2022 * ((1 + delta_prom) ^ n_periodos))|>
  mutate(tipo = "Tendencia base")

# Proyección deseable
delta_deseable <- delta_prom * 1.1

proyeccion_deseable <- data.frame(año = anios_proy,
                                  porcentaje = valor_2022 * ((1 + delta_deseable) ^ n_periodos))|>
  mutate(tipo = "Tendencia deseable")

prc_hog_tp <- bind_rows(prc_hog_tp,
                              proyeccion_base,
                              proyeccion_deseable)

lineas <- bind_rows(data.frame(año = c(2022, 2030),
                               porcentaje = c(prc_hog_tp$porcentaje[prc_hog_tp$año == 2022 & prc_hog_tp$tipo == "Porcentaje de hogares"],
                                              prc_hog_tp$porcentaje[prc_hog_tp$año == 2030 & prc_hog_tp$tipo == "Tendencia base"]),
                               tipo = "Tendencia base"),
                    data.frame(año = c(2022, 2030),
                               porcentaje = c(prc_hog_tp$porcentaje[prc_hog_tp$año == 2022 & prc_hog_tp$tipo == "Porcentaje de hogares"], 
                                              prc_hog_tp$porcentaje[prc_hog_tp$año == 2030 & prc_hog_tp$tipo == "Tendencia deseable"]),
                               tipo = "Tendencia deseable"))


paleta_colores <- c("Porcentaje de hogares" = "#9F2241",
                    "Tendencia base" = "#027A35",
                    "Tendencia deseable" = "#cfcfcf")

linetypes <- c("Porcentaje de hogares" = "solid",
               "Tendencia base" = "dotdash",
               "Tendencia deseable" = "dotdash")

# Gráfico con etiquetas y eje Y personalizado
ggplot(prc_hog_tp, aes(x = año, y = porcentaje, group = tipo, color = tipo, linetype = tipo)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.5) +
  geom_line(data = lineas, aes(x = año, y = porcentaje, group = tipo), linewidth = 1.2) +
  geom_text(aes(label = paste0(round(porcentaje, 1), "%")),
            vjust = -1, size = 4, family = "Roboto", show.legend = FALSE) +
  scale_color_manual(values = paleta_colores) +
  scale_linetype_manual(values = linetypes) +
  scale_y_continuous(breaks = seq(0, max(prc_hog_tp$porcentaje, na.rm = TRUE) + 5, by = 5),
                     labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Porcentaje de hogares que usan el transporte público masivo como principal medio de transporte",
       x = "Año",
       y = "Porcentaje de gasto",
       color = "",
       linetype = "",
       caption = "Nota: Las tendencias se calcularon con un método incremental usando la tasa de variación promedio anual entre los años disponibles.
    La tendencia deseable considera una reducción acelerada del indicador.") +
  theme_bw(base_size = 10, base_family = "Roboto") +
  theme( panel.grid.minor.y = element_line(color = "gray80", linetype = "dotted"),
         panel.grid.minor.x = element_blank(),
         panel.grid.major.y = element_blank(),
         panel.border = element_blank(),
         axis.line = element_line(color = "gray80"),
         legend.position = "top",
         plot.caption = element_text(hjust = 0))

