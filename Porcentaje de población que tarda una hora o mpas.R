##LIBRERIAS----
libraries <- c("tidyverse",
               "readr", 
               "srvyr",
               "foreign", 
               "haven", 
               "utils", 
               "forecast")

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


leer_censo <- function(anio) {
  urls <- list(
    "2015" = "https://www.inegi.org.mx/contenidos/programas/intercensal/2015/microdatos/eic2015_09_dta.zip",
    "2020" = "https://www.inegi.org.mx/contenidos/programas/ccpv/2020/microdatos/Censo2020_CA_cdmx_dta.zip"
  )
  
  temp_zip <- tempfile(fileext = ".zip")
  temp_dir <- tempdir()
  options(timeout = 800)
  
  download.file(urls[[as.character(anio)]], temp_zip, mode = "wb")
  unzip(temp_zip, exdir = temp_dir)
  
  archivos_dta <- list.files(temp_dir, pattern = "\\.dta$", full.names = TRUE, recursive = TRUE)
  archivo_personas <- archivos_dta[grepl("per|pers|persona|personas", archivos_dta, ignore.case = TRUE)][1]

  datos <- read_dta(archivo_personas)
  
  unlink(temp_zip)
  unlink(list.files(temp_dir, full.names = TRUE), recursive = TRUE)
  
  return(datos)
}


personas2015<-leer_censo(2015)
personas2020<-leer_censo(2020)


#TIEMPOS DE TRASLADO----
mun_cdmx <- data.frame(CVE_MUN = c("002", "003", "004", "005", "006", "007", "008", "009", 
              "010", "011", "012", "013", "014", "015", "016", "017"), 
              NOMGEO = c("Azcapotzalco", "Coyoacán", "Cuajimalpa de Morelos", "Gustavo A. Madero", 
             "Iztacalco", "Iztapalapa", "La Magdalena Contreras", "Milpa Alta", 
             "Álvaro Obregón", "Tláhuac", "Tlalpan", "Xochimilco", "Benito Juárez", 
             "Cuauhtémoc", "Miguel Hidalgo", "Venustiano Carranza"))|>
  mutate(cvegeo = paste0("09", CVE_MUN))

#2015
personas2015_t <- personas2015|>
  select(ent, mun, factor, upm, estrato, asisten, conact, ent_pais_trab, mun_trab, tie_traslado_trab)|>
  mutate(mun_orig = ifelse(!is.na(mun) & nchar(mun) < 3, paste0(strrep("0", 3 - nchar(mun)), mun), mun),
         mun_trab = ifelse(!is.na(mun_trab) & nchar(mun_trab) < 3, paste0(strrep("0", 3 - nchar(mun_trab)), mun_trab), mun_trab),
         ent_pais_trab = ifelse(!is.na(ent_pais_trab), substr(ent_pais_trab, 2, 3), NA),
         mun_t_cvegeo = ifelse(!is.na(ent_pais_trab) & !is.na(mun_trab), paste0(ent_pais_trab, mun_trab), NA),
         cvegeo = paste0(ent, mun_orig),
         mun_destinos = case_when(mun_t_cvegeo == "000" ~ "sin información", 
                                  TRUE ~ mun_t_cvegeo))


head(personas2015_t)

tiempos_traslado_2015<- personas2015_t|>
  filter(mun_destinos %in% mun_cdmx$cvegeo)|>
  mutate(tipo = case_when(tie_traslado_trab %in% c("4", "5") ~ "más tiempo",
                          TRUE ~ "menos tiempo"))  

#2020
personas2020_t <- personas2020|>
  select(ent, mun, factor, upm, estrato, asisten, conact, ent_pais_trab, mun_trab, tie_traslado_trab)|>
  mutate(mun_orig = ifelse(!is.na(mun) & nchar(mun) < 3, paste0(strrep("0", 3 - nchar(mun)), mun), mun),
         mun_trab = ifelse(!is.na(mun_trab) & nchar(mun_trab) < 3, paste0(strrep("0", 3 - nchar(mun_trab)), mun_trab), mun_trab),
         ent_pais_trab = ifelse(!is.na(ent_pais_trab), substr(ent_pais_trab, 2, 3), NA),
         mun_t_cvegeo = ifelse(!is.na(ent_pais_trab) & !is.na(mun_trab), paste0(ent_pais_trab, mun_trab), NA),
         cvegeo = paste0(ent, mun_orig),
         mun_destinos = case_when(mun_t_cvegeo == "000" ~ "sin información", 
                                  TRUE ~ mun_t_cvegeo))



tiempos_traslado_2020<- personas2020_t|>
  filter(mun_destinos %in% mun_cdmx$cvegeo)|>
  mutate(tipo = case_when(tie_traslado_trab %in% c("4", "5") ~ "más tiempo",
                          TRUE ~ "menos tiempo"))   



tiempos_traslado<-tiempos_traslado_2015|>
  bind_rows(tiempos_traslado_2020)


#Función para definir el diseño muestral
diseño_muestral <- function(anio) {
  
  objeto_datos <- get(paste0("tiempos_traslado_", anio))
  
  as_survey_design(objeto_datos,
                   strata = estrato,
                   weights = factor,
                   ids = upm,
                   nest = TRUE)
}


dm_2015 <- diseño_muestral(2015)
dm_2020 <- diseño_muestral(2020)


tot_tie_traslado_2015 <- dm_2015 |>
  group_by(tipo)|>
  summarise(tot_personas = survey_total(vartype = "cv"),
            porcentaje = survey_prop(vartype = "cv")*100)|>
  filter(tipo == "más tiempo")|>
  mutate(año = 2015)

tot_tie_traslado_2020 <- dm_2020 |>
  group_by(tipo)|>
  summarise(tot_personas = survey_total(vartype = "cv"),
            porcentaje = survey_prop(vartype = "cv")*100)|>
  filter(tipo == "más tiempo")|>
  mutate(año = 2020)

tot_tie_traslado<-tot_tie_traslado_2015|>
  bind_rows(tot_tie_traslado_2020)|>
  select(año,porcentaje)|>
  mutate(tipo = "Porcentaje de personas")


##ESCENARIOS TENDENCIALES 2030,2035,2045
# Tasa de variación
tcma<-(((tot_tie_traslado_2020$porcentaje /tot_tie_traslado_2015$porcentaje)^(1/5)) - 1) 
tcma

porcentaje_base <- tot_tie_traslado |>
  filter(año == 2020) |>
  pull(porcentaje)

# Años a proyectar
años <- c(2030, 2040, 2045)
año_base <- 2020

tendencia_base <- data.frame(año = años,
                             porcentaje = round(porcentaje_base * (1 + tcma ) ^ (años - año_base), 2),
                             tipo = "Tendencia base")


tendecia_deseable<-data.frame(año = años,
                              porcentaje = round(porcentaje_base * (1 + -tcma ) ^ (años - año_base), 2),
                              tipo = "Tendencia deseable")


tot_tie_traslado_tendencias <- tot_tie_traslado|>
  bind_rows(tendencia_base,tendecia_deseable)

tot_tie_traslado_tendencias

lineas <- bind_rows(data.frame(año = c(2020, 2030),
                               porcentaje = c(tot_tie_traslado_tendencias$porcentaje[tot_tie_traslado_tendencias$año == 2020 & tot_tie_traslado_tendencias$tipo == "Porcentaje de personas"],
                                              tot_tie_traslado_tendencias$porcentaje[tot_tie_traslado_tendencias$año == 2030 & tot_tie_traslado_tendencias$tipo == "Tendencia base"]),
                               tipo = "Tendencia base"),
                    data.frame(año = c(2020, 2030),
                               porcentaje = c(tot_tie_traslado_tendencias$porcentaje[tot_tie_traslado_tendencias$año == 2020 & tot_tie_traslado_tendencias$tipo == "Porcentaje de personas"], 
                                              tot_tie_traslado_tendencias$porcentaje[tot_tie_traslado_tendencias$año == 2030 & tot_tie_traslado_tendencias$tipo == "Tendencia deseable"]),
                               tipo = "Tendencia deseable"))


paleta_colores <- c("Porcentaje de personas" = "#9F2241",
                    "Tendencia base" = "#027A35",
                    "Tendencia deseable" = "#cfcfcf")

linetypes <- c("Porcentaje de personas" = "solid",
               "Tendencia base" = "dotdash",
               "Tendencia deseable" = "dotdash")

# Gráfico con etiquetas y eje Y personalizado
ggplot(tot_tie_traslado_tendencias, aes(x = año, y = porcentaje, group = tipo, color = tipo, linetype = tipo)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.5) +
  geom_line(data = lineas, aes(x = año, y = porcentaje, group = tipo), linewidth = 1.2) +
  geom_text(aes(label = paste0(round(porcentaje, 1), "%")),
            vjust = -1, size = 4, family = "Roboto", show.legend = FALSE) +
  scale_color_manual(values = paleta_colores) +
  scale_linetype_manual(values = linetypes) +
  scale_y_continuous(breaks = seq(0, max(tot_tie_traslado_tendencias$porcentaje, na.rm = TRUE) + 5, by = 5),
                     labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Porcentaje personas que tardan una hora o más en llegar a sus centros de estudio o trabajo",
       x = "Año",
       y = "Porcentaje de gasto",
       color = "",
       linetype = "",
       caption = "Se considera la tasa de crecimiento media anual (TCMA) observada entre 2015 y 2020.
       La línea de tendencia deseable representa un escenario hipotético de mejora, invirtiendo el sentido de la TCMA para simular una reducción del valor."
       ) +
  theme_bw(base_size = 10, base_family = "Roboto") +
  theme( panel.grid.minor.y = element_line(color = "gray80", linetype = "dotted"),
         panel.grid.minor.x = element_blank(),
         panel.grid.major.y = element_blank(),
         panel.border = element_blank(),
         axis.line = element_line(color = "gray80"),
         legend.position = "top",
         plot.caption = element_text(hjust = 0))


        