##########INDICADORES METROPOLITANOS PLAN GENERAL DE DESARROLLO DE LA CIUDAD DE MÉXICO############
libraries <- c("tidyverse",
               "sf", 
               "readr", 
               "readxl", 
               "utils", 
               "srvyr")

installed_packages <- rownames(installed.packages())
libraries_to_install <- setdiff(libraries, installed_packages)

if (length(libraries_to_install) > 0) {
  install.packages(libraries_to_install)
}

lapply(libraries, library, character.only = TRUE)

##LISTA DE MUNICIPIOS QUE INTEGRAN LA ZONA METROPOLITANA DEL VALLE DE MÉXICO----
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
  mutate(NOM_MET = "Ciudad de México", 
         MUN_DESTINOS = CVEGEO)

polignos_municipios<-st_read("D:/BASES Y DATOS GENERALES/mg_2024_integrado/conjunto_de_datos/00mun.shp")|>
  filter(CVEGEO %in% mun_zm$CVEGEO)|>
  st_transform(4326)


#DATOS DE POBLACIÓN DEL CENSO DE POBLACIÓN Y VIVIENDA 2020----
cdmx_url <- "https://www.inegi.org.mx/contenidos/programas/ccpv/2020/microdatos/iter/iter_09_2020_xlsx.zip"
mexico_url <- "https://www.inegi.org.mx/contenidos/programas/ccpv/2020/microdatos/iter/iter_15_2020_xlsx.zip"
hidalgo_url <- "https://www.inegi.org.mx/contenidos/programas/ccpv/2020/microdatos/iter/iter_13_2020_xlsx.zip"

temp_zip_cdmx <- tempfile(fileext = ".zip")     
temp_zip_mexico <- tempfile(fileext = ".zip") 
temp_zip_hidalgo <- tempfile(fileext = ".zip")
temp_dir <- tempdir()

download.file(cdmx_url, temp_zip_cdmx, mode = "wb")
unzip(temp_zip_cdmx, exdir = temp_dir)

download.file(mexico_url, temp_zip_mexico, mode = "wb")
unzip(temp_zip_mexico, exdir = temp_dir)

download.file(hidalgo_url, temp_zip_hidalgo, mode = "wb")
unzip(temp_zip_hidalgo, exdir = temp_dir)


leer_iter <- function(temp_dir, pattern) 
  {
  archivo <- list.files(temp_dir, pattern = pattern, full.names = TRUE, recursive = TRUE)
  read_excel(archivo) |>
  mutate(CVEGEO = as.character(paste0(ENTIDAD, MUN)))|>
  mutate_all(~ gsub("//*", "", .)) |>
  mutate(across(9:286, ~ as.numeric(.))) |>
  filter(NOM_LOC == "Total del Municipio")
  }


ITER_09 <- leer_iter(temp_dir, "ITER_09XLSX20.xlsx")
ITER_15 <- leer_iter(temp_dir, "ITER_15XLSX20.xlsx")
ITER_13 <- leer_iter(temp_dir, "ITER_13XLSX20.xlsx")


ITER_MUN <-ITER_09|>
  bind_rows(ITER_15, ITER_13)|>
  filter(CVEGEO %in% mun_zm$CVEGEO)|>
  mutate(ZM = "ZMVM")|>
  as.data.frame()

##TEMA: RIESGOS Y PROTECCIÓN CIVIL----
#Porcentaje de municipios de la ZM que cuentan con al menos un sistema de alerta temprana
alerta_temprana <- readxl::read_excel("C:/Users/brenp/Downloads/cngmd2023_prot_civil (1).xlsx",
                                      sheet = 11, 
                                      skip = 4)|>
  janitor::clean_names()|>
  mutate(CVEGEO = paste0(clave_entidad,clave_municipio_o_demarcacion))|>
  filter(CVEGEO %in%mun_zm$CVEGEO)|>
  mutate(alerta = ifelse(sismos == "1" | 
                           erupciones_volcanicas == "1" |
                           ciclones_tropicales == "1" |
                           otro =="1", "Con al menos una alerta temprana", "Sin alerta temprana"))|>
  group_by(alerta)|>
  summarise(municipios = n())|>
  mutate(porcentaje = round(municipios*100/sum(municipios), 1))

alerta_temprana

##Porcentaje de municipios que consideran la coordinación con otras unidades de Protección Civil municipal, en su Plan de Protección Civil.
coordina <- readxl::read_excel("C:/Users/brenp/Downloads/cngmd2023_prot_civil (1).xlsx",
                               sheet = 5, 
                               skip = 5)|>
  janitor::clean_names()|>
  mutate(CVEGEO = paste0(clave_entidad,clave_municipio_o_demarcacion))|>
  select(CVEGEO,
         unidad_de_proteccion_civil_u_homologa_de_otros_municipio_s_o_demarcacion_es_territorial_es_de_la_entidad_federativa,
         unidad_de_proteccion_civil_u_homologa_de_municipio_s_o_demarcacion_es_territorial_es_de_otra_s_entidad_es_federativa_s)|>
  filter(CVEGEO %in%mun_zm$CVEGEO)|>
  mutate(estatal_coord = ifelse(unidad_de_proteccion_civil_u_homologa_de_otros_municipio_s_o_demarcacion_es_territorial_es_de_la_entidad_federativa == "NA" |
                                  unidad_de_proteccion_civil_u_homologa_de_otros_municipio_s_o_demarcacion_es_territorial_es_de_la_entidad_federativa == "0", 0, 1), 
         municipal_coord = ifelse(unidad_de_proteccion_civil_u_homologa_de_municipio_s_o_demarcacion_es_territorial_es_de_otra_s_entidad_es_federativa_s == "NA"|
                                    unidad_de_proteccion_civil_u_homologa_de_municipio_s_o_demarcacion_es_territorial_es_de_otra_s_entidad_es_federativa_s == "0", 0, 1), 
         coordinacion = ifelse(estatal_coord == 1 | municipal_coord == 1, "Con consideración de coordinación con unidades de otros municipios", "Sin consideración de coordinación"))|>
  group_by(coordinacion)|>
  summarise(municipios = n())|>
  mutate(porcentaje = round(municipios*100/sum(municipios), 1))

coordina
##TEMA: EDUCACIÓN----
##Porcentaje de población de 18 años  y más con educación  posbásica
P18MAS<-ITER_MUN|>
  group_by(ZM)|>
  summarise(P18MAS = sum(P_18YMAS, na.rm = TRUE), 
            P18MASESCUELA = sum(P18YM_PB, na.rm = TRUE))|>
  mutate(porcentaje = round(P18MASESCUELA*100/P18MAS,1))


P18MAS

#TEMA:POBREZA----
##Porcentaje de la población en situación de pobreza extrema
tabla_pobreza_09 <- read_excel("C:/Users/brenp/Desktop/PGOTDU_CDMX/ZMCM_NBI_EVALÚA.xlsx", 
                               sheet = 1, 
                               skip = 2, 
                               n_max = 20)


fila_combinada <- tabla_pobreza_09[3, ] %>%
  mutate(across(everything(), ~ paste(.x, tabla_pobreza_09[4, cur_column()], sep = " ")))

tabla_pobreza_09[3, ] <- fila_combinada
tabla_pobreza_09 <- tabla_pobreza_09[-4,]

nuevos_nombres <- tabla_pobreza_09[3, ]

tabla_pobreza_09 <- tabla_pobreza_09[-c(1:3), ]

colnames(tabla_pobreza_09) <- nuevos_nombres

tabla_pobreza_09 <- tabla_pobreza_09 |>
  janitor::clean_names()|>
  mutate(across(5:23, ~ as.numeric(.)))

tabla_pobreza_15 <- read_excel("C:/Users/brenp/Desktop/PGOTDU_CDMX/ZMCM_NBI_EVALÚA.xlsx", 
                               sheet = 3, 
                               skip = 2, 
                               n_max = 129)


fila_combinada <- tabla_pobreza_15[3, ] %>%
  mutate(across(everything(), ~ paste(.x, tabla_pobreza_15[4, cur_column()], sep = " ")))

tabla_pobreza_15[3, ] <- fila_combinada
tabla_pobreza_15 <- tabla_pobreza_15[-4,]

nuevos_nombres <- tabla_pobreza_15[3, ]

tabla_pobreza_15 <- tabla_pobreza_15[-c(1:3), ]

colnames(tabla_pobreza_15) <- nuevos_nombres


tabla_pobreza_15 <- tabla_pobreza_15 |>
  janitor::clean_names()|>
  mutate(across(5:23, ~ as.numeric(.)))

tabla_pobreza_13 <- read_excel("C:/Users/brenp/Desktop/PGOTDU_CDMX/ZMCM_NBI_EVALÚA.xlsx", 
                               sheet = 2, 
                               skip = 2, 
                               n_max = 85)


fila_combinada <- tabla_pobreza_13[3, ] %>%
  mutate(across(everything(), ~ paste(.x, tabla_pobreza_13[4, cur_column()], sep = " ")))


tabla_pobreza_13[3, ] <- fila_combinada
tabla_pobreza_13 <- tabla_pobreza_13[-4,]

nuevos_nombres <- tabla_pobreza_13[3, ]

tabla_pobreza_13 <- tabla_pobreza_13[-c(1:3), ]

colnames(tabla_pobreza_13) <- nuevos_nombres


tabla_pobreza_13 <- tabla_pobreza_13 |>
  janitor::clean_names()|>
  mutate(across(5:23, ~ as.numeric(.)))


tabla_pobreza<-tabla_pobreza_09|>
  bind_rows(tabla_pobreza_13,tabla_pobreza_15)|>
  mutate(CVE_MUN = ifelse(nchar(clave_municipio_na) < 3, paste0(strrep("0", 3 - nchar(clave_municipio_na)), clave_municipio_na), clave_municipio_na), 
         ENT = ifelse(clave_entidad_na %in% as.character(1:9),paste0("0", clave_entidad_na), clave_entidad_na), 
         CVEGEO = paste0(ENT, CVE_MUN))|>
  filter(CVEGEO %in% mun_zm$CVEGEO)

ptotal<-sum(tabla_pobreza$poblacion_total_na)

p_pobextrema<-sum(tabla_pobreza$c_pobreza_extrema1_a_b_poblacion)

p_pob_pextrema<-(p_pobextrema*100)/ptotal
p_pob_pextrema


#TEMA: SEGURIDAD----
##Tasa de delitos del fuero común por cada 100,000 habitantes
delitos <- read.csv("https://repodatos.atdt.gob.mx/all_data/sesnsp/5c0d5419-d22e-482f-973e-af843b9e06f4/IDM_NM_mar25.csv", 
                    sep = ",", 
                    encoding = "latin1", 
                    header = TRUE) |>
  mutate(Cve..Municipio = ifelse(nchar(Cve..Municipio) < 5,paste0(strrep("0", 5 - nchar(Cve..Municipio)), Cve..Municipio), Cve..Municipio)) |>
  rename(CVEGEO = Cve..Municipio) |>
  filter(CVEGEO %in% mun_zm$CVEGEO, Año == 2020) |>
  rowwise() |>
  mutate(delitos = sum(c_across(Enero:Diciembre), na.rm = TRUE)) |>
  ungroup() |>
  group_by(CVEGEO) |>
  summarise(delitos = sum(delitos, na.rm = TRUE)) |>
  left_join(ITER_MUN, by = "CVEGEO")


delitos_final<-delitos|>
  select(ZM, CVEGEO,P_18YMAS, delitos)|>
  group_by(ZM)|>
  summarise(delitos_total = sum(delitos), 
            po18mas = sum(P_18YMAS, na.rm = TRUE))|>
  mutate(tdelitos = (delitos_total / po18mas)* 100000)

delitos_final


#TEMA:MEDIO AMBIENTE----
##Concentración media anual máxima de CO en la atmósfera
#https://code.earthengine.google.com/9350a0d3f14fa5c32bcfa35f2044d935

##Porcentaje de municipios de la ZMMVM que cuentan con al menos un instrumento de política climática
url <- "https://cambioclimatico.gob.mx/estadosymunicipios/Descargas/D_Instrumentos_municipios.xlsx"

download.file(url, destfile = "D_Instrumentos_municipios.xlsx", mode = "wb")                                   

instrumentos_clima<-read_excel("D_Instrumentos_municipios.xlsx",
                               sheet = 2)|>
  mutate(CVEGEO = CLV_MUN, 
         INSTRUMENTOS = "Con al menos un instrumento de política climática")|>
  filter(CVEGEO %in% mun_zm$CVEGEO)|>
  select(CVEGEO, INSTRUMENTOS, TOTAL_INSTRUMENTOS)
  

pc_mun_con_insclima <- mun_zm|>
  left_join(instrumentos_clima, by = "CVEGEO")|>
  mutate(INSTRUMENTOS= ifelse(is.na(INSTRUMENTOS), "Sin instrumentos de política climática", INSTRUMENTOS))|>
  group_by(INSTRUMENTOS)|>
  summarise(total_municipios = n())|>
  mutate(porcentaje = round(total_municipios*100/sum(total_municipios),2))

pc_mun_con_insclima
  

##SUPERFICIE CONTAMINADA NO COINCIDEN LOS DATOS
# contaminados<-read.csv("https://www.datos.gob.mx/dataset/e64bdd35-793a-4601-a17f-30c643b74f1a/resource/3279942f-a39e-4556-80ab-7d0b8813b2e5/download/sitios_contaminados_geoportal.csv", 
#                        sep = ",",
#                        encoding = "UTF-8", 
#                        header = TRUE)|>
#   st_as_sf(coords = c("longitud", "latitud"), crs = 4326)|>
#   st_intersection(polignos_municipios)|>
#   st_drop_geometry()|>
#   as.data.frame()|>
#   filter(cuenta_programa_remediacion_aprobado == "Si")|>
#   group_by(anio_identificacion)|>
#   summarise(sup_contaminada =sum(fn_area_suelo_contaminado_m, na.rm = TRUE))|>
#   rename(año = anio_identificacion)
# 
# 
# ##SUPERFICIE REMEDIADA
# remediada<-read.csv("https://www.datos.gob.mx/dataset/e64bdd35-793a-4601-a17f-30c643b74f1a/resource/5194cd4a-35df-448f-ac83-69298fbc5b85/download/sitios_remediados_geoportal.csv", 
#                        sep = ",",
#                        encoding = "UTF-8", 
#                        header = TRUE)|>
#   st_as_sf(coords = c("longitud", "latitud"), crs = 4326)|>
#   st_intersection(polignos_municipios)|>
#   st_drop_geometry()|>
#   as.data.frame()|>
#   group_by(anio_identificacion_sitio_contaminado)|> #group_by(anio_conclusion_remediacion)|>
#   summarise(sup_remediada =sum(fn_area_suelo_remediado_m, na.rm = TRUE))|>
#   rename(año = anio_identificacion_sitio_contaminado)
# 
# 
# #PORCENTAJE DE SUPERFICIE CONTAMINADA REMEDIADA POR AÑO DE IDENTIFICACIÓN DEL SITIO CONTAMINADO
# p_remediada<-remediada|>
#   left_join(contaminados, by = "año")|>
#   mutate(porcentaje_remediada = sup_remediada*100/sup_contaminada)
# 
# 
# p_remediada




##Porcentaje de superficie de áreas de conservación ambiental con conectividad de área natural muy alta. 
##http://www.conabio.gob.mx/informacion/gis/?vns=gis_root/biodiv/biatineco/biatieans/batieancod/hbnts7ctvgw


#TEMA: OCUPACIÓN Y EMPLEO----
##Tasa de desempleo
PDESOCUP<-ITER_MUN|>
  group_by(ZM)|>
  summarise(PDESOCUPADA = sum(PDESOCUP, na.rm = TRUE), 
            PEATOTAL= sum(PEA, na.rm = TRUE))|>
  mutate(tasa = round((PDESOCUPADA/PEATOTAL)*100,1))

PDESOCUP

##Porcentaje de población empleada en el sector informal

##SE OMITE ESTA PROPUESTA----
##Porcentaje de población ocupada que gana menos de un salario mínimo-
# ca_personas <- unzip('Censo2020_CA_cdmx_csv.zip', files = "Personas00.csv")
# 
# Personas <- read_csv("C:/Users/brenp/Documents/Censo2020_CA_eum_csv/Personas00.CSV", show_col_types = FALSE)|>
#   mutate(CVEGEO = paste0(ENT, MUN)) |>
#   filter(CVEGEO %in% mun_zm$CVEGEO,
#          INGTRMEN != 999999,
#          INGTRMEN != is.na(INGTRMEN))|>
#   select(CVEGEO, CONACT, ESTRATO, UPM, EDAD, INGTRMEN, FACTOR)|>
#   mutate(across(all_of(5:7), ~ as.numeric(., na.rm = TRUE)))|>
#   mutate(edades = ifelse(EDAD >=12 & EDAD<=130, 1, 0),
#          trabajo = ifelse(CONACT %in% c("10", "13", "14", "15", "16", "17", "18", "19", "20"), 1, 0),
#          salarios = ifelse(INGTRMEN < (123.22*30.4) & !is.na(INGTRMEN), 1, 0),
#          psalmin = ifelse(edades == 1 & trabajo ==1 & salarios == 1, 1, 0))%>%
#   filter(edades==1, trabajo==1)
# 
# dstrat <- Personas|>
#   as_survey_design(strata=ESTRATO,
#                    weights = FACTOR,
#                    ids=UPM,
#                    nest=TRUE)
# 
# options(survey.lonely.psu = "adjust")
# 
# porcentaje_psalar <- dstrat |>
#   group_by(psalmin) |>
#   summarise(psalariominimo = survey_total(vartype = "cv"),
#             porcentaje = survey_prop(vartype = "cv")) |>
#   filter(psalmin == 1) |>
#   mutate(PSALARIOSMINIMOS = porcentaje * 100)


#TEMA: INFRAESTRUCTURA----
##Porcentaje de PTAR que trabajan a menos del 60% de su capacidad
plantas<-read.csv("C:/Users/brenp/Downloads/Plantas tratamiento de agua residual_2022.csv", 
                  sep = ",",
                  encoding = "UTF-8", 
                  header = TRUE)|>
  mutate(CVE_MUN = ifelse(nchar(clvmun) < 3, paste0(strrep("0", 3 - nchar(clvmun)), clvmun), clvmun), 
         ENT = ifelse(clvedo %in% as.character(1:9),paste0("0", clvedo), clvedo), 
         CVEGEO = paste0(ENT, CVE_MUN))|>
  filter(CVEGEO %in% mun_zm$CVEGEO)|>
  mutate(prc_capacidad = caudal*100/capacidad, 
         subutilizadas = ifelse(prc_capacidad <= 60, 1,0))|>
  group_by(subutilizadas)|>
  summarise(total_en_sub = n())|>
  mutate(porcentaje = total_en_sub*100/sum(total_en_sub))

#Variación porcentual anual de la cantidad de residuos sólidos urbanos que ingresan a rellenos sanitarios


#TEMA: MOVILIDAD----
##Porcentaje de población que tarda una hora o más en llegar a su trabajo o centro de estudio
ca_personas <- unzip('Censo2020_CA_cdmx_csv.zip', files = "Personas00.csv")

Personas <- read_csv("C:/Users/brenp/Documents/Censo2020_CA_eum_csv/Personas00.CSV", show_col_types = FALSE) |>
  select(ENT, MUN, FACTOR, UPM, ESTRATO, ASISTEN, CONACT, ENT_PAIS_ASI, MUN_ASI, ENT_PAIS_TRAB, MUN_TRAB, TIE_TRASLADO_ESCU, TIE_TRASLADO_TRAB) |>
  mutate(MUN_orig = ifelse(!is.na(MUN) & nchar(MUN) < 3,
                      paste0(strrep("0", 3 - nchar(MUN)), MUN), MUN),
         MUN_TRAB = ifelse(!is.na(MUN_TRAB) & nchar(MUN_TRAB) < 3,
                      paste0(strrep("0", 3 - nchar(MUN_TRAB)), MUN_TRAB), MUN_TRAB),
         MUN_ASI = ifelse(!is.na(MUN_ASI) & nchar(MUN_ASI) < 3,
                     paste0(strrep("0", 3 - nchar(MUN_ASI)), MUN_ASI), MUN_ASI),
         ENT_PAIS_TRAB = ifelse(!is.na(ENT_PAIS_TRAB), substr(ENT_PAIS_TRAB, 2, 3), NA),
         ENT_PAIS_ASI = ifelse(!is.na(ENT_PAIS_ASI), substr(ENT_PAIS_ASI, 2, 3), NA),
         MUN_T_CVEGEO = ifelse(!is.na(ENT_PAIS_TRAB) & !is.na(MUN_TRAB),
                          paste0(ENT_PAIS_TRAB, MUN_TRAB), NA),
         MUN_A_CVEGEO = ifelse(!is.na(ENT_PAIS_ASI) & !is.na(MUN_ASI),
                          paste0(ENT_PAIS_ASI, MUN_ASI), NA),
         CVEGEO = paste0(ENT, MUN_orig),
         MUN_DESTINOS = case_when(is.na(MUN_A_CVEGEO) ~ MUN_T_CVEGEO,
                                  is.na(MUN_T_CVEGEO) ~ MUN_A_CVEGEO,
                                  TIE_TRASLADO_TRAB == "1" ~ MUN_A_CVEGEO,
                                  TIE_TRASLADO_ESCU == "1" ~ MUN_T_CVEGEO))
tiempos_traslado<- Personas |>
  filter(CVEGEO %in% mun_zm$CVEGEO, 
         MUN_DESTINOS %in% mun_zm$CVEGEO)|>
  mutate(tiempos = case_when(TIE_TRASLADO_TRAB %in% c("4", "5") ~ 1, 
                                         TIE_TRASLADO_ESCU %in% c("4", "5") ~ 1, 
                                         TRUE ~ 0))                            
  
options(survey.lonely.psu = "certainty")  

dm <- tiempos_traslado %>%
  as_survey_design(
    ids = UPM,
    strata = ESTRATO,
    weights = FACTOR, 
    nest=TRUE)


tot_tie_traslado <- dm |>
  group_by(tiempos)|>
  summarise(tot_tiempo = survey_total(vartype = "cv"),
            porcentaje = survey_prop(vartype = "cv")*100)

View(tot_tie_traslado)

#Porcentaje de municipios que implementaron acciones en materia de movilidad

accion_movilidad <- read_excel("C:/Users/brenp/Downloads/cngmd2021_gest_terr.xlsx",
                               sheet = 29, 
                               skip = 3) |>
  janitor::clean_names() |>
  slice(-c(1, 2))|>
  select(clave_geografica,se_implementaron_acciones_en_materia_de_movilidad_en_el_municipio_o_demarcacion_territorial)|>
  rename(CVEGEO =clave_geografica)|>
  filter(CVEGEO %in% mun_zm$CVEGEO)|>
  group_by(se_implementaron_acciones_en_materia_de_movilidad_en_el_municipio_o_demarcacion_territorial)|>
  summarise(programas = n())|>
  mutate(porcentaje = programas*100/sum(programas))



##TEMA: RESIDOS SOLIDOS----
#Porcentaje de municipios con más del 90% de población con acceso al servicio de recolección de residuos sólidos urbanos
pob_residuos <- read_excel("C:/Users/brenp/Downloads/CNGMD2019_M6.xlsx",
                               sheet = 52, 
                               skip = 4)|>
  janitor::clean_names()|>
  rename(CVEGEO =clave_geografica, 
         porcentaje = porcentaje_de_la_poblacion_con_acceso_al_servicio_de_recoleccion)|>
  mutate(CVEGEO = ifelse(!is.na(CVEGEO) & nchar(CVEGEO) < 5,
                        paste0(strrep("0", 6 - nchar(CVEGEO)), CVEGEO), CVEGEO))|>
  filter(CVEGEO %in% mun_zm$CVEGEO)|>
  mutate(cobertura = case_when(porcentaje >= 90 ~ "Con el 90% o más",
                                TRUE ~ "Con menos del 90%"))|>
  group_by(cobertura)|>
  summarise(municipios = n())|>
  mutate(porcentaje = municipios*100/sum(municipios))

#TEMA: GESTIÓN DEL RECURSO HÍDRICO----
##CALIDAD DEL AGUA----
leer_cali_agua <- function(anio)
  { 
  
  urls <- list("20" = "https://www.inegi.org.mx/contenidos/programas/enigh/nc/2016/microdatos/enigh2016_ns_concentradohogar_csv.zip",
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

  
}

