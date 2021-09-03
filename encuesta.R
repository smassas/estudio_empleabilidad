library(readr)
library(tidyverse)
library(janitor)
library(highcharter)
library(tidyr)
library(stringr)

# Cargar base
data <- readr::read_csv(file = "datos/multipleChoiceResponses.csv", col_names = TRUE)

# Vista de la base datos
View(data)

# Encuesta año 2017 -------------------------------------------------------

data_chile <- data %>% dplyr::filter(Country == "Chile")
data_chile <- data_chile %>% janitor::clean_names()

# Nombres de la base de datos
x <- as.vector(names(data_chile))
y <- seq(1, length(x), by = 1)
z <- sapply(data_chile, class)

variables <- data.frame(Nombre = x, Posición = y, Tipo_variable = z) 
row.names(variables) <- NULL
variables

# Rescatar etiquetas
etiquetas <- apply(data_chile, 2, unique)

labels <- c()
for (i in 1:length(etiquetas)) {
  labels[[i]] <- print(etiquetas[[i]])
}
labels

# Convertir a data.frame
data.frame(t(sapply(etiquetas, c))) %>% View()

# Convertir a tibble con los valores en lista.
etiquetas_tibble <- tibble::enframe(etiquetas)
etiquetas_tibble$value

# Sexo
data_chile %>% 
  dplyr::group_by(gender_select) %>% 
  dplyr::count()

# Promedio edad
data_chile %>% 
  dplyr::group_by(gender_select) %>% 
  dplyr::summarise(promedio_edad = mean(age, na.rm = TRUE),
                   mediana_edad = median(age, na.rm = TRUE),
                   sd = sd(age, na.rm = TRUE),
                   Q1 = quantile(age, probs = 0.25, na.rm = TRUE),
                   Q3 = quantile(age, probs = 0.75, na.rm = TRUE))

min(data_chile$age[data_chile$gender_select == "Male"])
max(data_chile$age[data_chile$gender_select == "Male"])
min(data_chile$age[data_chile$gender_select == "Female"])
max(data_chile$age[data_chile$gender_select == "Female"])

# Lenguajes de programación que recomiendan

data_chile %>% 
  dplyr::select(gender_select, age, starts_with(match = "ML"),
                language_recommendation_select) %>% 
  dplyr::group_by(language_recommendation_select) %>% 
  dplyr::count() %>% 
  tidyr::drop_na() %>% 
  dplyr::arrange(desc(n))

# Empleabilidad -----------------------------------------------------------

# Tipo empleabilidad actual.

data_chile %>% 
  dplyr::select(employment_status, current_employer_type) %>% 
  dplyr::group_by(current_employer_type) %>% 
  dplyr::count() %>% 
  dplyr::mutate(porcentaje = n/nrow(data_chile)*100) %>% 
  tidyr::drop_na() %>% 
  dplyr::arrange(desc(n))

# A qué se dedican los DS.

data_chile %>% 
  dplyr::select(current_job_title_select, gender_select) %>%
  dplyr::group_by(current_job_title_select, gender_select) %>% 
  dplyr::count() %>% 
  tidyr::drop_na() %>% 
  highcharter::hchart("column", hcaes(x = current_job_title_select, y = n, group = gender_select)) %>% 
  highcharter::hc_tooltip(crosshairs = TRUE) %>% 
  highcharter::hc_add_theme(hc_theme_hcrt()) %>% 
  highcharter::hc_title(text = "CAMPO OCUPACIONAL EN CHILE",
                        style = list(fontWeight = "bold", fontSize = "15px",
                                     fontFamily = "Oswald"))

# Cómo encuentran empleo

data_chile %>% 
  dplyr::select(employer_search_method) %>% 
  dplyr::group_by(employer_search_method) %>% 
  dplyr::summarise(n = n()) %>% 
  dplyr::arrange(desc(n)) %>% 
  tidyr::drop_na() %>% 
  highcharter::hchart("bar", hcaes(x = employer_search_method, y = n)) %>% 
  hc_tooltip(crosshairs = TRUE) %>% 
  hc_add_theme(hc_theme_economist())

# Área de la industria

data_chile %>% 
  dplyr::select(employer_industry) %>% 
  tidyr::drop_na() %>% 
  dplyr::group_by(employer_industry) %>% 
  dplyr::count() %>% 
  dplyr::arrange(desc(n))

# Importancia de factores en el trabajo

factores_trabajo <- data_chile %>% 
  dplyr::select(contains(match = "job_factor")) %>% 
  tidyr::pivot_longer(cols = starts_with(match = "job_factor"),
                      names_to = "Variable",
                      values_to = "Tipo") %>% 
  tidyr::drop_na() %>% 
  dplyr::distinct() %>% 
  dplyr::mutate(Tipo = "Likert importancia 5 niveles",
                Naturaleza = "Ordinal cualitativa") 

# Capacitación ------------------------------------------------------------

# Dónde prefieren autocapacitarse.

data_chile %>% 
  dplyr::select(learning_platform_select) %>% 
  tidyr::separate(col = learning_platform_select, into = paste("x", seq(1, 10), 
                  sep = ""), sep = ",") %>% 
  tidyr::pivot_longer(cols = starts_with(match = "x"), 
                      names_to = "x", 
                      values_to = "Plataforma",
                      values_drop_na = TRUE) %>% 
  dplyr::select(-x) %>% 
  dplyr::group_by(Plataforma) %>% 
  dplyr::summarise(n = n()) %>% 
  dplyr::mutate(ranking = min_rank(desc(n))) %>% 
  dplyr::arrange(ranking) %>% 
  highcharter::hchart("bar", hcaes(x = Plataforma, y = n)) %>% 
  highcharter::hc_tooltip(crosshairs = TRUE) %>% 
  highcharter::hc_add_theme(hc_theme_economist()) 

# Primer entrenamiento o acercamiento.

data_chile %>% 
  dplyr::select(first_training_select) %>% 
  dplyr::group_by(first_training_select) %>% 
  dplyr::count() %>% 
  dplyr::arrange(desc(n)) %>% 
  tidyr::drop_na()

# Utilidad de aprendizajes.

utilidad_aprendizajes <- data_chile %>% 
  dplyr::select(starts_with(match = "learning_platform_usefulness_")) %>% 
  tidyr::pivot_longer(cols = starts_with("learning_platform_usefulness_"),
                      names_to = "Variable",
                      values_to = "Tipo") %>% 
  dplyr::mutate(Tipo = "Likert relevancia 5 niveles",
                Naturaleza = "Ordinal cualitativa") %>% 
  dplyr::distinct() 

# Trabajo -----------------------------------------------------------------

# Herramienta de lenguaje que utiliza en el trabajo.

data_chile %>% 
  dplyr::select(work_tools_select) %>% 
  tidyr::separate_rows(work_tools_select, sep = ",") %>% 
  dplyr::mutate_all(str_to_upper) %>% 
  dplyr::group_by(work_tools_select) %>% 
  dplyr::count() %>% 
  dplyr::distinct() %>% 
  tidyr::drop_na() %>% 
  dplyr::arrange(desc(n)) %>% 
  highcharter::hchart("bar", hcaes(x = work_tools_select, y = n)) %>% 
  highcharter::hc_tooltip(crosshairs = TRUE) %>% 
  highcharter::hc_add_theme(hc_theme_economist())

# Desafíos en el trabajo.

data_chile %>% 
  dplyr::select(work_challenges_select) %>% 
  tidyr::separate_rows(work_challenges_select, sep = ",") %>% 
  dplyr::mutate_all(str_to_upper) %>% 
  tidyr::drop_na() %>% 
  dplyr::group_by(work_challenges_select) %>% 
  dplyr::count() %>% 
  dplyr::mutate(work_challenges_select = stringr::str_trim(work_challenges_select),
                n = n/nrow(data_chile)*100) %>% 
  dplyr::filter(!work_challenges_select %in% c("I PREFER NOT TO SAY", "OTHER")) %>%  
  highcharter::hchart("pie", hcaes(x = work_challenges_select, y = n), innerSize = 50) %>% 
  highcharter::hc_title(text = "DESAFÍOS COMO DATA SCIENTIST EN CHILE",
           style = list(fontWeight = "bold", fontSize = "15px",
                        fontFamily = "Oswald")) %>% 
  highcharter::hc_add_theme(hc_theme_hcrt()) %>% 
  highcharter::hc_tooltip(valueDecimals = 2, crosshairs = TRUE) %>% 
  highcharter::hc_credits(enabled = TRUE, text = "",
             style = list(fontSize = "12px"))
  

# ¿Cuánto ocupan de visualización?

data_chile %>% 
  dplyr::select(work_data_visualizations, 
                current_job_title_select,
                time_visualizing) %>% 
  tidyr::drop_na() %>% 
  dplyr::group_by(work_data_visualizations) %>% 
  dplyr::count() %>% 
  hchart(
  "treemap",
  hcaes(work_data_visualizations, value = n, colorValue = n),
  borderColor = NA # elimina border y se tiene un aspecto más limpio imho
) %>% 
  hc_colorAxis(stops  = color_stops()) %>% 
  hc_title(text = "Porcentaje dedicado a la visualización de datos") %>% 
  hc_colorAxis(endOnTick = FALSE)

# Habilidades requeridas en ML.

data_chile %>% 
  dplyr::select(ml_skills_select) %>% 
  tidyr::drop_na() %>% 
  tidyr::separate(ml_skills_select, into = c("Habilidades"),
                  sep = ",") %>% 
  dplyr::group_by(Habilidades) %>% 
  dplyr::count() %>% 
  dplyr::filter(!stringr::str_detect(Habilidades, "Other")) %>% 
  dplyr::arrange(desc(n)) %>% 
  wordcloud2::wordcloud2(shape = "circle", size = 0.2)

# Técnicas requeridas en ML.

data_chile %>% 
  dplyr::select(ml_techniques_select) %>% 
  tidyr::drop_na() %>% 
  tidyr::separate(ml_techniques_select, into = c("Tecnicas"),
                  sep = ",") %>% 
  dplyr::group_by(Tecnicas) %>% 
  dplyr::count() %>% 
  dplyr::filter(!stringr::str_detect(Tecnicas, "Other")) %>% 
  dplyr::arrange(desc(n)) %>% 
  wordcloud2::wordcloud2(shape = "circle", size = 0.2)

# Funciones en el trabajo.

data_chile %>% 
  dplyr::select(job_function_select) %>% 
  tidyr::drop_na() %>% 
  dplyr::group_by(job_function_select) %>% 
  dplyr::count()

# Frecuencias de uso ------------------------------------------------------

# Herramientas del trabajo.

herramientas_trabajo <- data_chile %>% 
  dplyr::select(starts_with(match = "work_tools_frequency_")) %>% 
  tidyr::pivot_longer(cols = starts_with("work_tools_frequency_"),
               names_to = "Variable",
               values_to = "Tipo") %>% 
  dplyr::mutate(Tipo = "Likert frecuencia 5 niveles",
                Dimension = "Cuantitativa discreta") %>% 
  dplyr::distinct()

# Métodos estadísticos utilizados en el trabajo.

metodos_utilizados <- data_chile %>% 
  dplyr::select(starts_with(match = "work_methods_frequency_")) %>% 
  tidyr::pivot_longer(cols = starts_with("work_methods_frequency_"),
                      names_to = "Variable",
                      values_to = "Tipo") %>% 
  dplyr::mutate(Tipo = "Likert frecuencia 5 niveles",
                Naturaleza = "Cuantitativa discreta") %>% 
  dplyr::distinct() 

# Tiempo dedicado a determinadas funciones.

tiempo_funciones <- data_chile %>% 
  dplyr::select(starts_with(match = "time_")) %>% 
  dplyr::mutate_all(as.numeric) %>% 
  tidyr::pivot_longer(cols = starts_with("time_"),
                      names_to = "Variable",
                      values_to = "Tipo") %>% 
  dplyr::mutate(Tipo = "Cuantitativa",
                Naturaleza = "Ordinal discreta") %>% 
  dplyr::distinct() 

# Tiempo dedicado a los desafíos 

tiempo_desafios <- data_chile %>% 
  dplyr::select(starts_with(match = "work_challenge_frequency_")) %>% 
  dplyr::mutate_all(as.numeric) %>% 
  tidyr::pivot_longer(cols = starts_with("work_challenge_frequency_"),
                      names_to = "Variable",
                      values_to = "Tipo") %>% 
  dplyr::mutate(Tipo = "Likert frecuencia 5 niveles",
                Naturaleza = "Ordinal cualitativa") %>% 
  dplyr::distinct() 

# Guardar variables -------------------------------------------------------


lista_variables <- list(utilidad_aprendizajes,
              tiempo_desafios,
              tiempo_funciones,
              factores_trabajo,
              metodos_utilizados,
              herramientas_trabajo)

writexl::write_xlsx(list("TIEMPO_DESAFIOS" = lista_variables[[1]],
                         "TIEMPO_FUNCIONES" = lista_variables[[2]],
                         "FACTORES_TRABAJO" = lista_variables[[3]],
                         "METODOS_UTILIZADOS" = lista_variables[[4]],
                         "HERRAMIENTAS_TRABAJO" = lista_variables[[5]]),
                    path = "datos/lista_variables.xlsx")

