
# HACKATÓN IGJ 2025: ANÁLISIS GEOGRÁFICO Y ECONÓMICO DE CAPITALES


# 1. CARGA DE LIBRERÍAS Y DATOS ------------------------------------------------

library(tidyverse) # Carga dplyr, ggplot2, readr, stringr y purrr
library(scales)    # Para formatos numéricos (puntos y comas)
library(lubridate) # Para manejo de fechas
library(dplyr)      # Para manipulación de datos
library(ggplot2)    # Para visualización de datos

# Importación de datasets oficiales
asambleas   <- read_csv("hackaton/igj-asambleas-202512.csv")
autoridades <- read_csv("hackaton/igj-autoridades-202512.csv")
balances    <- read_csv("hackaton/igj-balances-202512.csv")
domicilios  <- read_csv("hackaton/igj-domicilios-202512.csv")
entidades   <- read_csv("hackaton/igj-entidades-202512.csv")


# 2. LIMPIEZA DE "MOTOR" (PROCESAMIENTO BASE) ---------------------------------

# A. Limpieza de Balances: Conversión de moneda y filtrado de errores
balances_clean <- balances |> 
  mutate(
    # Transformamos el capital de texto ("1234,56") a número real
    capital_num = as.numeric(str_replace(capital_informado, ",", "."))
  ) |> 
  filter(!is.na(capital_num), capital_num > 0)

# B. Limpieza de Domicilios: Unificación de CABA y normalización de calles
domicilios_clean <- domicilios |> 
  mutate(
    # Consolidamos errores de carga (Sapital, Cap Fed, etc) en "CABA"
    localidad_clean = case_when(
      str_detect(localidad, "CAP|SAP|CDAD|CIUDAD|FED") ~ "CABA",
      is.na(localidad) ~ "CABA",
      TRUE ~ toupper(localidad)
    ),
    # Normalización de calles: sin números y en mayúsculas para agrupar
    calle_clean = calle |> 
      str_remove_all("[0-9]") |> 
      str_squish() |> 
      toupper()
  )


# 3. ANÁLISIS 1: DISTRIBUCIÓN POR TIPOS SOCIETARIOS ---------------------------

cap_por_tipo <- balances_clean |> 
  left_join(entidades |> select(numero_correlativo, descripcion_tipo_societario), 
            by = "numero_correlativo") |> 
  filter(!is.na(descripcion_tipo_societario.x)) |> 
  group_by(tipo = descripcion_tipo_societario.x) |> 
  summarize(total_capital = sum(capital_num, na.rm = TRUE)) |> 
  arrange(desc(total_capital)) |> 
  head(10)

grafico_tipos <- ggplot(cap_por_tipo, aes(x = reorder(tipo, total_capital), y = total_capital)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  scale_y_continuous(labels = label_comma(big.mark = ".", decimal.mark = ",")) +
  labs(title = "Top 10 Capital por Tipo Societario", x = "Sociedad", y = "Capital Total (ARS)") +
  theme_light()


# 4. ANÁLISIS 2: GEOGRAFÍA URBANA (BARRIOS DE CABA) ---------------------------

domicilios_barrios <- domicilios_clean |> 
  mutate(
    barrio = case_when(
      str_detect(calle_clean, "FLORIDA|RECONQUISTA|SAN MARTIN|CORRIENTES|LAVALLE|MAIPU|SARMIENTO|25 DE MAYO|ALVEAR") ~ "MICROCENTRO / RETIRO",
      str_detect(calle_clean, "LIBERTADOR|FIGUEROA ALCORTA|QUINTANA|CALLAO|PUEYRREDON|SANTA FE") ~ "RECOLETA / PALERMO",
      str_detect(calle_clean, "PUERTO MADERO|ALICIA MOREAU|JUANA MANSO|DELLAPIANE") ~ "PUERTO MADERO",
      str_detect(calle_clean, "CABILDO|OBLIGADO|PAMPAS|VERTIZ|SUCRE|LA PAMPA") ~ "BELGRANO / NUÑEZ",
      str_detect(calle_clean, "RIVADAVIA|YERBAL|RIVERO|AV. LA PLATA|CAMPANA") ~ "CABALLITO / FLORES",
      str_detect(calle_clean, "CASEROS|BRASIL|PATRICIOS|ALCORTA|CHICLANA") ~ "P. PATRICIOS / BARRACAS",
      str_detect(calle_clean, "CORRIENTES|BOEDO|INDEPENDENCIA|SAN JUAN|ENTRE RIOS") ~ "ALMAGRO / BOEDO",
      str_detect(calle_clean, "TRIUNVIRATO|ALVAREZ THOMAS|FOREST|ELCANO") ~ "CHACARITA / V. URQUIZA",
      TRUE ~ NA_character_ 
    )
  )

cap_por_barrio_clean <- balances_clean |> 
  left_join(domicilios_barrios |> select(numero_correlativo, barrio), by = "numero_correlativo") |> 
  filter(!is.na(barrio)) |> 
  group_by(barrio) |> 
  summarize(total_capital = sum(capital_num, na.rm = TRUE), cantidad_empresas = n()) |> 
  arrange(desc(total_capital))

grafico_barrios <- ggplot(cap_por_barrio_clean, aes(x = reorder(barrio, total_capital), y = total_capital)) +
  geom_col(fill = "#004d99") + 
  coord_flip() +
  scale_y_continuous(labels = label_comma(big.mark = ".", decimal.mark = ",")) +
  labs(
    title = "Ranking de Capital Acumulado por Barrio",
    subtitle = "Basado en sedes sociales (Jurisdicción IGJ)",
    x = "Zona / Barrio", y = "Capital Total (ARS)",
    caption = "Nota: Clasificación mediante análisis de ejes viales principales."
  ) +
  theme_minimal() +
  theme(axis.text = element_text(size = 10, face = "bold"), title = element_text(size = 12))


# 5. ANÁLISIS 3: ACTIVIDAD DE ASAMBLEAS POST-PANDEMIA -------------------------

asambleas_recientes <- asambleas |> 
  mutate(fecha = ymd(fecha_realizacion)) |> 
  filter(fecha >= ymd("2020-01-01")) |> 
  left_join(domicilios_barrios |> select(numero_correlativo, barrio), by = "numero_correlativo") |> 
  filter(!is.na(barrio)) |> 
  group_by(barrio) |> 
  summarize(cantidad_asambleas = n())

grafico_asambleas <- ggplot(asambleas_recientes, aes(x = reorder(barrio, cantidad_asambleas), y = cantidad_asambleas)) +
  geom_col(fill = "#009999") +
  coord_flip() +
  labs(title = "Cumplimiento Institucional: Asambleas por Barrio",
       subtitle = "Registros totales realizados entre 2020 y 2025",
       x = "Barrio", y = "Número de Asambleas") +
  theme_minimal()


# 6. SALIDA DE RESULTADOS FINAL -----------------------------------------------

# Visualizar gráficos
print(grafico_tipos)
print(grafico_barrios)
print(grafico_asambleas)

# Resumen numérico final combinado
print("--- RESUMEN FINAL: CAPITAL Y ACTIVIDAD POR BARRIO ---")
cap_por_barrio_clean |> 
  left_join(asambleas_recientes, by = "barrio") |> 
  arrange(desc(total_capital)) |> 
  print()

# FIN DEL SCRIPT
