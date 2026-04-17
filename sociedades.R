# importar librerías ------------------------------------------------------

library(tidyverse)
library(nycflights13)
library(dplyr)
library(ggplot2)
library(lubridate)

getwd()
entidades <- read_csv("repo-curso-2026/hackaton/igj-entidades-202512.csv") |>
  mutate(
    descripcion_tipo_societario = factor(descripcion_tipo_societario )
    )

glimpse(entidades)

entidades |>
  filter(!is.na(cuit)) |>
  nrow()

domicilios <- read_csv("repo-curso-2026/hackaton/igj-domicilios-202512.csv") |>
  mutate(
    tipo_societario = factor(tipo_societario),
    descripcion_tipo_societario = factor(descripcion_tipo_societario ),
    tipo_domicilio = factor(tipo_domicilio),
    descripcion_tipo_domicilio = factor(descripcion_tipo_domicilio),
    localidad = factor(localidad),
    provincia = factor(provincia)
  )

glimpse(domicilios) 


balances <- read_csv("repo-curso-2026/hackaton/igj-balances-202512.csv") |>
  mutate(fecha_balance = ymd(fecha_balance),
         fecha_presentacion = ymd(fecha_presentacion),
         capital_informado = as.numeric(gsub(",", ".", capital_informado)),
         tipo_societario = factor(tipo_societario),
         descripcion_tipo_societario = factor(descripcion_tipo_societario)
         )

glimpse(balances)


# Descripcion Domicilios --------------------------------------------------

glimpse(domicilios)
levels(domicilios$provincia)
levels(domicilios$localidad)
#Son todos de capital federal



# union balances y entidades ----------------------------------------------

resultado <- entidades |>
  left_join(domicilios, by = "numero_correlativo") |>
  left_join(balances, by = "numero_correlativo") |>
  select(
    -tipo_societario.x,
    -tipo_societario,y,
    -descripcion_tipo_societario.x,
    -descripcion_tipo_societario.y) |>
  filter(!is.na(provincia))  |>
  filter(!is.na(fecha_balance))
glimpse(resultado |> filter(!is.na(fecha_balance)))




df <- resultado |>
  filter(!is.na(fecha_balance)) |>
  group_by(cuit) |>
  slice_max(fecha_balance, n = 1, with_ties = FALSE) |>
  ungroup()

capital_por_provincia <- df |>
  group_by(provincia) |>
  summarise(capital_total = sum(capital_informado, na.rm = TRUE)) |>
  arrange(desc(capital_total))

ggplot(capital_por_provincia, aes(x = reorder(provincia, capital_total), y = capital_total)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Concentración de capital por provincia",
    x = "Provincia",
    y = "Capital total"
  )


capital_por_tipo <- df |>
  group_by(descripcion_tipo_societario) |>
  summarise(capital_total = sum(capital_informado, na.rm = TRUE)) |>
  arrange(desc(capital_total))

ggplot(capital_por_tipo, aes(x = reorder(descripcion_tipo_societario, capital_total), y = capital_total)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Capital por tipo societario",
    x = "Tipo societario",
    y = "Capital total"
  )

# autoridades <- read_csv("repo-curso-2026/hackaton/igj-autoridades-muestreo.csv")
# 
# 
# 
# glimpse(autoridades)
# 
# asambleas <- read_csv("repo-curso-2026/hackaton/igj-asambleas-muestreo.csv")
# 
# glimpse(asambleas)