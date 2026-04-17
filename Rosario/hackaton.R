library(tidyverse)
entidades=read_csv(file="Hackaton/igj-entidades-muestreo.csv")
domicilios=read_csv(file="Hackaton/igj-domicilios-muestreo.csv")
balances=read_csv(file="Hackaton/igj-balances-muestreo.csv")
autoridades=read_csv(file="Hackaton/igj-autoridades-muestreo.csv")
asambleas=read_csv(file="Hackaton/igj-asambleas-muestreo.csv")
codigos_postales <- read_csv2(file="Hackaton/codigos-postales-argentina.csv",  locale = locale(encoding = "Latin1")
)

# claves primarias --------------------------------------------------------

entidades |> 
  count(numero_correlativo) |> 
  filter(n>1)

entidades |> 
  filter(is.na(numero_correlativo))

domicilios |> 
  count(numero_correlativo) |> 
  filter(n>1)

domicilios |> 
  filter(is.na(numero_correlativo))

autoridades |> 
  count(numero_correlativo,apellido_nombre) |> 
  filter(n>1)

autoridades |> 
  filter(is.na(numero_correlativo)| is.na(apellido_nombre))

asambleas |> 
  distinct()
#hay registros repetidos en asambleas

#nos quedamos con los registros únicos
asambleas <- asambleas |> 
  distinct()

#clave primaria de asambleas
asambleas |> 
  count(numero_correlativo,numero_asamblea, fecha_realizacion, fecha_presentacion) |> 
  filter(n>1)

asambleas |> 
  filter(is.na(numero_correlativo)| is.na(numero_asamblea) | is.na(fecha_realizacion) |
           is.na(fecha_presentacion))
#hay varios numero_asamblea NA
#clave primaria de codigos_postales
codigos_postales |> 
  count(CP, Localidad) |> 
  filter(n>1)
#hay localidades que comparten código postal

codigos_postales |> 
  filter(is.na(CP)| is.na(Localidad))

balances |> 
  count(numero_correlativo,tipo_societario,razon_social,fecha_balance,fecha_presentacion)|> 
  filter (n>1)

balances |> 
  filter(is.na(numero_correlativo)| is.na(tipo_societario) |
           is.na(razon_social) |is.na(fecha_balance) |is.na(fecha_presentacion))

#¿dónde se concentran las empresas por capital informado?
#¿Qué tipos societarios concentran los capitales más altos?

empresas_con_mayor_capital <- balances |> 
  left_join(entidades, join_by("numero_correlativo"=="numero_correlativo")) |> 
  select(numero_correlativo,capital_informado,tipo_societario.x,descripcion_tipo_societario.x) |> 
  arrange(desc(capital_informado)) 
  
#ubicación de esas empresas

ubicacion_mayor_capital <- empresas_con_mayor_capital |> 
  inner_join(domicilios, join_by("numero_correlativo"=="numero_correlativo"))


empresas_con_mayor_capital |> 
  inner_join(domicilios, by = "numero_correlativo") |> 
  nrow() #no coinciden

#join entre domicilios y codigos_postales
barrios <- domicilios |> 
  inner_join(codigos_postales,join_by("codigo_postal"=="CP"))

#¿Dónde se ubica la mayor cantidad de sociedades dentro de CABA?

top_ubicaciones <- barrios |> 
  count( Localidad, name = "cantidad_de_sociedades") |>#agrupo por localidades
  mutate(Localidad = gsub("CABA - ", "", Localidad)) |> 
  arrange(desc(cantidad_de_sociedades)) 
#gráfico
top_10_ubicaciones <- top_ubicaciones |> 
  slice_max(order_by = cantidad_de_sociedades, n = 10)
graf_top_ubicaciones <- ggplot(top_10_ubicaciones,aes(y=reorder(Localidad, cantidad_de_sociedades), x=cantidad_de_sociedades))+
  geom_col()+labs(title="¿Dónde se ubica la mayor cantidad de sociedades dentro de CABA?",
                  subtitle="Top 10 localidades con mayor cantidad de sociedades registradas",
                  x="Cantidad de sociedades",y="Barrios")

graf_top_ubicaciones 
#fuente:https://www.coordenadas.com.es/argentina/pueblos-de-ciudad-buenos-aires/7/1
#dataset con longitud y latitud de los barrios
barrios_coords <- data.frame(
  Localidad = c(
    "San Nicolas","Palermo","Recoleta","Belgrano","Nuñez","Balvanera","Monserrat",
    "Caballito","Colegiales","Flores","Retiro","Saavedra","Velez Sarfield",
    "Villa Gral. Mitre","Villa Urquiza","Constitucion","Liniers","Zona Puerto",
    "Villa Ortuzar","Mataderos","Parque Chacabuco","Villa Devoto","Almagro",
    "Chacarita","San Cristobal","Villa Crespo","Nueva Pompeya","San Telmo",
    "Villa Lugano","Boedo","Barracas","La Boca","Parque Patricios"
  ),
  
  lat = c(
    -34.6037,-34.5889,-34.5873,-34.5627,-34.5431,-34.6090,-34.6120,
    -34.6200,-34.5730,-34.6340,-34.5910,-34.5540,-34.6370,
    -34.6120,-34.5700,-34.6270,-34.6470,-34.6030,
    -34.5790,-34.6550,-34.6350,-34.6000,-34.6100,
    -34.5880,-34.6220,-34.6000,-34.6450,-34.6210,
    -34.6850,-34.6220,-34.6400,-34.6350,-34.6440
  ),
  
  lon = c(
    -58.3815,-58.4305,-58.3915,-58.4560,-58.4630,-58.4050,-58.3810,
    -58.4430,-58.4490,-58.4630,-58.3730,-58.4900,-58.5030,
    -58.4710,-58.4850,-58.3810,-58.5280,-58.3700,
    -58.4570,-58.5080,-58.4330,-58.5140,-58.4200,
    -58.4550,-58.3960,-58.4400,-58.4150,-58.3730,
    -58.5150,-58.4120,-58.3770,-58.3640,-58.3950
  )
)


top_coords <- top_ubicaciones |> 
  inner_join(barrios_coords,join_by("Localidad"=="Localidad"))
mapa_graf1 <- ggplot(top_coords, aes(x = lon, y = lat)) +
  geom_point(aes(size = cantidad_de_sociedades, color = cantidad_de_sociedades)) +
  coord_quickmap(xlim = c(-58.55, -58.35),
                 ylim = c(-34.70, -34.53)) +
  labs(
    title = "Zonas de CABA con mayor cantidad de sociedades registradas",
    x = "Longitud",
    y = "Latitud",
    color = "Cantidad de sociedades",
    size = "Cantidad de sociedades"
  )

mapa_graf1 #rarooooo

#¿Qué tipos societarios concentran los capitales más altos?

tipos_societarios_cap <- empresas_con_mayor_capital |>
  mutate(capital_informado = as.numeric(gsub(",", ".", capital_informado))) |>
  group_by(tipo_societario.x,descripcion_tipo_societario.x) |> 
  summarize(suma_capital=sum(as.numeric(capital_informado,na.rm=TRUE))) |> 
  filter(suma_capital>0) |> 
  arrange(desc(suma_capital))
glimpse(empresas_con_mayor_capital)

grafico_tipos_societarios <- ggplot(tipos_societarios_cap,
                                    aes(y=reorder(descripcion_tipo_societario.x,suma_capital),x=suma_capital))+geom_col()+
  labs(title="Tipos societarios con mayor capital",
       x="Capital total", y="Tipo societario")
grafico_tipos_societarios




