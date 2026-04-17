

library(sf)
library(ggplot2)


#https://cdn.buenosaires.gob.ar/datosabiertos/datasets/innovacion-transformacion-digital/barrios/barrios.geojson

#descarga de datos de los barrios de CABA
caba <- st_read( "CS_DE_DATOS/Repo-curso-2026C1/barrios.geojson.txt")

ggplot(data = caba) +
  geom_sf()
