# ________________________________________________________________________ #
# MOOC BOSQUE DIGITAL - Análisis Estadístico de Datos Espaciales con R
#
# - Autor: Adrián Cidre González
# - https://adrian-cidre.com
#
# SEMANA 02 - Areal Data
# - Tema 07: Práctica en R
# ________________________________________________________________________ #

# 1. Cargar paquetes ------------------------------------------------------

library(pacman)

p_load(tidyverse, sf, sfdep, ggtext, mapview, leafpop)

# 2. Cargar datos ---------------------------------------------------------

## Cargar incendios Asturias
## - Nombre objeto: incendios_sf
## - layer: asturias o andalucia
incendios_sf <- read_sf(
  dsn   = "00_datos/incendios_ccaa.gpkg",
  layer = "andalucia"
)

# 3. Preparar datos -------------------------------------------------------

## Seleccionar columnas necesarias
## - Municipio, Total_incendios
incendios_sf <- select(incendios_sf, Municipio, Total_incendios)

## Transformar a EPSG:25830
incendios_sf <- st_transform(incendios_sf, "epsg:25830")

## Calcular densidad de incendios
## - Crear columna de area en km^2
## - Crear columna de d_incendios
incendios_sf <- incendios_sf %>%
  mutate(
    area        = st_area(incendios_sf) %>% units::set_units(km^2),
    d_incendios = as.numeric(Total_incendios / area),
    .before     = 3
  )

# 4. Visualización --------------------------------------------------------

## Histograma de distribución de d_incendios
## - Sigue una distribución normal?
incendios_sf %>%
  ggplot(aes(x = Total_incendios)) +
  geom_histogram()

## Visualizar mapa de coropletas de la variable d_incendios
incendios_sf %>%
  ggplot(aes(fill = cut_number(d_incendios, 2))) +
  geom_sf(color = "transparent", linewidth = 0) +
  scale_fill_viridis_d() +
  theme_void() +
  labs(fill = "Incendios / km^2^") +
  theme(legend.title = element_markdown())

# 5. Vecinos y pesos ------------------------------------------------------

## Asignar vecinos y pesos
## - Nombre objeto: incendios_wt_sf
## - Vecinos: contiguos (nb)
## - Utilizar método estandarizado por filas (wt)
incendios_wt_sf <- incendios_sf %>%
  mutate(
    nb = st_contiguity(incendios_sf),
    wt = st_weights(nb, style = "W"),
    .before = 2
  )

## Cuántos vecinos se han generado en total?
## Cuál es la densidad media de vecinos por región?
summary(incendios_wt_sf$nb)

# 6. Análisis de autocorrelación ------------------------------------------

## 6.1. Moran's I Global -----------------------

### Calcular I de Moran Global
### - Nombre objeto: incendios_moran
### - Utilizar 2000 simulaciones de Monte Carlo
### - Alternativa greater
incendios_moran <- global_moran_perm(
  x           = incendios_wt_sf$d_incendios,
  nb          = incendios_wt_sf$nb,
  wt          = incendios_wt_sf$wt,
  alternative = "greater",
  nsim        = 2000
)

### Visualizar y contestar
### - Cuál es el p-valor?
### - Para un nivel de significación del 0.05, cuál es nuestra conclusión?
plot(incendios_moran)
incendios_moran

## 6.2. Moran's I Local ------------------------

### Realizar el test de Moran Local
### - Nombre objeto: incendios_moran_sf
### - Utilizar alternative two.sided
### - Desanidar resultados
set.seed(137)
incendios_moran_sf <-
  incendios_wt_sf %>%
  mutate(
    moran_i = local_moran(
      x           = d_incendios,
      nb          = nb,
      wt          = wt,
      alternative = "two.sided"
    )
  ) %>%
  unnest(moran_i) %>%
  select(-nb, -wt)

### Explorar resultados
glimpse(incendios_moran_sf)

### Preparar para visualizar
### - Nombre objeto: incendios_final_sf
### - Convertir a "Insignificant" las observaciones de la columna pysal,
###   cuyo p_folded_sim sea superior a 0.01. Además convertir a factor
incendios_final_sf <-
  incendios_moran_sf %>%
  mutate(
    pysal = if_else(
      p_folded_sim >= 0.01, "Insignificant", pysal
    ) %>%
      as.factor() %>%
      fct_relevel(
        # c("Low-Low", "High-High", "Insignificant")
        c("Low-Low", "Low-High", "High-Low","High-High", "Insignificant")
      )
  )

### Explorar nivel de la columna pysal
### - Reordenarlos de una forma lógica en el código previo
levels(incendios_final_sf$pysal)

### Visualizar clasificación final con mapview
mapview(
  incendios_final_sf,
  zcol        = "pysal",
  layer.name  = "Valores",
  # col.regions = c("#0A2239", "#C41E3D", "snow"),
  col.regions = c("#0A2239", "#B2ECE1","#F67E7D","#C41E3D", "snow"),
  popup       = popupTable(
    x    = incendios_final_sf,
    zcol = c("Municipio", "Total_incendios", "d_incendios")
  )
)
