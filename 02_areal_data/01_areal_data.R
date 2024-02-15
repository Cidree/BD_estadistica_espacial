# ________________________________________________________________________ #
# MOOC BOSQUE DIGITAL - Análisis Estadístico de Datos Espaciales con R
#
# - Autor: Adrián Cidre González
# - https://adrian-cidre.com
#
# SEMANA 02 - Areal Data
# - Tema 05: Introducción a datos de área
# - Tema 06: Autocorrelación espacial
# ________________________________________________________________________ #

# 1. Cargar paquetes ------------------------------------------------------

library(pacman)

p_load(tidyverse, sf, sfdep, ggtext, mapview, leafpop)

# 2. Cargar datos ---------------------------------------------------------

## Cargar incendios Galicia
incendios_sf <- read_sf(
  dsn   = "00_datos/incendios_ccaa.gpkg",
  layer = "galicia"
)

# 3. Preparar datos -------------------------------------------------------

## Seleccionar solamente municipios y total incendios
incendios_sf <- select(incendios_sf, Municipio, Total_incendios)

## Transformar a EPSG:25829
incendios_sf <- st_transform(incendios_sf, "epsg:25829")

## Densidad de incendios?
incendios_sf <- incendios_sf %>%
  mutate(
    area        = st_area(incendios_sf) %>% units::set_units(km^2),
    d_incendios = as.numeric(Total_incendios / area),
    .before     = 3
  )

# 4. Visualizar -----------------------------------------------------------

## Histogramas incendios
incendios_sf %>%
  ggplot(aes(x = d_incendios)) +
  geom_histogram()

## Visualizar valores absolutos
incendios_sf %>%
  ggplot(aes(fill = Total_incendios)) +
  geom_sf() +
  scale_fill_viridis_c() +
  theme_void() +
  labs(fill = "Total incendios")

## Visualizar valores absolutos discretizados
incendios_sf %>%
  ggplot(aes(fill = cut_number(Total_incendios, 5))) +
  geom_sf() +
  scale_fill_viridis_d() +
  theme_void() +
  labs(fill = "Total incendios")

## Visualizar incendios/km2
incendios_sf %>%
  ggplot(aes(fill = cut_number(d_incendios, 5))) +
  geom_sf() +
  scale_fill_viridis_d() +
  theme_void() +
  labs(fill = "Incendios / km^2^") +
  theme(legend.title = element_markdown())

# 5. Vecinos y pesos ------------------------------------------------------

## 5.1. Proximidad: adyacente ----------------

### Detectar vecinos y pesos (estilo estandarizado por fila)
incendios_sf %>%
  mutate(
    nb = st_contiguity(incendios_sf),
    wt = st_weights(nb, style = "W"),
    .before = 2
  )

### Resumen análisis de proximidad
summary(
  st_contiguity(incendios_sf)
)

### Solucionar problema
incendios_sf <- incendios_sf[-312,]

### Detectar vecinos y pesos (estilo estandarizado por fila)
incendios_adyacencia_sf <- incendios_sf %>%
  mutate(
    nb = st_contiguity(incendios_sf),
    wt = st_weights(nb, style = "W"),
    .before = 2
  )

incendios_adyacencia_sf %>%
  unnest(c(nb, wt))

### Resumen análisis de proximidad
summary(incendios_adyacencia_sf$nb)

### Gráfico de vecinos
plot(
  st_geometry(incendios_adyacencia_sf)
)
plot(
  incendios_adyacencia_sf$nb,
  coords = st_geometry(incendios_adyacencia_sf),
  add    = TRUE
)

## 5.2. Proximidad: knn ------------------------

### Detectar vecinos y pesos
### - 5 vecinos más cercanos
### - Método pesos: distancia inversa
incendios_knn_sf <- incendios_sf %>%
  mutate(
    nb = st_knn(
      incendios_sf %>% st_geometry(),
      k = 5
    ),
    wt = st_inverse_distance(
      nb = nb,
      geometry = incendios_sf %>% st_geometry()
    ),
    .before = 2
  )

### Resumen análisis de proximidad
incendios_knn_sf$nb %>% summary()

incendios_knn_sf %>%
  unnest(c(nb, wt))

## 5.3. Lags -----------------------------------

## Aplicar lag 2
incendios_lag_sf <- incendios_sf %>%
  mutate(
    ## Vecinos
    nb = st_contiguity(incendios_sf),
    nb_2 = st_nb_lag(nb, order = 2),
    nb_cumul_2 = st_nb_lag_cumul(nb, order = 2),
    ## Pesos
    wt = st_weights(nb),
    wt_2 = st_weights(nb_cumul_2),
    ## Lags
    incendios_lag1 = st_lag(d_incendios, nb, wt),
    incendios_lag2 = st_lag(d_incendios, nb_cumul_2, wt_2),
    .before = 2
  )

## Visualizar incendios con lag
incendios_lag_sf %>%
  pivot_longer(
    cols = contains("_lag")
  ) %>%
  ggplot(
    aes(fill = cut_number(value, 5))
  ) +
  geom_sf(linewidth = 0) +
  scale_fill_viridis_d() +
  labs(fill = "Incendios / km^2^") +
  theme_void() +
  facet_wrap(~ name) +
  theme(
    legend.title = element_markdown()
  )

# 6. Moran's I Global -----------------------------------------------------

## 6.1. Vecinos adyacentes -------------------

### Moran's I test
global_moran_test(
  x  = incendios_adyacencia_sf$d_incendios,
  nb = incendios_adyacencia_sf$nb,
  wt = incendios_adyacencia_sf$wt
)

### Moran's I test con permutaciones de Monte Carlo
moran_global <- global_moran_perm(
  x  = incendios_adyacencia_sf$d_incendios,
  nb = incendios_adyacencia_sf$nb,
  wt = incendios_adyacencia_sf$wt,
  alternative = "greater",
  nsim        = 999
)

### Visualizar permutaciones de Monte Carlo
plot(
  moran_global,
  xlab = NULL,
  main = "Gráfico de densidad de las permutaciones de Monte Carlo"
)

abline(v = moran_global$statistic, col = "red", lwd = 2)

## 6.2. Knn ----------------------------------

### Moran's I test
global_moran_test(
  x = incendios_knn_sf$d_incendios,
  nb = incendios_knn_sf$nb,
  wt = incendios_knn_sf$wt
)

## 6.3. Lags -----------------------

### Moran's I test con permutaciones de Monte Carlo
global_moran_perm(
  x = incendios_lag_sf$d_incendios,
  nb = incendios_lag_sf$nb_cumul_2,
  wt = incendios_lag_sf$wt_2
)

# 7. Moran's I Local ------------------------------------------------------

## 7.1. Vecinos adyacentes -------------------

### Calcular Moran's I a dos colas
### - H0: ausencia de autocorrelación con los vecinos
### - H1: existencia de autocorrelación con los vecinos
set.seed(137)
incendios_adyacencia_moran_sf <-
  incendios_adyacencia_sf %>%
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

### Estructura
glimpse(incendios_adyacencia_moran_sf)

### Visualizar
mapview(
  incendios_adyacencia_moran_sf,
  zcol = "p_ii_sim",
  layer.name = "p-value",
  at = c(0, .01, 1)
)

### Visualizar clasificación
mapview(
  incendios_adyacencia_moran_sf,
  zcol = "pysal",
  col.regions = c("#0A2239", "#B2ECE1","#F67E7D","#C41E3D"),
  layer.name = "Valores",
  popup = popupTable(
    x = incendios_adyacencia_moran_sf,
    zcol = c("Municipio", "Total_incendios", "d_incendios")
  )
)

### Corrección de insignificantes


### Explorar niveles de la columna pysal


### Visualizar clasificación


## 7.2. Lag 2 -------------------------

### Calcular Moran's I
set.seed(137)
incendios_lag_2_moran_sf <-
  incendios_lag_sf %>%
  mutate(
    moran_i = local_moran(
      x           = d_incendios,
      nb          = nb_cumul_2,
      wt          = wt_2,
      alternative = "two.sided"
    )
  ) %>%
  unnest(moran_i) %>%
  select(-starts_with("nb"), -starts_with("wt")) %>%
  mutate(
    pysal = if_else(
      p_folded_sim >= 0.05, "Insignificant", pysal
    ) %>%
      as.factor() %>%
      fct_relevel(
        c("Low-Low", "Low-High", "High-Low","High-High", "Insignificant")
      )
  )

### Visualizar p-valores
mapview(
  incendios_lag_2_moran_sf,
  zcol       = "p_ii",
  layer.name = "p-value",
  at         = c(0, .01, 1)
)

### Visualizar clasificación
mapview(
  incendios_lag_2_moran_sf,
  zcol        = "pysal",
  layer.name  = "Valores",
  col.regions = c("#0A2239", "#B2ECE1","#F67E7D","#C41E3D", "snow"),
  popup       = popupTable(
    x    = incendios_lag_2_moran_sf,
    zcol = c("Municipio", "Total_incendios", "d_incendios")
  )
)

