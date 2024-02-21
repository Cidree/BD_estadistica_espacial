# ________________________________________________________________________ #
# MOOC BOSQUE DIGITAL - Análisis Estadístico de Datos Espaciales con R
#
# - Autor: Adrián Cidre González
# - https://adrian-cidre.com
#
# SEMANAS 03 y 04 - Interpolación espacial
# - Tema 11: Métodos de geoestadística
# ________________________________________________________________________ #

# 1. Cargar paquetes ------------------------------------------------------

library(pacman)

p_load(
  tidyverse, sf, terra, stars, mapSpain,
  gstat, tidyterra, patchwork
)

# 2. Cargar datos ---------------------------------------------------------

## Datos España sin islas
spain_sf <- esp_get_ccaa() %>%
  filter(!str_detect(iso2.ccaa.name.es, "Canarias|Baleares|Ceuta|Melilla")) %>%
  st_union() %>%
  st_transform(25830)

## Datos clima 2024-01-01
tmax_sf <- read_sf("00_datos/clima.gpkg")

# 3. Explorar datos -------------------------------------------------------

## 3.1. Visualizar dependencia espacial ---------------

ggplot() +
  geom_sf(data = spain_sf) +
  geom_sf(data = tmax_sf,
          aes(fill = cut_number(tmax, 4), size = cut_number(tmax, 4)),
          shape = 21
  ) +
  scale_fill_manual(
    values = hcl.colors(4, "Oslo")
  ) +
  theme_void() +
  labs(fill = "Temp", size = "Temp",
       title = "Temperatura máxima 01-ene-2024")

## 3.2. Analizar normalidad ----------------

### Test de Kolmogorov-Smirnov


# 4. Semivariograma -------------------------------------------------------

## 4.1. Nube variograma -------------------------

## Calcular semivarianza


## Visualizar


## 4.2. Semivariograma -------------------------

## Obtener semivariograma de nuestros datos


### Visualizar


### Buscar valores óptimos


### Visualizar modelo


### Manual


### Visualizar modelo


# 5. Kriging ordinario ----------------------------------------------------

## 5.1. Definir grilla ----------------------------

### Definir tamaño de píxel


### Crear grilla


### Visualizar grilla


## 5.2. Predicciones -----------------------------

### Predicciones kriging ordinario


### Cargar objeto guardado
tmax_ko_sf <- read_sf(
  dsn   = "00_datos/outputs/interpolaciones.gpkg",
  layer = "ko"
)

### Rasterizar


### Cambiar nombre a las capas


## 5.3. Visualizar resultados --------------------

### Tema de ggplot
theme_set(
  theme_void() +
    theme(
      plot.title    = element_text(size = 14, hjust = 0.5, face = 'bold'),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      legend.title  = element_text(face = 'bold')
    )
)

### Predicciones


### Incertidumbre


### Resultado final


## 5.4. RMSE ------------------------------------

### 5-fold CV


### Obtener RMSE


# 6. Kriging universal ----------------------------------------------------

## 6.1. Añadir coordenadas -----------------------

### Coordenadas a tmax_sf


### Añadir coordenadas a grilla


## 6.2. Análisis de la tendencia -----------------

### Test de correlación de Pearson


### Relación coordenadas con tmax


### Modelo lineal


### Residuales normales?


### Residuales con media 0 y constante?


## 6.3. Semivariograma ---------------------------

### Obtener semivariograma de nuestros datos


### Visualizar


### Buscar valores óptimos


### Visualizar modelo


## 6.4. Predicciones -----------------------------

### Predicciones kriging universal


### Cargar objeto guardado
tmax_ku_sf <- read_sf(
  dsn   = "00_datos/outputs/interpolaciones.gpkg",
  layer = "ku"
)

### Rasterizar
tmax_ku_sr <- st_rasterize(
  sf = tmax_ku_sf[c("var1.pred", "var1.var")],
  dx = pixel_size,
  dy = pixel_size
) %>%
  rast()

### Cambiar nombre a las capas
names(tmax_ku_sr) <- c("pred", "var")

## 6.5. Visualizar resultados --------------------

### Predicciones
pred_gg <- ggplot() +
  geom_spatraster(data = tmax_ku_sr$pred) +
  scale_fill_whitebox_c(
    palette = "muted"
  ) +
  labs(
    title = "Temperatura máxima el 01-ene-2024",
    subtitle = "Interpolación por Kriging Universal",
    x     = "",
    y     = "",
    fill  = "Temp (ºC)"
  )

### Incertidumbre
incertidumbre_sr <- sqrt(tmax_ku_sr$var) * 1.96

var_gg <- ggplot() +
  geom_spatraster(
    data = incertidumbre_sr
  ) +
  scale_fill_gradientn(
    colors   = hcl.colors(8, "Reds 3", rev = TRUE),
    na.value = NA
  ) +
  labs(
    title    = "Incertidumbre",
    x        = "",
    y        = "",
    fill     = "C.I 95% (ºC)"
  )

## Resultado final
pred_gg / var_gg

## 6.6. RMSE ------------------------------------

### 5-fold CV
set.seed(137)
cv_sf <- krige.cv(
  formula   = tmax ~ x + y,
  locations = tmax_coords_sf,
  model     = tmax_opt_ku_vgm,
  nfold     = 5
)

### Obtener RMSE
rmse_ku <- sqrt(cv_sf$residual^2) %>% mean()

### Comparar todos los RMSE






