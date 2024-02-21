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
nortest::lillie.test(tmax_sf$tmax)

# 4. Semivariograma -------------------------------------------------------

## 4.1. Nube variograma -------------------------

## Calcular semivarianza
nube_var <- variogram(
  object = tmax ~ 1,
  locations = tmax_sf,
  cloud = TRUE
)

## Visualizar
plot(nube_var)

## 4.2. Semivariograma -------------------------

## Obtener semivariograma de nuestros datos
tmax_var <- variogram(
  object = tmax ~ 1,
  locations = tmax_sf,
  cutoff = 800000
)

### Visualizar
plot(tmax_var)

### Buscar valores óptimos
tmax_opt_vgm <- fit.variogram(
  object = tmax_var,
  model  = vgm(c("Sph", "Gau", "Exp"))
)

### Visualizar modelo
plot(tmax_var, tmax_opt_vgm)

### Manual
tmax_vgm <- vgm(
  model = "Sph",
  range = 550000,
  nugget = 3,
  psill = 13.5
)

### Visualizar modelo
plot(tmax_var, tmax_vgm)

# 5. Kriging ordinario ----------------------------------------------------

## 5.1. Definir grilla ----------------------------

### Definir tamaño de píxel
pixel_size <- 5000

### Crear grilla
grid_sf <- st_make_grid(
  x = spain_sf,
  cellsize = c(pixel_size, pixel_size)
) %>%
  st_as_sf() %>%
  rename(geometry = x) %>%
  st_filter(
    spain_sf,
    .predicate = st_intersects
  )

### Visualizar grilla
plot(spain_sf)
plot(grid_sf, add = TRUE)

## 5.2. Predicciones -----------------------------

### Predicciones kriging ordinario
tmax_ko_sf <- krige(
  formula   = tmax ~ 1,
  locations = tmax_sf,
  newdata   = grid_sf,
  model     = tmax_vgm
)

### Cargar objeto guardado
tmax_ko_sf <- read_sf(
  dsn   = "00_datos/outputs/interpolaciones.gpkg",
  layer = "ko"
)

### Rasterizar
tmax_ko_sr <- st_rasterize(
  sf = tmax_ko_sf[c("var1.pred", "var1.var")],
  dx = pixel_size,
  dy = pixel_size
) %>%
  rast()

### Cambiar nombre a las capas
names(tmax_ko_sr) <- c("pred", "var")

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
pred_gg <- ggplot() +
  geom_spatraster(
    data = tmax_ko_sr$pred
  ) +
  scale_fill_whitebox_c(
    palette = "muted"
  ) +
  labs(
    title = "Temperatura máxima el 01-ene-2024",
    subtitle = "Interpolación por Kriging Ordinario",
    fill = "Temp (ºC)"
  )

### Incertidumbre
incertidumbre_sr <- sqrt(tmax_ko_sr$var) * 1.96

var_gg <- ggplot() +
  geom_spatraster(
    data = incertidumbre_sr
  ) +
  scale_fill_gradientn(
    colours = hcl.colors(10, "Reds 3", rev = TRUE),
    na.value = NA
  ) +
  labs(
    title = "Incertidumbre",
    subtitle = "Interpolación por Kriging Ordinario",
    fill = "C.I. 95% (ºC)"
  )

### Resultado final
pred_gg / var_gg

## 5.4. RMSE ------------------------------------

### 5-fold CV
set.seed(137)
cv_sf <- krige.cv(
  formula   = tmax ~ 1,
  locations = tmax_sf,
  model     = tmax_vgm,
  nfold     = 5
)

### Obtener RMSE
rmse_ko <- mean(cv_sf$residual^2) %>% sqrt()

# 6. Kriging universal ----------------------------------------------------

## 6.1. Añadir coordenadas -----------------------

### Coordenadas a tmax_sf
tmax_coords_sf <- tmax_sf %>%
  mutate(
    x = st_coordinates(tmax_sf)[,1],
    y = st_coordinates(tmax_sf)[,2],
    .before = 1
  )

### Añadir coordenadas a grilla
grid_coords_mat <- grid_sf %>%
  st_centroid() %>%
  st_coordinates()

grid_coords_sf <- grid_sf %>%
  mutate(
    x = grid_coords_mat[,1],
    y = grid_coords_mat[,2],
    .before = 1
  )

## 6.2. Análisis de la tendencia -----------------

### Test de correlación de Pearson
cor.test(
  tmax_coords_sf$x,
  tmax_coords_sf$tmax
)

cor.test(
  tmax_coords_sf$y,
  tmax_coords_sf$tmax
)

### Relación coordenadas con tmax
tmax_coords_sf %>%
  pivot_longer(cols = x:y) %>%
  ggplot(aes(value, tmax)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ name, scales = "free") +
  theme_bw()

### Modelo lineal
tmax_xy_lm <- lm(
  tmax ~ x + y,
  data = tmax_coords_sf
)

### Residuales normales?
plot(tmax_xy_lm, 2)
nortest::lillie.test(tmax_xy_lm$residuals)

### Residuales con media 0 y constante?
plot(tmax_xy_lm, 1)

## 6.3. Semivariograma ---------------------------

### Obtener semivariograma de nuestros datos
tmax_ku_var <- variogram(
  object    = tmax ~ x + y,
  locations = tmax_coords_sf,
  cutoff    = 800000
)

### Visualizar
plot(tmax_ku_var)

### Buscar valores óptimos
tmax_opt_ku_vgm <- fit.variogram(
  object = tmax_ku_var,
  model  = vgm(c("Sph", "Gau", "Exp"))
)

### Visualizar modelo
plot(tmax_ku_var, tmax_opt_ku_vgm)

## 6.4. Predicciones -----------------------------

### Predicciones kriging universal
tmax_ku_sf <- krige(
  formula   = tmax ~ x + y,
  locations = tmax_coords_sf,
  newdata   = grid_coords_sf,
  model     = tmax_opt_ku_vgm
)

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
rmse_ku <- mean(cv_sf$residual^2) %>% sqrt()

### Comparar todos los RMSE
idw_rmse <- read_rds("00_datos/rmse.rds")


c(
  idw_rmse,
  ko = rmse_ko,
  ku = rmse_ku
)

