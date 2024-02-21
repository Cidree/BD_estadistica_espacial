# ________________________________________________________________________ #
# MOOC BOSQUE DIGITAL - Análisis Estadístico de Datos Espaciales con R
#
# - Autor: Adrián Cidre González
# - https://adrian-cidre.com
#
# SEMANAS 03 y 04 - Interpolación espacial
# - Tema 12: Práctica del bloque de interpolación
# ________________________________________________________________________ #

# 1. Cargar paquetes ------------------------------------------------------

library(pacman)

p_load(tidyverse, sf, terra, stars, mapSpain, gstat, tidyterra, patchwork)

# 2. Cargar datos ---------------------------------------------------------

## Área de estudio
## - Nombre objeto: spain_sf
## - Descargar mapa de España (sin islas, Ceuta y Melilla)
## - Unir en un solo polígono y transformar a CRS 25830
spain_sf <- esp_get_ccaa() %>%
  filter(!str_detect(iso2.ccaa.name.es, "Canarias|Baleares|Ceuta|Melilla")) %>%
  st_union() %>%
  st_transform(25830)

## Datos estaciones de AEMET
## - Nombre objeto: tmin_sf
## - Se encuentra en clima.gpkg
tmin_sf <- read_sf(
  dsn   = "00_datos/clima.gpkg"
)

# 3. Explorar datos -------------------------------------------------------

## 3.1. Visualizar dependencia espacial ------------------

### Tema de ggplot
theme_set(
  theme_void() +
    theme(
      plot.title    = element_text(size = 14, hjust = 0.5, face = 'bold'),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      legend.title  = element_text(face = 'bold')
    )
)

### Visualizar dependencia espacial con ggplot2
### - Variable de estudio: tmin
ggplot() +
  geom_sf(data = spain_sf) +
  geom_sf(data = tmin_sf,
          aes(fill = cut_number(tmin, 4), size = cut_number(tmin, 4)),
          shape = 21
  ) +
  scale_fill_manual(
    values = hcl.colors(4, "Oslo")
  ) +
  labs(fill = "Temp", size = "Temp (ºC)",
       title = "Temperatura mínima 01-ene-2024")


## 3.2. Analizar normalidad ----------------

### Test de Kolmogorov-Smirnov
### - Sigue una distribución normal?
nortest::lillie.test(tmin_sf$tmin)

## 3.3. Analizar tendencia -----------------

### Añadir coordenadas a tmin_sf
tmin_sf <- tmin_sf %>%
  mutate(
    x       = st_coordinates(tmin_sf)[,1],
    y       = st_coordinates(tmin_sf)[,2],
    .before = 1
  )

### Visualizar tendencia
## - Crees que existe una tendencia lineal que justifique utilizar KU?
tmin_sf %>%
  pivot_longer(cols = x:y) %>%
  ggplot(aes(value, tmin)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ name, scales = "free") +
  theme_bw()

## 3.4. Crear grilla para predicciones -----

### Tamaño de píxel
### - Nombre objeto: pixel_size
### - Definir un tamaño de pixel adecuado (<5000 tardará más de 30min)
pixel_size <- 5000

### Definir grilla
### - Nombre objeto: grid_sf
### - Esta vez NO recortar (no utilizar st_filter)
### - Convertir a sf y renombrar columna de geometría
grid_sf <- st_make_grid(
  x        = spain_sf,
  cellsize = c(pixel_size, pixel_size)
) %>%
  st_as_sf() %>%
  rename(geometry = x)

# 4. IDW ------------------------------------------------------------------

## Interpolar con IDW (para 5x5km tardó 84 minutos en mi ordenador)
## - Nombre objeto: idw_sf
## - idp = 3
## - nmax = 25
idw_sf <- idw(
  formula   = tmin ~ 1,
  locations = tmin_sf,
  newdata   = grid_sf,
  idp       = 3,
  nmax      = 25
)

## Realizar 5-Fold Cross Validation
## - Como no lo realizamos directamente en la teoría, esta sería la forma
## para IDW, donde no incluimos el modelo de semivariograma.
set.seed(137)
cv_idw_sf <- krige.cv(
  formula   = tmin ~ 1,
  locations = tmin_sf,
  nfold     = 5,
  nmax      = 25,
  set       = list(idp = 3)
)

## Calcular RMSE
## - Nombre objeto: rmse_idw
rmse_idw <- mean(cv_idw_sf$residual^2) %>% sqrt()

# 5. Kriging ordinario ----------------------------------------------------

## Como no existe una tendencia aparente, para a realizar kriging ordinario

## 5.1. Semivariograma ------------------------

### Semivariograma empírico
### - Nombre objeto: ko_var
ko_var <- variogram(
  object    = tmin ~ 1,
  locations = tmin_sf,
  cutoff    = 800000
)

### Visualizar y aumentar cutoff si es necesario
plot(ko_var)

### Modelo de semivariograma
### - Nombre objeto: ko_vgm
### - Utilizar fit.variogram() o realizarlo manual con vgm()
ko_vgm <- fit.variogram(
  object = ko_var,
  model = vgm(c("Sph", "Exp", "Gau"))
)

ko_vgm <- vgm(
  model  = "Sph",
  range  = 500000,
  psill  = 14,
  nugget = 4
)

### Visualizar
plot(ko_var, ko_vgm)

## 5.2. Interpolación ------------------------

### Interpolar con KO (para 5x5km tardó 96 minutos en mi ordenador)
### - Nombre objeto: ko_sf
ko_sf <- krige(
  formula   = tmin ~ 1,
  locations = tmin_sf,
  newdata   = grid_sf,
  model     = ko_vgm
)

### Realizar 5-Fold Cross Validation
### - Nombre objeto: cv_ko_sf
set.seed(137)
cv_ko_sf <- krige.cv(
  formula   = tmin ~ 1,
  locations = tmin_sf,
  model     = ko_vgm,
  nfold     = 5
)

### Calcular RMSE
### - Nombre objeto: rmse_ko
rmse_ko <- mean(cv_ko_sf$residual^2) %>% sqrt()

# 6. Evaluar resultados ---------------------------------------------------

## 6.1. Selección modelo ----------------------

### Cuál es el RMSE de cada modelo?
### Cuál ha tenido mejor rendimiento?
rmse_idw
rmse_ko

## 6.2. Rasterizar ---------------------------

## Rasterizar IDW
## - Nombre objeto: idw_sr
idw_sr <- st_rasterize(
  sf = idw_sf["var1.pred"],
  dx = pixel_size,
  dy = pixel_size
) %>% rast()

## Rasterizar KO
## - Nombre objeto: ko_sr
ko_sr <- st_rasterize(
  sf = ko_sf[c("var1.pred", "var1.var")],
  dx = pixel_size,
  dy = pixel_size
) %>% rast()

## Renombar capas
names(ko_sr) <- c("pred", "var")

## 6.3. Visualizar predicciones -------------

## Mapa IDW
## - Nombre objeto: idw_gg
## - Añadir spain_sf al mapa
idw_gg <- ggplot() +
  geom_spatraster(data = idw_sr) +
  geom_sf(data = spain_sf, fill = NA) +
  scale_fill_whitebox_c(
    palette = "muted"
  ) +
  labs(
    title = "Temperatura mínima el 01-ene-2024",
    subtitle = "Interpolación por IDW",
    fill  = "Temp (ºC)"
  )

## Mapa KO
## - Nombre objeto: ko_gg
## - Añadir spain_sf al mapa
ko_gg <- ggplot() +
  geom_spatraster(data = ko_sr$pred) +
  geom_sf(data = spain_sf, fill = NA) +
  scale_fill_whitebox_c(
    palette = "muted"
  ) +
  labs(
    title = "Temperatura mínima el 01-ene-2024",
    subtitle = "Interpolación por Kriging Ordinario",
    fill  = "Temp (ºC)"
  )

## Visualizar con patchwork
idw_gg / ko_gg

## 6.4. Visualizar incertidumbre

### Calcular incertidumbre ( C.I. 95%)
### - Nombre objeto: incertidumbre_sr
incertidumbre_sr <- sqrt(ko_sr$var) * 1.96

## Mapa incertidumbre
## - Nombre objeto: incertidumbre_gg
incertidumbre_gg <- ggplot() +
  geom_spatraster(data = incertidumbre_sr) +
  geom_sf(data = spain_sf, fill = NA) +
  scale_fill_gradientn(
    colors   = hcl.colors(20, "Reds 3", rev = TRUE),
    na.value = NA
  ) +
  labs(
    title = "Incertidumbre KO",
    fill  = "C.I. 95% (ºC)"
  )

## Visualizar con patchwork
ko_gg + incertidumbre_gg

# 7. Extra ----------------------------------------------------------------

## Visualizar con intervalos

ggplot() +
  geom_spatraster_contour_filled(
    data   = ko_sr$pred,
    breaks = seq(-2, 12, 2)
  ) +
  geom_sf(data = spain_sf, fill = NA, linewidth = .5) +
  scale_fill_whitebox_d(
    palette = "muted"
  ) +
  labs(
    title = "Temperatura mínima el 01-ene-2024",
    subtitle = "Interpolación por Kriging Ordinario",
    fill  = "Temp (ºC)"
  )

ggplot() +
  geom_spatraster_contour_filled(
    data = incertidumbre_sr,
    breaks   = c(1.5, 2, 2.5, 3, 5, 8),
  ) +
  geom_sf(data = spain_sf, fill = NA) +
  scale_fill_manual(
    values   = hcl.colors(5, "Reds 3", rev = TRUE),
    na.value = NA
  ) +
  labs(
    title = "Incertidumbre KO",
    fill  = "C.I. 95% (ºC)"
  )






