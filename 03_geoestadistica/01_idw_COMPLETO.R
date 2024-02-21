# ________________________________________________________________________ #
# MOOC BOSQUE DIGITAL - Análisis Estadístico de Datos Espaciales con R
#
# - Autor: Adrián Cidre González
# - https://adrian-cidre.com
#
# SEMANAS 03 y 04 - Interpolación espacial
# - Tema 09: Métodos determinísticos
# ________________________________________________________________________ #

# 1. Cargar paquetes ------------------------------------------------------

library(pacman)

p_load(tidyverse, mapSpain, sf, terra, stars, gstat, dismo, tidyterra, patchwork)

# 2. Cargar datos ---------------------------------------------------------

## Datos España sin islas
spain_sf <- esp_get_ccaa() %>%
  filter(!str_detect(iso2.ccaa.name.es, "Canarias|Baleares|Ceuta|Melilla")) %>%
  st_union() %>%
  st_transform(25830)

## Datos clima 2024-01-01
tmax_sf <- read_sf("00_datos/clima.gpkg")

# 3. Explorar datos -------------------------------------------------------

## Visualizar
ggplot() +
  geom_sf(data = spain_sf) +
  geom_sf(
    data = tmax_sf,
    aes(fill = cut_number(tmax, 4), size = cut_number(tmax, 4)),
    shape = 21
  ) +
  scale_fill_manual(
    values = hcl.colors(4, "Oslo")
  ) +
  theme_void() +
  labs(
    fill = "Temp", size = "Temp",
    title = "Temperatura máxima el 01-ene-2024"
  )

# 4. Interpolar con IDW ---------------------------------------------------

## 4.1. Definir grilla ----------------------

## Definir tamaño de píxel
pixel_size <- 5000

## Crear grilla
grid_sfc <- st_make_grid(
  x = spain_sf,
  cellsize = c(pixel_size, pixel_size)
) %>%
  st_as_sf() %>%
  st_filter(
    spain_sf,
    .predicate = st_intersects
  ) %>%
  rename(geometry = x)

## Visualizar grilla
plot(st_geometry(spain_sf))
plot(grid_sfc, add = TRUE)

## 4.2. IDW --------------------------------

### Calcular IDW con valores por defecto (33min)
idw_defecto_sf <- idw(
  formula   = tmax ~ 1,
  locations = tmax_sf,
  newdata   = grid_sfc
)

idw_defecto_sf <- read_sf(
  "00_datos/outputs/interpolaciones.gpkg",
  layer = "idw_idp2"
)

### Rasterizar
idw_defecto_sr <- st_rasterize(
  sf = idw_defecto_sf["var1.pred"],
  dx = pixel_size,
  dy = pixel_size
) %>%
  rast()

## 4.3. Visualizar --------------

### Tema de ggplot
theme_set(
  theme_void() +
    theme(
      plot.title    = element_text(size = 14, hjust = 0.5, face = 'bold'),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      legend.title  = element_text(face = 'bold')
    )
)

## Visualizar raster
idw_defecto_gg <- ggplot() +
  geom_spatraster(data = idw_defecto_sr) +
  scale_fill_whitebox_c(
    palette = "muted"
  ) +
  labs(
    title = "Temperatura máxima el 01-ene-2024",
    subtitle = "Interpolación por IDW con IDP = 2",
    fill = "Temp (ºC)"
  )

# 5. Optimizar idp --------------------------------------------------------

## 5.1. Funciones -----------------------------

### Función para calcular RMSE
get_rmse <- function(obs, pred) {
  sqrt(mean((obs - pred)^2))
}

### Función que calcular el RMSE para cada fold y varios IDP
get_fold_rmse <- function(data, x, y) {
  ## Predicciones
  preds <- idw(
    formula = tmax ~ 1,
    locations = data %>% filter(fold != x),
    newdata = data %>% filter(fold == x),
    idp = y
  )
  ## Calcular RMSE
  rmse <- get_rmse(
    obs = data %>% filter(fold == x) %>% pull(tmax),
    pred = preds %>% pull(var1.pred)
  )
  ## Devolver tabla con resultados
  tibble(
    fold = x,
    idp  = y,
    rmse = rmse
  )
}

## 5.2. Calcular RMSE -------------------------

### Crear columna con folds
set.seed(137)
tmax_folds_sf <- tmax_sf %>%
  mutate(
    fold = kfold(tmax_sf, 5)
  )

### Crear grilla para iterar
folds_grid <- expand_grid(
  folds = 1:5,
  idp   = 1:10
)

### Calcular RMSE para cada iteración
rmse_list <- map2(
  .x = folds_grid$folds,
  .y = folds_grid$idp,
  .f = get_fold_rmse,
  data = tmax_folds_sf
)

### Calcular RMSE medio
rmse_medio_tbl <- rmse_list %>%
  reduce(bind_rows) %>%
  group_by(idp) %>%
  summarise(rmse = mean(rmse))

### Visualizar
rmse_medio_tbl %>%
  ggplot() +
  geom_line(aes(idp, rmse)) +
  theme_bw()

## 5.3. IDW con idp óptimo ---------------------

### idp óptimo
idp_optimo <- rmse_medio_tbl %>%
  slice_min(rmse)

### Calcular IDW (28min)
idw_optimo_sf <- idw(
  formula   = tmax ~ 1,
  locations = tmax_sf,
  newdata   = grid_sfc,
  idp       = idp_optimo$idp
)

idw_optimo_sf <- read_sf(
  "00_datos/outputs/interpolaciones.gpkg",
  layer = "idw_idp_optimo"
)

## Rasterizar
idw_optimo_sr <- st_rasterize(
  sf = idw_optimo_sf["var1.pred"],
  dx = pixel_size,
  dy = pixel_size
) %>%
  rast()

## 5.4. Visualizar ---------------
idw_optimo_gg <- ggplot() +
  geom_spatraster(data = idw_optimo_sr) +
  scale_fill_whitebox_c(
    palette = "muted"
  ) +
  labs(
    title = "Temperatura máxima el 01-ene-2024",
    subtitle = "Interpolación por IDW con IDP óptimo",
    fill = "Temp (ºC)"
  )

idw_defecto_gg + idw_optimo_gg

# 6. Optimizar idp y nmax -------------------------------------------------

## 6.1. Modificar función ---------------------
get_fold_rmse <- function(data, x, y, z) {
  ## Predicciones
  preds <- idw(
    formula   = tmax ~ 1,
    locations = data %>% filter(fold != x),
    newdata   = data %>% filter(fold == x),
    idp       = y,
    nmax      = z
  )
  ## RMSE
  rmse <- get_rmse(
    obs  = data %>% filter(fold == x) %>% pull(tmax),
    pred = preds %>% pull(var1.pred)
  )
  ## Devolver tabla
  tibble(
    fold = x,
    idp  = y,
    nmax = z,
    rmse = rmse
  )
}

## 6.2. Calcular RMSE -------------------------

### Crear grilla para iterar
folds_grid <- expand_grid(
  folds = 1:5,
  idp   = 0:10,
  nmax  = seq(5, 100, 5)
)

### Calcular RMSE para cada iteración
rmse_list <- pmap(
  list(
    folds_grid$folds,
    folds_grid$idp,
    folds_grid$nmax
  ),
  .f   = get_fold_rmse,
  data = tmax_folds_sf
)

### Calcular RMSE medio
rmse_medio_tbl <- rmse_list %>%
  reduce(bind_rows) %>%
  group_by(idp, nmax) %>%
  summarise(rmse = mean(rmse)) %>%
  ungroup()

### Visualizar
rmse_medio_tbl %>%
  ggplot() +
  geom_line(aes(idp, rmse, color = nmax, group = nmax)) +
  theme_bw()

## 6.3. IDW con idp y namx óptimos -----------

### Parámetros óptimos
optimos_tbl <- rmse_medio_tbl %>%
  slice_min(rmse, with_ties = FALSE)

### Calcular IDW (23min)
idw_optimo_2_sf <- idw(
  formula   = tmax ~ 1,
  locations = tmax_sf,
  newdata   = grid_sfc,
  idp       = optimos_tbl$idp,
  nmax      = optimos_tbl$nmax
)

### Cargar datos
idw_optimo_2_sf <- read_sf(
  "00_datos/outputs/interpolaciones.gpkg",
  layer = "idw_optimo"
)

## Rasterizar
idw_optimo_2_sr <- st_rasterize(
  sf = idw_optimo_2_sf["var1.pred"],
  dx = pixel_size,
  dy = pixel_size
) %>%
  rast() %>%
  mask(vect(spain_sf))

## 6.4. Visualizar ------------------------

ggplot() +
  geom_spatraster(data = idw_optimo_2_sr) +
  scale_fill_whitebox_c(
    palette = "muted"
  ) +
  labs(
    title = "Temperatura máxima el 01-ene-2024",
    subtitle = "Interpolación por IDW con IDP y nmax óptimos",
    fill = "Temp (ºC)"
  )

# 7. Guardar RMSE --------------------------------------------------------

rmse_list <- list(
  idw_idp_optimo = idp_optimo$rmse,
  idw_optimo = optimos_tbl$rmse
)

write_rds(rmse_list, "00_datos/rmse.rds")



