# ________________________________________________________________________ #
# MOOC BOSQUE DIGITAL - Análisis Estadístico de Datos Espaciales con R
#
# - Autor: Adrián Cidre González
# - https://adrian-cidre.com
#
# SEMANA 01 - Patrones de puntos espaciales
# - Tema 01: Introducción a procesos puntuales
# - Tema 02: Análisis basado en densidad
# ________________________________________________________________________ #

# 1. Cargar paquetes ------------------------------------------------------

library(pacman)

p_load(readxl, tidyverse, spatstat, patchwork, ggtext)

# 2. Cargar datos ---------------------------------------------------------

patron1_tbl <- read_xlsx("00_datos/puntos.xlsx")
patron2_tbl <- read_xlsx("00_datos/puntos.xlsx", sheet = "patron02")
patron3_tbl <- read_xlsx("00_datos/puntos.xlsx", sheet = "patron03")

# 3. Preparar datos -------------------------------------------------------

patron3_tbl <- patron3_tbl %>%
  mutate(
    sp = if_else(sp == 1, "Quercus ilex", "Quercus faginea") %>% as.factor()
  )

# 4. Visualizar -----------------------------------------------------------

## 4.1. Definir tema -----------------------
theme_set(
  theme_bw() +
    theme(panel.grid = element_blank())
)

## 4.2. Visualizar patrones ----------------

### Patrón 1 - Quercus faginea
p1_gg <- patron1_tbl %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  labs(
    title = "Patrón 01 - Quercus faginea"
  ) +
  coord_equal()

p1_gg

### Patrón 2 - Quercus ilex
p2_gg <- patron2_tbl %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  labs(
    title = "Patrón 02 - Quercus ilex"
  ) +
  coord_equal()

p2_gg

### Patrón 3 - Quercus ilex + Quercus faginea
patron3_tbl %>%
  ggplot(aes(x = x, y = y, color = sp)) +
  geom_point() +
  labs(
    title = "Patrón 03"
  ) +
  coord_equal() +
  scale_color_manual(
    values = c("#31730D", "#EFBC5D"),
    name = NULL
  )

## 4.3. Visualizar densidad en quadrats ------

### geom_bin2d(): cuenta el número de eventos en una ventana especificada

### Función para visualización
plot_pattern <- function(data, pattern_plot, v) {

  pattern_plot +
    geom_vline(
      xintercept = seq(0, 20, v)
    ) +
    geom_hline(
      yintercept = seq(0, 20, v)
    ) +
    data %>%
    ggplot(aes(x = x, y = y)) +
    geom_bin2d(
      binwidth = c(v, v),
      drop = FALSE
    ) +
    labs(
      title = str_glue("Tamaño de píxel {v^2}m^2^")
    ) +
    theme(
      plot.title = element_markdown()
    ) +
    scale_fill_distiller(
      palette = "RdYlGn",
      name    = "Densidad"
    ) +
    coord_equal()
}

### Patrón 1 - Quercus faginea
plot_pattern(
  data = patron1_tbl,
  pattern_plot = p1_gg,
  v = 4
)

### Patrón 2 - Quercus ilex
plot_pattern(
  data = patron2_tbl,
  pattern_plot = p2_gg,
  v = 4
)

## 4.4. Observaciones por quadrat ------------

### Patrón 1 - Quercus faginea

### Definir ventana
ventana_owin <- owin(
  xrange = c(0, 20),
  yrange = c(0, 20),
  unitname = c("meter", "meters")
)

### Convertir a ppp
patron1_ppp <- as.ppp(
  X = patron1_tbl,
  W = ventana_owin
)

### Observaciones por Quadrat
quadratcount(
  X  = patron1_ppp,
  nx = 10,
  ny = 10
)

### Test Chi-squared a dos colas
### -> h0: patrón aleatorio
### -> h1: patrón no aleatorio
quadrat.test(
  X  = patron1_ppp,
  nx = 5,
  ny = 5
)

### Test Chi-squared clustered
### -> h0: patrón aleatorio o regular
### -> h1: patrón en agregados
quadrat.test(
  X  = patron1_ppp,
  nx = 5,
  ny = 5,
  alternative = "clustered"
)

## 4.5. Visualizar densidad Kernel -----------

### Patrón 1 - Opción 1
patron1_tbl %>%
  ggplot(aes(x = x, y = y)) +
  geom_density2d_filled() +
  geom_point(alpha = .2) +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position = "none"
  )

### Patrón 1 - Opción 2
patron1_densidad <- patron1_ppp %>%
  density(
    sigma = 1.5,
    kernel = "epanechnikov"
  )

plot(
  patron1_densidad,
  col = viridisLite::viridis(n = 20),
  main = "Densidad Kernel (P1)"
)
contour(patron1_densidad, add = TRUE)

### Patrón 2 - Opción 1
patron2_tbl %>%
  ggplot(aes(x = x, y = y)) +
  geom_density2d_filled(
    h = 5
  ) +
  geom_point(alpha = .2) +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position = "none"
  )

### Patrón 2 - Opción 2
patron2_tbl %>%
  as.ppp(
    W = ventana_owin
  ) %>%
  density(
    kernel = "quartic",
    sigma  = 2
  ) %>%
  plot(
    col = viridisLite::viridis(n = 20),
    main = "Densidad Kernel (P2)"
  )

contour(patron2_tbl %>%
          as.ppp(
            W = ventana_owin
          ) %>%
          density(
            kernel = "quartic",
            sigma  = 2
          ) ,
        add = TRUE)

# 5. Patrón 3 -------------------------------------------------------------

## Convertir a ppp
patron3_ppp <- as.ppp(
  X = patron3_tbl,
  W = ventana_owin
)

## Gráfico de densidad
patron3_ppp %>%
  split() %>%
  density(
    sigma = 2
  ) %>%
  plot(
    col = hcl.colors(n = 50, palette = "RdYlGn", rev = TRUE)
  )

