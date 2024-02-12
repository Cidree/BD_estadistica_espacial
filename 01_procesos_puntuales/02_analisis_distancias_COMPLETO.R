# ________________________________________________________________________ #
# MOOC BOSQUE DIGITAL - Análisis Estadístico de Datos Espaciales con R
#
# - Autor: Adrián Cidre González
# - https://adrian-cidre.com
#
# SEMANA 01 - Patrones de puntos espaciales
# - Tema 02: Análisis basado en densidad
# - Tema 03: Análisis basado en distancias
# ________________________________________________________________________ #

# 1. Cargar paquetes ------------------------------------------------------

library(pacman)

p_load(readxl, tidyverse, spatstat)

# 2. Cargar datos ---------------------------------------------------------

patron1_tbl <- read_xlsx("00_datos/puntos.xlsx")
patron2_tbl <- read_xlsx("00_datos/puntos.xlsx", sheet = "patron02")
patron3_tbl <- read_xlsx("00_datos/puntos.xlsx", sheet = "patron03")

# 3. Preparar datos -------------------------------------------------------

patron3_tbl <- patron3_tbl %>%
  mutate(
    sp = if_else(sp == 1, "Quercus ilex", "Quercus faginea") %>% as.factor()
  )

# 4. Contraste de CSR -----------------------------------------------------

## 4.1. Tema de ggplot2 ---------------------
theme_set(
  theme_bw() +
    theme(panel.grid = element_blank())
)

## 4.2. Patrón 1 - Quercus ilex ------------

### Visualizar
patron1_tbl %>%
  ggplot(aes(x, y)) +
  geom_point()

### Definir ventana
ventana_owin <- owin(
  xrange   = c(0, 20),
  yrange   = c(0, 20),
  unitname = "metros"
)

### Convertir a ppp
p1_ppp <- as.ppp(
  X = patron1_tbl,
  W = ventana_owin
)

### Test para patrón en clusters
### -> h0: patrón aleatorio o regular
### -> h1: patrón en clusters
quadrat.test(
  X           = p1_ppp,
  nx          = 5,
  ny          = 5,
  alternative = "clustered"
)

## 4.3. Patrón 2 - Quercus ilex ------------

### Visualizar
patron2_tbl %>%
  ggplot(aes(x, y)) +
  geom_point()

## Convertir a ppp
p2_ppp <- as.ppp(
  X = patron2_tbl,
  W = ventana_owin
)

## Test para patrón en clusters
## -> h0: patrón aleatorio o regular
## -> h1: patrón en clusters
quadrat.test(
  X           = p2_ppp,
  nx          = 5,
  ny          = 5,
  alternative = "clustered"
)

# 5. Análisis de distancias I ---------------------------------------------

## -> Comparar nuestro patrón con 1 patrón CSR

## 5.1. Función K de Ripley -----------------

### Estimar función K para el patrón 1
p1_k <- Kest(
  X          = p1_ppp,
  correction = "Ripley"
)

### Visualizar
plot(p1_k)

### Estimar función K para el patrón 2
p2_k <- Kest(
  X          = p2_ppp,
  correction = "border"
)

### Visualizar
plot(p2_k)

## 5.2. Función L ---------------------------

### Estimar función L para el patrón 1
p1_l <- Lest(
  X = p1_ppp
)

### Visualizar
plot(p1_l)

### 5.3. Pair Correlation Function ----------

### Estimar PCF para el patrón 1
p1_pcf <- pcf(
  X = p1_ppp
)

### Visualizar
plot(p1_pcf)

### 5.4. Analizar patrón 3 -----------------

### - Estos patrones se analizan mediante "marks"
### - Tenemos las funciones del tipo *cross, que evalúan la interacción
###   entre dos marks distintas.
###
###  * H0: los procesos son independientes (f == CSR)
###  * H1: se atraen (f > CSR). Hay más puntos de j cerca de puntos de i
###        de lo esperado en un proceso aleatorio
###  * H1: se repelen (f < CSR)


### Patrón 3 como ppp
p3_ppp <- as.ppp(
  X = patron3_tbl,
  W = ventana_owin
)

### Función K
Kcross(
  X = p3_ppp,
  i = "Quercus ilex",
  j = "Quercus faginea"
) %>%
  plot()

### Función L
Lcross(
  X = p3_ppp,
  i = "Quercus ilex",
  j = "Quercus faginea",
  correction = "iso"
) %>%
  plot()

### Función PCF
pcfcross(
  X = p3_ppp,
  i = "Quercus ilex",
  j = "Quercus faginea",
  correction = "iso"
) %>%
  plot()

# 6. Análisis de distancias II --------------------------------------------

## -> Comparar nuestro patrón con simulaciones de Monte Carlo de CSR

## 6.1. Función K de Ripley -----------------

### Estimar función para el patrón 1
p1_k_mc <- envelope(
  Y    = p1_ppp,
  fun  = Kest,
  nsim = 500
)

### Visualizar
plot(p1_k_mc)

## 6.2. Función L --------------------------

### Estimar función para el patrón 1
p1_l_mc <- envelope(
  Y    = p1_ppp,
  fun  = Lest,
  nsim = 500
)

### Visualizar
plot(p1_l_mc)

## 6.3. PCF -------------------------------

### Estimar función para el patrón 1
p1_pcf_mc <- envelope(
  Y    = p1_ppp,
  fun  = pcf,
  nsim = 500
)

### Visualizar
plot(p1_pcf_mc)

## 6.4. Función Kcross -------------------

### Estimar función para el patrón 3
p3_kcross_mc <- envelope(
  Y    = p3_ppp,
  fun  = Kcross,
  nsim = 500
)

### Visualizar
plot(p3_kcross_mc)

## 6.4. PCF cross ------------------------

### Estimar función para el patrón 3
p3_pcfcross_mc <- envelope(
  Y    = p3_ppp,
  fun  = pcfcross,
  nsim = 500
)

### Visualizar con ggplot2
p3_pcfcross_mc %>%
  as_tibble() %>%
  slice(-1) %>%
  ## gráfico
  ggplot(
    aes(x = r, y = obs)
  ) +
  ## geometrías
  geom_ribbon(
    aes(ymin = lo, ymax = hi),
    fill = "gray85"
  ) +
  geom_line(aes(y = lo), color = "gray80") +
  geom_line(aes(y = hi), color = "gray80") +
  geom_line(aes(y = theo), linetype = 2) +
  geom_line() +
  ## etiquetas
  labs(
    x = "Radio (m)",
    y = "g(r)",
    title = "Pair Correlation Function",
    subtitle = "A distancias >1m, las plántulas de Q. ilex muestran atracción a Q. faginea"
  )



