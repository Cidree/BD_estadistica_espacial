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


### Convertir a ppp


### Test para patrón en clusters
### -> h0: patrón aleatorio o regular
### -> h1: patrón en clusters


## 4.3. Patrón 2 - Quercus ilex ------------

### Visualizar
patron2_tbl %>%
  ggplot(aes(x, y)) +
  geom_point()

## Convertir a ppp


## Test para patrón en clusters
## -> h0: patrón aleatorio o regular
## -> h1: patrón en clusters


# 5. Análisis de distancias I ---------------------------------------------

## -> Comparar nuestro patrón con 1 patrón CSR

## 5.1. Función K de Ripley -----------------

### Estimar función K para el patrón 1


### Visualizar


### Estimar función K para el patrón 2


### Visualizar


## 5.2. Función L ---------------------------

### Estimar función L para el patrón 1


### Visualizar


### 5.3. Pair Correlation Function ----------

### Estimar PCF para el patrón 1


### Visualizar


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


### Función K


### Función L


### Función PCF


# 6. Análisis de distancias II --------------------------------------------

## -> Comparar nuestro patrón con simulaciones de Monte Carlo de CSR

## 6.1. Función K de Ripley -----------------

### Estimar función para el patrón 1


### Visualizar


## 6.2. Función L --------------------------

### Estimar función para el patrón 1


### Visualizar


## 6.3. PCF -------------------------------

### Estimar función para el patrón 1


### Visualizar


## 6.4. Función Kcross -------------------

### Estimar función para el patrón 3


### Visualizar


## 6.5. PCF cross ------------------------

### Estimar función para el patrón 3


### Visualizar con ggplot2















