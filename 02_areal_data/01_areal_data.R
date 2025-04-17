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


# 3. Preparar datos -------------------------------------------------------

## Seleccionar solamente municipios y total incendios


## Transformar a EPSG:25829


## Densidad de incendios?


# 4. Visualizar -----------------------------------------------------------

## Histogramas incendios


## Visualizar valores absolutos


## Visualizar valores absolutos discretizados


## Visualizar incendios/km2


# 5. Vecinos y pesos ------------------------------------------------------

## 5.1. Proximidad: adyacente ----------------

### Detectar vecinos y pesos (estilo estandarizado por fila)


### Resumen análisis de proximidad


### Solucionar problema


### Detectar vecinos y pesos (estilo estandarizado por fila)


### Resumen análisis de proximidad


### Gráfico de vecinos

## 5.2. Proximidad: knn ------------------------

### Detectar vecinos y pesos
### - 5 vecinos más cercanos
### - Método pesos: distancia inversa


### Resumen análisis de proximidad


## 5.3. Lags -----------------------------------

## Aplicar lag 2


## Visualizar incendios con lag


# 6. Moran's I Global -----------------------------------------------------

## 6.1. Vecinos adyacentes -------------------

### Moran's I test


### Moran's I test con permutaciones de Monte Carlo


### Visualizar permutaciones de Monte Carlo


## 6.2. Knn ----------------------------------

### Moran's I test


## 6.3. Lags -----------------------

### Moran's I test con permutaciones de Monte Carlo


# 7. Moran's I Local ------------------------------------------------------

## 7.1. Vecinos adyacentes -------------------

### Calcular Moran's I a dos colas
### - H0: ausencia de autocorrelación con los vecinos
### - H1: existencia de autocorrelación con los vecinos


### Estructura


### Visualizar


### Visualizar clasificación


### Corrección de insignificantes


### Explorar niveles de la columna pysal


### Visualizar clasificación


## 7.2. Lag 2 -------------------------

### Calcular Moran's I


### Visualizar p-valores


### Visualizar clasificación


