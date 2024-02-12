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


# 3. Preparar datos -------------------------------------------------------


# 4. Visualizar -----------------------------------------------------------

## 4.1. Definir tema -----------------------


## 4.2. Visualizar patrones ----------------

### Patrón 1 - Quercus faginea


### Patrón 2 - Quercus ilex


### Patrón 3 - Quercus ilex + Quercus faginea


## 4.3. Visualizar densidad en quadrats ------

### geom_bin2d(): cuenta el número de eventos en una ventana especificada

### Patrón 1 - Quercus faginea


### Patrón 2 - Quercus ilex


## 4.4. Observaciones por quadrat ------------

### Patrón 1 - Quercus faginea

### Definir ventana


### Convertir a ppp


### Observaciones por Quadrat


### Test Chi-squared a dos colas
### -> h0: patrón aleatorio
### -> h1: patrón no aleatorio


### Test Chi-squared clustered
### -> h0: patrón aleatorio o regular
### -> h1: patrón en agregados


## 4.5. Visualizar densidad Kernel -----------

### Patrón 1 - Opción 1


### Patrón 1 - Opción 2


### Patrón 2 - Opción 1


### Patrón 2 - Opción 2


# 5. Patrón 3 -------------------------------------------------------------

## Convertir a ppp


## Gráfico de densidad











