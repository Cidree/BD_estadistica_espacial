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


## Datos estaciones de AEMET
## - Nombre objeto: tmin_sf
## - Se encuentra en clima.gpkg


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



## 3.2. Analizar normalidad ----------------

### Test de Kolmogorov-Smirnov
### - Sigue una distribución normal?


## 3.3. Analizar tendencia -----------------

### Añadir coordenadas a tmin_sf


### Visualizar tendencia
## - Crees que existe una tendencia lineal que justifique utilizar KU?


## 3.4. Crear grilla para predicciones -----

### Tamaño de píxel
### - Nombre objeto: pixel_size
### - Definir un tamaño de pixel adecuado (<5000 tardará más de 1 hora)


### Definir grilla
### - Nombre objeto: grid_sf
### - Esta vez NO recortar (no utilizar st_filter)
### - Convertir a sf y renombrar columna de geometría


# 4. IDW ------------------------------------------------------------------

## Interpolar con IDW (para 5x5km tardó 84 minutos en mi ordenador)
## - Nombre objeto: idw_sf
## - idp = 3
## - nmax = 25


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


# 5. Kriging ordinario ----------------------------------------------------

## Como no existe una tendencia aparente, para a realizar kriging ordinario

## 5.1. Semivariograma ------------------------

### Semivariograma empírico
### - Nombre objeto: ko_var


### Visualizar y aumentar cutoff si es necesario


### Modelo de semivariograma
### - Nombre objeto: ko_vgm
### - Utilizar fit.variogram() o realizarlo manual con vgm()


### Visualizar


## 5.2. Interpolación ------------------------

### Interpolar con KO (para 5x5km tardó 96 minutos en mi ordenador)
### - Nombre objeto: ko_sf


### Realizar 5-Fold Cross Validation
### - Nombre objeto: cv_ko_sf


### Calcular RMSE
### - Nombre objeto: rmse_ko


# 6. Evaluar resultados ---------------------------------------------------

## 6.1. Selección modelo ----------------------

### Cuál es el RMSE de cada modelo?
### Cuál ha tenido mejor rendimiento?


## 6.2. Rasterizar ---------------------------

## Rasterizar IDW
## - Nombre objeto: idw_sr


## Rasterizar KO
## - Nombre objeto: ko_sr


## Renombar capas


## 6.3. Visualizar predicciones -------------

## Mapa IDW
## - Nombre objeto: idw_gg
## - Añadir spain_sf al mapa


## Mapa KO
## - Nombre objeto: ko_gg
## - Añadir spain_sf al mapa


## Visualizar con patchwork


## 6.4. Visualizar incertidumbre

### Calcular incertidumbre ( C.I. 95%)
### - Nombre objeto: incertidumbre_sr


## Mapa incertidumbre
## - Nombre objeto: incertidumbre_gg


## Visualizar con patchwork


# 7. Extra ----------------------------------------------------------------

## Visualizar con intervalos

