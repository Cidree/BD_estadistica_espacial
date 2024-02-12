# ________________________________________________________________________ #
# MOOC BOSQUE DIGITAL - Análisis Estadístico de Datos Espaciales con R
#
# - Autor: Adrián Cidre González
# - https://adrian-cidre.com
#
# SEMANA 01 - Patrones de puntos espaciales
# - Tema 04: Práctica en R
# ________________________________________________________________________ #

# 1. Cargar paquetes ------------------------------------------------------

library(pacman)

p_load(tidyverse, spatstat)

# 2. Cargar datos ---------------------------------------------------------

## Cargar 00_datos/incendios_clm.csv
## Nombre objeto: incendios_tbl


## Explorar objeto
incendios_tbl

## Cargar bordes de Castilla-La Mancha
## Este fichero contiene las coordenadas relativas de Castilla-La Mancha en el
## formato necesario para la función owin() que utilizaremos posteriormente
## - Nombre objeto: incendios_owin
## - Utilizar función read_rds() para leer


## Explorar objeto
incendios_owin

## Convertir columnas a factores
## - Nombre objeto: incendios_tbl
## - Convertir factor:


# 3. Explorar datos -------------------------------------------------------

## Generar gráfico de puntos
## - Usar ggplot2
## - Utilizar aesthetic de color para la causa
## - Añadir función + facet_wrap(~ causa, ncol = 5)
## - Personalizar etiquetas, tema ...


# 4. Análisis de densidad -------------------------------------------------

## 4.1. Crear objeto ppp ---------------------------

## Crear ventana
## - Nombre objeto: ventana_owin
## - Coordenadas X: 4.13 a 391.38
## - Coordenadas Y: 18.56 a 385.19
## - Argumento poly: incendios_owin
## - Unidades: kilómetros


## Crear patrón de puntos
## - Nombre objeto: incendios_ppp


## 4.2. Visualizar ---------------------------------

## Generar gráfico de densidad según la causa
## - Utilizar split("causa)
## - Probar distintos valores de ancho de banda
## - Probar distintas funciones Kernel
## - Modificar paleta de colores


## Generar gráfico de densidad según el año
## - Utilizar split("year)
## - Probar distintos valores de ancho de banda
## - Probar distintas funciones Kernel
## - Modificar paleta de colores


# 5. Descripción del patrón -----------------------------------------------

## 5.1. Distribución general del patrón ----------

### Test 1


### Test 2


## 5.2. Distribución según variable causa --------- AVANZADO *****

### Definir función que filtre los datos según una variable,
### y convierta los datos a ppp
### - Nombre función: get_causa_ppp
### - Argumentos: data, var


### Aplicar función a cada causa


# 6. Análisis de distancias -----------------------------------------------

## 6.1. Función K ---------------------

## IMPORTANTE: utilizar en todas las funciones correction = "translate"

## - Nombre objeto: incendios_k
## - 100 simulaciones de Monte Carlo
## - Añadir argumento para calcular r hasta un máximo de 10km


## Visualizar con R base
## - Cómo se distribuyen los incendios en Castilla-La Mancha?
## - Cuál es el nivel de significación? Interpretarlo


## 6.2. Función pcf ------------------

## - Nombre objeto: incendios_pcf
## - 50 simulaciones de Monte Carlo


## Visualizar con R base
## - Cómo se distribuyen los incendios en Castilla-La Mancha?
## - Cuál es el nivel de significación? Interpretarlo


## 6.3. Análisis por marcas ----------

## -> Relación entre incendios intencionados y accidentes?

## Crear nuevo ppp sin la variable year
## - Nombre objeto: incendios_causa_ppp


## Recordar: para análisis con funciones *cross, necesitan ser Multitype
## Comparar:
incendios_ppp
incendios_causa_ppp

## Calcular función pcf para dos marks
## - Nombre objeto: incendios_ia_pcf
## - Marca i: Accidente
## - Marca j: Intencionado
## - Utilizar corrección "translate"
## - Utilizar 100 simulaciones de Monte Carlo


## Visualizar con plot() y contestar
## - Cuál es la relación entre los incendios intencionados y accidentes?
## - Cuál es el p-valor? Interpretar


## Tema de ggplot2
theme_set(
  theme_bw() +
    theme(panel.grid = element_blank())
)

## Visualizar con ggplot2
## - Realizar un gráfico similar al visto en la teoría
## - Además, filtrar solamente r <= 20km


## -> Relación entre incendios intencionados y rayos?

## Calcular función K para dos marks
## - Nombre objeto: incendios_ir_k
## - Marca i: Rayo
## - Marca j: Intencionado
## - Utilizar corrección "translate"
## - Utilizar 100 simulaciones de Monte Carlo


## Visualizar con ggplot2
## - Realizar un gráfico similar al visto en la teoría
## - Además, filtrar solamente r <= 20km


