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
incendios_tbl <- read_csv("00_datos/incendios_clm.csv")

## Explorar objeto
incendios_tbl

## Cargar bordes de Castilla-La Mancha
## Este fichero contiene las coordenadas relativas de Castilla-La Mancha en el
## formato necesario para la función owin() que utilizaremos posteriormente
## - Nombre objeto: incendios_owin
## - Utilizar función read_rds() para leer
incendios_owin <- read_rds("00_datos/incendios_owin.rds")

## Explorar objeto
incendios_owin

## Convertir columnas a factores
## - Nombre objeto: incendios_tbl
## - Convertir factor:
incendios_tbl <- incendios_tbl %>%
  mutate(
    causa = as.factor(causa),
    year  = as.factor(year)
  )

# 3. Explorar datos -------------------------------------------------------

## Generar gráfico de puntos
## - Usar ggplot2
## - Utilizar aesthetic de color para la causa
## - Añadir función + facet_wrap(~ causa)
## - Personalizar etiquetas, tema ...
incendios_tbl %>%
  ggplot(aes(x, y, color = causa)) +
  geom_point() +
  facet_wrap(~ causa) +
  scale_color_manual(
    name = NULL,
    values = c("#66C7F4", "#F34213", "#136F63", "#E0CA3C")
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Incendios Castilla-La Mancha",
    subtitle = "Periodo 1998-2007"
  ) +
  theme_bw() +
  theme(
    legend.position = "top"
  ) +
  coord_fixed()

# 4. Análisis de densidad -------------------------------------------------

## 4.1. Crear objeto ppp ---------------------------

## Crear ventana
## - Nombre objeto: ventana_owin
## - Coordenadas X: 4.13 a 391.38
## - Coordenadas Y: 18.56 a 385.19
## - Argumento poly: incendios_owin
## - Unidades: kilómetros
ventana_owin <- owin(
  xrange   = c(4.13, 391.38),
  yrange   = c(18.56, 385.19),
  unitname = c("kilometer","kilometers"),
  poly     = incendios_owin
)

## Crear patrón de puntos
## - Nombre objeto: incendios_ppp
incendios_ppp <- as.ppp(
  X = incendios_tbl,
  W = ventana_owin
)

## 4.2. Visualizar ---------------------------------

## Generar gráfico de densidad según la causa
## - Utilizar split("causa)
## - Probar distintos valores de ancho de banda
## - Probar distintas funciones Kernel
## - Modificar paleta de colores
incendios_ppp %>%
  split("causa") %>%
  density(
    kernel = "quartic",
    sigma  = 30
  ) %>%
  plot(
    col = hcl.colors(n = 20, "RdYlGn", rev = TRUE)
  )

## Generar gráfico de densidad según el año
## - Utilizar split("year)
## - Probar distintos valores de ancho de banda
## - Probar distintas funciones Kernel
## - Modificar paleta de colores
incendios_ppp %>%
  split("year") %>%
  density(
    kernel = "quartic",
    sigma  = 30
  ) %>%
  plot(
    col = hcl.colors(n = 20, "RdYlGn", rev = TRUE),
    ncols = 5
  )

# 5. Descripción del patrón -----------------------------------------------

## 5.1. Distribución general del patrón ----------

### Test 1
quadrat.test(
  X  = incendios_ppp,
  nx = 5,
  ny = 5
)

### Test 2
quadrat.test(
  X           = incendios_ppp,
  nx          = 5,
  ny          = 5,
  alternative = "clustered"
)

## 5.2. Distribución según variable causa --------- AVANZADO *****

### Definir función que filtre los datos según una variable,
### y convierta los datos a ppp
### - Nombre función: get_causa_ppp
### - Argumentos: data, var
get_causa_ppp <- function(data, var = "Intencionado") {

  ## Filtrar datos
  data_filtered <- data %>%
    filter(causa == var) %>%
    select(-causa)

  ## Definir ventana
  v <- owin(
    xrange   = c(4.13, 391.38),
    yrange   = c(18.56, 385.19),
    unitname = c("kilometer","kilometers"),
    poly     = incendios_owin
  )

  ## Crear patrón de puntos
  ## - Nombre objeto: incendios_ppp
  data_ppp <- as.ppp(
    X = data_filtered,
    W = v
  )

  ## Return
  return(data_ppp)

}

### Aplicar función a cada causa
causas_vec <- incendios_tbl %>% pull(causa) %>% unique() %>% as.character()

map(
  causas_vec,
  \(x) get_causa_ppp(
    data = incendios_tbl,
    var = x
  ) %>%
    quadrat.test(
      nx = 5,
      ny = 5,
      alternative = "clustered"
    )
) %>%
  set_names(
    causas_vec
  )

# 6. Análisis de distancias -----------------------------------------------

## 6.1. Función K ---------------------

## IMPORTANTE: utilizar en todas las funciones correction = "translate"

## - Nombre objeto: incendios_k
## - 100 simulaciones de Monte Carlo
## - Añadir argumento para calcular r hasta un máximo de 10km
incendios_k <- envelope(
  Y          = incendios_ppp,
  fun        = Kest,
  nsim       = 100,
  correction = "translate",
  rmax       = 10
)

## Visualizar con R base
## - Cómo se distribuyen los incendios en Castilla-La Mancha?
## - Cuál es el nivel de significación? Interpretarlo
plot(incendios_k)
incendios_k

## 6.2. Función pcf ------------------

## - Nombre objeto: incendios_pcf
## - 50 simulaciones de Monte Carlo
incendios_pcf <- envelope(
  Y          = incendios_ppp,
  fun        = pcf,
  nsim       = 50,
  correction = "translate"
)

## Visualizar con R base
## - Cómo se distribuyen los incendios en Castilla-La Mancha?
## - Cuál es el nivel de significación? Interpretarlo
plot(incendios_pcf)
incendios_pcf

## 6.3. Análisis por marcas ----------

## -> Relación entre incendios intencionados y accidentes?

## Crear nuevo ppp sin la variable year
## - Nombre objeto: incendios_causa_ppp
incendios_causa_ppp <- as.ppp(
  X = incendios_tbl %>% select(-year),
  W = ventana_owin
)

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
incendios_ia_pcf <- envelope(
  Y          = incendios_causa_ppp,
  i          = "Accidente",
  j          = "Intencionado",
  correction = "translate",
  fun        = pcfcross,
  nsim       = 100
)

## Visualizar con plot() y contestar
## - Cuál es la relación entre los incendios intencionados y accidentes?
## - Cuál es el p-valor? Interpretar
plot(incendios_ia_pcf)
incendios_ia_pcf

## Tema de ggplot2
theme_set(
  theme_bw() +
    theme(panel.grid = element_blank())
)

## Visualizar con ggplot2
## - Realizar un gráfico similar al visto en la teoría
## - Además, filtrar solamente r <= 20km
incendios_ia_pcf %>%
  as_tibble() %>%
  slice(-1) %>%
  filter(r <= 20) %>%
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
    x        = "Radio (m)",
    y        = "g(r)",
    title    = "Pair Correlation Function",
    subtitle = "A distancias < 2.5km de un incendio por accidente ocurren más incendios\nintencionados de lo esperado en un proceso aleatorio"
  )

## -> Relación entre incendios intencionados y rayos?

## Calcular función K para dos marks
## - Nombre objeto: incendios_ir_k
## - Marca i: Rayo
## - Marca j: Intencionado
## - Utilizar corrección "translate"
## - Utilizar 100 simulaciones de Monte Carlo
incendios_ir_k <- envelope(
  Y          = incendios_causa_ppp,
  i          = "Rayo",
  j          = "Intencionado",
  correction = "translate",
  fun        = Kcross,
  nsim       = 100
)

## Visualizar con ggplot2
## - Realizar un gráfico similar al visto en la teoría
## - Además, filtrar solamente r <= 20km
incendios_ir_k %>%
  as_tibble() %>%
  slice(-1) %>%
  filter(r <= 20) %>%
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
    x        = "Radio (km)",
    y        = "K(r)",
    title    = "K de Ripley",
    subtitle = "A distancias <7km, ocurren más incendios intenciados cerca de incendios por rayos de lo\nesperado en un proceso aleatorio. A distancias >12km, existe una aparente dispersión"
  )














