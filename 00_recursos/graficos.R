## Crear datos

wildfires_tbl <- readxl::read_xlsx("00_recursos/wildfires_2006_2015.xlsx", sheet = 3)
wildfires_tbl <- wildfires_tbl %>%
  select(
    mun = NOMBRE, code = CODIGOINE, woodland_area = ARBOLADO,
    treeless_area = NOARBOLADO, total_area = TOTAL, everything()
  )

## Get data
esp_mun_sf <- esp_get_munic() %>%
  filter(!(ine.ccaa.name %in% c("Melilla", "Ceuta"))) %>%
  mutate(code = as.numeric(LAU_CODE))
esp_mun_wildfires_sf <- esp_mun_sf %>%
  left_join(
    wildfires_tbl,
    by = join_by(code == code)
  )

incendios_sf  %>%
  filter(str_detect(Municipio, "Cotobade"))


wildfires_2_tbl <- esp_mun_wildfires_sf %>%
  filter(str_detect(ine.ccaa.name, "Andaluc"))

asturias_sf <- wildfires_2_tbl %>%
  select(
    Municipio = name,
    Conatos = conatos,
    Incendios = incendios,
    Total_incendios = TOTAL_INCENDIOS,
    Area_quemada_ha = total_area
  ) %>%
  mutate(
    Total_incendios = if_else(
      is.na(Total_incendios), 0, Total_incendios
    ),
    Area_quemada_ha = if_else(
      is.na(Area_quemada_ha), 0, Area_quemada_ha
    )
  )


st_write(
  asturias_sf,
  "00_datos/incendios_ccaa.gpkg",
  layer = "andalucia",
  append = FALSE
)

######################
aa <- tibble(
  x = x,
  y = y
) %>%
  mutate(
    w = case_when(
      x == 1 | y == 1 | x == 10 | y == 10 ~ 1,
      .default = 0
    )
  )

###
ggplot(aa) +
  geom_point(aes(x, y, color = w)) +
  coord_equal() +
  labs(x = NULL, y = NULL)

###
ggplot(patron2_tbl, aes(x, y)) +
  geom_point(alpha = .5) +
  theme_minimal() +
  scale_x_continuous(limits = c(0, 20), breaks = seq(0, 20, by = 4)) +
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, by = 4)) +
  theme(panel.grid = element_blank(),
        panel.grid.major = element_line(color = "black"))

###
set.seed(123)
aa <- patron2_tbl  %>% slice_sample(n = 400)

aa %>%
  ggplot(aes(x, y)) +
  geom_point(size = 3) +
  geom_hline(
    yintercept = seq(0, 20, 4)
  ) +
  geom_vline(xintercept = seq(0, 20, 4)) +
  labs(x = NULL, y = NULL) + xlim(c(0, 20)) + ylim(c(0, 20)) +
  theme_bw() +
  theme(
    panel.grid = element_blank()
  ) +
  scale_x_continuous(expand = expansion(0)) +
  scale_y_continuous(expand = expansion(0))

###
ggplot() +
  geom_bin2d(data = aa,
             aes(x = x,
                 y = y),
             binwidth = c(5, 5)) +
  labs(x = NULL, y = NULL) + xlim(c(0, 20)) + ylim(c(0, 20)) +
  theme_bw() +
  theme(
    panel.grid = element_blank()
  ) +
  scale_x_continuous(expand = expansion(0)) +
  scale_y_continuous(expand = expansion(0))



####
ggplot(data = data.frame(dist = c(-2, 2)),
       aes(dist)) +
  stat_function(fun = function(x) dnorm(x) * ifelse(abs(x) <= 1, 3/4 * (1 - x^2), 0),
                n = 101) +
  ylim(c(0, 0.4))  + labs(y = "Densidad", x = "Distancia") +
  ggtitle("Kernel Epanechnikov") +
  ggplot(data = data.frame(dist = c(-3, 3)),
         aes(dist)) +
  stat_function(fun = dnorm,
                n = 101,
                args = list(mean = 0,
                            sd = 1)) +
  ylim(c(0, 0.45)) + labs(y = "Densidad", x = "Distancia") +
  ggtitle("Kernel Gaussiano") +
  ggplot(data = data.frame(dist = c(-3, 3)),
         aes(dist)) +
  stat_function(fun = function(x) dnorm(x) * exp(-abs(x)),
                n = 101) +
  ylim(c(0, 0.45)) +
  labs(y = "Densidad", x = "Distancia") +
  ggtitle("Kernel Exponencial")

##### bandwidth kernel gausiano
g1 <- ggplot(data = data.frame(dist = c(-3, 3)),
       aes(dist)) +
  stat_function(fun = dnorm,
                n = 101,
                args = list(mean = 0,
                            sd = 1)) +
  ylim(c(0, 0.45)) + labs(y = "Densidad", x = "Distancia") +
  ggtitle("Bandwidth: 1")

g2 <- ggplot(data = data.frame(dist = c(-3, 3)),
         aes(dist)) +
  stat_function(fun = dnorm,
                n = 101,
                args = list(mean = 0,
                            sd = 2)) +
  ylim(c(0, 0.45)) + labs(y = "Densidad", x = "Distancia") +
  ggtitle("Bandwidth: 2")

g1/g2


### bandwidth difference
par(mfrow = c(2,1))
density(patron1_ppp, sigma = 1) %>%
  plot(col = viridisLite::viridis(n = 10), main = "Bandwidth: 1")
density(patron1_ppp, sigma = 2) %>%
  plot(col = viridisLite::viridis(n = 10), main = "Bandwidth: 2")
par(mfrow = c(1,1))

###
density(clmfires, sigma = 20) %>% plot()
aa <- clmfires %>%
  as_tibble() %>%
  select(x:cause) %>%
  as.ppp(
    W = owin(c(4.1311, 391.38), c(18.565, 385.19))
  )

density(aa, sigma = 20) %>% plot()



#### CRS
phom <- rpoispp(lambda = 1.81,
                win = owin(xrange = c(0, 20), yrange = c(0, 20)))

phom %>%
  as_tibble() %>%
  ggplot(aes(x, y)) +
  geom_point(size = 3) +
  geom_hline(
    yintercept = seq(0, 20, 5)
  ) +
  geom_vline(xintercept = seq(0, 20, 5)) +
  labs(x = NULL, y = NULL) + xlim(c(0, 20)) + ylim(c(0, 20)) +
  theme_bw() +
  theme(
    panel.grid = element_blank()
  ) +
  scale_x_continuous(expand = expansion(0)) +
  scale_y_continuous(expand = expansion(0))

####
p2_ppp <- as.ppp(patron2_tbl, W = ventana_owin)
plot(p2_ppp)
quadratcount(p2_ppp) %>% plot(add = TRUE, cex = 2, col = "red")


phom %>% plot()
quadratcount(phom) %>% plot(add = TRUE, cex = 2, col = "red")

quadratcount(phom) %>%
  as_tibble() %>%
  bind_cols(quadratcount(p2_ppp) %>% as_tibble()) %>%
  select(x1 = n...3, x2 = n...6) %>%
  mutate(
    w = ((x2 - x1)^2) / x1
  ) %>%
  summarise(total = sum(w))


#### Distribucion chi cuadrado
generate_chi_squared <- function(df, n_points = 1000) {
  x <- seq(0, 3 * df, length.out = n_points)
  y <- dchisq(x, df)
  data.frame(x = x, y = y)
}

# Definir los grados de libertad
df <- 24

# Generar los datos
data <- generate_chi_squared(df)

# Crear el gráfico
ggplot(data, aes(x, y)) +
  geom_line() +
  geom_area(data = subset(data, x >= qchisq(0.95, df)), fill = "#588B8B", alpha = 0.5) +
  geom_area(data = subset(data, x <= qchisq(0.95, df)), fill = "#F28F3B", alpha = 0.5) +
  scale_fill_manual(values = "red") +
  labs(x = NULL, y = NULL, title = "Distribución Chi-cuadrado") +
  theme_minimal() +
  theme(legend.position = "none") +
  annotate(
    "segment",
    x = 25, xend = 25, y = 0, yend = 0.013,
    linetype = 2
  ) +
  annotate(
    "richtext",
    label = "X^2^ = 25",
    x = 25, y = .016,
    hjust = 0,
    fill = NA, label.color = NA
  ) +
  annotate(
    "text",
    x = 32,
    y = .015,
    label = "Agregados",
    hjust = 0,
    size = 3
  ) +
  annotate(
    "text",
    x = .5,
    y = .03,
    label = "Regular",
    hjust = 0,
    size = 3
  ) +
  annotate(
    "curve",
    x    = 34,
    xend = 27,
    y    = .015 - .003,
    yend = .005,
    curvature = -.2,
    linewidth = .3,
    arrow     = arrow(angle = 30, type = "closed", length = unit(2, "mm"))
  ) +
  annotate(
    "curve",
    x    = 4,
    xend = 7.5,
    y    = .028,
    yend = .02,
    curvature = .2,
    linewidth = .3,
    arrow     = arrow(angle = 30, type = "closed", length = unit(2, "mm"))
  )


##
ggplot(data, aes(x, y)) +
  geom_line() +
  geom_area(data = subset(data, x >= qchisq(0.95, df)), fill = "#588B8B", alpha = 0.5) +
  geom_area(data = subset(data, x <= qchisq(0.05, df)), fill = "#F28F3B", alpha = 0.5) +
  scale_fill_manual(values = "red") +
  labs(x = NULL, y = NULL, title = "Distribución Chi-cuadrado",
       subtitle = "Df: 24") +
  theme_minimal() +
  theme(legend.position = "none") +
  annotate(
    "segment",
    x = qchisq(0.05, df),
    xend = qchisq(0.05, df),
    y = 0,
    yend = 0.02,
    linetype = 2
  ) +
  annotate(
    "richtext",
    label = "X^2^ = 13.8<br>p = 0.05",
    x = 13, y = .021,
    hjust = 1,
    size = 3,
    fill = NA, label.color = NA
  ) +
  annotate(
    "segment",
    x = qchisq(0.95, df), xend = qchisq(0.95, df), y = 0, yend = 0.013,
    linetype = 2
  ) +
  annotate(
    "richtext",
    label = "X^2^ = 36.4<br>p = 0.05",
    x = 36, y = .015,
    hjust = 0,
    size = 3,
    fill = NA, label.color = NA
  ) +
  annotate(
    "text",
    x = 42,
    y = .008,
    label = "Agregados",
    hjust = 0,
    size = 3
  ) +
  annotate(
    "text",
    x = 5,
    y = .013,
    label = "Regular",
    hjust = 0,
    size = 3
  ) +
  annotate(
    "curve",
    x    = 45,
    xend = 40,
    y    = .006,
    yend = .003,
    curvature = -.2,
    linewidth = .3,
    arrow     = arrow(angle = 30, type = "closed", length = unit(2, "mm"))
  ) +
  annotate(
    "curve",
    x    = 7,
    xend = 12,
    y    = .011,
    yend = .006,
    curvature = .2,
    linewidth = .3,
    arrow     = arrow(angle = 30, type = "closed", length = unit(2, "mm"))
  ) +
  annotate(
    "richtext",
    label = "Región de<br>aceptación de H~0~",
    x = 23, y = 0.03,
    hjust = .5,
    size = 3,
    fill = NA, label.color = NA
  )



################## BBLOQUE 2 ######################## -----

incendios_sf %>%
  ggplot(aes(fill = cut_number(Total_incendios, 5))) +
  geom_sf(color = "transparent", linewidth = 0) +
  scale_fill_viridis_d() +
  theme_void() +
  labs(fill = "Total incendios") +
  theme(plot.title = element_text(hjust = .5))

ran <- incendios_sf %>%
  mutate(
    random = rpois(313, 20)
  ) %>%
  ggplot(aes(fill = random)) +
  geom_sf(color = NA) +
  scale_fill_viridis_c() +
  theme_void() +
  labs(title = "No autocorrelación") +
  theme(legend.position = "none", plot.title = element_text(hjust = .5))

inc + ran

### Densidad incendios
incendios_sf %>%
  ggplot(aes(fill = cut_number(d_incendios, 5))) +
  geom_sf(color = "transparent", linewidth = 0) +
  scale_fill_viridis_d() +
  theme_void() +
  labs(fill = "Incendios / km^2^") +
  theme(
    plot.title = element_text(hjust = .5),
    legend.title = element_markdown()
  )

###
incendios_sf %>%
  ggplot() +
  geom_sf(fill = "transparent", linewidth = 0.1) +
  geom_sf(
    data = incendios_sf %>%
      filter(Municipio == "Bóveda"), fill = "grey20"
  ) +
  geom_sf(
    data = incendios_sf %>%
      filter(
        Municipio %in% c("Saviñao, O", "Pobra do Brollón, A",
                         "Monforte de Lemos", "Paradela", "Incio, O")
      ),
    fill = "grey60"
  ) +
  theme_void() +
  labs(fill = "Incendios / km^2^") +
  theme(
    plot.title = element_text(hjust = .5),
    legend.title = element_markdown()
  )

ids <- incendios_sf %>%
  mutate(
    nb = sfdep::st_knn(st_geometry(incendios_sf), k = 3),
    wt = st_weights(nb)
  ) %>%
  filter(
    Municipio == "Bóveda"
  ) %>%
  unnest(nb) %>%
  pull(nb)

##
incendios_sf %>%
  ggplot() +
  geom_sf(fill = "transparent", linewidth = 0.1) +
  geom_sf(
    data = incendios_sf %>%
      filter(Municipio == "Bóveda"), fill = "grey20"
  ) +
  geom_sf(
    data = incendios_sf[ids,],
    fill = "grey60"
  ) +
  geom_sf(
    data = st_centroid(incendios_sf)
  ) +
  theme_void() +
  labs(fill = "Incendios / km^2^") +
  theme(
    plot.title = element_text(hjust = .5),
    legend.title = element_markdown()
  )

##
ids <- incendios_sf %>%
  mutate(
    nb = st_dist_band(st_geometry(incendios_sf), upper = 30000),
    wt = st_weights(nb)
  ) %>%
  filter(
    Municipio == "Bóveda"
  ) %>%
  unnest(nb) %>%
  pull(nb)

incendios_sf %>%
  ggplot() +
  geom_sf(fill = "transparent", linewidth = 0.1) +
  geom_sf(
    data = incendios_sf %>%
      filter(Municipio == "Bóveda"), fill = "grey20"
  ) +
  geom_sf(
    data = incendios_sf[ids,],
    fill = "grey60"
  ) +
  geom_sf(
    data = st_centroid(incendios_sf)
  ) +
  geom_sf(
    data = st_buffer(
      incendios_sf %>%
        filter(Municipio == "Bóveda") %>%
        st_centroid(),
      dist = units::set_units(30, km)
    ),
    fill = "transparent",
    color = "red"
  ) +
  theme_void() +
  labs(fill = "Incendios / km^2^") +
  theme(
    plot.title = element_text(hjust = .5),
    legend.title = element_markdown()
  )


incendios_sf %>%
  st_geometry() %>%
  st_knn(3) ->aa

plot(aa, st_geometry(incendios_sf))

####
aa <- st_contiguity(incendios_sf)
st_weights(aa, allow_zero = TRUE, style = "C")
st_inverse_distance(aa, incendios_sf %>% st_geometry())
##
incendios_adyacencia_sf %>%
  filter(Municipio == "Bóveda") %>%
  unnest(c(nb, wt))
## Autoclrreación positiva


g1 <- expand_grid(
  x = 1:7,
  y = 1:7
) %>%
  mutate(
    z = rep(c(0, 1), 25) %>% .[-50] %>% as.factor()
  ) %>%
  ggplot(
    aes(x, y, fill = z)
  ) +
  geom_tile(color = "gray20") +
  scale_fill_manual(
    values = c("snow", "gray20")
  ) +
  labs(
    title = "Autocorrelación negativa"
  ) +
  coord_equal() +
  theme_void() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = .5, face = "bold")
  )


g2 <- expand_grid(
  x = 1:7,
  y = 1:7
) %>%
  mutate(
    z = c(rep(1, 21), rep(0, 28)) %>% as.factor()
  ) %>%
  ggplot(
    aes(x, y, fill = z)
  ) +
  geom_tile(color = "gray20") +
  scale_fill_manual(
    values = c("snow", "gray20")
  ) +
  labs(
    title = "Autocorrelación positiva"
  ) +
  coord_equal() +
  theme_void() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = .5, face = "bold")
  )

set.seed(137)
g3 <- expand_grid(
  x = 1:7,
  y = 1:7
) %>%
  mutate(
    z = sample(c(0, 1), 49, replace = TRUE) %>% as.factor()
  ) %>%
  ggplot(
    aes(x, y, fill = z)
  ) +
  geom_tile(color = "gray20") +
  scale_fill_manual(
    values = c("snow", "gray20")
  ) +
  labs(
    title = "Ausencia de autocorrelación"
  ) +
  coord_equal() +
  theme_void() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = .5, face = "bold")
  )


g1 + g3 + g2


###
incendios_sf %>%
  ggplot(aes(fill = Total_incendios)) +
  geom_sf(color = "transparent", linewidth = 0) +
  scale_fill_viridis_c() +
  theme_void() +
  labs(fill = "Total incendios") +
incendios_adyacencia_sf %>%
  ggplot(aes(fill = lag_1)) +
  geom_sf(color = "transparent", linewidth = 0) +
  scale_fill_viridis_c() +
  theme_void() +
  labs(fill = "Total incendios (lag)")


###
incendios_lag_sf <- incendios_sf %>%
  mutate(
    nb        = st_contiguity(incendios_sf),
    nb_2      = st_nb_lag(nb, order = 2),
    nb_acum_2 = st_nb_lag_cumul(nb, order = 2),
    across(
      starts_with("nb"),
      \(x) st_weights(x, allow_zero = TRUE, style = "W"),
      .names = "wt_{.col}"
    ),
    .before   = 2
  ) %>%
  mutate(
    incendios_lag2 = st_lag(Total_incendios, nb, wt_nb)
  )

## Visualizar incendios con lag
incendios_lag_sf %>%
  ggplot(aes(fill = cut_number(incendios_lag2, 5))) +
  geom_sf(color = "transparent", linewidth = 0) +
  scale_fill_viridis_d() +
  theme_void() +
  labs(fill = "Incendios") +
  theme(
    plot.title = element_text(hjust = .5),
    legend.title = element_markdown()
  )

####
incendios_sf %>%
  group_by(Provincia) %>%
  summarise(Total_incendios = sum(Total_incendios)) %>%
  ungroup() %>%
  ggplot(aes(fill = Total_incendios)) +
  geom_sf(color = "transparent", linewidth = 0) +
  scale_fill_viridis_c() +
  theme_void() +
  labs(fill = "Incendios") +
  theme(
    plot.title = element_text(hjust = .5),
    legend.title = element_markdown()
  )









####
plot_pvalue <- function(alt) {
  incendios_adyacencia_sf %>%
    mutate(
      moran_i = local_moran(
        x           = d_incendios,
        nb          = nb,
        wt          = wt,
        alternative = alt
      )
    ) %>%
    unnest(moran_i) %>%
    select(-nb, -wt) %>%
    # mutate(
    #   p_folded_sim = 2 * p_folded_sim
    # ) %>%
    pivot_longer(
      cols = starts_with("p_")
    ) %>%
    mutate(
      value = if_else(
        value < 0.05, "Significant", "Not significant"
      ) %>% as.factor()
    ) %>%
    ggplot() +
    geom_sf(
      aes(fill = value)
    ) +
    scale_fill_manual(
      values = c("grey20", "grey80"),
      name   = NULL
    ) +
    facet_wrap(~ name) +
    labs(
      title = str_glue("Alternative: {alt}")
    ) +
    theme_void() +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = .5, face = "bold")
    )
}

library(patchwork)
plot_pvalue("less") / plot_pvalue("two.sided") / plot_pvalue("greater") +
  theme(legend.position = "bottom")

### Josiah ----
plot_line_pvalue <- function(alt) {
  dat <- incendios_adyacencia_sf %>%
    mutate(
      moran_i = local_moran(
        x           = d_incendios,
        nb          = nb,
        wt          = wt,
        alternative = alt
      )
    ) %>%
    unnest(moran_i) %>%
    mutate(
      p_folded_sim = 2 * p_folded_sim
    ) %>%
    select(-nb, -wt) %>%
    select(starts_with("p_"))

  dat %>%
    ggplot(aes(p_ii, p_ii_sim)) +
    geom_point() +
    theme_bw() +
    labs(title = str_glue("Alternative: {alt}")) +
    dat %>%
    ggplot(aes(p_ii, p_folded_sim)) +
    geom_point() +
    theme_bw() +
    dat %>%
    ggplot(aes(p_ii_sim, p_folded_sim)) +
    geom_point() +
    theme_bw()

}

plot_line_pvalue("less") / plot_line_pvalue("two.sided") / plot_line_pvalue("greater")











