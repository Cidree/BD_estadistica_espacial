
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








