# Always run this line to initiate the setup
source("setup.R")

# ============================================================
# 1. Model parameters
# ============================================================

set.seed(123)

mu <- 1.5
r <- 0
sigma <- 0.6
T <- 1

n_paths <- 15
n_steps <- 200

dt <- T / n_steps
times <- seq(0, T, length.out = n_steps + 1)

# ============================================================
# 2. Brownian Motion
# ============================================================

dW <- matrix(
  rnorm(n_paths * n_steps, sd = sqrt(dt)),
  nrow = n_paths
)

W <- cbind(0, t(apply(dW, 1, cumsum)))

# Arithmetic Brownian motion under P and Q
X_P <- sweep(sigma * W, 2, mu * times, "+")
X_Q <- sweep(sigma * W, 2, r * times, "+")

# Convert paths to plotting data
make_paths <- function(X, label) {
  data.frame(
    time = rep(times, times = n_paths),
    value = as.vector(t(X)),
    path = rep(1:n_paths, each = length(times)),
    measure = label
  )
}

paths <- rbind(
  make_paths(X_P, "Physical (P)"),
  make_paths(X_Q, "Risk-neutral (Q)")
)

# ============================================================
# 3. Terminal Stock Density
# ============================================================

sd_T <- sigma * sqrt(T)

y_grid <- seq(
  min(mu, r) * T - 3 * sd_T,
  max(mu, r) * T + 3 * sd_T,
  length.out = 300
)

make_density <- function(drift, label) {
  
  density <- dnorm(
    y_grid,
    mean = drift * T,
    sd = sd_T
  )
  
  # Rotate density horizontally at time T
  data.frame(
    time = c(T, T + 0.30 * density / max(density), T),
    value = c(y_grid[1], y_grid, tail(y_grid, 1)),
    measure = label
  )
}

densities <- rbind(
  make_density(mu, "Physical (P)"),
  make_density(r, "Risk-neutral (Q)")
)

# ============================================================
# 4. VISUALIZATION
# ============================================================

p <- ggplot() +
  
  geom_vline(
    xintercept = T,
    linetype = "dashed",
    color = "grey50"
  ) +
  
  geom_polygon(
    data = densities,
    aes(x = time, y = value, fill = measure),
    alpha = 0.30
  ) +
  
  geom_path(
    data = densities,
    aes(x = time, y = value, color = measure),
    linewidth = 0.8
  ) +
  
  geom_line(
    data = paths,
    aes(x = time, y = value,
        group = path, color = measure),
    alpha = 0.55,
    linewidth = 0.5
  ) +
  
  facet_wrap(~measure, nrow = 1) +
  
  scale_color_manual(values = measure_colors) +
  scale_fill_manual(values = measure_colors) +
  
  coord_cartesian(xlim = c(0, T + 0.35)) +
  
  labs(
    title = "Brownian Motion under Two Probability Measures",
    subtitle = "Drift changes while volatility is preserved",
    x = "Time",
    y = expression(X[t])
  ) +
  
  theme(legend.position = "none")

print(p)

save_plot(p, "brownian_measure_change.png", 11, 5)
