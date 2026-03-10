# ============================================================
# itscausal — showcase simulation figure
# 100 individuals × 84 months | 24 post-intervention periods
# Models: Random Forest vs XGBoost vs Ensemble
# ============================================================

devtools::load_all(quiet = TRUE)
library(data.table)
library(ggplot2)

set.seed(20240202)

# ── 1. Simulate panel ────────────────────────────────────────
n_id        <- 100
n_time      <- 84        # 60 pre + 24 post
INDEX       <- 60L
true_effect <- -5        # level shift applied at intervention

ar1 <- function(n, phi = 0.65, sigma = 1.2) {
  e <- rnorm(n, 0, sigma)
  x <- numeric(n)
  x[1] <- e[1]
  for (t in 2:n) x[t] <- phi * x[t - 1] + e[t]
  x
}

X     <- sample(c(0, 1), n_id, replace = TRUE)
id_fx <- rnorm(n_id, 0, 4)

df <- data.table(
  ID    = rep(seq_len(n_id), each = n_time),
  time  = rep(seq_len(n_time), n_id),
  X     = rep(X, each = n_time),
  id_fx = rep(id_fx, each = n_time)
)

season_coefs <- c(0, 1.8, 3.2, 3.6, 2.5, 0.8, -1.0, -2.3, -2.6, -1.7, -0.3, 0.5)
df[, month  := ((time - 1) %% 12) + 1]
df[, year   := ((time - 1) %/% 12) + 1]
df[, season := season_coefs[month]]
df[, error  := unlist(replicate(n_id, ar1(n_time), simplify = FALSE))]
df[, post   := as.integer(time >= INDEX)]
df[, y      := 10 + 0.08 * time        # upward trend
              + 1.2 * X                 # group effect
              + season                  # seasonality
              + id_fx                   # individual FE
              + true_effect * post      # treatment
              + error]                  # AR noise

# ── 2. Run ITS models ────────────────────────────────────────
args_common <- list(
  time            = "time",
  key             = "ID",
  y               = "y",
  INDEX           = INDEX,
  WINDOW          = 12L,
  STEPS           = 6L,
  covariates_time = c("month", "year"),
  covariates_fix  = "X",
  K               = 5L
)

message("Fitting Random Forest …")
fore_rf  <- do.call(forecastITS, c(list(data = copy(df), method = "rf"),       args_common))
message("Fitting XGBoost …")
fore_xgb <- do.call(forecastITS, c(list(data = copy(df), method = "xgboost"),  args_common))
message("Fitting Ensemble …")
fore_ens <- do.call(forecastITS, c(list(data = copy(df), method = c("rf", "xgboost")), args_common))

# ── 3. ITE & ATE ─────────────────────────────────────────────
ite_rf  <- iteITS(fore_rf)
ite_xgb <- iteITS(fore_xgb)
ite_ens <- iteITS(fore_ens)

n_post <- max(ite_ens$ites$time)

# ── 4. Tidy helper ───────────────────────────────────────────
tidy_out <- function(fore, label) {
  d <- data.table(fore$out)
  setnames(d, c("ID", "time_c", "y_hat"))
  d[time_c > 0, .(y_hat = mean(y_hat, na.rm = TRUE)), by = time_c][, model := label]
}

tidy_ite <- function(ite, label) {
  d <- as.data.frame(ite$ites)[, c("time", "ite", "sd")]
  d <- aggregate(cbind(ite, sd) ~ time, data = d, FUN = mean)
  d$model <- label
  d
}

obs <- df[, .(y = mean(y)), by = .(time_c = time - INDEX)]

preds <- rbindlist(list(
  tidy_out(fore_rf,  "Random Forest"),
  tidy_out(fore_xgb, "XGBoost"),
  tidy_out(fore_ens, "Ensemble")
))

ites <- rbind(
  tidy_ite(ite_rf,  "Random Forest"),
  tidy_ite(ite_xgb, "XGBoost"),
  tidy_ite(ite_ens, "Ensemble")
)

# ── 5. Palette & theme ───────────────────────────────────────
pal <- c(
  "Random Forest" = "#e07b54",
  "XGBoost"       = "#4a90d9",
  "Ensemble"      = "#5aab61"
)

base_theme <- theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position    = "bottom",
    legend.title       = element_blank(),
    plot.title         = element_text(face = "bold", size = 14),
    plot.subtitle      = element_text(colour = "grey40", size = 11),
    axis.title         = element_text(size = 11),
    strip.text         = element_text(face = "bold")
  )

# ── 6. Panel A: Observed vs counterfactual ───────────────────
# Pre-period observed line
obs_pre  <- obs[time_c < 0]
obs_post <- obs[time_c >= 0]

p_forecast <- ggplot() +
  # Pre-period: single black line
  geom_line(data = obs_pre, aes(x = time_c, y = y),
            colour = "grey20", linewidth = 0.9) +
  # Post-period observed
  geom_line(data = obs_post, aes(x = time_c, y = y),
            colour = "grey20", linewidth = 0.9) +
  # Counterfactual lines by model
  geom_line(data = preds, aes(x = time_c, y = y_hat, colour = model, linetype = model),
            linewidth = 0.85) +
  # Shade the gap (ensemble vs observed) in the post period
  geom_ribbon(
    data = merge(
      obs_post,
      preds[model == "Ensemble"],
      by = "time_c"
    ),
    aes(x = time_c, ymin = pmin(y, y_hat), ymax = pmax(y, y_hat)),
    fill = pal["Ensemble"], alpha = 0.12
  ) +
  # Intervention line
  geom_vline(xintercept = 0, linetype = "dashed", colour = "firebrick",
             linewidth = 0.7) +
  annotate("text", x = 0.5, y = max(obs$y) - 0.3,
           label = "Intervention", hjust = 0, colour = "firebrick", size = 3.5) +
  scale_colour_manual(values = pal) +
  scale_linetype_manual(values = c("Random Forest" = "solid",
                                   "XGBoost"       = "solid",
                                   "Ensemble"      = "longdash")) +
  labs(
    title    = "Observed outcome vs ML-based counterfactual forecast",
    subtitle = paste0("Population mean | true level shift = ", true_effect,
                      " | 24 post-intervention periods"),
    x = "Time relative to intervention (months)",
    y = "Mean outcome"
  ) +
  base_theme

# ── 7. Panel B: ITE over time ────────────────────────────────
p_ite <- ggplot(ites, aes(x = time, y = ite, colour = model, fill = model)) +
  geom_hline(yintercept = true_effect, linetype = "dashed",
             colour = "firebrick", linewidth = 0.7) +
  geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.4) +
  geom_ribbon(aes(ymin = ite - sd, ymax = ite + sd), alpha = 0.15,
              colour = NA) +
  geom_line(linewidth = 0.85) +
  annotate("text", x = n_post - 0.5, y = true_effect - 0.4,
           label = paste("True effect =", true_effect),
           colour = "firebrick", hjust = 1, size = 3.5) +
  scale_colour_manual(values = pal) +
  scale_fill_manual(values = pal) +
  labs(
    title    = "Estimated individual treatment effect (ITE) per post-intervention period",
    subtitle = "Mean ITE across units \u00b1 1 SD ribbon",
    x = "Post-intervention period (months)",
    y = "ITE (observed \u2212 counterfactual)"
  ) +
  base_theme

# ── 8. Combine & save ────────────────────────────────────────
library(patchwork)

fig <- (p_forecast / p_ite) +
  plot_annotation(
    caption = "itscausal package \u2014 RF vs XGBoost vs Ensemble | n = 100 units, 84 time periods",
    theme   = theme(plot.caption = element_text(colour = "grey50", size = 9))
  ) &
  theme(legend.position = "bottom")

ggsave("simulation_figure.png", fig,
       width = 10, height = 10, dpi = 300, bg = "white")

message("Figure saved to simulation_figure.png")
