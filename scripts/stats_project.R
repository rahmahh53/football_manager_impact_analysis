library(dplyr)
library(lubridate)
library(ggplot2)
library(mvtnorm)

set.seed(123)

# =================================
# Load data and basic preprocessing 
# =================================

data <- read.csv(
  "\\\\wsl.localhost\\Ubuntu\\home\\rah_mahh53\\stats_project\\epl_team_match_with_managers_windows.csv"
)

data$Date         <- as.Date(data$Date)
data$Season       <- as.character(data$Season)
data$ManagerStart <- as.Date(data$ManagerStart)
data$ManagerEnd   <- as.Date(data$ManagerEnd)

data$Season_raw    <- as.character(data$Season)
data$SeasonEndYear <- as.numeric(sub(".*(\\d{4})$", "\\1", data$Season_raw))

# =======================
# convert GF/GA to points
# =======================

points_from_goals <- function(gf, ga) {
  ifelse(gf > ga, 3,
         ifelse(gf == ga, 1, 0))
}

# =======================================================
# Season–by–season “5–match bounce’’ exploratory analysis
# =======================================================

# First match for each (Team, Manager) in the dataset
first_matches <- data %>%
  filter(!is.na(Manager), !is.na(MatchNumMgr)) %>%
  filter(MatchNumMgr == 1) %>%
  arrange(Date)

nrow(first_matches)
head(first_matches[, c("Season","Team","Manager","Date")])

compute_delta_for_change <- function(team_name, change_date, season_str) {
  # Filter to that club + season
  df_season <- data %>%
    filter(Team == team_name,
           Season == season_str) %>%
    arrange(Date)
  
  # "New manager" is whoever manages on the change_date
  new_mgr_row <- df_season %>%
    filter(Date == change_date)
  if (nrow(new_mgr_row) == 0) return(NA_real_)  
  
  new_mgr <- new_mgr_row$Manager[1]
  
  # 5 matches BEFORE change (any manager)
  before <- df_season %>%
    filter(Date < change_date) %>%
    arrange(Date) %>%
    tail(5)
  
  # 5 matches AFTER change (that manager only)
  after <- df_season %>%
    filter(Date >= change_date,
           Manager == new_mgr) %>%
    arrange(Date) %>%
    head(5)
  
  # Require at least 5 matches on each side
  if (nrow(before) < 5 || nrow(after) < 5) {
    return(NA_real_)
  }
  
  pts_before <- points_from_goals(before$GF, before$GA)
  pts_after  <- points_from_goals(after$GF, after$GA)
  
  delta <- sum(pts_after) - sum(pts_before)
  return(delta)
}

# Build a data frame of candidate changes
change_candidates <- first_matches %>%
  select(Season, Team, Manager, Date) %>%
  distinct()

# Compute Δ for each candidate
bounce_changes <- change_candidates %>%
  rowwise() %>%
  mutate(
    delta_5 = compute_delta_for_change(
      team_name   = Team,
      change_date = Date,
      season_str  = Season
    )
  ) %>%
  ungroup() %>%
  filter(!is.na(delta_5))   # to keep only valid changes

nrow(bounce_changes)
head(bounce_changes)

bounce_by_season <- bounce_changes %>%
  group_by(Season) %>%
  summarise(
    n_changes   = n(),
    mean_delta  = mean(delta_5),
    sd_delta    = sd(delta_5),
    se_delta    = sd_delta / sqrt(n_changes),
    prop_pos    = mean(delta_5 > 0),
    .groups     = "drop"
  ) %>%
  mutate(
    ci_lower = mean_delta - 1.96 * se_delta,
    ci_upper = mean_delta + 1.96 * se_delta
  )

bounce_by_season

ggplot(bounce_by_season,
       aes(x = Season, y = mean_delta, group = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(size = 2) +
  geom_line() +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.15) +
  labs(
    title = "Average 5-match new-manager bounce by season",
    x = "Season",
    y = "Mean Δ points (after 5 – before 5)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ====================================
# 3. Bayesian multivariate regression 
# ====================================

Y <- as.matrix(data[, c("GF", "GA")])
dim(Y)
colnames(Y)

X <- model.matrix(~ HomeFlag + Window1 + Window2 + Window3, data)
dim(X)
colnames(X)

any(is.na(X))
any(is.na(Y))

summary(data$GF)
summary(data$GA)
colSums(data[, c("Window1","Window2","Window3")])

# checking with a simple OLS
lm(GF ~ HomeFlag + Window1 + Window2 + Window3, data)
lm(GA ~ HomeFlag + Window1 + Window2 + Window3, data)

lm(GF ~ Team + HomeFlag + Window1 + Window2 + Window3, data)
lm(GA ~ Team + HomeFlag + Window1 + Window2 + Window3, data)

# Conjugate MN–IW posterior
n <- nrow(X); p <- ncol(X); d <- ncol(Y)

S0 <- diag(d)
B0 <- matrix(0, nrow = p, ncol = d)
V0 <- 10 * diag(p)
V0_inv <- solve(V0)

Vn <- solve(V0_inv + t(X) %*% X)
Bn <- Vn %*% (V0_inv %*% B0 + t(X) %*% Y)

little_v0 <- d + 2
little_vn <- little_v0 + n

residual <- Y - X %*% Bn
data_fit <- t(residual) %*% residual
reg <- t(Bn - B0) %*% V0_inv %*% (Bn - B0)
Sn <- S0 + data_fit + reg

# Quick check: one example predictive mean/cov for a specific x_new
x_new <- c(1, 0, 0, 1, 0)  
x_new <- matrix(x_new, ncol = 1)  

mu_pred <- t(Bn) %*% x_new

vn_pred <- little_vn - d + 1
scale_factor <- as.numeric(1 + t(x_new) %*% Vn %*% x_new)
Lambda <- (scale_factor / vn_pred) * Sn

mu_pred
Lambda

# ===============================================================
# 4. Predictive distribution + expected points for a single match
# ===============================================================

expected_points_for_x <- function(x_new, n_samp = 5000) {
  x_new <- matrix(x_new, ncol = 1)      # p x 1 matrix
  
  # predictive df
  d <- nrow(Sn)                         # here d = 2
  nu_pred <- little_vn - d + 1
  
  # predictive mean
  mu_pred <- t(Bn) %*% x_new            # 2 x 1 matrix
  
  # scale factor: 1 + x^T Vn x
  scale_factor <- as.numeric(1 + t(x_new) %*% Vn %*% x_new)
  
  # predictive scale matrix Lambda (2 x 2 matrix)
  Lambda <- (scale_factor / nu_pred) * Sn
  
  # draw samples from predictive distribution
  y_pred <- rmvt(
    n     = n_samp,
    sigma = Lambda,
    df    = nu_pred,
    delta = as.vector(mu_pred),
    type  = "shifted"
  )
  colnames(y_pred) <- c("GF", "GA")
  
  # convert goals to points
  pts <- ifelse(y_pred[, "GF"] > y_pred[, "GA"], 3,
                ifelse(y_pred[, "GF"] == y_pred[, "GA"], 1, 0))
  
  list(
    expected_points = mean(pts),
    pts_samples     = pts,
    gf_ga_samples   = y_pred
  )
}

# ==================================
# 5. Baseline vs Window1/2/3 at home
# ==================================

x_base_home <- c(1, 0, 0, 0, 0)
x_win1_home <- c(1, 0, 1, 0, 0)
x_win2_home <- c(1, 0, 0, 1, 0)
x_win3_home <- c(1, 0, 0, 0, 1)

n_samp <- 5000
res_base_home <- expected_points_for_x(x_base_home, n_samp = n_samp)  # NO WINDOWS
res_w1_home   <- expected_points_for_x(x_win1_home, n_samp = n_samp)  # WINDOW 1
res_w2_home   <- expected_points_for_x(x_win2_home, n_samp = n_samp)  # WINDOW 2
res_w3_home   <- expected_points_for_x(x_win3_home, n_samp = n_samp)  # WINDOW 3

res_base_home$expected_points  
res_w1_home$expected_points
res_w2_home$expected_points
res_w3_home$expected_points

delta1_samples <- res_w1_home$pts_samples - res_base_home$pts_samples
delta2_samples <- res_w2_home$pts_samples - res_base_home$pts_samples
delta3_samples <- res_w3_home$pts_samples - res_base_home$pts_samples

delta1_mean <- mean(delta1_samples)
delta2_mean <- mean(delta2_samples)
delta3_mean <- mean(delta3_samples)

prob_delta1_pos <- mean(delta1_samples > 0)
prob_delta2_pos <- mean(delta2_samples > 0)
prob_delta3_pos <- mean(delta3_samples > 0)

delta1_mean; prob_delta1_pos
delta2_mean; prob_delta2_pos
delta3_mean; prob_delta3_pos

# ============================================================
# 6. Simulate schedule points for a sequence of future matches
# ============================================================

simulate_schedule_points <- function(X_future, n_samp = 5000) {
  H <- nrow(X_future)               # number of remaining matches
  
  pts_mat <- matrix(NA_real_, nrow = n_samp, ncol = H)
  
  for (j in 1:H) {
    res_j <- expected_points_for_x(X_future[j, ], n_samp = n_samp)
    pts_mat[, j] <- res_j$pts_samples
  }
  
  total_pts <- rowSums(pts_mat)
  total_pts
}

# ==========================================
# 7. Club-level spells: example for Chelsea
# ==========================================

manager_sack_date <- as.Date("2021-01-25")  # Frank Lampard sack date example

chelsea <- data %>%
  filter(Team == "Chelsea") %>%
  arrange(Date)

chelsea_spells <- chelsea %>%
  group_by(Manager, ManagerStart) %>%
  summarise(
    spell_start = min(Date),
    spell_end   = max(Date),
    .groups = "drop"
  ) %>%
  arrange(spell_start)

chelsea_spells

# ================
# 8. KEEP vs SACK
# ================

evaluate_spell <- function(club_df, spell_end_date,
                           n_samp = 5000, C = 2) {
  future <- club_df %>%
    filter(Date > spell_end_date) %>%
    arrange(Date)
  
  H <- nrow(future)
  if (H == 0) {
    return(NULL)
  }
  
  X_keep <- cbind(
    Intercept = rep(1, H),
    HomeFlag  = future$HomeFlag,
    Window1   = rep(0, H),
    Window2   = rep(0, H),
    Window3   = rep(0, H)
  )
  
  X_sack <- X_keep
  if (H >= 1) X_sack[1, "Window1"] <- 1
  if (H >= 2) X_sack[2, "Window2"] <- 1
  if (H >= 3) X_sack[3, "Window3"] <- 1
  
  pts_keep <- simulate_schedule_points(X_keep, n_samp)
  pts_sack <- simulate_schedule_points(X_sack, n_samp)
  
  raw_delta <- pts_sack - pts_keep
  delta_C   <- raw_delta - C
  
  data.frame(
    H_future        = H,
    mean_raw        = mean(raw_delta),
    prob_raw_pos    = mean(raw_delta > 0),
    C_star          = median(raw_delta),
    mean_delta_C    = mean(delta_C),
    prob_deltaC_pos = mean(delta_C > 0)
  )
}

# ================================
# 9. Evaluate all Chelsea managers
# ================================

results_list <- list()

for (i in seq_len(nrow(chelsea_spells))) {
  mgr_name  <- chelsea_spells$Manager[i]
  spell_end <- chelsea_spells$spell_end[i]
  
  res_i <- evaluate_spell(
    club_df        = chelsea,
    spell_end_date = spell_end,
    n_samp         = 5000,
    C              = 1
  )
  
  if (!is.null(res_i)) {
    res_i$Manager  <- mgr_name
    res_i$SpellEnd <- spell_end
    results_list[[length(results_list) + 1]] <- res_i
  }
}

chelsea_results <- bind_rows(results_list) %>%
  select(Manager, SpellEnd, H_future,
         mean_raw, prob_raw_pos,
         C_star, mean_delta_C, prob_deltaC_pos)

chelsea_results

# ==========================================================
# 10. Specific Frank Lampard sack vs keep scenario (2020/21)
# ==========================================================

chelsea_lampard <- chelsea %>%
  filter(Date >= as.Date("2020-08-01"),
         Date <= as.Date("2021-06-30")) %>%
  arrange(Date)

chelsea_future <- chelsea_lampard %>%
  filter(Date > manager_sack_date) %>%
  arrange(Date)

H <- nrow(chelsea_future)

X_keep <- cbind(
  Intercept = rep(1, H),
  HomeFlag  = chelsea_future$HomeFlag,
  Window1   = rep(0, H),
  Window2   = rep(0, H),
  Window3   = rep(0, H)
)

X_sack <- X_keep
if (H >= 1) X_sack[1, "Window1"] <- 1
if (H >= 2) X_sack[2, "Window2"] <- 1
if (H >= 3) X_sack[3, "Window3"] <- 1

n_samp <- 5000
pts_keep <- simulate_schedule_points(X_keep, n_samp)
pts_sack <- simulate_schedule_points(X_sack, n_samp)

raw_delta <- pts_sack - pts_keep 
mean(raw_delta)
mean(raw_delta > 0)
C_star <- median(raw_delta)
C_star

# ===========================
# 11. Policy curve: Pr(Δ > 0) 
# ===========================

C_values <- seq(-5, 5, length.out = 200)

prob_curve <- sapply(C_values, function(C){
  mean(raw_delta - C > 0)
})

plot(C_values, prob_curve, type = "l", lwd = 2,
     xlab = "Firing Cost C", ylab = "Pr(Δ > 0)",
     main = "Posterior Probability That Sacking Is Beneficial")

abline(h = 0.5, col = "red", lty = 2)
abline(v = C_star, col = "blue", lty = 2)

# ===================================================
# 12. Posterior distribution of net gain from sacking 
# ===================================================

C <- 1
delta_samples <- raw_delta - C

delta_df <- data.frame(delta = delta_samples)

ggplot(delta_df, aes(x = delta)) +
  geom_density(fill = "grey80") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Posterior distribution of net gain from sacking",
    x = "Δ points (sack - keep - cost)",
    y = "Density"
  )

# =========================================================
# 13. Probability sacking is better vs different sack costs
# =========================================================

C_grid <- seq(0, 6, by = 0.5)
prob_pos_vec <- numeric(length(C_grid))

for (k in seq_along(C_grid)) {
  Ck <- C_grid[k]
  delta_Ck <- pts_sack - pts_keep - Ck
  prob_pos_vec[k] <- mean(delta_Ck > 0)
}

policy_df <- data.frame(
  cost = C_grid,
  prob_better_than_keep = prob_pos_vec
)

ggplot(policy_df, aes(x = cost, y = prob_better_than_keep)) +
  geom_line() +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  labs(
    title = "Probability sacking is better than keeping vs sack cost",
    x = "Cost of sacking (points equivalent)",
    y = "Pr(Δ > 0)"
  ) +
  ylim(0, 1)

# ==========================================================
# 14. Window-level bounce summary plot (Window1/2/3 vs base)
# ==========================================================

mean_cl_normal <- function(x) {
  m <- mean(x)
  se <- sd(x) / sqrt(length(x))
  data.frame(y = m, ymin = m - 1.96 * se, ymax = m + 1.96 * se)
}

window_df <- rbind(
  data.frame(window = "Window1", delta = delta1_samples),
  data.frame(window = "Window2", delta = delta2_samples),
  data.frame(window = "Window3", delta = delta3_samples)
)

ggplot(window_df, aes(x = window, y = delta)) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  geom_hline(yintercept = 0) +
  labs(
    title = "Posterior mean and intervals of bounce effect",
    x = "Window",
    y = "Δ expected points vs no-window"
  )



