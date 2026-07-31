# Model:
#   GF ~ HomeFlag + ManagerPeriod
#        + team effect
#        + opponent effect
#        + season effect

library(brms)
library(cmdstanr)
library(dplyr)
library(readr)
library(here)
library(ggplot2)

set.seed(42)

options(brms.backend = "cmdstanr", mc.cores = min(2, parallel::detectCores()))

data_path <- here("data", "processed_data", "manager_analysis_data.csv")
data <- read_csv(data_path, col_types = cols(Date = col_date(format = "%Y-%m-%d"), ManagerStart = col_date(format = "%Y-%m-%d"), ManagerEnd = col_date(format = "%Y-%m-%d"),.default = col_guess()), show_col_types = FALSE)

message("Loaded", nrow(data), " rows and ", ncol(data), " columns.")

# prepare modeling variables

data <- data %>% mutate(Team = factor(Team), Opponent = factor(Opponent), Season = factor(Season), Manager = factor(Manager),
                        HomeFlag = as.integer(HomeFlag),
                        ManagerPeriod = factor(ManagerPeriod, levels = c("Established", "Matches 1-3", "Matches 4-6", "Matches 7-10")))

# validate modeling data

required_columns <- c("GF", "GA", "HomeFlag", "ManagerPeriod", "Team", "Opponent", "Season", "Manager", "MatchID")
stopifnot(nrow(data) == 7600, all(data$GF >= 0), all(data$GA >= 0), all(data$HomeFlag %in% c(0L, 1L)), !any(is.na(data$GF)), !any(is.na(data$ManagerPeriod)), !any(is.na(data$Team)), !any(is.na(data$Opponent)), !any(is.na(data$Season)))

# inspect overall goal distribution

goals_summary <- data %>% summarise(observations = n(), mean_goals = mean(GF), variance_goals = var(GF), standard_deviation = sd(GF), zero_goal_proportion = mean(GF == 0), maximum_goals = max(GF))
print(goals_summary)

# goals-scored model formula

goals_model <- bf(GF ~ HomeFlag + ManagerPeriod + (1|Team) + (1|Opponent) + (1|Season))

# inspect available prior parameters

priors <- get_prior(formula=goals_model, data=data, family=poisson(link = "log"))
print(priors)

# prior specification

goals_priors <- c(prior(normal(log(1.4), 0.20), class = "Intercept"),
                  prior(normal(0, 0.15), class = "b"),
                  prior(normal(0, 0.20), class = "sd", group = "Team"),
                  prior(normal(0, 0.20), class = "sd", group = "Opponent"),
                  prior(normal(0, 0.10), class = "sd", group = "Season"))

# validate specified prior

validated_priors <- validate_prior(prior = goals_priors, formula = goals_model, data = data, family = poisson(link = "log"), sample_prior = "only")
print(validated_priors)

# prior-predictive model

goals_prior_model <- brm(formula = goals_model, data = data, family = poisson(link = "log"), prior = goals_priors,
                         sample_prior = "only", chains = 2, cores = 2, iter = 1000, warmup = 500, backend = "cmdstanr", seed = 42, refresh = 100,
                         file = here("outputs", "models", "goals_prior_predictive"),
                         file_refit = "on_change")

prior_predictions <- posterior_predict(goals_prior_model, ndraws = 200)
prior_prediction_summary <- tibble(draw = seq_len(nrow(prior_predictions)), mean_goals = rowMeans(prior_predictions), maximum_goals = apply(prior_predictions, 1, max), proportion_above_9 = rowMeans(prior_predictions > 9))
print(prior_prediction_summary %>% summarise(median_mean_goals = median(mean_goals), lower_mean_goals = quantile(mean_goals, 0.025), upper_mean_goals = quantile(mean_goals, 0.975), median_maximum_goals = median(maximum_goals), upper_maximum_goals = quantile(maximum_goals, 0.975), median_proportion_above_9 = median(proportion_above_9)))

goals_prior_plot <- pp_check(goals_prior_model, type = "bars", ndraws = 100)
print(goals_prior_plot)

ggsave(filename = here("outputs", "figures", "goals_prior_predictive.png"), plot = goals_prior_plot, width = 9, height = 6, dpi = 300)
message("Prior-predictive figure saved to: ", here("outputs", "figures", "goals_prior_predictive.png"))


# posterior smoke-test model

goals_for_smoke_model <- brm(formula = goals_model, data = data, family = poisson(link = "log"), prior = goals_priors, sample_prior = "yes",
                             chains = 2, cores = 2, iter = 1000, warmup = 500, backend = "cmdstanr", seed = 42,
                             control = list(adapt_delta = 0.95), refresh = 100,
                             file = here("outputs", "models", "goals_for_poisson_smoke"), file_refit = "on_change")
print(summary(goals_for_smoke_model))

nuts_diagnostics <- nuts_params(goals_for_smoke_model)
divergence_count <- nuts_diagnostics %>% filter(Parameter == "divergent__") %>% summarise(divergences = sum(Value))
print(divergence_count)

smoke_draws <- posterior::as_draws_array(goals_for_smoke_model)
rhat_summary <- posterior::summarise_draws(smoke_draws, posterior::default_convergence_measures())
valid_rhat_summary <- rhat_summary %>% filter(is.finite(rhat))
maximum_rhat <- max(valid_rhat_summary$rhat)
print(valid_rhat_summary %>% arrange(desc(rhat)) %>% slice_head(n = 10))
message("Maximum R-hat: ", round(maximum_rhat, 4))

# full goals-scored Poisson model

goals_poisson_model <- brm(formula = goals_model, data = data, family = poisson(link = "log"), prior = goals_priors, sample_prior = "yes",
                           chains = 4, cores = 4, iter = 4000, warmup = 2000, backend = "cmdstanr", seed = 42,
                           control = list(adapt_delta = 0.95), refresh = 100,
                           file = here("outputs", "models", "goals_for_poisson_4000"), file_refit = "on_change")
print(summary(goals_poisson_model))

full_nuts_diagnostics <- nuts_params(goals_poisson_model)
full_divergence_count <- full_nuts_diagnostics %>% filter(Parameter == "divergent__") %>% summarise(divergences = sum(Value))
print(full_divergence_count)

# model diagnostics

full_draws <- posterior::as_draws_array(goals_poisson_model)
full_diagnostic_summary <- posterior::summarise_draws(full_draws, posterior::default_convergence_measures())
valid_full_diagnostics <- full_diagnostic_summary %>% filter(is.finite(rhat))
print(valid_full_diagnostics %>% arrange(desc(rhat)) %>% slice_head(n = 10))

message("Maximum full-model R-hat: ", round(max(valid_full_diagnostics$rhat), 4))
message("Minimum bulk ESS: ", round(min(valid_full_diagnostics$ess_bulk), 0))
message("Minimum tail ESS: ", round(min(valid_full_diagnostics$ess_tail), 0))

goals_parameter_summary <- posterior::summarise_draws(full_draws, "mean", "median", "sd", ~posterior::quantile2(.x, probs = c(0.025, 0.975)), posterior::default_convergence_measures())
write_csv(goals_parameter_summary, here("outputs", "tables", "goals_poisson_summary.csv"))

# posterior-predictive check

goals_posterior_plot <- pp_check(goals_poisson_model, type = "bars", ndraws = 100)
print(goals_posterior_plot)
ggsave(filename=here("outputs", "figures", "goals_poisson_posterior_predictive.png"), plot = goals_posterior_plot, width = 9, height = 6, dpi = 300)

goals_replications <- posterior_predict(goals_poisson_model, ndraws = 500)
replicated_mean <- rowMeans(goals_replications)
replicated_variance <- apply(goals_replications, 1, var)
replicated_zero_proportion <- rowMeans(goals_replications == 0)
replicated_maximum <- apply(goals_replications, 1, max)
goals_predictive_summary <- tibble(metric = c("Mean goals", "Variance of goals", "Zero-goal proportion", "Maximum goals"),
                                                observed = c(mean(data$GF), var(data$GF), mean(data$GF == 0), max(data$GF)),
                                   predicted_median = c(median(replicated_mean), median(replicated_variance), median(replicated_zero_proportion), median(replicated_maximum)),
                                   predicted_lower_95 = c(quantile(replicated_mean, 0.025), quantile(replicated_variance, 0.025), quantile(replicated_zero_proportion, 0.025), quantile(replicated_maximum, 0.025)),
                                   predicted_upper_95 = c(quantile(replicated_mean, 0.975), quantile(replicated_variance, 0.975), quantile(replicated_zero_proportion, 0.975), quantile(replicated_maximum, 0.975)))
print(goals_predictive_summary)
write_csv(goals_predictive_summary, here("outputs", "tables", "goals_poisson_predictive_summary.csv")) 

# fixed-effect rate ratios

goals_fixed_effects <- as.data.frame(fixef(goals_poisson_model)) %>% tibble::rownames_to_column("term") %>% transmute(term, log_rate_estimate = Estimate, log_rate_lower_95 = Q2.5, log_rate_upper_95 = Q97.5, rate_ratio = exp(Estimate), rate_ratio_lower_95 = exp(Q2.5), rate_ratio_upper_95 = exp(Q97.5), percent_change = 100 * (exp(Estimate) - 1))
print(goals_fixed_effects)
write_csv(goals_fixed_effects, here("outputs", "tables", "goals_poisson_rate_ratios.csv"))

# goals-conceded model formula

goals_against_model <- bf(GA ~ HomeFlag + ManagerPeriod + (1 | Team) + (1 | Opponent) + (1 | Season))

goals_against_priors <- c(prior(normal(log(1.4), 0.20), class = "Intercept"),
                          prior(normal(0, 0.15), class = "b"),
                          prior(normal(0, 0.20), class = "sd", group = "Team"),
                          prior(normal(0, 0.20), class = "sd", group = "Opponent"),
                          prior(normal(0, 0.10), class = "sd", group = "Season"))
validated_goals_against_priors <- validate_prior(prior = goals_against_priors, formula = goals_against_model, data = data, family = poisson(link = "log"), sample_prior = "only")
print(validated_goals_against_priors)

# goals-conceded prior-predictive model
 
goals_against_prior_model <- brm(formula = goals_against_model, data = data, family = poisson(link = "log"), prior = goals_against_priors, sample_prior = "only",
                                 chains = 2, cores = 2, iter = 1000, warmup = 500,
                                 backend = "cmdstanr", seed = 42, refresh = 100,
                                 file = here("outputs", "models", "goals_against_prior_predictive"),  
                                 file_refit = "on_change")
goals_against_prior_plot <- pp_check(goals_against_prior_model, type = "bars", ndraws = 100)
print(goals_against_prior_plot)
ggsave(filename = here("outputs", "figures", "goals_against_prior_predictive.png"), plot = goals_against_prior_plot, width = 9, height = 6, dpi = 300)

goals_against_prior_predictions <- posterior_predict(goals_against_prior_model, ndraws = 200)
goals_against_prior_summary <- tibble(draw = seq_len(nrow(goals_against_prior_predictions)), mean_goals = rowMeans(goals_against_prior_predictions), maximum_goals = apply(goals_against_prior_predictions, 1, max), proportion_above_9 = rowMeans(goals_against_prior_predictions > 9))
print(goals_against_prior_summary %>% summarise(median_mean_goals = median(mean_goals), lower_mean_goals = quantile(mean_goals, 0.025), upper_mean_goals = quantile(mean_goals, 0.975), median_maximum_goals = median(maximum_goals), upper_maximum_goals = quantile(maximum_goals, 0.975), median_proportion_above_9 = median(proportion_above_9)))

# goals-conceded smoke test
goals_against_smoke_model <- brm(formula = goals_against_model, data = data, family = poisson(link = "log"), prior = goals_against_priors, sample_prior = "yes",
                                 chains = 2, cores = 2, iter = 1000, warmup = 500, backend = "cmdstanr", seed = 42, control = list(adapt_delta = 0.95), refresh = 100,
                                 file = here("outputs", "models", "goals_against_poisson_smoke"), file_refit = "on_change")
print(summary(goals_against_smoke_model))

goals_against_smoke_nuts <- nuts_params(goals_against_smoke_model)
goals_against_smoke_divergences <- goals_against_smoke_nuts %>% filter(Parameter == "divergent__") %>% summarise(divergences = sum(Value))
print(goals_against_smoke_divergences)

goals_against_smoke_draws <- posterior::as_draws_array(goals_against_smoke_model)

goals_against_smoke_diagnostics <- posterior::summarise_draws(goals_against_smoke_draws, posterior::default_convergence_measures())
valid_goals_against_smoke_diagnostics <- goals_against_smoke_diagnostics %>% filter(is.finite(rhat))
print(valid_goals_against_smoke_diagnostics %>% arrange(desc(rhat)) %>% slice_head(n = 10))

message( "Maximum goals-against smoke-model R-hat: ", round(max(valid_goals_against_smoke_diagnostics$rhat), 4))

# full goals-conceded Poisson model

goals_against_poisson_model <- brm(formula = goals_against_model, data = data, family = poisson(link = "log"), prior = goals_against_priors, sample_prior = "yes",
                                   chains = 4, cores = 4, iter = 4000, warmup = 2000, backend = "cmdstanr", seed = 42, control = list(adapt_delta = 0.95), refresh = 200,
                                   file = here("outputs", "models", "goals_against_poisson_4000"), file_refit = "on_change")
print(summary(goals_against_poisson_model))

# goals-conceded convergence diagnostics

goals_against_nuts <- nuts_params(goals_against_poisson_model)
goals_against_divergences <- goals_against_nuts %>% filter(Parameter == "divergent__") %>% summarise(divergences = sum(Value))
print(goals_against_divergences)

goals_against_draws <- posterior::as_draws_array(goals_against_poisson_model)

goals_against_diagnostics <- posterior::summarise_draws(goals_against_draws, posterior::default_convergence_measures())
valid_goals_against_diagnostics <- goals_against_diagnostics %>% filter(is.finite(rhat))
print(valid_goals_against_diagnostics %>% arrange(desc(rhat)) %>% slice_head(n = 10))
message("Maximum goals-against R-hat: ", round(max(valid_goals_against_diagnostics$rhat), 4))
message("Minimum goals-against bulk ESS: ", round(min(valid_goals_against_diagnostics$ess_bulk), 0))
message("Minimum goals-against tail ESS: ", round(min(valid_goals_against_diagnostics$ess_tail), 0))

# goals-conceded posterior-predictive check

goals_against_posterior_plot <- pp_check(goals_against_poisson_model, type = "bars", ndraws = 100)
print(goals_against_posterior_plot)

ggsave(filename = here("outputs", "figures", "goals_against_poisson_posterior_predictive.png"), plot = goals_against_posterior_plot, width = 9, height = 6, dpi = 300)

goals_against_replications <- posterior_predict(goals_against_poisson_model, ndraws = 500)
against_replicated_mean <- rowMeans(goals_against_replications)
against_replicated_variance <- apply(goals_against_replications, 1, var)
against_replicated_zero_proportion <- rowMeans(goals_against_replications == 0)
against_replicated_maximum <- apply(goals_against_replications, 1, max)
goals_against_predictive_summary <- tibble(metric = c("Mean goals conceded", "Variance of goals conceded", "Clean-sheet proportion", "Maximum goals conceded"),
                                           observed = c(mean(data$GA), var(data$GA), mean(data$GA == 0), max(data$GA)),
                                           predicted_median = c(median(against_replicated_mean), median(against_replicated_variance), median(against_replicated_zero_proportion), median(against_replicated_maximum)),
                                           predicted_lower_95 = c(quantile(against_replicated_mean, 0.025), quantile(against_replicated_variance, 0.025), quantile(against_replicated_zero_proportion, 0.025), quantile(against_replicated_maximum, 0.025)),
                                           predicted_upper_95 = c(quantile(against_replicated_mean, 0.975), quantile(against_replicated_variance, 0.975), quantile(against_replicated_zero_proportion, 0.975), quantile(against_replicated_maximum, 0.975)))
print(goals_against_predictive_summary)
write_csv(goals_against_predictive_summary, here("outputs", "tables", "goals_against_poisson_predictive_summary.csv"))

# goals-conceded rate ratios

goals_against_fixed_effects <- as.data.frame(fixef(goals_against_poisson_model)) %>% tibble::rownames_to_column("term") %>% transmute(term,
    log_rate_estimate = Estimate,
    log_rate_lower_95 = Q2.5,
    log_rate_upper_95 = Q97.5,
    rate_ratio = exp(Estimate),
    rate_ratio_lower_95 = exp(Q2.5),
    rate_ratio_upper_95 = exp(Q97.5),
    percent_change = 100 * (exp(Estimate) - 1)
  )
print(goals_against_fixed_effects)

write_csv(goals_against_fixed_effects, here("outputs", "tables", "goals_against_poisson_rate_ratios.csv"))

manager_period_comparison <- bind_rows(goals_fixed_effects %>% filter(grepl("^ManagerPeriod", term)) %>% mutate(outcome = "Goals scored"),
                                       goals_against_fixed_effects %>% filter(grepl("^ManagerPeriod", term)) %>% mutate(outcome = "Goals conceded")) %>% select(outcome, term, rate_ratio, rate_ratio_lower_95, rate_ratio_upper_95, percent_change)
print(manager_period_comparison)
write_csv(manager_period_comparison, here("outputs", "tables", "manager_period_rate_ratio_comparison.csv"))
