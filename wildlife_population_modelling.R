
# install.packages(c("popbio", "ggplot2", "dplyr"))

library(popbio)
library(ggplot2)
library(dplyr)

# Load demographic data
data("tortoise", package = "popbio")

# Examine available fertility scenarios
names(tortoise)

# Select published medium-high fertility matrix
A <- tortoise[["med.high"]]

print(A)

# Examine stage classes
rownames(A)
colnames(A)

# Initial population
# Starting population: 10 individuals per stage (assumed, not observed).

initial_population <- rep(10, nrow(A))

names(initial_population) <- rownames(A)

# Project population
years <- 30

results <- matrix(
  NA,
  nrow = years + 1,
  ncol = length(initial_population)
)

results[1, ] <- initial_population

for (t in 1:years) {
  
  results[t + 1, ] <- A %*% results[t, ]
  
}

colnames(results) <- rownames(A)

# Population growth rate
lambda <- popbio::lambda(A)

cat("Population growth rate:",
    round(lambda, 3), "\n")

cat("Annual percentage change:",
    round((lambda - 1) * 100, 2), "%\n")

# Stable stage distribution
stable_stage <- popbio::stable.stage(A)

print(stable_stage)

stage_data <- data.frame(
  Stage = names(stable_stage),
  Proportion = as.numeric(stable_stage)
)

ggplot(stage_data,
       aes(Stage, Proportion)) +
  geom_col(fill = "steelblue") +
  theme_classic() +
  labs(
    title = "Stable stage distribution",
    x = "Life stage",
    y = "Proportion of population"
  ) +
  theme(axis.text.x = element_text(
    angle = 45, hjust = 1
  ))

# Population trajectory
population_data <- data.frame(
  Year = 0:years,
  Total = rowSums(results)
)

ggplot(population_data,
       aes(Year, Total)) +
  geom_line(
    colour = "steelblue",
    linewidth = 1
  ) +
  theme_classic() +
  labs(
    title = "Projected desert tortoise population",
    x = "Year",
    y = "Projected population size"
  )

# Compare fertility scenarios
scenario_results <- data.frame()

for (scenario in names(tortoise)) {
  
  matrix_scenario <- tortoise[[scenario]]
  
  growth <- popbio::lambda(matrix_scenario)
  
  scenario_results <- rbind(
    scenario_results,
    data.frame(
      Scenario = scenario,
      Lambda = growth
    )
  )
}

print(scenario_results)

ggplot(scenario_results,
       aes(Scenario, Lambda)) +
  geom_col(fill = "darkgreen") +
  geom_hline(
    yintercept = 1,
    linetype = "dashed"
  ) +
  theme_classic() +
  labs(
    title = "Population growth under fertility scenarios",
    x = "Fertility scenario",
    y = "Population growth rate (lambda)"
  )

# Demographic elasticity
# Proportional importance of matrix elements
elasticity_matrix <- popbio::elasticity(A)

print(round(elasticity_matrix, 3))

# Plot elasticity matrix
popbio::image2(elasticity_matrix)


# Sensitivity to final-stage stasis
# Vary final-stage stasis.

last_stage <- nrow(A)
original_value <- A[last_stage, last_stage]

# Keep values between 0 and 1
values <- seq(
  max(0, original_value - 0.15),
  min(1, original_value + 0.15),
  length.out = 20
)

sensitivity <- data.frame(
  Stasis = values,
  Lambda = NA_real_
)

for (i in seq_along(values)) {
  
  A_test <- A
  
  A_test[last_stage, last_stage] <- values[i]
  
  sensitivity$Lambda[i] <- popbio::lambda(A_test)
  
}

ggplot(sensitivity,
       aes(Stasis, Lambda)) +
  geom_line(
    colour = "darkgreen",
    linewidth = 1
  ) +
  geom_hline(
    yintercept = 1,
    linetype = "dashed"
  ) +
  theme_classic() +
  labs(
    title = "Sensitivity to final-stage stasis",
    x = "Final-stage stasis probability",
    y = "Population growth rate (lambda)"
  )

# Save results
dir.create("results", showWarnings = FALSE)

write.csv(
  population_data,
  "results/tortoise_population_projection.csv",
  row.names = FALSE
)

write.csv(
  scenario_results,
  "results/tortoise_fertility_scenarios.csv",
  row.names = FALSE
)

# Tortoise demographic matrices
set.seed(123)

# Uses 'tortoise' and 'initial_population'
# from Project 1 above.

# Settings
n_sim <- 500
years <- 30

# Quasi-extinction threshold (assumed)
threshold <- 20

trajectories <- matrix(
  NA,
  nrow = years + 1,
  ncol = n_sim
)

threshold_reached <- logical(n_sim)

# Stochastic projections
for (s in 1:n_sim) {
  
  N <- initial_population
  
  trajectories[1, s] <- sum(N)
  
  for (t in 1:years) {
    
    # Randomly select one published fertility scenario
    # Equal probabilities assumed.
    scenario <- sample(names(tortoise), 1)
    
    A_t <- tortoise[[scenario]]
    
    # Project population
    N <- as.vector(A_t %*% N)
    
    trajectories[t + 1, s] <- sum(N)
    
  }
  
  threshold_reached[s] <- any(
    trajectories[, s] < threshold
  )
}

# Quasi-extinction risk
risk <- mean(threshold_reached)

cat("Quasi-extinction probability:",
    round(risk * 100, 1), "%\n")

# Summarise projections
results_pva <- data.frame(
  Year = 0:years,
  Median = apply(trajectories, 1, median),
  Lower = apply(trajectories, 1, quantile, 0.025),
  Upper = apply(trajectories, 1, quantile, 0.975)
)

# Plot
ggplot(results_pva, aes(Year, Median)) +
  geom_ribbon(
    aes(ymin = Lower, ymax = Upper),
    fill = "steelblue",
    alpha = 0.2
  ) +
  geom_line(
    colour = "steelblue",
    linewidth = 1
  ) +
  geom_hline(
    yintercept = threshold,
    colour = "red",
    linetype = "dashed"
  ) +
  theme_classic() +
  labs(
    title = "Stochastic desert tortoise population projection",
    x = "Year",
    y = "Projected population size"
  )

# Save results
write.csv(
  results_pva,
  "results/tortoise_stochastic_projection.csv",
  row.names = FALSE
)


# PROJECT 2: POPULATION DYNAMICS
# Species: Yellowstone grizzly bear
# Historical population data

# Load real population data
data("grizzly", package = "popbio")

# Examine dataset
head(grizzly)
str(grizzly)

# Extract historical population estimates
population <- data.frame(
  Year = 1959:1997,
  Count = grizzly$N
)

# Remove missing or invalid values
population <- population %>%
  filter(!is.na(Count), Count > 0) %>%
  arrange(Year)

# Visualise population history
ggplot(population, aes(Year, Count)) +
  geom_line(
    colour = "steelblue",
    linewidth = 1
  ) +
  geom_point(size = 2) +
  theme_classic() +
  labs(
    title = "Historical Yellowstone grizzly bear population",
    x = "Year",
    y = "Estimated population size"
  )

# Calculate annual growth
population <- population %>%
  mutate(
    NextCount = lead(Count),
    NextYear = lead(Year),
    GrowthRate = (NextCount - Count) / Count,
    LogGrowth = log(NextCount / Count)
  ) %>%
  filter(NextYear - Year == 1)

summary(population)

# Density-dependent model
# Test whether growth changes with population size
logistic_model <- lm(
  GrowthRate ~ Count,
  data = population
)

summary(logistic_model)

# Density-independent model
exponential_model <- lm(
  GrowthRate ~ 1,
  data = population
)

summary(exponential_model)

# Compare models
AIC(exponential_model, logistic_model)

# Estimate growth parameters
r_est <- unname(coef(logistic_model)[1])
slope <- unname(coef(logistic_model)[2])

cat("Estimated intrinsic growth rate:",
    round(r_est, 3), "\n")

# Only calculate K for positive growth and a negative density effect

if (r_est > 0 && slope < 0) {
  
  K_est <- -r_est / slope
  
  cat("Estimated carrying capacity:",
      round(K_est, 1), "\n")
  
} else {
  
  cat("Carrying capacity cannot be meaningfully",
      "estimated from these coefficients.\n")
  
}

# Visualise density dependence
ggplot(population,
       aes(Count, GrowthRate)) +
  geom_point(size = 2) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    colour = "steelblue"
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  theme_classic() +
  labs(
    title = "Density dependence in grizzly bears",
    x = "Estimated population size",
    y = "Annual per-capita growth rate"
  )

# Examine log population growth
ggplot(population,
       aes(Year, LogGrowth)) +
  geom_line(colour = "darkgreen") +
  geom_point() +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  theme_classic() +
  labs(
    title = "Annual population growth",
    x = "Year",
    y = "Log population growth rate"
  )

# Model diagnostics
par(mfrow = c(2, 2))
plot(logistic_model)
par(mfrow = c(1, 1))

# Residual autocorrelation
acf(residuals(logistic_model))

# Save results
dir.create("results", showWarnings = FALSE)

write.csv(
  population,
  "results/grizzly_population_data.csv",
  row.names = FALSE
)

