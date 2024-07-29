## dixon critical values

qdixon <- function(n, alpha = 0.05, rep = 1e5) {
  if (length(n) > 1) {
    tmp <- matrix(0, nrow = length(n), ncol = length(alpha))
    for (i in seq_along(n)) {
      print(n[i])
      tmp[i, ] <- qdixon(n[i], alpha = alpha, rep = rep)
    }
    colnames(tmp) <- alpha
    return(as_tibble(tmp))
  } else {
    if (n < 100) {
      rep <- 1e6
    }
    Q <- numeric(rep)
    for (i in seq(rep)) {
      y <- sort(rnorm(n))
      Q[i] <- max(y[2] - y[1], y[n] - y[n - 1]) / (y[n] - y[1])
    }
    quantile(Q, prob = 1 - alpha, type = 8)
  }
}

# n grid from 3:50, then equally spaced in log(log(n)).
n <- unique(c(3:50, round(exp(exp(seq(1.4, 2.6, by = 0.1))))))
# alpha grid equally spaced from -9 to 9 on logit space
alpha <- exp(-9:9) / (1 + exp(-9:9))

dixon_cv <- bind_cols(
  n = n,
  qdixon(n, alpha = alpha, rep = 1e5)
) |>
  pivot_longer(-n, names_to = "alpha", values_to = "cv") |>
  mutate(alpha = as.numeric(alpha))

usethis::use_data(dixon_cv, overwrite = TRUE, internal = TRUE)

# Fit local quadratic model in loglog(n) and logit(alpha) to find critical values
# for other alpha and n values.
dixon_cv <- weird:::dixon_cv
alpha <- 0.05
n <- 55
# Find critical value using linear model fitted to simulated critical values
# Subset data to nearest alpha and n values
logit <- function(u) {
  log(u / (1 - u))
}
loglog <- function(u) {
  log(log(u))
}
# Find four nearest alpha values
alpha_grid <- sort(unique(dixon_cv$alpha))
nearest_alpha <- (alpha_grid[order(abs(logit(alpha_grid) - logit(alpha)))])[1:4]
# Fit model using only alpha values?
alpha_only_model <- (n %in% 3:50)
# Find nearest n values
if (alpha_only_model) {
  nearest_n <- n
} else {
  # Find four nearest n values
  n_grid <- sort(unique(dixon_cv$n))
  nearest_n <- (n_grid[order(abs(loglog(n_grid) - loglog(n)))])[1:4]
}
cv_subset <- dixon_cv[dixon_cv$alpha %in% nearest_alpha & dixon_cv$n %in% nearest_n, ]
cv_subset$loglogn <- loglog(cv_subset$n)
cv_subset$logitalpha <- logit(cv_subset$alpha)
if (alpha_only_model) {
  # Cubic interpolation to 4 points. 4 df
  dixonfit <- stats::lm(log(cv) ~ poly(logitalpha, 3), data = cv_subset)
} else {
  # Quadratic bivariate model to 16 points. 6 df
  dixonfit <- stats::lm(log(cv) ~ poly(loglogn, 2) + poly(logitalpha, 2) + I(logitalpha * loglogn),
    data = cv_subset
  )
}

summary(dixonfit)
pred <- exp(predict(dixonfit))

# Check fits
cv_subset |>
  ggplot(aes(x = logit(alpha), y = cv, group = n, col = as.factor(n))) +
  geom_point() +
  scale_y_log10() +
  geom_line(aes(y = pred))

cv_subset |>
  ggplot(aes(x = loglog(n), y = cv, group = alpha, col = as.factor(alpha))) +
  geom_point() +
  geom_line(aes(y = pred)) +
  scale_y_log10()
