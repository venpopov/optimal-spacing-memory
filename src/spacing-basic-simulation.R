library(ggplot2)
library(purrr)

optimal_y_power <- function(x_bar, delta, gamma) {
  alpha <- delta^(-1 / (gamma + 1))
  term1 <- x_bar - 1
  term2 <- alpha * x_bar^(gamma / (1 + gamma)) - 1
  term1 / term2
}

construct_curves <- function(delta, gamma) {
  data.frame(
    delta = delta,
    gamma = gamma,
    x_bar = test_lag + 1,
    y_bar = optimal_y_power(test_lag + 1, delta, gamma)
  )
}

SECONDS_PER_DAY <- 24 * 60 * 60
MAX_DELAY <- 1000 * SECONDS_PER_DAY
MIN_DELAY <- 1
test_lag <- 10^seq(log10(MIN_DELAY), log10(MAX_DELAY), length = 1000)

conditions <- expand.grid(delta = c(0.25, 0.5, 0.8), gamma = c(0.25, 0.5, 0.8))
curves_data <- pmap(conditions, construct_curves) |> dplyr::bind_rows()

f1 <- curves_data |>
  ggplot(aes(x_bar / SECONDS_PER_DAY, y_bar / SECONDS_PER_DAY, color = as.factor(delta))) +
  geom_line() +
  facet_wrap(~gamma, scales = "free") +
  scale_x_continuous(name = "Test delay (days + 1 second offset)", expand = c(0, 0)) +
  scale_y_continuous(name = "Study gap (days + 1 second offset)", expand = c(0, 0)) +
  ggtitle("Panels: decay rate (gamma)") +
  scale_color_discrete("Learning rate (delta)") +
  theme(legend.position = "bottom")

breaks <- c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000)

f2 <- curves_data |>
  ggplot(aes(x_bar / SECONDS_PER_DAY, (y_bar) / SECONDS_PER_DAY, color = as.factor(delta))) +
  geom_line() +
  scale_x_log10(name = "Test delay (days + 1 second offset)", breaks = breaks, labels = breaks) +
  scale_y_log10(name = "Study gap (days + 1 second offset)", breaks = breaks, labels = breaks) +
  geom_hline(yintercept = 1) +
  geom_vline(xintercept = 1) +
  ggtitle("Panels: decay rate (gamma)") +
  scale_color_discrete("Learning rate (delta)") +
  facet_wrap(~gamma, scales = "free") +
  theme(legend.position = "bottom")


ggsave("figures/f1_spacing_parameters_linear.png", f1, width = 9, height = 4)
ggsave("figures/f2_spacing_parameters_log_log.png", f2, width = 11, height = 5)
