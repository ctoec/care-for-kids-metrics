source("data.r")
library(broom)

windows <- redets %>%
  count(recert_date) %>%
  mutate(
    relative_start = min(calls$date) - recert_date,
    relative_end = max(calls$date) - recert_date
  )

weights <- data.frame(relative_time = seq(min(windows$relative_start), max(windows$relative_end)), k = 1) %>%
  full_join(mutate(windows, k = 1), by = "k") %>%
  mutate(valid = relative_start <= relative_time & relative_end >= relative_time) %>%
  filter(valid) %>%
  group_by(relative_time) %>%
  summarize(n = sum(n)) %>%
  transmute(relative_time, weight = 1 / n)

joined_calls_redets <- calls %>%
  left_join(redets, by = c("caller" = "phone")) %>%
  mutate(relative_time = as.integer(date - recert_date)) %>%
  left_join(weights, by = "relative_time")

sum(!is.na(joined_calls_redets$family_id)) / nrow(joined_calls_redets)

length(unique(joined_calls_redets$family_id)) / length(unique(redets$family_id))

calls_by_relative_time <- joined_calls_redets %>%
  filter(!is.na(relative_time)) %>%
  group_by(relative_time) %>%
  summarize(density = sum(weight))

ggplot(calls_by_relative_time, aes(x = relative_time, y = density)) +
  geom_point() +
  geom_smooth(method = "loess", formula = y ~ x, span = .75, se = FALSE)

calls_after_recert_date <- calls_by_relative_time %>%
  filter(relative_time >= 0)

decay_fit <- nls(density ~ SSasymp(relative_time, yf, y0, log_alpha), data = calls_after_recert_date)

decay_fit

ggplot(calls_after_recert_date, aes(x = relative_time, y = density)) +
  geom_point() +
  geom_line(data = augment(decay_fit), aes(y = .fitted), color = "blue", size = 1)

yf <- coef(decay_fit)["yf"]
y0 <- coef(decay_fit)["y0"]
alpha = -exp(coef(decay_fit)["log_alpha"])
oct_redet_count <- sum(redets$recert_date == as_date("2019-10-31"))

expected_matching_call_volume <- tibble(
  relative_time = 0:60,
  call_volume = (yf + (y0 - yf) * exp(alpha * relative_time)) * oct_redet_count,
  cumulative_call_volume = cumsum(call_volume)
)

ggplot(expected_matching_call_volume, aes(x = relative_time, y = cumulative_call_volume)) +
  geom_line(color = "blue", size = 1)

redets_by_pilot_participation <- redets %>%
  filter(recert_date == as_date("2019-10-31")) %>%
  left_join(calls, by = c("phone" = "caller")) %>%
  group_by(family_id) %>%
  summarize(call_count = sum(!is.na(date))) %>%
  left_join(mutate(contacted, contacted = TRUE), by = "family_id") %>%
  left_join(mutate(pilot_participants, opted_in = TRUE), by = "family_id") %>%
  replace_na(list(contacted = FALSE, opted_in = FALSE))

redets_by_pilot_participation %>%
  group_by(contacted, opted_in) %>%
  summarize(n = n(), mean(call_count))

redets_by_pilot_participation %>%
  group_by(contacted) %>%
  summarize(n = n(), mean(call_count))

t.test(call_count ~ contacted, data = redets_by_pilot_participation)

  