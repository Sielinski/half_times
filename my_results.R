my_pace <- dat %>% 
  filter(str_detect(str_to_upper(name), 'SIELINSKI')) %>% 
  select(decimal_pace) %>% 
  as.numeric()

max_half_pace <- 22

dat_stats <- dat %>%
  filter(decimal_pace <= max_half_pace) %>%
  summarise(
    mean = mean(decimal_pace),
    sd = sd(decimal_pace),
    mode = d_mode(decimal_pace),
    median = median(decimal_pace)
  )

dat_stats

# probability of running that pace
dnorm(my_pace, mean = dat_stats$mean, sd = dat_stats$sd)

# percentage of placement
dat %>% 
  filter(str_detect(str_to_upper(name), 'SIELINSKI')) %>% 
  select(place) %>% 
  #as.numeric() / max(dat$place)
  as.numeric() / max(filter(dat, decimal_pace <= max_half_pace)$place)

