library(readr)
library(lubridate)
library(magrittr)
library(stringr)
library(purrr)
library(ggplot2)
library(dplyr)

# Find the statistical mode
Mode <- function(x, na.rm = FALSE) {
  # x is the vector, ux will be the bins/candidates that we consider
  # if na.rm = TRUE, remove NA from vector
  ux <- if (na.rm) x[!is.na(x)] else x
  
  # identify all unique/distinct numbers in the vector
  ux <- unique(ux)
  
  # put numbers in ascending order so, if there's a tie, smallest is chosen
  ux <- ux[order(ux)]
  
  # find the bin that contains the most numbers
  ux[which.max(tabulate(match(x, ux)))][1]
}

# see https://stackoverflow.com/questions/16255622/peak-of-the-kernel-density-estimation
# dominant mode of kde
d_mode <- function(x) {
  den <- density(x, kernel = 'gaussian')
  den$x[which.max(den$y)]
}

n_modes <- function(x) {
  den <- density(x, kernel = 'gaussian')
  den.s <- smooth.spline(den$x, den$y, all.knots = TRUE, spar = 0.8)
  s.0 <- predict(den.s, den.s$x, deriv = 0)
  s.1 <- predict(den.s, den.s$x, deriv = 1)
  s.derv <- data.frame(s0 = s.0$y, s1 = s.1$y)
  nmodes <- length(rle(den.sign <- sign(s.derv$s1))$values) / 2
  if ((nmodes > 10) == TRUE) {
    nmodes <- 10
  }
  if (is.na(nmodes) == TRUE) {
    nmodes <- 0
  }
  (nmodes)
}


#################################
## Read the half-marathon data ##
#################################

dat <- read_tsv('data/half_times.txt')

# convert pace into minutes:seconds
dat$pace <- dat$pace %>% 
  map(toString) %>% 
  map(str_sub, 1, 5) %>% 
  ms()

max_half_pace <- max(dat$decimal_pace)  


######################
## Read the 5k data ##
######################

# make sure every finish time has hours
pad_h <- function(x) {
  if(str_count(x, ':') == 1) {paste0('0:', x)} else x
}

pad_H <- Vectorize(pad_h)

dat <- read_tsv('data/5k_times.txt', 
                col_types = cols(
                  place = col_double(),
                  name = col_character(),
                  city = col_character(),
                  state_prov = col_character(),
                  country = col_character(),
                  finish_time = col_character()
                ))

dat$finish_time <- dat$finish_time %>% 
  pad_H() %>% 
  hms() %>% 
  as.duration(.) 

dat$decimal_pace <- (as.numeric(dat$finish_time) / 60) / 3.10685596


##
## Take a sample of the population and analyze 
##

# establish sample size and target variable
sample_size <- 30
# remove outliers from people's 5k pace
max_half_pace <- 21.57

dat_pop <- dat %>% 
  filter(decimal_pace <= max_half_pace) %>% 
  select(decimal_pace) %>% 
  unlist()

# take the sample
seed_val <- 549 
set.seed(seed_val)
dat_sample <- sample(dat_pop, sample_size) 

# test the fit of the sample
# for p-value < .05, we reject the null hypothesis: 
# the sample data does not come from a normal distribution.
test_fit <- ks.test(dat_sample, dat_pop)
test_fit$p.value
test_fit$statistic

# test the fit of a bell curve
qqnorm(dat_pop)
qqline(dat_pop)

ks.test(dat_pop,
        "pnorm",
        mean = mean(dat_pop),
        sd = sd(dat_pop))


# find best seed
#best_fit <- Inf
#best_seed <- 0
#
#for (i in 1:1000) {
#  set.seed(i)
#  dat_sample <- sample(dat_pop, sample_size) 
#  
#  test_fit <- ks.test(dat_sample, dat_pop)
#  if (test_fit$p.value > 0.05) {
#    if (test_fit$statistic < best_fit) {
#      best_fit <- test_fit$statistic
#      best_seed <- i
#    }
#  }
#}


#seek <- data.frame(seed = 1:10000,
#           p = 0, 
#           stat = 0
#           )
#
#for (i in 1:10000) {
#  set.seed(i)
#  dat_sample <- sample(dat_pop, sample_size) 
#  test_fit <- ks.test(dat_sample, dat_pop)
#  seek$p[i] = test_fit$p.value
#  seek$stat[i] = test_fit$statistic
#}
#
#seek <- seek %>% 
#  mutate(race = '5k')
##combo <- seek
#
#c(combo, seek) %>% 
#  as.data.frame() %>% 
#  filter(p > 0.05 & p.1 > 0.05) %>% 
#  mutate(combo_stat = stat + stat.1) %>% 
#  #select(min(combo_stat)) %>% 
#  View()


# compare mean and standard deviation
mean(dat_pop)
mean(dat_sample)

sd(dat_pop)
sd(dat_sample)

# mode
d_mode(dat_pop)
d_mode(dat_sample)

# note that the sample might not be large enough to
# simply identify the most frequently occurring number 
Mode(dat_pop)
Mode(dat_sample)

# determine size of confidence interval (CI)
conf_int <- .95
z_score <- qnorm((1 - conf_int) / 2, lower.tail=FALSE)

# calculate the standard error and look at the CI range
se <- sd(dat_sample) / sqrt(sample_size)
CI_upper <- mean(dat_sample) + z_score * se
CI_lower <- mean(dat_sample) - z_score * se

# orders of magnitude slower
mean(dat_pop)
mean(dat_pop) * 10 ^ 6


# visually compare the results
ggplot() +
  # draw the population's pdf
  geom_density(data = data.frame(pace = dat_pop), aes(x = pace)) + 
  geom_vline(xintercept = mean(dat_pop), linetype = 'longdash') + 
  #geom_vline(xintercept = d_mode(dat_pop), linetype = 'dotdash') +
  
  # draw the sample's pdf
  #geom_density(data = data.frame(pace = dat_sample), aes(x = pace), color = 'red') +
  #geom_vline(xintercept = mean(dat_sample), color = 'red', linetype = 'longdash') + 
  #geom_rect(aes(xmin = CI_lower, xmax = CI_upper, ymin = -Inf, ymax = Inf), fill = 'red', alpha = 0.05) +
  #geom_vline(xintercept = d_mode(dat_sample), color = 'red', linetype = 'dotdash') +
  
  # draw the gaussian, based upon the population
  stat_function(fun = dnorm, n = 1000, args = list(mean = mean(dat_pop), sd = sd(dat_pop)), color = 'blue') + ylab("density") +
  geom_vline(xintercept = mean(dat_pop), linetype = 'dotted', color = 'blue') +
  
  # draw the gaussian, based upon the sample
  #stat_function(fun = dnorm, n = 100, args = list(mean = mean(dat_sample), sd = sd(dat_pop)), color = 'blue') + ylab("density") +
  #geom_vline(xintercept = mean(dat_pop), linetype = 'dotted', color = 'blue') +
  
  # add a title
  #xlim(c(0,30)) +
  #labs(title = 'pdf') 
  labs(title = paste0('Seed: ', seed_val, ', K-S stat: ', round(test_fit$statistic, 2), ', p-val: ', round(test_fit$p.value, 2)))


