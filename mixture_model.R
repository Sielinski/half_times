library(flexmix)
library(nortest)

fun_prop <- function(x, mean, sd, proportion) {
  proportion * dnorm(x = x, mean = mean, sd = sd)
}


# create a dataframe for flexmix
dat_mm <- data.frame(x = dat_pop)

# fit two gaussians
mm_fit <-
  flexmix(
    x ~ 1,
    dat_mm,
    k = 2,
    model = FLXMCnorm1(),
    control = list(verbose = 1)
  )

# add the cluster data into the dataframe
dat_mm$cluster <- as.factor(clusters(mm_fit))

# how many of each cluster?
table(dat_mm$cluster)

# see if clusters are normal
filter(dat_mm, cluster == 1)$x %>% 
  sample(100) %>% 
  ad.test() # normal

filter(dat_mm, cluster == 2)$x %>% 
  sample(100) %>% 
  ad.test() # normal

# from https://stats.stackexchange.com/questions/2492/is-normality-testing-essentially-useless?noredirect=1&lq=1
# Formal normality tests always reject on the huge sample sizes we work 
# with today. It's even easy to prove that when n gets large, even the 
# smallest deviation from perfect normality will lead to a significant 
# result. 
#filter(dat_mm, cluster == 2)$x %>% 
#  sample(100) %>% 
#  ad.test()
#
#ad.test(dat_source)
#cvm.test(dat_source)
#lillie.test(dat_source)
#pearson.test(dat_source)
#sf.test(dat_source)
#
#dat_sim <- rnorm(length(dat_source), mean = mean(dat_source), sd = sd(dat_source)) 
#ad.test(dat_sim)
#cvm.test(dat_sim)
#lillie.test(dat_sim)
#pearson.test(dat_sim)
#sf.test(dat_sim)


# calculate summary statistics for the clusters
cluster_stats <- dat_mm %>% 
  group_by(cluster) %>% 
  summarise(mean = mean(x),
            sd = sd(x),
            mode = d_mode(x),
            median = median(x)) %>% 
  ungroup()

cluster_stats$proportion <- prior(mm_fit)

# compare mean of first cluster with mode of population
cluster_stats$mean[1]
cluster_stats$mode[1]

# plot the clusters and gaussian
# this doesn't work beause it scales the cluster by their individual
# denisties
#ggplot(dat_mm, aes(x = x, fill = cluster)) +
#  #geom_density(alpha = 0.5) +
#  geom_histogram(aes(x = x, y = ..density.., fill = cluster),
#                 alpha = 0.5,
#                 bins = 2 * ceiling(max(dat_mm$x) - min(dat_mm$x))) +
#  stat_function(fun = dnorm, args = list(mean = cluster_stats$mean[1], sd = cluster_stats$sd[1]), color = 'red') +
#  stat_function(fun = dnorm, args = list(mean = cluster_stats$mean[2], sd = cluster_stats$sd[2]), color = 'blue') +
#  # make sure this matches the other plot
#  xlim(c(0, max(dat_pop) * 1.05)) +
#  labs(title = '5k pace times', x = 'pace (minutes/mile)', y = 'density')

# plot the clusters as a histogram
ggplot(dat_mm) +
  geom_histogram(aes(x = x, y = ..density..),
                 alpha = 0.5,
                 bins = 2 * ceiling(max(dat_mm$x) - min(dat_mm$x))) +
  #stat_function(fun = dnorm, args = list(mean = mean(dat_mm$x), sd = sd(dat_mm$x))) +
  stat_function(
    geom = 'line',
    fun = fun_prop,
    args = list(
      mean = cluster_stats$mean[1],
      sd = cluster_stats$sd[1],
      proportion = cluster_stats$proportion[1]
    ),
    color = 'red'
  ) +
  stat_function(
    geom = 'line',
    fun = fun_prop,
    args = list(
      mean = cluster_stats$mean[2],
      sd = cluster_stats$sd[2],
      proportion = cluster_stats$proportion[2]
    ),
    color = 'blue'
  ) +
  
  #stat_function(
  #  geom = 'line',
  #  fun = fun_prop,
  #  args = list(
  #    mean = cluster_stats$mean[3],
  #    sd = cluster_stats$sd[3],
  #    proportion = cluster_stats$proportion[3]
  #  ),
  #  color = 'black'
  #) +

  # summary statistics
  #geom_vline(xintercept = cluster_stats$mean[1], linetype = 'longdash') + 
  ##geom_text(aes(x = cluster_stats$mean[1] * 0.97, label = "mean", y = .01), colour = 'black', angle = 90) +
  #geom_vline(xintercept = cluster_stats$mode[1], linetype = 'dotdash') +
  ##geom_text(aes(x = cluster_stats$mode[1] * 0.965, label = "mode", y = .01), colour = 'black', angle = 90) +
  #geom_vline(xintercept = cluster_stats$median[1], linetype = 'dotted') +
  ##geom_text(aes(x = cluster_stats$median[1] * 0.965, label = "median", y = .01), colour = 'black', angle = 90) +
  #
  #geom_vline(xintercept = cluster_stats$mean[2], linetype = 'longdash') + 
  ##geom_text(aes(x = cluster_stats$mean[2] * 0.97, label = "mean", y = .01), colour = 'black', angle = 90) +
  #geom_vline(xintercept = cluster_stats$mode[2], linetype = 'dotdash') +
  ##geom_text(aes(x = cluster_stats$mode[2] * 0.965, label = "mode", y = .01), colour = 'black', angle = 90) +
  #geom_vline(xintercept = cluster_stats$median[2], linetype = 'dotted') +
  ##geom_text(aes(x = cluster_stats$median[2] * 0.965, label = "median", y = .01), colour = 'black', angle = 90) +
  
  # my pace
  #geom_vline(xintercept = my_pace, linetype = 'dotted', color = 'purple') +
  
  # make sure this matches the other plot
  scale_x_continuous(limits = c(0, max(dat_pop) * 1.05), labels = function(x) paste0(x, ':00')) +
  labs(title = '5k pace times', x = 'pace (minutes/mile)')

 cluster_stats
cluster_stats$mean
round(cluster_stats$sd ^ 2, 1)


