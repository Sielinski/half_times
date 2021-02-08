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

# see if cluster 1 is normal
# it is not
dat_source <- filter(dat_mm, cluster == 1)$x

# test 
ad.test(dat_source)
cvm.test(dat_source)
lillie.test(dat_source)
pearson.test(dat_source)
sf.test(dat_source)

dat_sim <- rnorm(length(dat_source), mean = mean(dat_source), sd = sd(dat_source)) 
ad.test(dat_sim)
cvm.test(dat_sim)
lillie.test(dat_sim)
pearson.test(dat_sim)
sf.test(dat_sim)


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
d_mode(dat_pop)

# plot the clusters and gaussian
ggplot(dat_mm, aes(x = x, fill = cluster)) +
  geom_density(alpha = 0.5) +
  stat_function(fun = dnorm, args = list(mean = cluster_stats$mean[1], sd = cluster_stats$sd[1]), color = 'red') +
  stat_function(fun = dnorm, args = list(mean = cluster_stats$mean[2], sd = cluster_stats$sd[2]), color = 'blue') +
  ylab("density") 

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
  # make sure this matches the other plot
  xlim(c(0, max(dat_pop) * 1.05)) +
  labs(title = '5k pace times', x = 'pace (minutes/mile)')

cluster_stats
cluster_stats$mean
cluster_stats$sd ^ 2

