df <- approxfun(density(dat_pop))
plot(density(dat_pop))
xnew <- mean(dat_pop)
points(xnew, df(xnew), col=2)

df(mean(dat_pop))
df(d_mode(dat_pop))
