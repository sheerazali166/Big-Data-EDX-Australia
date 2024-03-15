

ggplot2::mpg

sample_mean <- mean(ggplot2::mpg$cty)
sample_sd <- sd(ggplot2::mpg$cty)

N <- length(ggplot2::mpg$cty)

t <- qt(p = 0.025, df = N - 1, lower.tail = FALSE)

lwr <- sample_mean - t * sample_sd / sqrt(N)

# Calculate the upper point:
upr <- sample_mean + t * sample_sd / sqrt(N)

# Look at interval:
ci <- c(lwr = lwr, upr = upr)
ci






