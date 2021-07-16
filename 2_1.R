library(dslabs)
data(heights)
prop.table(table(heights$sex))

# To create a CDF graph:
a <- seq(min(heights$height), max(heights$height), length = 50)    # define range of values spanning the dataset
cdf_function <- function(x) {    # computes prob. for a single value
  mean(heights$height <= x)
}
cdf_values <- sapply(a, cdf_function)
plot(a, cdf_values)

# define x as vector of male heights
library(tidyverse)
index <- heights$sex=="Male"
x <- heights$height[index]

# calculate the mean and standard deviation manually
average <- sum(x)/length(x)
SD <- sqrt(sum((x - average)^2)/length(x))

# built-in mean and sd functions
average <- mean(x)
SD <- sd(x)
c(average = average, SD = SD)

# calculate standard units
z <- scale(x)

# calculate proportion of values within 2 SD of mean
mean(abs(z) < 2)

1 - pnorm(70.5,average,SD)

quantile(heights$height,.5)

p <- seq(.01,.99,.01)
quantile(heights$height,p)

summary(heights$height)

percentiles <- quantile(heights$height,p)

percentiles[names(percentiles) == "25%"]
percentiles[names(percentiles) == "75%"]

qnorm(p,mean(heights$height),sd(heights$height))

# Para darnos una idea de si los datos caen en una distribución normal, 
# podemos comparar los cuantiles teorícos y los observados.
p <- seq(.05,.95,.05)
observed_quantiles <- quantile(x,p)
theoretical_quantiles <- qnorm(p,mean(x),sd(x))
plot(theoretical_quantiles,observed_quantiles)
abline(0,1)

# It is simpler to do this with standard units.
observed_quantiles <- quantile(z,p)
theoretical_quantiles <- qnorm(p)
plot(theoretical_quantiles,observed_quantiles)
abline(0,1)

boxplot(heights$height)

boxplot(x)

indexa <- heights$sex=="Female"
xa <- heights$height[indexa]

nombres <- c("Female", "Male")
colores <- c(color = "pink", color = "lightblue", color = "orange")

boxplot(xa,x,
        xlab = "Height",
        names = nombres,
        col = colores)
