my.mean <- function(x) {
  return(sum(x) / length(x))
}

data<-c(1,10,100,1000,10000)
meanRes<-my.mean(data)
print(meanRes)

my.median <- function(x) {
  sorted.x <- sort(x)

  if (length(x) %% 2 == 0) {
    indices <- c(length(x) / 2, lentgth(x) / 2 + 1)
    return(my.mean(sorted.x[indices]))
  } else {
    index <- ceiling(length(x) / 2)
    return(sorted.x[index])
  }
}

medRes = my.median(data)
print(medRes)

print(min(data))
print(max(data))
print(range(data))
print(quantile(data))
print(quantile(data, probs = seq(0, 1, by = .2)))
print(quantile(data, probs = .33))

my.var <- function(x) {
  return ((sum((x - my.mean(x)) ^ 2)) / (length(x) - 1))
}

print(my.var(data))
print(var(data))

my.sd <- function(x) {
  return(sqrt(my.var(x)))
}

print(my.sd(data))
print(sd(data))

# crtkaranje
library('ggplot2')
heights.file <- file.path('01_heights_weights_genders.csv')
heights.weights <- read.csv(heights.file, header = TRUE, sep = ',')
#ggplot(heights.weights, aes(x = Height)) + geom_histogram(binwidth = 1)
#ggplot(heights.weights, aes(x = Height)) + geom_histogram(binwidth = 0.001)
#ggplot(heights.weights, aes(x = Height)) + geom_density()
#ggplot(heights.weights, aes(x = Height, fill = Gender)) + geom_density()
#ggplot(heights.weights, aes(x = Weight, fill = Gender)) + geom_density()
ggplot(heights.weights, aes(x = Weight, fill = Gender)) + geom_density() + facet_grid(Gender ~ .)