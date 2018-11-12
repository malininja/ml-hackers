library(methods)

data.file <- file.path('01_heights_weights_genders.csv')
heights.weights <- read.csv(data.file, header = TRUE, sep = ',')
heights <- with(heights.weights, Height)
summary(heights)

my.mean <- function(x) {
  return(sum(x) / length(x))
}

my.median <- function(x) {
  sorted.x = sort(x)

  if (length(x) %% 2 == 0) {
    # kako ne postoji vrijednost koja je točno u sredini vraćam
    # aritmetičku sredinu dvije središnje vrijednosti
    indices <- c(length(x) / 2, length(x) / 2 + 1) 
    return(mean(sorted.x[indices]))
  } else {
    index <- ceiling(length(x) / 2)
    return(sorted.x[index])
  }
}

print("mean")
my.mean(heights)
mean(heights)
print("median")
my.median(heights)
median(heights)

# par najveće i najmanje vrijednosti
print("min and max")
c(min(heights), max(heights))
print("range heights")
range(heights)

# unaprijed zadane vrijednosti kvantila
quantile(heights)
# postavljanje vrijednosti kvantila
quantile(heights, probs = seq(0, 1, by = .2))

# data spread
c(quantile(heights, probs = .25), quantile(heights, .75))
c(quantile(heights, probs = .025), quantile(heights, .975))

my.var <- function(x) {
  m <- mean(x)
  return (sum((x - m) ^ 2) / (length(x) - 1))
}

print("variance")
my.var(heights)
var(heights)

my.sd <- function (x) {
  return(sqrt(my.var(x)))
}

print("standardna devijacija")
my.sd(heights)
sd(heights)

library('ggplot2')
#ggplot(heights.weights, aes(x = Height)) + geom_histogram(binwidth = .001) # histogram
#ggplot(heights.weights, aes(x = Height)) + geom_density() # density plot
#ggplot(heights.weights, aes(x = Height, fill = Gender)) + geom_density() # density plot divided by gender

#ggplot(heights.weights, aes(x = Height, y = Weight)) + geom_point()
#ggplot(heights.weights[1:2000,], aes(x = Height, y = Weight)) + geom_point() + geom_smooth()

#ggplot(heights.weights, aes(x = Height, y = Weight, color = Gender)) + geom_point() # distribution by gender

heights.weights <- transform(heights.weights, Male = ifelse(Gender == 'Male', 1, 0))
logit.model <- glm(Male ~ Height + Weight, data = heights.weights, family = binomial(link = 'logit'))

#ggplot(heights.weights, aes(x = Weight, y = Heights, color = Gender)) + geom_point() +
#  stat_abline(intercept = - ceof(logit.model)[1] / coef(login.model)[2],
#    slope = - ceof(logit.model)[3] / coef(login.model)[2],
#    geom = 'abline', color = 'black')

# doesn't work cos 'stat_abline' is depricated and removed from new ggplot

# page 73