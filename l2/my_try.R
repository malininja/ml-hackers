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

my.mean(heights)
mean(heights)
my.median(heights)
median(heights)

# par najveće i najmanje vrijednosti
c(min(heights), max(heights))
range(heights)

# unaprijed zadane vrijednosti kvantila
quantile(heights)
# postavljanje vrijednosti kvantila
quantile(heights, probs = seq(0, 1, by = .2))

# 41. stranica
