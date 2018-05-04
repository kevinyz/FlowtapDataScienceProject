ratio_calc <- function(n) { # n = number of couples in the country
  boys <- 0
  girls <- 0
  for (i in 1:n) {
    gender <- sample(0:1,1) # randomly determine gender (0: boy, 1: girl)
    while (gender == 1) {
      girls <- girls + 1
      gender <- sample(0:1,1)
    }
    boys <- boys + 1
  }
  boys/girls
}
ratio_calc(1000000)

# The long-term ratio of boys to girls in the country is 1.
