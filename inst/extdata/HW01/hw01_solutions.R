# hw01_solutions.R

# my_sum(1:5)
# sum(1:5)

my_sum <- function(x) {
  out <- 0
  for(i in x) out <- out + i
  out
}


tests_to_run <-
  list(
    "my_sum" = list(1, 5, 1:5, 100:105)
  )
