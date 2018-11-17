# hw01_solutions.R

# my_sum(1:5)
# sum(1:5)

my_sum <- function(x) {
  out <- 0
  for(i in x) out <- out + i
  out
}

my_pwr <- function(x, p) {
  x^p
}



tests_to_run <-
  list(
  "my_sum" = list(1, 5, 1:5, 100:105),
	"my_pwr" = list(
					list(x=5, p =1),
					list(x=1:4, p =2),
					list(x=1:4, p =3),
					list(x=10:14, p =1/2)
					)
  )


