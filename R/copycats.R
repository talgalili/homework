# copycats




#
# list.files("sol")
# a <- readLines("sol\\HW_01_sol.R")
# # removing trailing spaces
# # https://stackoverflow.com/questions/9532340/how-do-i-remove-trailing-whitespace-using-a-regular-expression
# gsub("[ \t]+$", "", a)


# add trailing spaces of 0 and 7 length repeateadly in order to find when a student will copy paste a solution from one year to the next.



#' @title Help catch copycats
#' @description
#' Takes a vector of .R files which includes solutions to homework
#' and adds 7 trailing spaces to each even line in the file.
#' This way, if the next semester you get homework which includes such a line,
#' it is clear that the student copied these homework from a solution another student
#' gave him from a previous year.
#' @param file an .R file to update.
#' @param show_print logical (TRUE) if to print the rows with the suspected extra spaces.
#' @param ... not used
#'
#' @return
#' invisible TRUE. Also modifies the .R file that was in the input.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname copycats_trap
#' @export
copycats_trap <- function(file, ...) {
  if (!file.exists(file)) return(invisible(FALSE))

  R_txt <- readLines(file)
  # removing trailing spaces
  # https://stackoverflow.com/questions/9532340/how-do-i-remove-trailing-whitespace-using-a-regular-expression
  R_txt <- gsub("[ \t]+$", "", R_txt)

  # add 7 trailing spaces to each second line in the file
  R_txt <- paste0(R_txt, c("", "       "))

  writeLines(R_txt, file)

  invisible(TRUE)
}



#' @rdname copycats_trap
#' @export
copycats_find <- function(file, show_print = TRUE, ...) {
  if (!file.exists(file)) return(invisible(FALSE))

  R_txt <- readLines(file)
  # removing trailing spaces
  # https://stackoverflow.com/questions/9532340/how-do-i-remove-trailing-whitespace-using-a-regular-expression
  space_loc <- grepl("       $", R_txt)

  if (any(space_loc)) {
    if (show_print) print(R_txt[space_loc])
    return(TRUE)
  }
  return(FALSE)
}


#
#
# # I'm making sure this will be run everytime so that
#
# for(i in 1:9) {
#   trap_copycats(paste0("sol\\HW_0",i,"_sol.R"))
# }
# trap_copycats("sol\\HW_10_sol.R")
# trap_copycats("sol\\HW_11_sol.R")
# trap_copycats("sol\\HW_12_sol.R")
# trap_copycats("sol\\HW_13_sol.R")
#
#
#
#
