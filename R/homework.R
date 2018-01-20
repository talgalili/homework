
# # to not have functions crash in case of errors.
# options(error = function(e) NULL)
# ?stop("afaffa")
# options(error = expression(NULL))
# http://stackoverflow.com/questions/19111956/suppress-error-message-in-r


# ------ Get the teacher's solutions
# -----------------------------


#' @title Loads sources function into an envir
#' @description
#' Sources an R file to get its functions and content into the environment.
#' @param file the location of the .R file to source.
#' @param env_name A name for the envir in which to store the data.
#' @param envir_home the environment into which to assign the object (env_name). The default is .GlobalEnv.
#' @return A named environment with the content of the .R file
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
source_to_env <- function(file, env_name, envir_home = .GlobalEnv) {
  assign(env_name, new.env(), envir = envir_home) # create a new mystical env
  source(file, local = get(env_name, envir = envir_home)) # brings all the functions to the local env created by the function
}


# ".teacher_env"

#
# create_solutions <- function(file) {
#
#   # source(file, local = TRUE) # brings all the functions to the local env created by the function
#   # fun_vec <- as.vector(lsf.str())
#   #
#   #
#   # assign(".teacher_env", new.env(), envir = .GlobalEnv) # create a new mystical env
#   #
#   # for(i in fun_vec) {
#   #   # assign(paste0(i,"s"), get(i),  envir = .GlobalEnv)
#   #   assign(i, get(i),  envir = .teacher_env)
#   # }
#
#
#   assign(".teacher_env", new.env(), envir = .GlobalEnv) # create a new mystical env
#   source(file, local = .teacher_env) # brings all the functions to the local env created by the function
#
#   NULL
#
# }
# # run this everytime we want the teachers solutions
# # create_solutions("sol\\HW_01_sol.R")
#




# ------ Function to check student vs teacher answers
# -----------------------------




#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param hw_submitters a vector of .R files to check
#' @param sol_file the location of the .R file with the correct solution.
#' This file should have the functions that solves the homework's questions.
#' @param tests_to_run a list with elements as the number of questions in the homework assignment.
#' Each element in the list is named by the name of the function.
#' So if the homework said to create a function called fo then the list will contain an element named "fo".
#' The "fo" element will itself be a list with the inputs to check on the functions.
#' If the input is NA then the function will be run as `fo()`.`
#' If the function fo includes several parameters (say fo(a = "something", b = "another smthng")) then
#' each element inside "fo" will be a list of the form list(a = "input", b = "b input"). The function do.call will be used to run this
#' input in fo.
#' @param student_id_fun a character string indicating the name of the function a student was instructed to create that returns is id (for example my_id() {"id number"})
#'                 If NULL, then the file name is used.
#' @param timeout The number of seconds to wait for the function to end before deciding
#' the student got into an infinite loop and to exist the function and declare the student failed to answer
#' the question. Default: 0.5
#' @param use_do.call if to force the use of do.call on the list_of_inputs. By default is not set, in which case the function will try to guess if to use it or not (based on the solution by the teacher and the arguments in the list_of_inputs)
#' @param check_sol_fun the function to use to compare the solutions. if you wish to set a specific function for a test, the
#'           "check_sol_fun" attribute should be added to that test in the list.
#'           attr(current_test, "check_sol_fun")
#'           PARAM_DESCRIPTION, Default:
#'                       function(student_sol, teacher_sol) {
#'                         isTRUE(all.equal(
#'                           student_sol, teacher_sol, tolerance = 0.01,
#'                           check.attributes = FALSE
#'                         ))
#'                       }
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname test_students
#' @importFrom R.utils withTimeout
#' @export
test_students <- function(hw_submitters, sol_file, tests_to_run,
                          student_id_fun = NULL, # my_id
                          timeout = .5,
                          use_do.call,
                          check_sol_fun = function(student_sol, teacher_sol) {
                            isTRUE(all.equal(student_sol, teacher_sol, tolerance = 1e-2, check.attributes = FALSE))
                          }) {

  # clear old error files
  errors_files <- list.files("grades")
  errors_files <- errors_files[grepl("error_file", errors_files)]
  errors_files <- file.path("grades", errors_files)
  unlink(errors_files) # so that it would start from scratch everytime



  grades <- data.frame(ID = NA)

  # get teacher's solutions
  # create_solutions(sol_file)
  # lsf.str(envir = .teacher_env)

  source_to_env(file = sol_file, env_name = ".teacher_env")
  # lsf.str(envir = .teacher_env)
  #

  functions_to_check <- names(tests_to_run)

  for (i in seq_along(hw_submitters)) {


    # no longer needed since we now use env
    # clear ALL functions except "create_solutions"
    # fun to remove:
    # rm(list=as.vector(lsf.str())[-1]) # -1 so to not remove "create_solutions"


    # if (exists(".student_env")) rm(.student_env)
    assign(".student_env", NULL, envir = .GlobalEnv)


    # get student's functions
    # try(source(hw_submitters[i]), silent = TRUE)
    try(
      source_to_env(file = hw_submitters[i], env_name = ".student_env"),
      silent = TRUE
    )

    # if (!exists(".student_env")) next # skip current file as the source failed...
    if (is.null(.student_env)) next


    if (is.null(student_id_fun)) {
      # use file name
      # https://stackoverflow.com/questions/2548815/find-file-name-from-full-file-path
      grades[i, 1] <- basename(hw_submitters[i]) # gets the filename
      current_id <- grades[i, 1]
    } else {
      # # lsf.str(envir = .student_env)
      # moved to using the file name.
      # ls()
      # lsf.str()
      try(my_id <- get(student_id_fun, envir = .student_env), silent = TRUE)
      if (!exists("my_id")) next # skip current file since we don't have the my_id function!
      try(grades[i, 1] <- my_id(), silent = TRUE)
      current_id <- grades[i, 1]
    }




    # go through every question
    for (i_fun in functions_to_check) {
      fun_to_get <- i_fun

      fun_teacher <- if (exists(fun_to_get, envir = .teacher_env)) {
        get(fun_to_get, envir = .teacher_env)
      } else {
        function(...) {
          print("This function was missing!")
        }
      }

      fun_student <- if (exists(fun_to_get, envir = .student_env)) {
        get(fun_to_get, envir = .student_env)
      } else {
        function(...) {
          print("This function was missing!")
        }
      }

      # fun_student <- get(paste0("q", i_question), envir = .student_env)

      # get("q3", envir = .student_env)

      teachers_tests <- tests_to_run[[i_fun]]
      teachers_tests_seq <- if (all(is.na(teachers_tests))) 1 else seq_along(teachers_tests)


      current_test_attr <- names(attributes(teachers_tests))

      # this is when the teachers write fo <- function(x) {...}
      # And the student writes fo <- function(y) {...}
      # we can make sure to fix the student's mistake
      if ("update_student_fun" %in% current_test_attr) {
        fun_student <- attr(teachers_tests, "update_student_fun")(fun_student)
      }

      for (i_tests in teachers_tests_seq) {
        # teachers_tests = tests_to_run
        # i_tests = 2
        current_test <- teachers_tests[[i_tests]]

        student_sol <- "The function didn't complete"
        teacher_sol <- "The student's function didn't complete"

        # I'm using R.utils::withTimeout so to deal with infinite loops...
        # https://stackoverflow.com/questions/7891073/time-out-an-r-command-via-something-like-try

        # library(R.utils)


        if (missing(use_do.call)) {
          if (all(names(current_test) %in% names(formals(fun_teacher)))) {
            use_do.call <- TRUE
          } else {
            use_do.call <- FALSE
          }
        }


        R.utils::withTimeout({
          try({
            # if(is.list(current_test) && length(current_test) > 1) {

            if (is.na(current_test)) {
              student_sol <- fun_student()
              teacher_sol <- fun_teacher()
            } else {
              if (is.list(current_test) && use_do.call) {
                # then we must be having to use a function with several arguments
                student_sol <- do.call(fun_student, current_test)
                teacher_sol <- do.call(fun_teacher, current_test)
              } else {
                # it is a simple function with only one argument
                student_sol <- fun_student(current_test)
                teacher_sol <- fun_teacher(current_test)
              }
            }
          }, silent = TRUE)
        }, timeout = timeout, onTimeout = "warning")

        # post modifications due to issue that might happen from rounding or others
        if (!is.null(current_test_attr) & is.numeric(student_sol) & is.numeric(teacher_sol)) {
          if ("sort" %in% current_test_attr && isTRUE(attr(current_test, "sort"))) {
            student_sol <- sort(student_sol)
            teacher_sol <- sort(teacher_sol)
          }
          if ("round" %in% current_test_attr && is.numeric(attr(current_test, "round"))) {
            how_much_to_round <- attr(current_test, "round")
            # print(paste(how_much_to_round, "----------------------"))
            student_sol <- round(student_sol, how_much_to_round)
            teacher_sol <- round(teacher_sol, how_much_to_round)
          }
        }

        if ("check_sol_fun" %in% current_test_attr) {
          current_check_sol_fun <- attr(teachers_tests, "check_sol_fun")
        } else {
          current_check_sol_fun <- check_sol_fun
        }


        # is_correct_answer <- identical( student_sol, teacher_sol) # does not fully work...
        # is_correct_answer <- isTRUE(all.equal( student_sol, teacher_sol, tolerance = 1e-2))

        is_correct_answer <- current_check_sol_fun(student_sol, teacher_sol)






        # try(grades[i, paste0("q", i_question, "_test_", i_tests)] <-
        #       identical( student_sol, teacher_sol) ,
        #     silent = TRUE)
        # grades[i, paste0("q", i_question, "_test_", i_tests)] <- is_correct_answer
        grades[i, paste0(i_fun, "_test_", i_tests)] <- is_correct_answer

        if (!is_correct_answer) {
          # then - save the function and test to a file, so that the TA could more easily check it.
          # errors_file <- paste0(sol_file, "_students_errors.R")
          txt_fun_student <- capture.output(fun_student)
          txt_current_test <- capture.output(current_test)
          txt_teacher_sol <- capture.output(teacher_sol)
          txt_student_sol <- capture.output(student_sol)

          errors_file <- paste0("grades\\error_file_", i_fun, ".R")

          write("# ======================", file = errors_file, append = TRUE)
          write(paste0("# Student's file: ", current_id), file = errors_file, append = TRUE)
          write("# ======================", file = errors_file, append = TRUE)
          write(paste0("# A wrong solution to question: ", i_fun, " test: ", i_tests), file = errors_file, append = TRUE)
          write("# The test:", file = errors_file, append = TRUE)
          write(txt_current_test, file = errors_file, append = TRUE)
          write("# --------------------", file = errors_file, append = TRUE)
          write("# The correct solution:", file = errors_file, append = TRUE)
          write(txt_teacher_sol, file = errors_file, append = TRUE)
          write("# --------------------", file = errors_file, append = TRUE)
          write("# The student's solution:", file = errors_file, append = TRUE)
          write(txt_student_sol, file = errors_file, append = TRUE)
          write("# --------------------", file = errors_file, append = TRUE)
          write("# The student's function:", file = errors_file, append = TRUE)
          write(txt_fun_student, file = errors_file, append = TRUE)
          write("# ======================", file = errors_file, append = TRUE)
        }
      }
    }
  }

  # all the errors mean the function failed
  grades[is.na(grades)] <- FALSE


  # hw_grades <- rowSums(grades[,-1] / (ncol(grades)-1)) * 150
  hw_grades <- rowMeans(grades[, -1]) * 150

  # last value is the median of the grades
  # question_difficulty <- c(colMeans(grades[,-1]) , median(hw_grades))

  grades[, "grade"] <- hw_grades
  # grades["question_difficulty",] <- question_difficulty


  # clean the .GlobalEnv
  rm(.student_env, envir = .GlobalEnv)
  rm(.teacher_env, envir = .GlobalEnv)


  grades
}


# https://stackoverflow.com/questions/7963898/extracting-the-last-n-characters-from-a-string-in-r


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param n PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname substrRight
#' @export
substrRight <- function(x, n) {
  substr(x, nchar(x) - n + 1, nchar(x))
}






#' @title Check that .R file can be sourced without errors
#' @description
#' The function gets a vector of .R file names and returns for each of them if it can be sourced or not.
#' This is helpful as an initial step before checking the homework (to make sure it can be loaded).
#' @param files a charachter vector of R file names to be sourced and checked if they can be run with no problem.
#' @param ... not used.
#' @return
#' A data.frame with the name of the file, it's status (TRUE if was sourced properly, and FALSE otherwise),
#' and a note indicating possible issues.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname can_source
#' @export
can_source <- function(files, ...) {
  # find any errors...
  file_status <- data.frame(file = files, status = TRUE, note = "ok", stringsAsFactors = FALSE)
  # outputs <- character(length(hw_submitters))
  for (i in seq_along(hw_submitters)) {
    # print(hw_submitters[i])
    # flush.console()
    source_failed <- TRUE
    source_txt <- character(0)

    try({
      source_txt <- capture.output(source(hw_submitters[i]))
      source_failed <- FALSE
    }, silent = TRUE)

    if (length(source_txt) > 0) {
      # outputs[i] <- tmp
      file_status$note[i] <- "Unnecessary printing of output when running source"
      # next
    }
    # } else {
    # }
    # else?! I have no idea how this coule happen...
    # source(hw_submitters[i])
    if (source_failed) {
      file_status$note[i] <- "Failed to source!"
      file_status$status[i] <- FALSE
    }
  }

  file_status
}




#' @title Get only .R files
#' @description
#' Give a vector of possible file names, returns only the ones that are .R files.
#' @param files - a charachter vector of file names
#' @param case_sensitive PARAM_DESCRIPTION, Default: FALSE
#' @return only files which are R/r files.
#' @examples
#'
#' files <- c("a", "b.R", "c.RR", "d.Rdata", "e.R")
#' only_R_files(files)
#'
#' @seealso
#'  \code{\link[tools]{file_ext}}
#' @rdname file_ext_to_keep
#' @export
#' @importFrom tools file_ext
file_ext_to_keep <- function(files, file_ext = c("R"), case_sensitive = FALSE) {
  files_ext <- tools::file_ext(files)
  if (!case_sensitive) files_ext <- toupper(files_ext)
  files[files_ext %in% file_ext]
}


#' @rdname file_ext_to_keep
#' @export
only_R_files <- function(files, case_sensitive = FALSE) {
  file_ext_to_keep(files = files, file_ext = "R", case_sensitive = case_sensitive)
}




#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param results PARAM_DESCRIPTION
#' @param HW_number PARAM_DESCRIPTION
#' @param tests_to_run PARAM_DESCRIPTION
#' @param grades_folder PARAM_DESCRIPTION, Default: grades
#' @param char_to_trim PARAM_DESCRIPTION, Default: 6
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname create_grade_files
#' @export
create_grade_files <- function(results, HW_number, tests_to_run, grades_folder = "grades\\", char_to_trim = 6) {
  results2 <- results
  success_per_question <- round(colMeans(results2[, -1]), 2) # this includes the mean final grade
  results2 <- rbind(results2, c("Success", success_per_question))
  write.csv(results2, paste0(grades_folder, HW_number, "_grades.csv"), row.names = FALSE)

  results2$ID[-nrow(results2)] <- substrRight(results2$ID[-nrow(results2)], char_to_trim)
  colnames(results2)[1] <- "ID (last 4 digits)"
  write.csv(results2, paste0(grades_folder, HW_number, "_grades_for_students.csv"), row.names = FALSE)


  # which questions should Yarden check (by order)
  tests_per_question <- sapply(tests_to_run, length)
  tests_per_question <- rep(names(tests_to_run), times = tests_per_question)
  success_per_question2 <- head(success_per_question, -1)
  success_per_question <- sort(tapply(success_per_question2, tests_per_question, mean))
  what_to_check <- data.frame(question = names(success_per_question), success_per_question = round(success_per_question, 2))
  write.csv(what_to_check, paste0(grades_folder, HW_number, "_what_to_chack.csv"), row.names = FALSE)
  #
  invisible(TRUE)
}










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
#' @return
#' invisible TRUE. Also modifies the .R file that was in the input.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname trap_copycats
#' @export
trap_copycats <- function(file) {
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
