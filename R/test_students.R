
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


#' @title Change the first argument of a function
#' @description
#' Useful when the teacher uses a function like function(x) and the student
#' does something like function(X) or function(y)
#' If the student had the first argument correct, it would not be changed.
#' @param fun the function to change
#' @param first_arg the name of the first argument of the function to return, Default: x
#' @return
#' The original function, just with a different arg.
#' @examples
#' fo <- function(y, ...) {
#'   x+3
#' }
#' # fo(x=5) # errors...
#' fo_x <- fix_first_arg_in_fun(fo, "x")
#' fo_x(x=5)
#' @rdname fix_first_arg_in_fun
#' @export
fix_first_arg_in_fun <- function(fun, first_arg = "x") {
  if (first_arg != names(formals(fun))[1]) {
    # n_Args <- length(formals(fun))
    formals(fun)[first_arg] <- NA
    # formals(fun)
    formals(fun)[[1]] <- as.name(first_arg)
  }
  fun
}


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
#' each element inside "fo" will be a list of the form list("input", "b input"). (you can also use
#' list(a = "input", b = "b input") but then if the student wrote the function as function(A="not a", B = "not b")
#' then his function would fail. Indicating the input just by the order makes it simpler).
#' The function do.call will be used to run this input in fo.
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
#' @param update_student_fun the function to use on the student's function to fix a problem.
#'           "update_student_fun" attribute can be added to that test in the list.
#'           default is NULL.
#' this is when the teachers write fo <- function(x) {...}
#' And the student writes fo <- function(y) {...}
#' we can make sure to fix the student's mistake using:
#' function(f) fix_first_arg_in_fun(f, "x")
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
                          # student_id_fun = NULL, # my_id
                          timeout = .5,
                          use_do.call,
                          check_sol_fun = function(student_sol, teacher_sol) {
                            isTRUE(all.equal(student_sol, teacher_sol, tolerance = 1e-4, check.attributes = FALSE))
                          },
                          update_student_fun = NULL,
                          max_grade = 100,
                          mistakes_folder = "mistakes") {

  if(dir.exists(mistakes_folder)) {
    # clear all mistakes files
    unlink(list.files(mistakes_folder))
  } else { # let's make sure we have this folder available!
    dir.create(mistakes_folder)
  }



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


    if (exists(".student_env")) rm(.student_env, envir = .GlobalEnv)
    assign(".student_env", NULL, envir = .GlobalEnv)


    # get student's functions
    # try(source(hw_submitters[i]), silent = TRUE)
    try(
      source_to_env(file = hw_submitters[i], env_name = ".student_env"),
      silent = TRUE
    )


    # if (is.null(student_id_fun)) {
    #   # use file name
    #   # https://stackoverflow.com/questions/2548815/find-file-name-from-full-file-path
    #   grades[i, 1] <- basename(hw_submitters[i]) # gets the filename
    #   current_id <- grades[i, 1]
    # } else {
    #   # # lsf.str(envir = .student_env)
    #   # moved to using the file name.
    #   # ls()
    #   # lsf.str()
    #   try(my_id <- get(student_id_fun, envir = .student_env), silent = TRUE)
    #   if (!exists("my_id")) next # skip current file since we don't have the my_id function!
    #   try(grades[i, 1] <- my_id(), silent = TRUE)
    #   current_id <- grades[i, 1]
    # }
    grades[i, 1] <- basename(hw_submitters[i]) # gets the filename
    current_id <- grades[i, 1]

    # if (!exists(".student_env")) next # skip current file as the source failed...
    if (length(.student_env) == 0) next



    # go through every question
    for (i_fun in functions_to_check) {
      fun_to_get <- i_fun

      fun_teacher <- if (exists(fun_to_get, envir = .teacher_env, inherits = FALSE)) {
        get(fun_to_get, envir = .teacher_env, inherits = FALSE)
      } else {
        function(...) {
          print("This function was missing!")
        }
      }

      fun_student <- if (exists(fun_to_get, envir = .student_env, inherits = FALSE)) {
        get(fun_to_get, envir = .student_env, inherits = FALSE)
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
      if (!is.null(update_student_fun)) {
        # a general fix to all questions
        fun_student <- update_student_fun(fun_student)
      }
      if ("update_student_fun" %in% current_test_attr) {
        # a specific fix to only one q
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

            if (is.na(current_test[1])) {
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
          # mistakes_file <- paste0(sol_file, "_students_errors.R")
          txt_fun_student <- capture.output(dput(fun_student))
          txt_current_test <- capture.output(current_test)
          txt_teacher_sol <- capture.output(teacher_sol)
          txt_student_sol <- capture.output(student_sol)

          mistakes_file <- file.path(mistakes_folder, paste0("mistakes_in_", i_fun, ".R"))

          write("# ======================", file = mistakes_file, append = TRUE)
          write(paste0("# Student's file: ", current_id), file = mistakes_file, append = TRUE)
          write("# ======================", file = mistakes_file, append = TRUE)
          write(paste0("# A wrong solution to question: ", i_fun, " test: ", i_tests), file = mistakes_file, append = TRUE)
          write("# The test:", file = mistakes_file, append = TRUE)
          write(txt_current_test, file = mistakes_file, append = TRUE)
          write("# --------------------", file = mistakes_file, append = TRUE)
          write("# The correct solution:", file = mistakes_file, append = TRUE)
          write(txt_teacher_sol, file = mistakes_file, append = TRUE)
          write("# --------------------", file = mistakes_file, append = TRUE)
          write("# The student's solution:", file = mistakes_file, append = TRUE)
          write(txt_student_sol, file = mistakes_file, append = TRUE)
          write("# --------------------", file = mistakes_file, append = TRUE)
          write("# The student's function:", file = mistakes_file, append = TRUE)
          write(txt_fun_student, file = mistakes_file, append = TRUE)
          write("# ======================", file = mistakes_file, append = TRUE)
        }
      }
    }
  }

  # all the errors mean the function failed
  grades[is.na(grades)] <- FALSE


  # hw_grades <- rowSums(grades[,-1] / (ncol(grades)-1)) * 150
  hw_grades <- rowMeans(grades[, -1, drop = FALSE]) * max_grade

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
  hw_submitters <- files
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








