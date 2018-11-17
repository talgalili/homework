# check_hw

if(F) {
  demo_base_dir <- file.path(system.file(package = "homework"), "extdata")
  check_hw("HW01", demo_base_dir, max_grade = 150)

  # in mac, how to quickly browse this folder:
  system(paste("open ", demo_base_dir))

  list.files(demo_base_dir)
  list.files(file.path(demo_base_dir, "HW01"))
  list.files(file.path(demo_base_dir, "HW01", "submissions"))

  r_files <- file.path(demo_base_dir, "HW01", "submissions", list.files(file.path(demo_base_dir, "HW01", "submissions")))
  a <- can_source(r_files)

  if(!all(a$status))

  check_hw("HW01", demo_base_dir)

  debug(check_hw)
  check_hw("HW01", demo_base_dir)
  undebug(check_hw)

  debug(test_students)
  check_hw("HW01", demo_base_dir)
  undebug(test_students)
}




#' @export
create_grade_files <- function(grades, hw_sub_dir, grades_sub_dir = "grades", char_to_keep = 5,
                               get_id_from_file_name = TRUE) {

  grades_sub_dir <- file.path(hw_sub_dir, grades_sub_dir)
  if(dir.exists(grades_sub_dir)) {
    # clear all grades files
    unlink(list.files(grades_sub_dir))
  } else { # let's make sure we have this folder available!
    dir.create(grades_sub_dir)
  }

  if(get_id_from_file_name) {
    # remove .R
    grades$ID <- tools::file_path_sans_ext(grades$ID)
    # remove initial 01_
    grades$ID <- sub("[0-9]+_", "", grades$ID)
  }

  grades2 <- grades
  grades2$ID <- substr(grades2$ID, 1, char_to_keep)

  write.csv(grades, file.path(grades_sub_dir, "grades.csv"), row.names = FALSE)
  write.csv(grades2, file.path(grades_sub_dir, "grades_for_students.csv"), row.names = FALSE)

  NULL
}





#' @export
check_hw <- function(hw_sub_dir = "", base_dir = getwd(),
                     submissions_sub_dir = "submissions", sol_file, tests_to_run,
                     create_grade_files = TRUE,
                     unzip_submissions = TRUE,
                     submission_file_ext_to_keep = c("R", "zip"),
                     ...) {
  # if sol_file empty, find a file that includes the word "solutions"
  hw_sub_dir <- file.path(base_dir, hw_sub_dir)

  # finding the solutions file to work with.
  # this file should include all the functions that we want to test, and the tests_to_run object
  if(missing(sol_file)) {
    hw_sub_dir_files <- list.files(hw_sub_dir)
    sol_file_loc <- grepl("solutions", hw_sub_dir_files)
    if(!any(sol_file_loc)) stop("Cannot find a solutions.R file")
    if(sum(sol_file_loc) > 1) stop("I see more than one file called solutions.R, please specify the file you wish to use in the sol_file argument.")
    sol_file <- file.path(hw_sub_dir, hw_sub_dir_files[sol_file_loc])
  }

  # a good workflow is that every solutions file will include
  # a tests_to_run object at the end of it, including all the things that need to be checked.
  if(missing(tests_to_run)) {
    # create an env object with the objects from the solutions file
    source_to_env(sol_file, "solutions_objects")
    tests_to_run <- solutions_objects$tests_to_run
  }

  # temp$copycats_find
  # system.file("tools", "HW01", package = "homework")
  # file.path(system.file(package = "homework"), "tools")

  # the place where all the hw files of the students (the submissions) should be located
  # submissions_sub_dir <- file.path(base_dir, hw_sub_dir, submissions_sub_dir)
  submissions_sub_dir <- file.path(hw_sub_dir, submissions_sub_dir)
  # hw_submitters = list.files(submissions_sub_dir)
  # list.files("/Library/Frameworks/R.framework/Versions/3.3/Resources/library/homework/extdata//Library/Frameworks/R.framework/Versions/3.3/Resources/library/homework/extdata/HW01/submissions")
  # list.files("/Library/Frameworks/R.framework/Versions/3.3/Resources/library/homework/extdata/")


  hw_submissions_files <- file.path(submissions_sub_dir, list.files(submissions_sub_dir))

  if(unzip_submissions) {
    # if the submissions folder has ANY zip files,
    # it will extract it and remove all files that are not .zip and .R files
    if(any(tools::file_ext(hw_submissions_files) %in% "zip")) {
      zip_files <- hw_submissions_files[tools::file_ext(hw_submissions_files) %in% "zip"]
      unzip(zip_files, exdir = submissions_sub_dir, junkpaths= TRUE) # extract all .R files

      # remove all non R or zip files.
      hw_submissions_files <- file.path(submissions_sub_dir, list.files(submissions_sub_dir))
      to_keep <- tools::file_ext(hw_submissions_files) %in% submission_file_ext_to_keep
      unlink(hw_submissions_files[!to_keep])
    }
  }



  # warning if some files could not be checked because they couldn't be sourced properly...
  check_if_can_source <- can_source(hw_submissions_files)
  if(!all(check_if_can_source$status)) {
    files_with_issues <- check_if_can_source[!check_if_can_source$status, ]
    for(i in 1:nrow(files_with_issues)) {
      warning("The following file could not be sourced (it would get a 0 grade): ", basename(files_with_issues$file[i]))
    }
  }

  results <- test_students(hw_submitters = hw_submissions_files,
                sol_file = sol_file,
                tests_to_run = tests_to_run ,
                mistakes_folder = file.path(hw_sub_dir, "mistakes"),
                ...
                )

  if(create_grade_files) create_grade_files(results, hw_sub_dir)

  results
}
