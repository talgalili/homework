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
check_hw <- function(hw_sub_dir = "", base_dir = getwd(),
                     submissions_sub_dir = "submissions", sol_file, tests_to_run,
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

  results
}