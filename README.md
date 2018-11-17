
<!-- README.md is generated from README.Rmd. Please edit that file -->
homework
========

The `homework` package let's R teachers automatically check homeworks that they are giving to their students. A recommended workflow is to have a master directory for all homework files, and then a sub-directory for each homework assignment (such as "HW01", "HW02", etc.). The master directory will have a `check_hw_master.R` master file to include the code to check homework everyweek. A subfolder of homework (say, HW01, HW02, etc.), must have a folder with the submissions of the students (called "submissions"), an R file with the correct solutions (it should be called solutions.R or hw01\_solutions.R). The submissions folder can include one zip file with all assignments, or just all the assignments (each student's assignment is an R file). An answer for each homework question should be a function (for example "write a function that calculates the sum of a vector"). The solution file should include an object called `tests_to_run` at the end of it. This object is a nested list, each element of the list is a name of a function that the hw has asked for, and in it is either a list of possible inputs to check against the function, OR, a list of lists, each sublist will check some other input (see later for an example). After running the `check_hw` on the subdirectory "HW01", the function will create a "mistakes" folder with an .R file for each function in the hw, and in the files will be a list of the mistakes that were found for each question/student. There is also a "grades" folder, with a csv including the grades of students (based on the filenames the students submit). The standard of the filenames of hw assignment is assingment\_number\_student\_id.R (e.g.: 01\_123456.R). In the grades folder there is the grades file we need for giving students the grade at the end of the course, plus a file to send the students (this one will include only the first 5 characters of the student's id, so that people won't know who got which grade).

An example of a folder structure before running check\_hw:

    hw
    -check_hw_master.R
    -hw01
    --submissions
    ---students_homework.zip (maybe from moodle)
    --hw01_solutions.R (includes the corrects functions and the inputs to check)
    --hw.txt/hw.pdf/hw.docx/etc. (ignored)
    -hw02
    --...
    -hw03
    --...

Folder structure AFTER running check\_hw:

    hw
    -check_hw_master.R
    -hw01
    --submissions
    ---students_homework.zip (maybe from moodle)
    ---01_123456.R
    ---01_456987.R
    ---01_879456.R
    --hw01_solutions.R (includes the corrects functions and the inputs to check)
    --mistakes
    ---mistakes_in_foo.R
    ---mistakes_in_bar.R
    --grades
    ---grades.csv
    ---grades_for_students.csv
    --hw.txt/hw.pdf/hw.docx/etc. (ignored)
    -hw02
    --...
    -hw03
    --...

Installation
------------

You can install homework from github with:

``` r
# install.packages("devtools")
devtools::install_github("talgalili/homework")
```

Example
-------

The package comes with a simple example. The following code shows where the example is, and how to run a homework check on it.

Some of the homework file have intentional problems in them to deomnstrate how the function is able to deal with them:

``` r
library(homework)
# it is best to just create an RStudio project for the homework checking of a course...
demo_base_dir <- file.path(system.file(package = "homework"), "extdata")
demo_base_dir
#> [1] "C:/R/library/homework/extdata"
check_hw("HW01", demo_base_dir)
#> Warning in check_hw("HW01", demo_base_dir): The following file could not be
#> sourced (it would get a 0 grade): 01_159_student_03_file_cannot_source.R
#> Warning in fun_student(current_test): The function 'my_sum' was not found
#> in file: 01_789_student_05_wrong_function_name.R
#> Warning in (function (...) : The function 'my_pwr' was not found in file:
#> 01_789_student_05_wrong_function_name.R
#>                                        ID my_sum_test_1 my_sum_test_2
#> 1             01_123_student_01_correct.R          TRUE          TRUE
#> 2        01_147_student_02_always_wrong.R         FALSE         FALSE
#> 3  01_159_student_03_file_cannot_source.R         FALSE         FALSE
#> 4              01_456_student_04_only_5.R         FALSE          TRUE
#> 5 01_789_student_05_wrong_function_name.R         FALSE         FALSE
#>   my_sum_test_3 my_sum_test_4 my_pwr_test_1 my_pwr_test_2 my_pwr_test_3
#> 1          TRUE          TRUE          TRUE          TRUE          TRUE
#> 2         FALSE         FALSE         FALSE         FALSE         FALSE
#> 3         FALSE         FALSE         FALSE         FALSE         FALSE
#> 4         FALSE         FALSE          TRUE         FALSE         FALSE
#> 5         FALSE         FALSE         FALSE         FALSE         FALSE
#>   my_pwr_test_4 grade
#> 1          TRUE   100
#> 2         FALSE     0
#> 3         FALSE     0
#> 4         FALSE    25
#> 5         FALSE     0
```
