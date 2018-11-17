
library(homework)
demo_base_dir <- file.path(system.file(package = "homework"), "extdata")
demo_base_dir
check_hw("HW01", demo_base_dir)
# run the following to see any warnings within R:
warnings()


# etc.
check_hw("HW02", demo_base_dir)
