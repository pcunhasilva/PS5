###############################################
## PS 5625 - Applied Statistical Programming
## Problem Set 5
## Author: Patrick Cunha Silva
## Tasks: 1 - Develop the package FitStatisticsPack

rm(list = ls())

# Load libraries
library(devtools)
library(roxygen2)

# Define WD
setwd("~/Google Drive/2. Academicos/6. Doutorado/Class/2017/Spring/PS 5625 - Applied Statistical Programming/Homeworks/Problem Sets/PS5/")

# Generate the Package Directory:
if(!dir.exists("FitStatisticsPack")){
   create("FitStatisticsPack", rstudio = FALSE)
}

# Add the function into R file.
file.copy(from = "PS5_CreateFunction.R" , to = "FitStatisticsPack/R/") 
# Rename the function file.
file.rename(from = "FitStatisticsPack/R/PS5_CreateFunction.R", to = "FitStatisticsPack/R/FitStats.R")

# Package the code with the new DESCRIPTION file
current.code <- as.package("FitStatisticsPack")

# Generate the Documentation 
document(current.code)

# Edit the DESCRIPTION FILE
descriptionfile <- read.dcf("FitStatisticsPack/DESCRIPTION")
descriptionfile[1, 2] <- "Calculate fit statistics" 
descriptionfile[1, 3] <- "0.1"
descriptionfile[1, 4] <- "person(\"Patrick\", \"C. Silva\", email = \"pcunhasilva@wustl.edu\", role = c(\"aut\", \"cre\"))"
descriptionfile[1, 5] <- "Calculates fit statistics for statistical models."
descriptionfile[1, 6] <- c("\nR (>= 3.3.2), \n stats")
descriptionfile[1, 7] <- "GPL (>= 2)"
# Add the file "Imports" to DESCRIPTION FILE.
Imports <- "testthat"
descriptionfile <- cbind(descriptionfile, Imports)

# Save the new version of the  DESCRIPTION FILE
write.dcf(descriptionfile, "FitStatisticsPack/DESCRIPTION")

# Generate tests folder
if(!dir.exists("FitStatisticsPack/tests/")){
   dir.create("FitStatisticsPack/tests/")
}

# Generate testthat folder
if(!dir.exists("FitStatisticsPack/tests/testthat")){
   dir.create("FitStatisticsPack/tests/testthat")
}

# Add testall to test folder
file.copy(from = "PS5_test_all.R" , to = "FitStatisticsPack/tests/") 

# Rename testall file.
file.rename(from = "FitStatisticsPack/tests/PS5_test_all.R", 
            to = "FitStatisticsPack/tests/test-all.R")

# Add tests into testthat folder
file.copy(from = "PS5_test_class.R", to = "FitStatisticsPack/tests/testthat")
file.copy(from = "PS5_test_error_Pclass.R", to = "FitStatisticsPack/tests/testthat")

# Rename files 
file.rename(from = "FitStatisticsPack/tests/testthat/PS5_test_class.R", 
            to = "FitStatisticsPack/tests/testthat/test_class.R")
file.rename(from = "FitStatisticsPack/tests/testthat/PS5_test_error_Pclass.R", 
            to = "FitStatisticsPack/tests/testthat/test_Pclass.R")

# Package (Again) the code with the new DESCRIPTION file
current.code <- as.package("FitStatisticsPack")

# Check if the package content
check(current.code)

# Build the package
build("FitStatisticsPack", path = getwd())

# Install the package
install("FitStatisticsPack", reload = TRUE)


