###############################################
## PS 5625 - Applied Statistical Programming
## Problem Set 5
## Author: Patrick Cunha Silva
## Tasks: 1 - Import dataset
##        2 - Clean and recode data
##        3 - Estimate the models and generate the predict values.


rm(list = ls())
setwd("~/Google Drive/2. Academicos/6. Doutorado/Class/2017/Spring/PS 5625 - Applied Statistical Programming/Homeworks/Problem Sets/PS5/")

# Load the libraries
library(randomForest)
library(foreign)
library(AER)

# Import dataset
anesTS2012 <- read.dta("anes_timeseries_2012_stata12.dta")

# Select the variables of interest
keep_var <- c("ft_dpc", "gender_respondent_x", "ecblame_pres", "econ_unpast", 
              "dem_raceeth_x", "libcpre_self", "dem_edu")

# Keep only the variables of interest
anesTS2012sub <- anesTS2012[,is.element(colnames(anesTS2012), keep_var)]

# Recode variables
# Female is recoded as 1
anesTS2012sub$female <- ifelse(anesTS2012sub$gender_respondent_x=="2. Female", 1, 0)

# Is president blamed by the economy?
# "A great deal", "a lot", and "A moderate amount" are recoded as 1.
anesTS2012sub$pres_blamed <- ifelse(anesTS2012$ecblame_pres=="-9. Refused" | 
                                        anesTS2012$ecblame_pres=="-8. Don't know", NA, 
                                             ifelse(anesTS2012$ecblame_pres=="4. A little" |
                                                       anesTS2012$ecblame_pres=="5. Not at all", 0, 1))

# It Unemployment better or worse than 1 year ago? 
# Worse and Same are recoded as 0
anesTS2012sub$unemployment <- ifelse(anesTS2012$econ_unpast=="-9. Refused" | 
                                        anesTS2012$econ_unpast=="-8. Don't know", NA, 
                                          ifelse(anesTS2012$econ_unpast=="1. Better", 1, 0))

# Race and ethnicity group
# "White and non-Hispanic" is recoded as 1
anesTS2012sub$white <- ifelse(anesTS2012sub$dem_raceeth_x == "-9. Refused" |
                                 anesTS2012$dem_raceeth_x == "-8. Don't know", NA, 
                                     ifelse(anesTS2012sub$dem_raceeth_x == "1. White non-Hispanic", 1, 0)) 


# 7pt scale Liberal/conservative self-placement
anesTS2012sub$lcscale <- ifelse(anesTS2012sub$libcpre_self == "-9. Refused" |
                                anesTS2012$libcpre_self == "-8. Don't know" |
                                anesTS2012$libcpre_self == "-2. Haven't thought much about this", NA, 
                                ifelse(anesTS2012sub$libcpre_self=="1. Extremely liberal", 1,
                                    ifelse(anesTS2012sub$libcpre_self=="2. Liberal", 2, 
                                       ifelse(anesTS2012sub$libcpre_self=="3. Slightly liberal", 3,
                                          ifelse(anesTS2012sub$libcpre_self=="4. Moderate; middle of the road", 4,
                                             ifelse(anesTS2012sub$libcpre_self=="5. Slightly conservative", 5,
                                                ifelse(anesTS2012sub$libcpre_self=="6. Conservative", 6, 7)))))))

# Education
# 95. Other {SPECIFY} is recoded as NA
anesTS2012sub$education <- ifelse(anesTS2012sub$dem_edu == "-9. Refused" |
                                     anesTS2012$dem_edu == "-8. Don't know" |
                                     anesTS2012$dem_edu == "95. Other {SPECIFY}", 
                                          NA, as.numeric(anesTS2012$dem_edu))
j <- 0
for(i in sort(unique(anesTS2012sub$education))){
   anesTS2012sub$education[anesTS2012sub$education==as.numeric(i)] <- j
   j <- j+1
}
# Remove the vector j
rm(j)

# Obama Thermometer
anesTS2012sub$obamather <- ifelse(anesTS2012sub$ft_dpc>=0, anesTS2012sub$ft_dpc, NA)


# Split the sample
training_set <- row.names(anesTS2012sub) %in% sample(1:nrow(anesTS2012sub),
                                                     nrow(anesTS2012sub)/2, replace = FALSE)

# Estimate the models
# OLS
model_OLS <- lm(obamather ~ female + pres_blamed + unemployment + white + education
             + lcscale, data = anesTS2012sub[training_set,])
# Tobit
model_Tobit <- tobit(obamather ~ female + pres_blamed + unemployment + white + education
                     + lcscale, data=anesTS2012sub[training_set,], left = 0, right = 100)
# Random Forest
model_RM <- randomForest(obamather ~ female + pres_blamed + unemployment + white + education
                         + lcscale, data = na.omit(anesTS2012sub[training_set,]), trees = 1000)

# Generate the predict values and add them into a matrix
predOLS <- predict(model_OLS, newdata = anesTS2012sub[!training_set,], type="response")
predTobit <- predict(model_Tobit, newdata = anesTS2012sub[!training_set,], type="response")
predRM <- predict(model_RM, newdata = anesTS2012sub[!training_set,], type="response")
pred_matrix <- matrix(c(predOLS, predTobit, predRM), ncol = 3)
