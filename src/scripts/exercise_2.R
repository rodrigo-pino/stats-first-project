# ------------------------------------
# Loading dataset
# ------------------------------------
data <- read.csv(file = "../raw_data/adult.data.csv")
df <- data.frame(data)
head(df)


# ------------------------------------
# Function to encode non-numerical variables
# ------------------------------------
categorical_to_numerical  <- function(data, map) {
 n  <- c()
 for (val in data) {
  if (val %in% names(map)) {
    n <-  append(n, map[val])
  }
 }
 return(n)
}

# ------------------------------------
# Function to replace missing values by certain value
# ------------------------------------
replace_missing_data  <- function(data, map, value) {
 n <- c()
 for (val in data) {
  if (val %in% names(map)) {
   n  <- append(n, map[val])
  } else {
   n  <- append(n, value)
  }
 }
 return(n)
}

# ------------------------------------
# Function to get info collected from a boxplot
# ------------------------------------
box_data  <-  function(data) {
 f <- fivenum(data)
 fivenumber <- cbind(f[1], f[2], f[3], f[4], f[5])
 colnames(fivenumber) <- c("Min", "Lower-hinge", "Median", "Upper-hinge", "Max")
 return(fivenumber[1,])
}

# ------------------------------------
# Function to calculate mode of a given variable
# ------------------------------------
mode <- function(x) {
  uniqv <- unique(x)
  t  <- tabulate(match(x, uniqv))
  return(uniqv[which.max(t)])
}


# ------------------------------------
# This vectors act like mapper from categorical data
# to their numerical encoded value
# ------------------------------------
workclass  <- seq(1, 8)
names(workclass)  <- c(" Private" , " Self-emp-not-inc" , " Self-emp-inc" , " Federal-gov" , " Local-gov" , " State-gov" , " Without-pay" , " Never-worked" )  # nolint

education  <- seq(1, 16)
names(education)  <- c(" Bachelors" , " Some-college" , " 11th" , " HS-grad" , " Prof-school" , " Assoc-acdm" , " Assoc-voc" , " 9th" , " 7th-8th" , " 12th" , " Masters" , " 1st-4th" , " 10th" , " Doctorate" , " 5th-6th" , " Preschool" ) # nolint

marital_status  <- seq(1, 7)
names(marital_status) <- c(" Married-civ-spouse" , " Divorced" , " Never-married" , " Separated" , " Widowed" , " Married-spouse-absent" , " Married-AF-spouse" ) # nolint

occupation  <- seq(1, 14)
names(occupation) <- c(" Tech-support" , " Craft-repair" , " Other-service" , " Sales" , " Exec-managerial" , " Prof-specialty" , " Handlers-cleaners" , " Machine-op-inspct" , " Adm-clerical" , " Farming-fishing" , " Transport-moving" , " Priv-house-serv" , " Protective-serv" , " Armed-Forces" ) # nolint

relationship  <- seq(1, 6)
names(relationship)  <- c(" Wife" , " Own-child" , " Husband" , " Not-in-family" , " Other-relative" , " Unmarried" ) # nolint

race  <- seq(1, 5)
names(race)  <- c(" White" , " Asian-Pac-Islander" , " Amer-Indian-Eskimo" , " Other" , " Black" ) # nolint

sex  <-  seq(1, 2)
names(sex) <- c(" Female", " Male")

native_country  <- seq(1, 41)
names(native_country)  <- c(" United-States" , " Cambodia" , " England" , " Puerto-Rico" , " Canada" , " Germany" , " Outlying-US(Guam-USVI-etc)" , " India" , " Japan" , " Greece" , " South" , " China" , " Cuba" , " Iran" , " Honduras" , " Philippines" , " Italy" , " Poland" , " Jamaica" , " Vietnam" , " Mexico" , " Portugal" , " Ireland" , " France" , " Dominican-Republic" , " Laos" , " Ecuador" , " Taiwan" , " Haiti" , " Columbia" , " Hungary" , " Guatemala" , " Nicaragua" , " Scotland" , " Thailand" , " Yugoslavia" , " El-Salvador" , " Trinadad&Tobago" , " Peru" , " Hong" , " Holand-Netherlands" )  # nolint

income  <- seq(1, 2)
names(income)  <- c(" >50K", " <=50K")



# ------------------------------------
# Descriptive stats from selected variables
# ------------------------------------


# ------------------------------------
# Occupation
# ------------------------------------
occupation_mean <- round(mean(categorical_to_numerical( df$occupation, occupation)))
numerical_occupation <- replace_missing_data(df$occupation, occupation, occupation_mean )

occupation_median <- median(numerical_occupation)
occupation_var  <-  var(numerical_occupation)
occupation_sd  <- sd(numerical_occupation)
occupation_mode  <- mode(numerical_occupation)
occupation_vc  <- occupation_sd / occupation_median

#jpeg(file="hist_occupation.jpeg")
boxplot(numerical_occupation,ylab = "Occupation",las = 2)
hist(numerical_occupation, col="blue")
#dev.off()
print("Occupation stats:")
print(names(occupation))
print(box_data(numerical_occupation))
print(paste("Mean: ", occupation_mean))
print(paste("Median: ", occupation_median))
print(paste("Var: ", occupation_var))
print(paste("SD: ", occupation_sd))
print(paste("Mode: ", occupation_mode))
print(paste("CV:", occupation_vc))

# ------------------------------------
# Education
# ------------------------------------
education_mean <- round(mean(categorical_to_numerical( df$education, education)))
numerical_education <- replace_missing_data(df$education, education, education_mean )

education_median <- median(numerical_education)
education_var  <-  var(numerical_education)
education_sd  <- sd(numerical_education)
education_mode  <- mode(numerical_education)
education_vc  <- education_sd / education_median

#jpeg(file="hist_education.jpeg")
boxplot(numerical_education, ylab = "Education", las = 2)
hist(numerical_education, col = "green")
#dev.off()
print("Education stats:")
print(names(education))
print(box_data(numerical_education))
print(paste("Mean: ", education_mean))
print(paste("Median: ", education_median))
print(paste("Var: ", education_var))
print(paste("SD: ", education_sd))
print(paste("Mode: ", education_mode))
print(paste("CV:", education_vc))

 
# ------------------------------------
# Sex
# ------------------------------------
sex_mean <- round(mean(categorical_to_numerical( df$sex, sex)))
numerical_sex <- replace_missing_data(df$sex, sex, sex_mean )

sex_median <- median(numerical_sex)
sex_var  <-  var(numerical_sex)
sex_sd  <- sd(numerical_sex)
sex_mode  <- mode(numerical_sex)
sex_vc  <- sex_sd / sex_median

#jpeg(file="hist_sex.jpeg")
boxplot(numerical_sex,ylab = "Sex",las = 2)
hist(numerical_sex, col="yellow")
#dev.off()
print("Sex stats:")
print(names(sex))
print(box_data(numerical_sex))
print(paste("Mean: ", sex_mean))
print(paste("Median: ", sex_median))
print(paste("Var: ", sex_var))
print(paste("SD: ", sex_sd))
print(paste("Mode: ", sex_mode))
print(paste("CV:", sex_vc))
