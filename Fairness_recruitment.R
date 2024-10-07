devtools::install_github("dplecko/fairadapt")
install.packages("fairadapt")

library(faircause)
library(ggplot2)
library(fairadapt)
# check functions in the package
ls("package:fairadapt")
library(Matrix)
library(brew)
library(sos)
all_data <- read.csv("C:/Users/wiets/OneDrive - UvA/THESIS RHITE/CFA-main/CFA-main/data/recruitmentdataset-2022-1.3.csv")

## There are 4 sensitive attributes: Gender, Nationality, Age and sport

set.seed(2022)

data <- all_data[all_data$gender != "other",]

# NOTE: outcome must be in 1s and 0s
data$decision <- ifelse(data$decision == "True", 1, 0)
# transfer all True to 1 and False to 0 for all columns
data$ind.debateclub <- ifelse(data$ind.debateclub == "True", 1, 0)
data$ind.programming_exp <- ifelse(data$ind.programming_exp == "True", 1, 0)
data$ind.international_exp <- ifelse(data$ind.international_exp == "True", 1, 0)
data$ind.entrepeneur_exp <- ifelse(data$ind.entrepeneur_exp == "True", 1, 0)
data$ind.exact_study <- ifelse(data$ind.exact_study == "True", 1, 0)

# count the occurences of phd, master and bachelor in the degree column
table(data$ind.degree)
# transfer all phd to 3, master to 2 and bachelor to 1
data$ind.degree <- ifelse(data$ind.degree == "PhD", 3, ifelse(data$ind.degree == "Master", 2, 1))


data$nationality <- ifelse(data$nationality == "Dutch", "dutch", "non-dutch")
# change column title to is_dutch
names(data)[names(data) == "nationality"] <- "is_dutch"

unique(data$gender)
unique(all_data$sport)

# only id is removed
col.keep <- which(
  names(data) %in% c("age", "gender", "nationality","decision",
                     "ind.degree","ind.university_grade","ind.debateclub",
                     "ind.programming_exp","ind.international_exp","ind.entrepeneur_exp",
                     "ind.languages","ind.exact_study","company")
)

data <- data[, col.keep]

# SFM projection
X <- "gender"
Z <- c("age", "nationality")
W <- c("ind.degree","ind.university_grade","ind.debateclub",
       "ind.programming_exp","ind.international_exp","ind.entrepeneur_exp",
       "ind.languages","ind.exact_study")
Y <- c("decision")

# Choose the company
companyA = data[data$company == "A",]
# remove company column
companyA <- companyA[, -which(names(companyA) %in% c("company"))]

fc_company <- fairness_cookbook(companyA, X = X, W = W, Z = Z, Y = Y,
                                        x0 = "female", x1 = "male")
autoplot(fc_company, decompose = "xspec") + 
  ggtitle("$Y$ disparity decomposition for A")

unique(fc_company$measures$measure)

Disp_treatment = fc_company$measures[fc_company$measures$measure == "ctfde", ]
#mean(Disp_treatment$value)
#sd(Disp_treatment$value)
#Accept h_0 if mean is smaller than the sd
print(abs(mean(Disp_treatment$value)) < abs(sd(Disp_treatment$value)))

Disp_impact_IE = fc_company$measures[fc_company$measures$measure == "ctfie", ]
#mean(Disp_impact_IE$value)
#sd(Disp_impact_IE$value)
print(abs(mean(Disp_impact_IE$value)) < abs(sd(Disp_impact_IE$value)))

Disp_impact_SE = fc_company$measures[fc_company$measures$measure == "ctfse", ]
#mean(Disp_impact_SE$value)
#sd(Disp_impact_SE$value)
print(mean(abs(Disp_impact_SE$value)) < abs(sd(Disp_impact_SE$value)))

#### suppose we reject hypothesis:
### every thing has to be numeric and integer input
# build our own predictor and decompose the true one
fair_pred <- fair_predictions(data, X, Z, W, Y, x0 = 0, x1 = 1,
                              BN = c("IE"))


library(faircause)
#ls("package:faircause")
library(ggplot2)
library(fairadapt)
library(Matrix)
library(ranger)
#library(brew)
#library(sos)
#library(latex2exp)
#library(scales)

colnames(data_compas_W)
set.seed(2022)
mdata <- SFM_proj("compas")
# add the columns that are in data_compas_W to W
mdata$W <- colnames(data_compas_W)[!colnames(data_compas_W) %in% c(mdata$X, mdata$Y, mdata$Z)]
# add the columns that are in data_compas_W to W



