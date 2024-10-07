### Method 2, directly from root (only has 1 extra column "decile_score")
data_compas <- read.csv("C:/Users/wiets/OneDrive - UvA/THESIS RHITE/usecases/COMPAS/compas-scores-two-years.csv")
col.keep <- which(
  names(data_compas) %in% c("age", "sex", "juv_fel_count","juv_misd_count", 
                     "juv_other_count", "priors_count", "c_charge_degree", 
                     "race", "two_year_recid", "decile_score",
                     "c_jail_in", "c_jail_out")
)
data_compas <- data_compas[, col.keep]

data_compas$sex <- ifelse(data_compas$sex == "Male", 1, 0)
data_compas$race <- ifelse(data_compas$race == "Caucasian", 1, 0)
data_compas$c_charge_degree <- ifelse(data_compas$c_charge_degree == "M", 1, 0)

# take the number of days in jail as a feature
data_compas$c_jail_in <- as.Date(data_compas$c_jail_in, format = "%Y-%m-%d")
data_compas$c_jail_out <- as.Date(data_compas$c_jail_out, format = "%Y-%m-%d")
data_compas$days_in_jail <- as.numeric(difftime(data_compas$c_jail_out, data_compas$c_jail_in, units = "days"))

data_compas <- data_compas[, -which(names(data_compas) %in% c("c_jail_in", "c_jail_out"))]
# count number of missing values in each column
colSums(is.na(data_compas))

# remove rows with missing values
data_compas <- data_compas[complete.cases(data_compas),]
