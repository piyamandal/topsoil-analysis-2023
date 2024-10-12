install.packages("readxl")
install.packages("pscl")
library(readxl)
library(pscl)
library(tidyr)

df <- read_excel('Seed_bank_data.xlsx', sheet = 'GM_FM_combined')


###################### Zero Inflated Model ###################

df_zero <- df[, c("GM", "FM")]

#reshaping the data to long format
#'Type' for containing column types and 'Count' for containing their values
df_long <- pivot_longer(
  df_zero, cols = c("GM", "FM"),
  names_to = "Type",
  values_to = "Count"
  )

df_long$Count <- as.numeric(df_long$Count)

df_long$Type <- as.factor(df_long$Type)

head(df_long)

hurdle_model <- hurdle(
  formula = Count ~ Type,
  data = df_long,
  dist = "poisson"
  )

summary(hurdle_model)

coefficients <- coef(hurdle_model)

model_summary <- summary(hurdle_model)

#extracting coefficients
coefficients_count <- model_summary$coefficients$count
coefficients_zero <- model_summary$coefficients$zero


print("Coefficients for the count component:")
print(coefficients_count)


print("Coefficients for the zero hurdle component:")
print(coefficients_zero)





####################### Moran's I ########################

if (!require("spdep")) {
  install.packages("spdep")
}
library(spdep)

df$GM_FM_sum <- rowSums(df[, c("GM", "FM")])


#creating a function to perform Moran's I
perform_morans_test <- function(data, plot_name) {
  locations <- data[, c("X", "Y")]
  #creating neighborhood object with closest cells scored as 1 and others as 0
  nb <- dnearneigh(locations, 0, 1)
  #creating spatial weights object from the neighborhood object
  weights <- nb2listw(nb, style = "W")
  moran_i <- moran.test(data$GM_FM_sum, weights)
  print(paste("Moran's I for", plot_name))
  print(moran_i)
}


#plot 1
df_plot1 <- subset(df, Plot == "P1")
perform_morans_test(df_plot1, "Plot 1")


#plot 2
df_plot2 <- subset(df, Plot == "P2")
perform_morans_test(df_plot2, "Plot 2")
 

#plot 3
df_plot3 <- subset(df, Plot == "P3")
perform_morans_test(df_plot3, "Plot 3")


#plot 4
df_plot4 <- subset(df, Plot == "P4")
perform_morans_test(df_plot4, "Plot 4")

