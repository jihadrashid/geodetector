
#loading packages
library(raster)
ccc <- shapefile(".../Drivers_LU/CCC.shp")
drivers <- shapefile(".../Drivers_LU/Factors_Final3.shp")



df <- as.data.frame(drivers)
head(df)
ndf <- df[c(2:17, 20:21)]
head(ndf)
names(ndf)<- c("BU","For","Riv","Cst","Adm","Road","Rail","TS","Cnl","Edu","Hos","ReS","GrC","CBD","Elv","Slp","x","y")
head(ndf)




# Load required library
library(GD)

# Set the dependent variable (change as needed)
var_to_test <- "BU"

# Create a copy of your data
temp_df <- ndf

# Discretize all predictors using the "quantile" method
for (col in names(temp_df)[-which(names(temp_df) == var_to_test)]) {
  temp_df[[col]] <- discretize(temp_df[[col]], method = "equal", num.class = 5)
}

# Run GeoDetector
g_quantile <- gd(as.formula(paste(var_to_test, "~ .")), data = temp_df)

# Create a copy of your data
temp_df <- ndf
# Discretize all predictors using the "quantile" method
for (col in names(temp_df)[-which(names(temp_df) == var_to_test)]) {
  temp_df[[col]] <- discretize(temp_df[[col]], method = "quantile", num.class = 5)
}

# Run GeoDetector
h_quantile <- gd(as.formula(paste(var_to_test, "~ .")), data = temp_df)


# Create a copy of your data
temp_df <- ndf
# Discretize all predictors using the "quantile" method
for (col in names(temp_df)[-which(names(temp_df) == var_to_test)]) {
  temp_df[[col]] <- discretize(temp_df[[col]], method = "fisher", num.class = 5)
}

# Run GeoDetector
i_quantile <- gd(as.formula(paste(var_to_test, "~ .")), data = temp_df)

# View results
print(g_quantile)
plot(g_quantile)
# View results
print(h_quantile)
plot(h_quantile)
# View results
print(i_quantile)
plot(i_quantile)

# Create a data frame summarizing the global q-statistics from each method
results_table <- data.frame(
  Method = c("Equal Interval", "Quantile", "Fisher-Jenks"),
  Q_statistic = c(g_quantile$q, h_quantile$q, i_quantile$q)
)

# Print the summary table
print(results_table)




