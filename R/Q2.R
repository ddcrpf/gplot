
Q2 <- function(){
  cat("

install.packages('ggplot2')
library(ggplot2)
data(mpg)
mpg

#i. Visualize the distribution of city mpg
# Histogram of City MPG
hist(mpg$cty, main = 'Distribution of City MPG', xlab = 'City MPG')

#ii. Show the counts of each car type:

barplot(table(mpg$class), main = 'Counts of Each Car Type', xlab = 'Car Type')

#iii. Visualize the relationship between displacement and highway mpg:

plot(mpg$displ, mpg$hwy, main = 'Displacement vs. Highway MPG', xlab = 'Displacement', ylab = 'Highway MPG')

#iv. Display the distribution of engine cylinder counts:

boxplot(mpg$cyl, main = 'Distribution of Engine Cylinder Counts', ylab = 'Cylinder Count')

#v. Subset the dataset to include cars manufactured by Toyota:

toyota_cars <- subset(mpg, manufacturer == 'toyota')
toyota_cars


#vi. Find cars with a city mpg greater than 20 and highway mpg greater than 30:

high_mileage_cars <- subset(mpg, cty > 20 & hwy > 30)
high_mileage_cars

#vii. Create a new column mileage_difference as the difference between city mpg a
# Create Mileage Difference Column
mpg$mileage_difference <- mpg$cty - mpg$hwy
mpg$mileage_difference


#viii. Calculate the average city mpg for each car class and sort them in descendin
# Average City MPG by Car Class
avg_city_mpg <- aggregate(cty ~ class, data = mpg, FUN = mean)
avg_city_mpg_sorted <- avg_city_mpg[order(avg_city_mpg$cty, decreasing = TRUE), ]
avg_city_mpg_sorted


#ix. Group the data by transmission type and calculate the median engine displaceme
# Median Displacement by Transmission Type
median_displacement <- aggregate(displ ~ trans, data = mpg, FUN = median)
median_displacement


#x. Investigate if there is a trend between model year and city mpg:
# Scatter Plot of Model Year vs. City MPG
plot(mpg$year, mpg$cty, main = 'Model Year vs. City MPG', xlab = 'Model Year', ylab = 'City MPG')

#xi. Detect and list the cars with exceptionally high highway mpg values (potential
# Identify Outliers (e.g., highway mpg > 40)
outliers <- subset(mpg, hwy > 40)
outliers
 ")}
