Q3<- function(){
  cat("

airlines <- read.csv('D:/NMIT/AEC R Programming/airlines.csv')
airlines

#i Find descriptive statistics for all columns
summary(airlines)



#ii. Plot the distribution of numerical columns
# Specify numerical columns
numerical_columns <- c('Flight', 'DayOfWeek', 'Time', 'Length', 'Delay')
# Create histograms for each numerical column
for (column in numerical_columns) {
   # Create a histogram with density plot (kde)
   hist_data <- airlines[[column]]
   hist_density <- density(hist_data)

   # Plot the histogram
   hist_obj <- hist(hist_data, main = paste('Distribution of', column), xlab = column, col = 'lightblue')
   # Add a density plot (kde)
   lines(hist_density, col = 'blue, lwd = 2)

   # Add labels
   xlabel <- paste(column, '(Value)')
   ylabel <- 'Frequency'
   title <- paste('Distribution of', column)
   legend('topright', legend = 'KDE', col = 'blue, lwd = 2)

   # Customize labels and titles
   xlabel <- paste(column, '(Value))
   ylabel <- 'Frequency'
   title <- paste('Distribution of', column)

   # Add labels and title
   xlabel <- paste(column, '(Value)')
   ylabel <- 'Frequency'
   title <- paste('Distribution of', column)
   title(main = title, xlab = xlabel, ylab = ylabel)

}

#iii. Plot the distribution of numerical columns
# Load the ggplot2 library
library(ggplot2)
# Define your categorical columns
categorical_columns <- c('Airline', 'AirportFrom', 'AirportTo')
# Loop through each categorical column
for (column in categorical_columns) {
   # Create a ggplot object
   plot <- ggplot(data = airlines, aes_string(x = column)) +
   geom_bar() + # Create a bar plot
   labs(x = column, y = 'Count', title = paste('Counts of', column)) +
   theme(axis.text.x = element_text(angle = 90, hjust = 1)) # Rotate x-axis label

   # Print the plot
   print(plot)
}


# iv. Find and plot the top 5 airlines with maximum delay
library(dplyr)
library(ggplot2)
# Assuming your dataset is named 'df'
# Group by 'Airline' and calculate the sum of 'Delay', then arrange in descending o
top_airlines <- airlines %>%
   group_by(Airline) %>%
   summarize(Delay = sum(Delay)) %>%
   arrange(desc(Delay)) %>%
   head(5)
# Create a bar plot
ggplot(top_airlines, aes(x = reorder(Airline, -Delay), y = Delay)) +
   geom_bar(stat = 'identity', fill = 'blue') +
   labs(x = 'Airline', y = 'Delay', title = 'Top 5 Airlines with Maximum Delay') +
   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
   coord_flip() # To make the bars horizontal

# v. Plot the distribution of time of delay and no delay(bivariate). Write the infe
# Create a new column 'Delay_Status' with values 'Delay' or 'No Delay'
airlines$Delay_Status <- ifelse(airlines$Delay == 1, 'Delay', 'No Delay')
# Load the ggplot2 package if not already loaded
library(ggplot2)
# Create a boxplot
ggplot(airlines, aes(x = Delay_Status, y = Time)) +
   geom_boxplot() +
   labs(x = 'Delay Status', y = 'Time', title = 'Distribution of Time for Delay and no delay')


#vi.Plot the distribution of length of delay and no delay(bivariate)
# Load the ggplot2 package if not already loaded
library(ggplot2)
# Create a boxplot
ggplot(airlines, aes(x = as.factor(Delay), y = Length)) +
   geom_boxplot() +
   labs(x = 'Delay', y = 'Length', title = 'Distribution of Length for Delay and No dealy')



#vii. Determine the distribution of flight counts for each airport of arrival (Airp
# Load the necessary libraries
library(dplyr)
library(ggplot2)
# Group the data by AirportTo and calculate the flight counts
flight_counts <- airlines %>%
 group_by(AirportTo) %>%
 summarise(Flight_Count = n())
# Create a bar plot to visualize the distribution of flight counts
ggplot(flight_counts, aes(x = reorder(AirportTo, -Flight_Count), y = Flight_Count))
 geom_bar(stat = 'identity', fill = 'lightblue', color = 'black) +
 labs(title = 'Distribution of Flight Counts by Airport of Arrival', x = 'Airport
theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggsave('Flight_Counts_by_AirportTo.png')


 ")
}
