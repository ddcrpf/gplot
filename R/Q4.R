Q4 <- function(){

  cat("
library(ggplot2)
# Load the economics dataset
data('economics')
economics


# i. Average Unemployment Rate
avg_unemployment_rate <- mean(economics$unemploy)
avg_unemployment_rate


# ii. Quarterly GDP Changes
library(lubridate)
library(dplyr)
quarterly_changes <- economics %>%
   mutate(year_quarter = paste(year(date), 'Q', quarter(date), sep='')) %>%
   group_by(year_quarter) %>%
   summarise(quarterly_change = (max(pce) - min(pce)) / min(pce) * 100)
max_change <- quarterly_changes[which.max(quarterly_changes$quarterly_change), ]
min_change <- quarterly_changes[which.min(quarterly_changes$quarterly_change), ]
min_change
max_change



# iii. Inflation Analysis
avg_inflation_rate <- mean(economics$psavert)
avg_inflation_rate


# iv. Unemployment Trends
unemployment_trends_plot <- ggplot(data=economics, aes(x=date, y=unemploy)) +
 geom_line() +
 labs(x='Date', y='Unemployment Rate') +
 ggtitle('Unemployment Rate Trends Over Time')
unemployment_trends_plot



# v. Inflation vs. Unemployment
inflation_unemployment_plot <- ggplot(data=economics, aes(x=psavert, y=unemploy)) +
 geom_point() +
 labs(x='Personal Savings Rate', y='Unemployment Rate') +
 ggtitle('Relationship Between Personal Savings Rate and Unemployment Rate')
inflation_unemployment_plot



# vi. Unemployment Rate by Year
unemployment_by_year_plot <- ggplot(data=economics, aes(x=year(date), y=unemploy))
 geom_bar(stat='summary', fun=mean) +
 labs(x='Year', y=Average Unemployment Rate') +
 ggtitle('Average Unemployment Rate by Year')
unemployment_by_year_plot



# vii. GDP and Personal Consumption Comparison
# Create a bar plot comparing GDP and Personal Consumption by Year
gdp_personal_consumption_comparison_plot <- ggplot(data = economics, aes(x = year(date))) +
  geom_bar(aes(y = pce, fill = 'Personal Consumption'), alpha = 0.6, stat = 'identity') +
  geom_bar(aes(y = pop, fill = 'GDP'), alpha = 0.6, stat = 'identity') +
  labs(x = 'Year', y = 'Value') +
  scale_fill_manual(values = c('Personal Consumption' = 'blue', 'GDP' = 'green')) +
  ggtitle('Comparison of Personal Consumption and GDP by Year')

# Display the plot
print(gdp_personal_consumption_comparison_plot)



# viii. Inflation Rate Fluctuations
inflation_fluctuations_plot <- ggplot(data=economics, aes(x=date, y=psavert)) +
 geom_point() +
 labs(x='Date', y='Personal Savings Rate') +
 ggtitle('Personal Savings Rate Fluctuations)
inflation_fluctuations_plot


# ix. Economic Indicators Comparison
economic_indicators_comparison_plot <- ggplot(data=economics, aes(x=date)) +
 geom_line(aes(y=pce, color='Personal Consumption'), alpha=0.6) +
 geom_line(aes(y=pop, color='GDP'), alpha=0.6) +
 labs(x='Date', y='Value') +
 scale_color_manual(values=c('Personal Consumption'='blue', 'GDP'='green')) +
 ggtitle('Comparison of Economic Indicators Over Time')
economic_indicators_comparison_plot


 ")
}
