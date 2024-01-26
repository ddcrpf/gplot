Q1 <- function(){
  cat("
  # Load the necessary library,
  library(dplyr)
  # Read the dataset
  phone_data <- read.csv()
  phone_data
# i. Find the number of Vodafone users
vodafone_users <- phone_data %>%
  filter(network == 'Vodafone') %>%
  nrow()
vodafone_users

# ii. Find the number of calls whose duration is greater than 15 minutes in the 'ph
number_of_calls <- sum(phone_data$duration > 15)
 # Print the number of calls
number_of_calls

#iii.Find the month of top 5 duration calls
top5_duration_calls <- phone_data %>%
    filter(item == 'call') %>%
    arrange(desc(duration)) %>%
    distinct(month, .keep_all = TRUE) %>%
    head(5)
# Print the top 5 months with their corresponding durations
print(top5_duration_calls)

# iv. Find the average duration of each network type
avg_duration_by_network_type <- phone_data %>%
     group_by(network_type) %>%
    summarise(avg_duration = mean(duration))
avg_duration_by_network_type

# v. Find the average duration of each item for each network
avg_duration_by_item_network <- phone_data %>%
    group_by(item, network) %>%
    summarise(avg_duration = mean(duration))
avg_duration_by_item_network

# vi. Find the count of each item for each network
count_by_item_network <- phone_data %>%
    group_by(item, network) %>%
    summarise(count = n())
count_by_item_network

# vii. Find which month contains a greater number of calls
month_with_max_calls <- phone_data %>%
  filter(item == 'call') %>%
  group_by(month) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(1)
month_with_max_calls

# viii. Find the total number of messages sent by world users
total_messages_world_users <- phone_data %>%
    filter(network == 'world' & item == 'sms') %>%
    nrow()
total_messages_world_users

# ix. Find the proportion of Meteor users in the network
proportion_meteor_users <- phone_data %>%
    filter(network == 'Meteor') %>%
    nrow() / nrow(phone_data)
proportion_meteor_users

# x. Check which month has the highest duration
month_with_highest_duration <- phone_data %>%
    group_by(month) %>%
    summarise(total_duration = sum(duration)) %>%
    arrange(desc(total_duration)) %>%
    head(1)
month_with_highest_duration
 ")
}


