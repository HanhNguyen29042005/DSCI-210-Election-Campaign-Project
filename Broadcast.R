library(tidyverse)
library(readr)
library(lubridate)
library(dplyr)
early.vote <- read_csv("data/AbsenteeListExport-newest_update.csv")
early.vote2 <- read_csv("data/AbsenteeListExport-671858a5cb7fc.csv")





election.date <- as.Date("2024-11-05")
election.date2 <- as.Date("2020-11-03")
####Overall graph of early vote####  
  # Process early.vote
early.vote_processed <- early.vote %>%
    mutate(`Return Ballot Date` = as.Date(`Return Ballot Date`)) %>%
    mutate(type = case_when(
      `Request Application Date` == `Return Ballot Date` ~ "In person",
      `Request Application Date` != `Return Ballot Date` ~ "Returned",
      TRUE ~ "Requested")) %>%
    filter(type == "In person") %>%
    mutate(day.from.election = as.numeric(difftime(election.date, `Return Ballot Date`, units = "days"))*(-1)+28) %>%
    group_by(day.from.election) %>%
    summarize(count = n(), .groups = "drop") %>%
    mutate(year = "2024")
  
  # Process early.vote2

early.vote2_processed <- early.vote2 %>%
    mutate(`Return Ballot Date` = as.Date(`Return Ballot Date`)) %>%
    mutate(type = case_when(
      `Request Application Date` == `Return Ballot Date` ~ "In person",
      `Request Application Date` != `Return Ballot Date` ~ "Returned",
      TRUE ~ "Requested")) %>%
    filter(type == "In person") %>%
    mutate(day.from.election = as.numeric(difftime(election.date2, `Return Ballot Date`, units = "days"))*(-1)+28) %>%
    group_by(day.from.election) %>%
    summarize(count = n(), .groups = "drop") %>%
    mutate(year = "2020")
  
  # Combine both datasets
combined_data <- bind_rows(early.vote_processed, early.vote2_processed)
combined_data <- combined_data %>%
  group_by( year) %>%
  arrange(day.from.election) %>%
  mutate(cumulative_count = cumsum(count))
  
  # Create the line graph
p1 <- ggplot(combined_data, aes(x = day.from.election, y = cumulative_count, color = factor(year))) +
  geom_line(linewidth = 1.5) +
  labs(
    title = "Early In-Person Voting by Days from Election",
    x = "Days from Election",
    y = "Count of Early Vote",
    color = "Election Year"
  ) +
  theme_minimal() +
  scale_x_continuous(
    breaks = seq(0, 28, by = 7),  # Set the breaks at specific intervals
    labels = c("4 weeks before", "3 weeks before", "2 weeks before", "1 week before", "Election day")
  ) +
  scale_color_manual(values = c("2020" = "black", "2024" = "purple"))+
  theme(
    legend.position = "right",
    legend.title = element_text(size = 14),  # Increased legend title size
    legend.text = element_text(size = 12),    # Increased legend text size
    axis.title = element_text(size = 14),      # Increased axis title size
    axis.text = element_text(size = 12),       # Increased axis text size
    plot.title = element_text(size = 16, hjust = 0.5)  # Increased plot title size and centered
  )
ggsave("plot1.png", plot=p1, width = 8, height = 6, units = "in")

####Early vote by Affiliated Party####
early.vote_processed <- early.vote %>%
  mutate(`Return Ballot Date` = as.Date(`Return Ballot Date`)) %>%
  mutate(type = case_when(
    `Request Application Date` == `Return Ballot Date` ~ "In person",
    `Request Application Date` != `Return Ballot Date` ~ "Returned",
    TRUE ~ "Requested")) %>%
  filter(type == "In person") %>%
  filter(VoterParty=="R"| VoterParty=="D") %>% 
  mutate(day.from.election = as.numeric(difftime(election.date, `Return Ballot Date`, units = "days"))*(-1)+28) %>%
  group_by(day.from.election, VoterParty) %>%
  summarize(count = n(), .groups = "drop") %>%
  mutate(year = "2024")

# Process early.vote2
early.vote2_processed <- early.vote2 %>%
  mutate(`Return Ballot Date` = as.Date(`Return Ballot Date`)) %>%
  mutate(type = case_when(
    `Request Application Date` == `Return Ballot Date` ~ "In person",
    `Request Application Date` != `Return Ballot Date` ~ "Returned",
    TRUE ~ "Requested")) %>%
  filter(type == "In person") %>%
  filter(VoterParty=="R"| VoterParty=="D") %>% 
  mutate(day.from.election = as.numeric(difftime(election.date2, `Return Ballot Date`, units = "days"))*(-1)+28) %>%
  group_by(day.from.election, VoterParty) %>%
  summarize(count = n(), .groups = "drop") %>%
  mutate(year = "2020")

# Combine both datasets
combined_data <- bind_rows(early.vote_processed, early.vote2_processed)
combined_data <- combined_data %>%
  group_by(VoterParty, year) %>%
  arrange(day.from.election) %>%
  mutate(cumulative_count = cumsum(count))

# Create the line graph
p2 <- ggplot(combined_data, aes(x = day.from.election, y = cumulative_count, color = VoterParty, linetype = factor(year))) +
  geom_line(size = 1.5) +
  labs(
    title = "Early In-Person Voting by Days from Election(by affiliated party)",
    x = "Days from Election",
    y = "Count of Early Vote",
    color = "Affiliated Party",
    linetype = "Election Year"
  ) +
  theme_minimal() +
  scale_x_continuous(
    breaks = seq(0, 28, by = 7),  # Set the breaks at specific intervals
    labels = c("4 Weeks Before", "3 Weeks Before", "2 Weeks Before", "1 Week Before", "Election Day")
  ) +
  scale_color_manual(values = c("D" = "blue", "R" = "red"))+
  theme(
    legend.position = "right",
    legend.title = element_text(size = 14),  # Increased legend title size
    legend.text = element_text(size = 12),    # Increased legend text size
    axis.title = element_text(size = 14),      # Increased axis title size
    axis.text = element_text(size = 12),       # Increased axis text size
    plot.title = element_text(size = 16, hjust = 0.5)  # Increased plot title size and centered
  )
# Correctly call scale_color_manual
  # scale_color_manual(values = c("Democrat" = "blue", "Republican" = "red")) + 
  # scale_linetype_manual(values = c("2024" = "dashed", "2020" = "solid")) 
ggsave("plot2.png", plot=p2, width = 8, height = 6, units = "in")


####Early Voter by unaffiliated group####
early.vote_processed <- early.vote %>%
  mutate(`Return Ballot Date` = as.Date(`Return Ballot Date`)) %>%
  mutate(type = case_when(
    `Request Application Date` == `Return Ballot Date` ~ "In person",
    `Request Application Date` != `Return Ballot Date` ~ "Returned",
    TRUE ~ "Requested")) %>%
  filter(type == "In person") %>%
  # filter(VoterParty=="N") %>% 
  mutate(day.from.election = as.numeric(difftime(election.date, `Return Ballot Date`, units = "days"))*(-1)+28) %>%
  group_by(day.from.election, VoterParty) %>%
  summarize(count = n(), .groups = "drop") %>%
  mutate(year = "2024")

# Process early.vote2
early.vote2_processed <- early.vote2 %>%
  mutate(`Return Ballot Date` = as.Date(`Return Ballot Date`)) %>%
  mutate(type = case_when(
    `Request Application Date` == `Return Ballot Date` ~ "In person",
    `Request Application Date` != `Return Ballot Date` ~ "Returned",
    TRUE ~ "Requested")) %>%
  filter(type == "In person") %>%
  # filter(VoterParty=="N") %>% 
  mutate(day.from.election = as.numeric(difftime(election.date2, `Return Ballot Date`, units = "days"))*(-1)+28) %>%
  group_by(day.from.election, VoterParty) %>%
  summarize(count = n(), .groups = "drop") %>%
  mutate(year = "2020")

# Combine both datasets
combined_data <- bind_rows(early.vote_processed, early.vote2_processed)
combined_data <- combined_data %>%
  group_by(VoterParty, year) %>%
  arrange(day.from.election) %>%
  mutate(cumulative_count = cumsum(count))
# Create the line graph
p3 <- ggplot(combined_data, aes(x = day.from.election, y = cumulative_count, color = VoterParty, linetype=year)) +
  geom_line(size = 1.5) +
  labs(
    title = "Early In-Person Voting by Days from Election (sorted by Political Party)",
    x = "Days from Election",
    y = "Count of In-Person Early Vote",
    color = "Political Party") +
  theme_minimal() +
  scale_x_continuous(
    breaks = seq(0, 28, by = 7),  # Set the breaks at specific intervals
    labels = c("4 Weeks Before", "3 Weeks Before", "2 Weeks Before", "1 Week Before", "Election Day"))+
  scale_color_manual(values = c("N" = "green", "D" = "blue", "R" = "red"),
                     labels = c("D" = "Dem", "N" = "Unaffiliated", "R" = "Rep"))+
  theme(
    legend.position = "right",
    legend.title = element_text(size = 14),  # Increased legend title size
    legend.text = element_text(size = 12),    # Increased legend text size
    axis.title = element_text(size = 14),      # Increased axis title size
    axis.text = element_text(size = 12),       # Increased axis text size
    plot.title = element_text(size = 16, hjust = 0.5)  # Increased plot title size and centered
  )

ggsave("plot3_new.png", plot=p3, width = 8, height = 6, units = "in")




















early.vote_processed <- early.vote %>%
  mutate(`Return Ballot Date` = as.Date(`Return Ballot Date`)) %>%
  mutate(type = case_when(
    `Request Application Date` == `Return Ballot Date` ~ "In person",
    `Request Application Date` != `Return Ballot Date` ~ "Returned",
    TRUE ~ "Requested")) %>%
  filter(type == "In person") %>%
  filter(VoterParty=="N") %>% 
  mutate(day.from.election = as.numeric(difftime(election.date, `Return Ballot Date`, units = "days"))*(-1)+28) %>%
  group_by(day.from.election, VoterParty) %>%
  summarize(count = n(), .groups = "drop") %>%
  mutate(year = "2024")

# Process early.vote2
early.vote2_processed <- early.vote2 %>%
  mutate(`Return Ballot Date` = as.Date(`Return Ballot Date`)) %>%
  mutate(type = case_when(
    `Request Application Date` == `Return Ballot Date` ~ "In person",
    `Request Application Date` != `Return Ballot Date` ~ "Returned",
    TRUE ~ "Requested")) %>%
  filter(type == "In person") %>%
  filter(VoterParty=="N") %>% 
  mutate(day.from.election = as.numeric(difftime(election.date2, `Return Ballot Date`, units = "days"))*(-1)+28) %>%
  group_by(day.from.election, VoterParty) %>%
  summarize(count = n(), .groups = "drop") %>%
  mutate(year = "2020")

# Combine both datasets
combined_data <- bind_rows(early.vote_processed, early.vote2_processed)
combined_data <- combined_data %>%
  group_by(VoterParty, year) %>%
  arrange(day.from.election) %>%
  mutate(cumulative_count = cumsum(count))
# Create the line graph
p4 <- ggplot(combined_data, aes(x = day.from.election, y = cumulative_count, color = VoterParty, linetype=year)) +
  geom_line(size = 1.5) +
  labs(
    title = "Early In-Person Voting by Days from Election (Unaffiliated Group)",
    x = "Days from Election",
    y = "Count of In-Person Early Vote",
    color = "Political Party") +
  theme_minimal() +
  scale_x_continuous(
    breaks = seq(0, 28, by = 7),  # Set the breaks at specific intervals
    labels = c("4 Weeks Before", "3 Weeks Before", "2 Weeks Before", "1 Week Before", "Election Day"))+
  scale_color_manual(values = "green")+
  theme(
    legend.position = "right",
    legend.title = element_text(size = 14),  # Increased legend title size
    legend.text = element_text(size = 12),    # Increased legend text size
    axis.title = element_text(size = 14),      # Increased axis title size
    axis.text = element_text(size = 12),       # Increased axis text size
    plot.title = element_text(size = 16, hjust = 0.5)  # Increased plot title size and centered
  )
ggsave("plot4.png", plot=p4, width = 8, height = 6, units = "in")
