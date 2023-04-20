## Ximeng Deng
## Apr.20 2023
## IST 719 Information Visualization
## Final Poster 

# loading the packages
library(tidyverse)
library(ggplot2)
library(ggridges)

data <- read.csv("/Users/ximengdeng/Desktop/2023spring/ist719/poster/draft/data.csv", 
                 stringsAsFactors = FALSE)
str(data)

# this is a data set of Broadway Grosses
# Data is by week
# 2018-08-05 to 2020-03-08
# 2021-08-08 to 2023-03-12
# Data from 84 weeks before and 84 weeks after the pandemic


# data cleaning
data$This_Week_Gross <- gsub(",", "", data$This_Week_Gross)
data$This_Week_Gross <- gsub("\\$", "", data$This_Week_Gross)
data$This_Week_Gross <- as.numeric(data$This_Week_Gross)

data$Difference <- gsub(",","", data$Difference)
data$Difference <- gsub("\\$", "", data$Difference)
data$Difference <- as.numeric(data$Difference)

data$Average_ticket <- gsub(",", "", data$Average_ticket)
data$Average_ticket <- gsub("\\$", "", data$Average_ticket)
data$Average_ticket <- as.numeric(data$Average_ticket)

data$Top_ticket <- gsub(",", "", data$Top_ticket)
data$Top_ticket <- gsub("\\$", "", data$Top_ticket)
data$Top_ticket <- as.numeric(data$Top_ticket)

data$Seats_Sold  <- gsub(",", "", data$Seats_Sold )
data$Seats_Sold  <- gsub("\\$", "", data$Seats_Sold )
data$Seats_Sold  <- as.numeric(data$Seats_Sold )

data$Seats_in_theater  <- gsub(",", "", data$Seats_in_theater )
data$Seats_in_theater  <- gsub("\\$", "", data$Seats_in_theater )
data$Seats_in_theater  <- as.numeric(data$Seats_in_theater )

data$X.cap <- gsub("%", "", data$X.cap)
data$X.cap <- as.numeric(data$X.cap)

data$diff_cap <- gsub("%", "", data$diff_cap)
data$diff_cap <- as.numeric(data$diff_cap)

data$date <- as.Date(data$date)
data$total_perfs_previews <- data$Perfs + data$Previews

data$Show_theatre <- ifelse(data$Show_theatre == "James Earl Jones Theatre", 
                            "Cort Theatre", data$Show_theatre)
data$Show_theatre <- ifelse(data$Show_theatre == "Lena Horne Theatre", 
                            "Brooks Atkinson Theatre", data$Show_theatre)

# View the data
View(data)
str(data)
#write.csv(data, "/Users/ximengdeng/Desktop/2023spring/ist719/poster/draft/data_clean.csv", row.names = FALSE)

# creating the subset
data_pre <- data[data$date_type == "pre", ]
data_post <- data[data$date_type == "post", ]
#View(data_pre)
#View(data_post)
data_pre%>% 
  summarise(n_distinct_dates = n_distinct(date))
data_post%>% 
  summarise(n_distinct_dates = n_distinct(date))

str(data_pre)
str(data_post)
################################################################################
################################################################################

# 1 time series
##Total Grosses

options(scipen = 90)

data_total <- data %>%
  group_by(date) %>%
  summarise(total_gross = sum(This_Week_Gross))

data_musicals <- data %>%
  filter(show_type == "Musical") %>%
  group_by(date) %>%
  summarise(total_gross_musicals = sum(This_Week_Gross))

data_plays <- data %>%
  filter(show_type == "Play") %>%
  group_by(date) %>%
  summarise(total_gross_plays = sum(This_Week_Gross))

data_special <- data %>%
  filter(show_type == "Special") %>%
  group_by(date) %>%
  summarise(total_gross_special = sum(This_Week_Gross))

data_merged <- merge(data_total, data_musicals, by = "date", all = TRUE)
data_merged <- merge(data_merged, data_plays, by = "date", all = TRUE)
data_merged <- merge(data_merged, data_special, by = "date", all = TRUE)

ggplot(data_merged, aes(x = date)) +
  geom_line(aes(y = total_gross, color = "Total Gross")) +
  geom_line(aes(y = total_gross_musicals, color = "Musicals")) +
  geom_line(aes(y = total_gross_plays, color = "Plays")) +
  geom_line(aes(y = total_gross_special, color = "Special")) +
  scale_color_manual(name = "Show Type", values = c("Total Gross" = "black", 
                                                    "Musicals" = "red", 
                                                    "Plays" = "blue", 
                                                    "Special" = "green")) +
  labs(x = "Date", y = "Gross Revenue", title = "Weekly Gross by Show Type") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.title.x = element_blank())+
  scale_x_date(date_breaks = "1 week", date_labels = "%m/%d/%y") 


################################################################################

# 2

type_count <- data %>%
  group_by(date, show_type) %>%
  summarize(count = n()) 

data_total_count <- type_count %>%
  group_by(date) %>%
  summarize(total_count = sum(count))

data_merged <- merge(type_count, data_total_count, by = "date")

ggplot(data_merged, aes(x = date, y = count, color = show_type)) +
  geom_line() +
  geom_line(aes(y = total_count, color = "Total")) +
  scale_x_date(date_breaks = "1 week", labels = scales::date_format("%Y-%m-%d"), minor_breaks = NULL) +
  labs(x = "Date", y = "Count of Productions", title = "Time Series of Productions by Show Type") +
  scale_color_manual(values = c("Musical" = "blue", "Play" = "green", "Special" = "orange", "Total" = "red")) +  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))


################################################################################

# 3 
data_count <- data %>%
  group_by(show_type, date_type) %>%
  summarise(count = n_distinct(Show_name))
data_count$date_type <- factor(data_count$date_type, levels = c("pre", "post"))

ggplot(data_count, aes(x = show_type, y = count, fill = date_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Show Type", y = "Count of Distinct Show Name", 
       title = "Count of Distinct Show Name by Type and Date Type") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme_minimal()

################################################################################

# 4

data_count <- data %>%
  group_by(show_type, Show_name, date_type) %>%
  count()

ggplot(data_count, aes(x = n, y = show_type, fill = date_type)) +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
  ggtitle("Production Running Weeks") +
  xlab("Count") +
  ylab("Show Type") +
  theme_minimal()


################################################################################

# 5

pre_cap <- data_pre%>%
  group_by(Show_name)%>%
  summarise(Mean_pre = mean(X.cap))

post_cap <- data_post%>%
  group_by(Show_name)%>%
  summarise(Mean_post = mean(X.cap))

show <- pre_cap %>%
  inner_join(post_cap, by = "Show_name") %>%
  mutate(Diff = Mean_post - Mean_pre) %>%
  arrange(desc(Diff)) %>%
  mutate(Show_name = paste0(11:(10 + nrow(.)), "- ", Show_name))

pivot_show <- show %>%
  pivot_longer(cols = c("Mean_pre", "Mean_post"), 
               names_to = "cap_period", 
               values_to = "cap_value")
pivot_show$cap_period <- factor(pivot_show$cap_period, 
                                levels = c("Mean_pre", "Mean_post"))

ggplot(pivot_show, aes(x = Show_name, y = cap_value, fill = cap_period)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Show Name", y = "Capacity", fill = "cap_period") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())

################################################################################

# 6

count_week <- data %>%
  group_by(date_type, Show_name) %>%
  summarize(count = n()) 

show_avg_gross <- data %>% 
  group_by(date_type, Show_name) %>% 
  summarize(avg_gross = mean(This_Week_Gross))

top_pre <- show_avg_gross %>% 
  filter(date_type == "pre") %>% 
  top_n(10, avg_gross) %>% 
  arrange(desc(avg_gross))

top_post <- show_avg_gross %>% 
  filter(date_type == "post") %>% 
  top_n(10, avg_gross) %>% 
  arrange(desc(avg_gross))

top_data <- data.frame(
  period = c(rep("pre", 10), rep("post", 10)),
  Show_name = c(top_pre$Show_name, top_post$Show_name),
  avg_gross = c(top_pre$avg_gross, top_post$avg_gross)
)

top_data_count <- inner_join(top_data, count_week, by = c("period" = "date_type", "Show_name"))
top_data_count$period <- factor(top_data_count$period, levels = c("pre", "post"))


ggplot(top_data_count, aes(x = reorder(interaction(Show_name, period, sep = "_"), -avg_gross), y = avg_gross, fill = period)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_text(aes(label = count), position = position_dodge(width = 0.9), vjust = -0.25) +
  facet_grid(. ~ period, scales = "free_x", switch = "x") + 
  labs(x = "Show Name", y = "Average Weekly Gross", fill = "period") +
  ggtitle("Top 10 Shows by Average Weekly Gross Pre- and Post-Pandemic") +
  scale_fill_manual(values = c("pre" = "darkgreen", "post" = "darkorange")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.spacing = unit(1, "cm"),
        strip.text = element_blank(),
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_x_discrete(labels = function(x) gsub("^(.+)_.*$", "\\1", x)) 


################################################################################

# 7

show_total_gross <- data %>% 
  group_by(date_type, Show_name) %>% 
  summarize(total_gross = sum(This_Week_Gross)) 

top_pre <- show_total_gross %>% 
  filter(date_type == "pre") %>% 
  top_n(10, total_gross) %>% 
  arrange(desc(total_gross))

top_post <- show_total_gross %>% 
  filter(date_type == "post") %>% 
  top_n(10, total_gross) %>% 
  arrange(desc(total_gross))

top_data <- data.frame(
  period = c(rep("pre", 10), rep("post", 10)),
  Show_name = c(top_pre$Show_name, top_post$Show_name),
  total_gross = c(top_pre$total_gross, top_post$total_gross)
)

count_week <- data %>%
  group_by(date_type, Show_name) %>%
  summarize(count = n()) 

top_data_count <- inner_join(top_data, count_week, by = c("period" = "date_type", "Show_name"))
top_data_count$period <- factor(top_data_count$period, levels = c("pre", "post"))

ggplot(top_data_count, aes(x = reorder(interaction(Show_name, period, sep = "_"), -total_gross), y = total_gross, fill = period)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_text(aes(label = count), position = position_dodge(width = 0.9), vjust = -0.25) +
  facet_grid(. ~ period, scales = "free_x", switch = "x") + 
  labs(x = "Show Name", y = "Total Gross", fill = "Period") +
  ggtitle("Top 10 Shows by Total Gross Pre- and Post-Pandemic") +
  scale_fill_manual(values = c("pre" = "darkgreen", "post" = "darkorange")) +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        panel.spacing = unit(1, "cm"),
        strip.text = element_blank(),
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_x_discrete(labels = function(x) gsub("^(.+)_.*$", "\\1", x))


################################################################################

# 8

overall_avg_ticket <- mean(data$Average_ticket)
overall_median_ticket <- median(data$Average_ticket)
ticket_by_type <- data %>%
  group_by(show_type, date_type) %>% 
  summarize(Avg_Ticket = Average_ticket)

ticket_by_type$date_type <- factor(ticket_by_type$date_type, levels = c("pre", "post"))

ggplot(ticket_by_type, aes(x = show_type, y = Avg_Ticket, fill = date_type)) +
  geom_boxplot(position = "dodge") + 
  labs(x = "Show Type", y = "Avg Ticket Price", title = "Avg Ticket Price by Show Type and Period") +
  scale_fill_manual(values = c("pre" = "darkgreen", "post" = "darkorange")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_hline(yintercept = overall_avg_ticket, linetype = "dashed", color = "gray") +
  geom_hline(yintercept = overall_median_ticket, linetype = "dashed", color = "yellow") 

################################################################################

# 9

earliest_dates <- data %>%
  select(Show_name, date) %>%
  group_by(Show_name) %>%
  summarise(min_date = min(date)) %>%
  arrange(min_date)

data$Show_name <- factor(data$Show_name, levels = rev(earliest_dates$Show_name))

unique_shows_dates <- data %>%
  select(Show_name, date, This_Week_Gross) %>%
  distinct()

ggplot(unique_shows_dates, aes(x = date, y = Show_name, color = This_Week_Gross)) + 
  geom_point() + 
  labs(x = "Date", y = "Show Name", title = "Calendar with weekly gross") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal() +
  scale_color_gradientn(colours = c("white", "lightblue", "darkolivegreen3", "#FFFFBF", "#FDAE61", "#D7191C", "purple4", "black"), 
                        limits = range(unique_shows_dates$This_Week_Gross))

################################################################################

# 10

plot_data <- data %>%
  group_by(Show_theatre, show_type) %>%
  summarise(n = n_distinct(Show_name)) %>%
  ungroup() %>%
  mutate(Musical_count = if_else(show_type == "Musical", n, 0L)) %>%
  group_by(Show_theatre) %>%
  mutate(Musical_count = sum(Musical_count)) %>%
  arrange(desc(Musical_count), Show_theatre, show_type) %>%
  mutate(show_type = factor(show_type, levels = c("Special", "Play", "Musical")))


ggplot(plot_data, aes(x = fct_reorder(Show_theatre, -Musical_count), y = n, fill = show_type)) +
  geom_col() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Show Theatre")






