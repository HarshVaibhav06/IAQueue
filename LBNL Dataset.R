library(readxl)
library(tidyverse)
library(plotly)

# Load data
queue_data_raw <- read_excel("C:/Users/Harsh Vaibhav/Downloads/queues_2023_clean_data_r1.xlsx", sheet = "data")

queue_data <- queue_data_raw %>%
  mutate(
    total_mw = rowSums(across(c(mw1, mw2, mw3)), na.rm = TRUE),
    type_clean = factor(type_clean) 
  )

#0 Number of Projects vs Operational Status

queue_data$q_status <- ifelse(is.na(queue_data$q_status), "unknown", queue_data$q_status)

status_over_time <- queue_data %>%
  filter(!is.na(q_year)) %>%
  group_by(q_status, q_year) %>%
  summarise(project_count = n(), .groups = "drop")

ggplot(status_over_time, aes(x = q_year, y = project_count, color = q_status)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Project Counts Over Time by Queue Status",
    x = "Year of Queue Entry",
    y = "Number of Projects",
    color = "Queue Status"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = unique(status_over_time$q_year)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 1. Active projects
active_projects <- queue_data %>% filter(q_status == "active") %>%
  mutate(
    total_mw = rowSums(across(c(mw1, mw2, mw3)), na.rm = TRUE)
  )

tech_summary <- active_projects %>%
  group_by(type_clean) %>%
  summarize(
    project_count = n(),
    avg_mw = mean(total_mw, na.rm = TRUE),
    total_mw = sum(total_mw, na.rm = TRUE)
  )

ggplot(tech_summary, aes(x = reorder(type_clean, -project_count), y = project_count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Number of Active Projects by Technology", x = "Technology", y = "Number of Projects")

#2. Projects by region, by status

status_by_region <- queue_data %>%
  filter(!is.na(q_status), !is.na(region)) %>%
  group_by(region, q_status) %>%
  summarise(project_count = n(), .groups = "drop")

# Plot the data as a stacked bar plot
ggplot(status_by_region, aes(x = region, y = project_count, fill = q_status)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Project Status by Region",
    x = "Region",
    y = "Number of Projects",
    fill = "Queue Status"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#3. Total MW, by type, by status

total_mw_by_type_status <- queue_data %>%
  filter(!is.na(q_status), !is.na(type_clean)) %>%
  mutate(
    total_mw = rowSums(across(c(mw1, mw2, mw3)), na.rm = TRUE)  # Sum MW columns using rowSums and across
  ) %>%
  group_by(type_clean, q_status) %>%
  summarise(total_mw = sum(total_mw, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_mw))  # Sort by total MW in descending order

# Plot the data as a bar plot
ggplot(total_mw_by_type_status, aes(x = reorder(type_clean, -total_mw), y = total_mw, fill = q_status)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Total MW by Project Type and Status",
    x = "Project Type",
    y = "Total MW",
    fill = "Queue Status"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#3a Inference: Solar + Battery vs Solar. Is the MW size different between Solar-only and Solar+Storage projects?

solar_vs_solar_storage <- queue_data %>%
  filter(type_clean %in% c("Solar", "Solar+Battery")) %>%
  mutate(
    total_mw = rowSums(across(c(mw1, mw2, mw3)), na.rm = TRUE)
  )

t_test_result <- t.test(total_mw ~ type_clean, data = solar_vs_solar_storage)

print(t_test_result)

ggplot(solar_vs_solar_storage, aes(x = type_clean, y = total_mw, fill = type_clean)) +
  geom_boxplot(outlier.alpha = 0.2) +
  labs(
    title = "MW Size Comparison: Solar vs Solar+Storage",
    x = "Project Type",
    y = "Total MW"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

#3b ANOVA and TukeyHSD, to check for pairwise significance
queue_data$type_clean <- relevel(queue_data$type_clean, ref = "Solar")

anova_result <- aov(total_mw ~ type_clean, data = queue_data)
summary(anova_result)

TukeyHSD(anova_result,conf.level=0.95)

lm_model <- lm(total_mw ~ type_clean, data = queue_data)
summary(lm_model)

library(broom)

tidy(lm_model) %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(x = reorder(term, estimate), y = estimate)) +
  geom_col() +
  coord_flip() +
  labs(title = "Effect of Project Type on Total MW",
       x = "Project Type (vs. Baseline)",
       y = "Difference in Average Total MW")

#4 Time in ia queue and becoming online after entering queue, by project type

# Base cleaning
queue_data <- queue_data %>%
  mutate(
    q_date = parse_date_time(q_date, orders = c("mdy", "ymd", "B d, Y")),
    ia_date = parse_date_time(ia_date, orders = c("mdy", "ymd", "B d, Y")),
    wd_date = parse_date_time(wd_date, orders = c("mdy", "ymd", "B d, Y")),
    on_date = parse_date_time(on_date, orders = c("mdy", "ymd", "B d, Y"))
  ) %>%
  filter(q_date >= as.Date("1995-01-01") | is.na(q_date)) %>%
  mutate(
    days_to_ia = ifelse(!is.na(q_date) & !is.na(ia_date) & ia_date > q_date,
                        as.numeric(difftime(ia_date, q_date, units = "days")), NA),
    days_to_on = ifelse(!is.na(q_date) & !is.na(on_date) & on_date > q_date,
                        as.numeric(difftime(on_date, q_date, units = "days")), NA)
  )

#Calc for avg MW
avg_mw_ia <- queue_data %>%
  filter(!is.na(days_to_ia)) %>%
  group_by(type_clean) %>%
  summarise(avg_mw = mean(total_mw, na.rm = TRUE)) %>%
  ungroup()

avg_mw_on <- queue_data %>%
  filter(!is.na(days_to_on)) %>%
  group_by(type_clean) %>%
  summarise(avg_mw = mean(total_mw, na.rm = TRUE)) %>%
  ungroup()

#Plots data
queue_data_plot_ia <- queue_data %>%
  filter(!is.na(days_to_ia)) %>%
  left_join(avg_mw_ia, by = "type_clean")

queue_data_plot_on <- queue_data %>%
  filter(!is.na(days_to_on)) %>%
  left_join(avg_mw_on, by = "type_clean")

# IA plot
ggplot(queue_data_plot_ia, aes(x = type_clean, y = days_to_ia)) +
  geom_boxplot(outlier.alpha = 0.2, fill = "#69b3a2") +
  geom_text(
    aes(y = max(days_to_ia, na.rm = TRUE) * 1.05, 
        label = paste0(round(avg_mw, 1), " MW")),
    hjust = 0, size = 3, color = "black"
  ) +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.3))) +  
  labs(
    title = "Time to Interconnection Agreement (IA) by Project Type",
    subtitle = "Average project size (MW) shown on the right",
    x = "Project Type",
    y = "Days from Queue Entry to IA"
  ) +
  theme_minimal() +
  theme(plot.margin = margin(10, 80, 10, 10))

# ON plot
ggplot(queue_data_plot_on, aes(x = type_clean, y = days_to_on)) +
  geom_boxplot(outlier.alpha = 0.2, fill = "#404080") +
  geom_text(
    aes(y = max(days_to_on, na.rm = TRUE) * 1.05, 
        label = paste0(round(avg_mw, 1), " MW")),
    hjust = 0, size = 3, color = "black"
  ) +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.3))) +  
  labs(
    title = "Time to Commercial Operation (ON) by Project Type",
    subtitle = "Average project size (MW) shown on the right",
    x = "Project Type",
    y = "Days from Queue Entry to Operation"
  ) +
  theme_minimal() +
  theme(plot.margin = margin(10, 80, 10, 10))

#5 Projects (count i.e number) by status and type, across/by region

project_summary <- queue_data %>%
  count(region, q_status, type_clean) %>%
  arrange(desc(n))

ggplot(project_summary, aes(x = type_clean, y = n, fill = q_status)) +
  geom_col(position = "dodge") +
  facet_wrap(~ region) +
  labs(
    title = "Projects by Type and Status across Regions",
    x = "Technology Type",
    y = "Number of Projects",
    fill = "Status"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(project_summary, aes(x = q_status, y = n, fill = type_clean)) +
  geom_col(position = "dodge") +
  facet_grid(region ~ .) +
  labs(
    title = "Projects by Status and Type, by Region",
    x = "Status",
    y = "Number of Projects",
    fill = "Technology Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(project_summary, aes(x = type_clean, y = n, fill = q_status)) +
  geom_col() +
  facet_wrap(~ region) +
  labs(
    title = "Projects (stacked) by Type and Status across Regions",
    x = "Technology Type",
    y = "Number of Projects",
    fill = "Status"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#6 Projects (capacity i.e MW) by status and type, across/by region 

mw_summary <- queue_data %>%
  group_by(region, q_status, type_clean) %>%
  summarise(total_mw = sum(total_mw, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_mw))

ggplot(mw_summary, aes(x = type_clean, y = total_mw, fill = q_status)) +
  geom_col(position = "dodge") +
  facet_wrap(~ region) +
  labs(
    title = "Projects by Type and Status across Regions",
    x = "Technology Type",
    y = "Total MW",
    fill = "Status"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(mw_summary, aes(x = q_status, y = total_mw, fill = type_clean)) +
  geom_col(position = "dodge") +
  facet_grid(region ~ .) +
  labs(
    title = "Projects by Status and Type, by Region",
    x = "Status",
    y = "Total MW",
    fill = "Technology Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(mw_summary, aes(x = type_clean, y = total_mw, fill = q_status)) +
  geom_col() +
  facet_wrap(~ region) +
  labs(
    title = "Projects (stacked) by Type and Status across Regions",
    x = "Technology Type",
    y = "Total MW",
    fill = "Status"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#7 POI Congestion

poi_congestion <- queue_data %>%
  filter(!is.na(poi_name), !poi_name %in% c("Unknown", "TBD", "tbd", "0")) %>%
  group_by(poi_name) %>%
  summarise(
    total_projects = n(),
    total_mw = sum(total_mw, na.rm = TRUE)
  ) %>%
  arrange(desc(total_mw))

top_pois <- poi_congestion %>%
  slice_max(total_mw, n = 20) 

ggplot(top_pois, aes(x = reorder(poi_name, total_mw), y = total_mw)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 20 Most Congested POIs by Total MW",
    x = "Point of Interconnection (POI)",
    y = "Total MW"
  ) +
  theme_minimal()

#8 Developer Behavior - Top 20 Developers by total MW, and by count

developer_plot_data_mw <- queue_data %>%
  filter(!is.na(developer), developer != "N/A") %>%
  group_by(developer) %>%
  summarise(
    project_count = n(),
    total_mw = sum(total_mw, na.rm = TRUE)
  ) %>%
  arrange(desc(total_mw)) %>%
  slice_head(n = 20) %>%
  mutate(developer = reorder(developer, total_mw))

p <- ggplot(developer_plot_data_mw, aes(x = reorder(developer, total_mw), y = total_mw, 
                                     text = paste("Developer:", developer,
                                                  "<br>Total MW:", total_mw,
                                                  "<br>Projects:", project_count))) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(
    title = "Top 20 Developers by Total MW in Queue",
    x = "Developer",
    y = "Total MW"
  ) +
  theme_minimal()

# Make interactive
ggplotly(p, tooltip = "text")

#Same thing, but By Count

developer_plot_data_count <- queue_data %>%
  filter(!is.na(developer), developer != "N/A") %>%
  group_by(developer) %>%
  summarise(
    project_count = n(),
    total_mw = sum(total_mw, na.rm = TRUE)
  ) %>%
  arrange(desc(project_count)) %>%
  slice_head(n = 20) %>%
  mutate(developer = reorder(developer, project_count))

q <- ggplot(developer_plot_data_count, aes(x = reorder(developer, project_count), y = project_count, 
                                     text = paste("Developer:", developer,
                                                  "<br>Total MW:", total_mw,
                                                  "<br>Projects:", project_count))) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(
    title = "Top 20 Developers by Total Projects in Queue",
    x = "Developer",
    y = "Project Count"
  ) +
  theme_minimal()

# Make interactive
ggplotly(q, tooltip = "text")
