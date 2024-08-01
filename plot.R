state_codes <- data.frame(
  GESTFIPS = c(1, 2, 4, 5, 6, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56),
  State = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
)

gestfips_counts <- merge(gestfips_counts, state_codes, by = "GESTFIPS")

gestfips_counts = gestfips_counts %>%
  top_n(15, count)


barplot = ggplot(gestfips_counts, aes(x = reorder(State, -count), y = count)) +
  geom_bar(stat = "identity", fill = "#1F77B4", alpha = 0.8) +
  labs(x = "State", y = "Count") +
  scale_y_continuous(breaks = seq(0, max(gestfips_counts$count), by = 100)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.y = element_line(color = "gray90", linetype = "dashed"),
    panel.grid.minor.y = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  ) +
  ggtitle("Count of Participants in Different States")


test_filtered <- test %>%
  filter(!is.na(DurationGroup))

barplot <- ggplot(test_filtered, aes(x = DurationGroup)) +
  geom_bar(fill = "#1F77B4", alpha = 0.8) +
  labs(x = "Duration Group", y = "Frequency") +
  scale_x_discrete(labels = c("0-10", "10-15", "15-20", "20-30", "30-40",
                              "40-60", "60-80", "80-120", "120-180", "180-1430")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.y = element_line(color = "gray90", linetype = "dashed"),
    panel.grid.minor.y = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  ) +
  ggtitle("Distribution of Activity Durations")


ggsave("duration.png", barplot, width = 10, height = 6, dpi = 800)



durationminutes

working = final_table %>% 
  filter(TELFS == 5) %>% 
  filter(TUTIER1CODE == 2)

nonworking = final_table %>% 
  filter(TELFS == 4) %>% 
  filter(TUTIER1CODE == 2)

# Combine working and nonworking data frames
combined_data <- rbind(
  working %>% mutate(group = "Working"),
  nonworking %>% mutate(group = "Nonworking")
)

# Create the distribution plot
ggplot(combined_data, aes(x = DurationMinutes, fill = group)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(
    title = "Distribution of Duration Minutes",
    x = "Duration (minutes)",
    y = "Density",
    fill = "Group"
  ) +
  theme_minimal()


library(ggplot2)
library(gridExtra)

# Create data frames with labels for each variable
sex_labels <- data.frame(
  TESEX = c("1", "2"),
  sex_label = c("Male", "Female")
)

age_labels <- data.frame(
  age_group = c("1", "2", "3", "4", "5"),
  age_label = c("10-25", "26-41", "42-57", "58-76", "77-94")
)

labor_labels <- data.frame(
  TELFS = c("1", "2", "3"),
  labor_label = c("Employed", "Unemployed", "Not in Labor Force")
)

education_labels <- data.frame(
  TESCHENR = c("-1", "1", "2"),
  education_label = c("Age < 15, Age > 49", "At Least High School", "Not Enrolled")
)

race_labels <- data.frame(
  PTDTRACE = c("1", "2"),
  race_label = c("White", "Non-White")
)

# Merge the labels with the count data
sex_count_labeled <- merge(sex_count, sex_labels, by = "TESEX")
age_count_labeled <- merge(age_count, age_labels, by = "age_group")
labor_count_labeled <- merge(labor_count, labor_labels, by = "TELFS")
education_count_labeled <- merge(education_count, education_labels, by = "TESCHENR")
race_count_labeled <- merge(race_count, race_labels, by = "PTDTRACE")

# Create individual plots
plot_sex <- ggplot(sex_count_labeled, aes(x = sex_label, y = sex_count, fill = sex_label)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  labs(x = "Sex", y = "Count") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "none"
  ) 

ggsave("sex.png", plot_sex, width = 10, height = 6, dpi = 800)


plot_age <- ggplot(age_count_labeled, aes(x = age_label, y = age_count, fill = age_label)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  labs(x = "Age Group", y = "Count") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) 

ggsave("age.png", plot_age, width = 10, height = 6, dpi = 800)


plot_labor <- ggplot(labor_count_labeled, aes(x = labor_label, y = labor_count, fill = labor_label)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  labs(x = "Labor Status", y = "Count") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) 

ggsave("labor.png", plot_labor, width = 10, height = 6, dpi = 800)


plot_education <- ggplot(education_count_labeled, aes(x = education_label, y = education_count, fill = education_label)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  labs(x = "School Enrollment", y = "Count") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) 

ggsave("education.png", plot_education, width = 10, height = 6, dpi = 800)

plot_race <- ggplot(race_count_labeled, aes(x = race_label, y = labor_count, fill = race_label)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  labs(x = "Race", y = "Count") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "none"
  ) 

ggsave("race.png", plot_race, width = 10, height = 6, dpi = 800)


income = final_table %>% 
  distinct(TUCASEID, .keep_all = TRUE) %>%
  filter(TRERNWA != 0) %>% 
  filter(TRERNWA != -1) %>% 
  mutate(TRERNWA = TRERNWA/100)


plot_income <- ggplot(income, aes(x = TRERNWA)) +
  geom_histogram(bins = 30, fill = "#1F77B4", alpha = 0.8) +
  labs(x = "Weekly Income ($)", y = "Count") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10)
  ) 

ggsave("income.png", plot_income, width = 10, height = 6, dpi = 800)


arranged_plots <- grid.arrange(
  plot_sex, plot_age, plot_labor, plot_education, plot_race, plot_income,
  nrow = 2, ncol = 3
)

