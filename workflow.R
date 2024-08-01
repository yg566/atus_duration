library(dplyr)
library(ggplot2)
library(stringr)



atuscps_2022 <- read.csv("~/chds/atus_duration/atusact_2023.dat")

atusresp_2022 <- read.csv("~/chds/atus_duration/atusresp_2023.dat")

atusrost_2022 <- read.csv("~/chds/atus_duration/atusrost_2023.dat")

atussum_2022 <- read.csv("~/chds/atus_duration/atussum_2023.dat")

atusact_2022 <- read.csv("~/chds/atus_duration/atusact_2023.dat")



atuscps_state_info <- atuscps_2022 %>%
  select(TUCASEID, GEDIV, GEREG, GESTFIPS, GTCO, GTMETSTA, PTDTRACE) %>%
  distinct(TUCASEID, .keep_all = TRUE)

atussum_2022 <- atussum_2022 %>%
  select(TUCASEID, TUFINLWGT, TEAGE, TESEX, TELFS, TESCHENR, TRERNWA, TUDIARYDAY, TRHOLIDAY)

atusact_2022 <- atusact_2022 %>%
  select(TUCASEID, TEWHERE, TUSTARTTIM, TUSTOPTIME, TUTIER1CODE)

merged_data <- atusresp_2022 %>%
  left_join(atuscps_state_info, by = "TUCASEID")

gestfips_counts <- merged_data %>%
  count(GESTFIPS, name = "count") %>%
  arrange(desc(count))

atusact_2022 <- atusact_2022 %>%
  mutate(TUTIER1CODE = case_when(
    TUTIER1CODE == 1 ~ "1",
    TUTIER1CODE == 2 ~ "1",
    TUTIER1CODE == 5 ~ "2",
    TUTIER1CODE == 7 ~ "3",
    TUTIER1CODE == 11 ~ "4",
    TUTIER1CODE == 12 ~ "5",
    TUTIER1CODE == 18 ~ "18",
    TUTIER1CODE == 16 ~ "16",
    TRUE ~ "6"  # Combine all other groups into "6"
  ))


atusact_2022 <- atusact_2022 %>%
  mutate(
    group = cumsum(TUTIER1CODE != lag(TUTIER1CODE, default = first(TUTIER1CODE)))
  ) %>%
  group_by(TUCASEID, group) %>%
  summarize(
    TUSTARTTIM = first(TUSTARTTIM),
    TUSTOPTIME = last(TUSTOPTIME),
    TUTIER1CODE = first(TUTIER1CODE), # Add the activity code back
    .groups = 'drop'
  ) %>%
  select(TUCASEID, TUSTARTTIM, TUSTOPTIME, TUTIER1CODE)


# Convert start and end times to POSIXct
atusact_2022$TUSTARTTIM <- as.POSIXct(atusact_2022$TUSTARTTIM, format = "%H:%M:%S")
atusact_2022$TUSTOPTIME <- as.POSIXct(atusact_2022$TUSTOPTIME, format = "%H:%M:%S")

# Calculate duration in minutes for each row
atusact_2022$DurationMinutes <- apply(atusact_2022, 1, function(row) {
  duration <- as.numeric(difftime(row["TUSTOPTIME"], row["TUSTARTTIM"], units = "mins"))
  if (duration < 0) {
    duration <- duration + 1440
  }
  return(duration)
})

atusact_2022 <- atusact_2022 %>%
  mutate(
    TUSTARTTIM = format(as.POSIXct(TUSTARTTIM, format = "%H:%M:%S"), format = "%H:%M:%S"),
    TUSTOPTIME = format(as.POSIXct(TUSTOPTIME, format = "%H:%M:%S"), format = "%H:%M:%S")
  )

merged_data <- atusact_2022 %>%
  right_join(atussum_2022, by = "TUCASEID")


final_table <- merged_data %>% 
  left_join(atuscps_state_info, by = "TUCASEID")

activity_counts <- atusact_2022 %>%
  filter(TUTIER1CODE != 18) %>% 
  filter(TUTIER1CODE != 16) %>% 
  group_by(TUTIER1CODE) %>%
  summarize(activity_count = n()) %>%
  ungroup()

sex_count <- final_table %>%
  distinct(TUCASEID, .keep_all = TRUE) %>%
  group_by(TESEX) %>%
  summarize(sex_count = n()) %>%
  ungroup() #1 is male, 2 is female

age_count <- final_table %>%
  distinct(TUCASEID, .keep_all = TRUE) %>%
  group_by(age_group) %>%
  summarize(age_count = n()) %>%
   ungroup() #    TEAGE >= 10 & TEAGE <= 25 ~ "1",
# TEAGE >  25 & TEAGE <= 41 ~ "2",
# TEAGE >  41 & TEAGE <= 57 ~ "3",
# TEAGE >  57 & TEAGE <= 76 ~ "4",
# TEAGE >  76 & TEAGE <= 94 ~ "5",


labor_count <- final_table %>%
  distinct(TUCASEID, .keep_all = TRUE) %>%
  group_by(TELFS) %>%
  summarize(labor_count = n()) %>%
  ungroup() # 1 is employeed, 2 is umployeed, 3 is not in labor force

education_count <- final_table %>%
  distinct(TUCASEID, .keep_all = TRUE) %>%
  group_by(TESCHENR) %>%
  summarize(education_count = n()) %>%
  ungroup() # -1 is not in the age range, 1 is enroll in at least high school, 2 is not

race_count <- final_table %>%
  distinct(TUCASEID, .keep_all = TRUE) %>%
  group_by(PTDTRACE) %>%
  summarize(labor_count = n()) %>%
  ungroup() # 1 is white, 2 is non-white

final_table$TRERNWA # it's a

weekday_count <- final_table %>%
  distinct(TUCASEID, .keep_all = TRUE) %>%
  group_by(TUDIARYDAY) %>%
  summarize(education_count = n()) %>%
  ungroup()

holiday_count <- final_table %>%
  distinct(TUCASEID, .keep_all = TRUE) %>%
  group_by(TRHOLIDAY) %>%
  summarize(education_count = n()) %>%
  ungroup()


div_count <- final_table %>%
  distinct(TUCASEID, .keep_all = TRUE) %>%
  group_by(GEDIV) %>%
  summarize(education_count = n()) %>%
  ungroup()

earnings_stats <- final_table %>%
  distinct(TUCASEID, .keep_all = TRUE) %>%
  filter(TRERNWA != -1) %>%
  summarize(
    mean_earnings = mean(TRERNWA, na.rm = TRUE),
    median_earnings = median(TRERNWA, na.rm = TRUE),
    sd_earnings = sd(TRERNWA, na.rm = TRUE),
    min_earnings = min(TRERNWA, na.rm = TRUE),
    max_earnings = max(TRERNWA, na.rm = TRUE),
    count = n()
  )


# final_table %>%
#   distinct(TUCASEID, .keep_all = TRUE) %>%
#   filter(TEAGE > 49) %>%
#   summarize(age_count_over_59 = n())

earnings_stats <- final_table %>%
  summarize(
    mean_earnings = mean(DurationMinutes, na.rm = TRUE),
    median_earnings = median(DurationMinutes, na.rm = TRUE),
    sd_earnings = sd(DurationMinutes, na.rm = TRUE),
    min_earnings = min(DurationMinutes, na.rm = TRUE),
    max_earnings = max(DurationMinutes, na.rm = TRUE),
    count = n()
  )

test <- final_table

# test <- test %>%
#   mutate(DurationGroup = cut(DurationMinutes, 
#                              breaks = seq(0, 1440, by = 10), 
#                              right = FALSE, 
#                              labels = paste(seq(0, 1430, by = 10), 
#                                             seq(10, 1440, by = 10), 
#                                             sep = "-")))
# DurationGroup <- test %>%
#   group_by(DurationGroup) %>%
#   summarize(count = n()) %>%
#   ungroup()

# breaks <- c(0, 300, 600, 900, 1200, 1440)
# labels <- c("1", "2", "3", "4", "5")
# 
# test <- test %>%
#   mutate(DurationGroup = cut(DurationMinutes, 
#                              breaks = breaks, 
#                              right = FALSE, 
#                              labels = labels))
# 
# DurationGroup <- test %>%
#   group_by(DurationGroup) %>%
#   summarize(count = n()) %>%
#   ungroup()
