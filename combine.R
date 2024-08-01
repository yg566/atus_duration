options(scipen = 500)
library(dplyr)
library(ggplot2)
library(stringr)


test <- final_table

test$TUTIER1CODE <- as.integer(as.character(test$TUTIER1CODE))

test <- test %>%
  group_by(TUCASEID) %>%
  filter(!(TUTIER1CODE == 1 & (row_number() == 1 | row_number() == n()))) %>%
  ungroup()


travelling <- test %>%
  filter(TUTIER1CODE == 18)

working <- test %>% 
  filter(TUTIER1CODE == 2)


test <- test %>%
  filter(TUTIER1CODE != 18)

test <- test %>%
  filter(TUTIER1CODE != 16)

# test <- test %>%
#   filter(TUTIER1CODE != 50)

# test <- test %>%
#   mutate(TUTIER1CODE = case_when(
#     TUTIER1CODE == 1 ~ "1",
#     TUTIER1CODE == 2 ~ "2",
#     TUTIER1CODE == 3 ~ "3",
#     TUTIER1CODE == 4 ~ "4",
#     TUTIER1CODE == 5 ~ "5",
#     TRUE ~ "6"  # Combine all other groups into "5"
#   ))

# num_bins <- 8
# 
# quantiles <- quantile(ca_weekday_evening$DurationMinutes, probs = seq(0, 1, length.out = num_bins + 1), na.rm = TRUE)
# 
# labels <- 1:num_bins
# 
# 
# 
# 
breaks <- c(0, 10, 15, 20, 30, 40, 60, 80, 120, 180, 1430)
labels <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")

# test <- test %>%
#   mutate(DurationGroup = cut(DurationMinutes,
#                              breaks = quantiles,
#                              include.lowest = TRUE,
#                              labels = labels))
# 
# 
test <- test %>%
  mutate(DurationGroup = cut(DurationMinutes,
                             breaks = breaks,
                             right = FALSE,
                             labels = labels))
# 
table(test$DurationGroup)

final_table <- final_table %>%
  mutate(TELFS = case_when(
    TELFS == 1 ~ "1",
    TELFS == 2 ~ "1",
    TELFS == 3 ~ "2",
    TELFS == 4 ~ "2",
    TELFS == 5 ~ "3"
  ))

final_table <- final_table %>%
  mutate(PTDTRACE = case_when(
    PTDTRACE == 1 ~ "1",
    TRUE ~ "2"  # Combine all other groups into "5"
  ))

final_table <- final_table %>%
  mutate(age_group = case_when(
    TEAGE >= 10 & TEAGE <= 25 ~ "1",
    TEAGE >  25 & TEAGE <= 41 ~ "2",
    TEAGE >  41 & TEAGE <= 57 ~ "3",
    TEAGE >  57 & TEAGE <= 76 ~ "4",
    TEAGE >  76 & TEAGE <= 94 ~ "5",
    TRUE ~ NA_character_  # Handle any missing or out of range values
  ))

test$TUTIER1CODE <- as.integer(as.character(test$TUTIER1CODE))
test$age_group <- as.integer(as.character(test$age_group))

test$TELFS <- as.integer(as.character(test$TELFS))
test$PTDTRACE <- as.integer(as.character(test$PTDTRACE))


