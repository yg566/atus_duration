library(dplyr)
library(ggplot2)
library(stringr)


test <- test %>%
  mutate(
    TUSTARTTIM = format(as.POSIXct(TUSTARTTIM, format = "%H:%M:%S"), format = "%H:%M:%S"),
    TUSTOPTIME = format(as.POSIXct(TUSTOPTIME, format = "%H:%M:%S"), format = "%H:%M:%S")
  )


test <- test %>%
  group_by(TUCASEID) %>%
  mutate(
    previous = lag(TUTIER1CODE, default = NA),
    first_activity_is_1 = first(TUTIER1CODE) == 1
  ) %>%
  filter(!(first_activity_is_1 & row_number() == 1)) %>%
  mutate(
    previous = if_else(row_number() == 1, 1, lag(TUTIER1CODE, default = 1))
  ) %>%
  select(-first_activity_is_1) %>%
  ungroup()

test <- test %>%
  mutate(
    start_hour = as.numeric(substr(TUSTARTTIM, 1, 2)),
    .start = case_when(
      start_hour >= 6 & start_hour < 10 ~ 1,  # Between 6 a.m. and 10 a.m.
      start_hour >= 15 & start_hour < 19 ~ 1, # Between 3 p.m. and 7 p.m.
      TRUE ~ 2                             # All other times
    )
  ) %>%
  select(-start_hour)


test$DurationGroup <-  factor(test$DurationGroup)
test$age_group <-  factor(test$age_group)

test$TUTIER1CODE <-  factor(test$TUTIER1CODE)
test$TESEX <-  factor(test$TESEX)
test$TELFS <-  factor(test$TELFS)
test$TESCHENR <-  factor(test$TESCHENR)
test$TUDIARYDAY <-  factor(test$TUDIARYDAY)
test$TRHOLIDAY <-  factor(test$TRHOLIDAY)
test$GEREG <-  factor(test$GEREG)
test$GEDIV <-  factor(test$GEDIV)
test$previous <-  factor(test$previous)
test$.start <-  factor(test$.start)
test$TRERNWA <- scale(test$TRERNWA)
test$TELFS <-  factor(test$TELFS)
test$PTDTRACE <-  factor(test$PTDTRACE)

ca <- test %>% filter(GESTFIPS == 6)
tx <- test %>% filter(GESTFIPS == 48)
fl <- test %>% filter(GESTFIPS == 12)
ny <- test %>% filter(GESTFIPS == 36)
pa <- test %>% filter(GESTFIPS == 42)

weekday <- test %>% filter(TUDIARYDAY %in% c(2, 3, 4, 5, 6))

weekend <- test %>% filter(TUDIARYDAY == 1 | TUDIARYDAY == 7)


ca_weekend <- ca %>%
  filter(TUDIARYDAY == 1 | TUDIARYDAY == 7)

ca_weekday <- ca %>%
  filter(TUDIARYDAY %in% c(2, 3, 4, 5, 6))

tx_weekend <- tx %>%
  filter(TUDIARYDAY == 1 | TUDIARYDAY == 7)

tx_weekday <- tx %>%
  filter(TUDIARYDAY %in% c(2, 3, 4, 5, 6))

fl_weekend <- fl %>%
  filter(TUDIARYDAY == 1 | TUDIARYDAY == 7)

fl_weekday <- fl %>%
  filter(TUDIARYDAY %in% c(2, 3, 4, 5, 6))

ny_weekend <- ny %>%
  filter(TUDIARYDAY == 1 | TUDIARYDAY == 7)

ny_weekday <- ny %>%
  filter(TUDIARYDAY %in% c(2, 3, 4, 5, 6))

pa_weekend <- pa %>%
  filter(TUDIARYDAY == 1 | TUDIARYDAY == 7)

pa_weekday <- pa %>%
  filter(TUDIARYDAY %in% c(2, 3, 4, 5, 6))


