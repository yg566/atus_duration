library(dplyr)


bin_duration <- function(dataset, num_bins) {
  # Calculate quantiles for binning
  quantiles <- quantile(dataset$DurationMinutes, probs = seq(0, 1, length.out = num_bins + 1), na.rm = TRUE)
  
  # Create labels for the bins
  labels <- 1:num_bins
  
  # Bin the DurationMinutes into DurationGroup
  dataset <- dataset %>%
    mutate(DurationGroup = cut(DurationMinutes, 
                               breaks = quantiles, 
                               include.lowest = TRUE, 
                               labels = labels))
  dataset$DurationGroup <-  factor(dataset$DurationGroup)
  
  return(dataset)
}

test <- bin_duration(test, 6)

test <- test %>%
  mutate(DurationGroup = case_when(
    DurationGroup == 1 ~ "1",
    DurationGroup == 2 ~ "2",
    DurationGroup == 3 ~ "3",
    DurationGroup == 4 ~ "3",
    DurationGroup == 5 ~ "4",
    TRUE ~ "5"  # Combine all other groups into "5"
  ))

table(ca_weekend_afternoon$DurationGroup)
