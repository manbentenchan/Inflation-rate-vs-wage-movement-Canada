# Import data and library
getwd()
canada_average_hourly_wage = read.csv("Canada_Average_Hourly_Wage.csv")
canada_inflation_rate = read.csv("Canada_Inflation_Rate.csv")
ontario_average_hourly_wage = read.csv("Ontario_Average_Hourly_Wage.csv")
ontario_inflation_rate = read.csv("Ontario_Inflation_Rate.csv")

library(dplyr)
library(lubridate)
library(ggplot2)
library(grDevices)

# Q1.a.
# Tidy table data and keep only fields required
canada_average_hourly_wage <- canada_average_hourly_wage %>%
  filter(North.American.Industry.Classification.System..NAICS. == "Total employees, all industries") %>%
  select(REF_DATE, GEO, Wages, VALUE) %>%
  rename(TYPE = Wages)

canada_inflation_rate <- canada_inflation_rate %>%
  filter(Products.and.product.groups == "All-items") %>%
  select(REF_DATE, GEO, Products.and.product.groups, VALUE) %>%
  rename(TYPE = Products.and.product.groups) %>%
  mutate(TYPE = ifelse(TYPE == "All-items", "Inflation rate", TYPE))

# Change REF_DATE format
canada_average_hourly_wage$REF_DATE <- as.Date(paste0(canada_average_hourly_wage$REF_DATE, "-01"))
canada_inflation_rate$REF_DATE <- as.Date(paste0(canada_inflation_rate$REF_DATE, "-01"))

# Calculate the average hourly wage changes in Canada on a year-over-year basis in the last 4 years
canada_average_hourly_wage <- canada_average_hourly_wage %>%
  mutate(YEARLY_CHANGE_PERCENTAGE = ((VALUE - VALUE[match(REF_DATE %m-% months(12), REF_DATE)]) / VALUE[match(REF_DATE %m-% months(12), REF_DATE)]) * 100) %>%
  na.omit()

canada_average_hourly_wage

# Calculate the inflation rate changes in Canada on a year-over-year basis in the last 4 years
canada_inflation_rate <- canada_inflation_rate %>%
  mutate(YEARLY_CHANGE_PERCENTAGE = ((VALUE - VALUE[match(REF_DATE %m-% months(12), REF_DATE)]) / VALUE[match(REF_DATE %m-% months(12), REF_DATE)]) * 100) %>%
  na.omit()

canada_inflation_rate

# Set PNG plot size
png("plot1.png", height = 400, width = 600)

# Plot Data
plot(canada_average_hourly_wage$YEARLY_CHANGE_PERCENTAGE, type="o", xlab="Last 48 Months",ylab="Change in Percentage %", col="blue",pch=21)
lines(canada_inflation_rate$YEARLY_CHANGE_PERCENTAGE, type="o", pch=22, lty=2, col="red")
title(main="The Wage VS. Inflation Movement in Canada", font.main=4)
legend("topright", legend=c("Wage", "Inflation"), col=c("blue", "red"), pch=c(21, 22), lty=c(1,2))

# Close PNG device
dev.off()

# Q1.b.
# Tidy table data and keep only fields required
ontario_average_hourly_wage <- ontario_average_hourly_wage %>%
  filter(North.American.Industry.Classification.System..NAICS. == "Total employees, all industries") %>%
  select(REF_DATE, GEO, Wages, VALUE) %>%
  rename(TYPE = Wages)

ontario_inflation_rate <- ontario_inflation_rate %>%
  filter(Products.and.product.groups == "All-items") %>%
  select(REF_DATE, GEO, Products.and.product.groups, VALUE) %>%
  rename(TYPE = Products.and.product.groups) %>%
  mutate(TYPE = ifelse(TYPE == "All-items", "Inflation rate", TYPE))

# Change REF_DATE format
ontario_average_hourly_wage$REF_DATE <- as.Date(paste0(ontario_average_hourly_wage$REF_DATE, "-01"))
ontario_inflation_rate$REF_DATE <- as.Date(paste0(ontario_inflation_rate$REF_DATE, "-01"))

# Calculate the average hourly wage changes in Ontario on a year-over-year basis in the last 4 years
ontario_average_hourly_wage <- ontario_average_hourly_wage %>%
  mutate(YEARLY_CHANGE_PERCENTAGE = ((VALUE - VALUE[match(REF_DATE %m-% months(12), REF_DATE)]) / VALUE[match(REF_DATE %m-% months(12), REF_DATE)]) * 100) %>%
  na.omit()

ontario_average_hourly_wage

# Calculate the inflation rate changes in Ontario on a year-over-year basis in the last 4 years
ontario_inflation_rate <- ontario_inflation_rate %>%
  mutate(YEARLY_CHANGE_PERCENTAGE = ((VALUE - VALUE[match(REF_DATE %m-% months(12), REF_DATE)]) / VALUE[match(REF_DATE %m-% months(12), REF_DATE)]) * 100) %>%
  na.omit()

ontario_inflation_rate

# Set PNG plot size
png("plot2.png", height = 400, width = 600)

# Plot Data
plot(ontario_average_hourly_wage$YEARLY_CHANGE_PERCENTAGE, type="o", xlab="Last 48 Months",ylab="Change in Percentage %", col="blue",pch=21)
lines(ontario_inflation_rate$YEARLY_CHANGE_PERCENTAGE, type="o", pch=22, lty=2, col="red")
title(main="The Wage VS. Inflation Movement in Ontario", font.main=4)
legend("topright", legend=c("Wage", "Inflation"), col=c("blue", "red"), pch=c(21, 22), lty=c(1,2))

# Close PNG device
dev.off()

# Q1.c.
# Set PNG plot size
png("plot3.png", height = 400, width = 600)

# Plot Data
plot(canada_average_hourly_wage$YEARLY_CHANGE_PERCENTAGE, type="o", xlab="Last 48 Months",ylab="Change in Percentage %", col="blue", ylim = c(-2,13), pch=21)
lines(canada_inflation_rate$YEARLY_CHANGE_PERCENTAGE, type="o", pch=22, col="green")
lines(ontario_average_hourly_wage$YEARLY_CHANGE_PERCENTAGE, type="o", pch=23, col="orange")
lines(ontario_inflation_rate$YEARLY_CHANGE_PERCENTAGE, type="o", pch=24, col="red")
title(main="Ontario resident’s financial ability vs Canada’s", font.main=4)
legend("topright", 
       legend=c("Canada Wage", "Canada Inflation", "Ontario Wage", "Ontario Inflation"),
       col=c("blue", "green", "orange", "red"), 
       pch=c(21, 22, 23, 24),
       lty=c(1,1,1,1))

# Close PNG device
dev.off()
