# Setup
setwd("D:/Documents/Anteryon")
library(tidyverse)
library(readxl)
library(scales)
library(lubridate)

# Read
BG_data <- read_excel("BG_filter_yield.xls")

# General stats
BG_grpd <- BG_data %>%
  group_by(Description) %>%
  summarise(total_completed = sum(`Quantity Completed`),
            total_value = sum(`Total Value`),
            total_ordered = sum(`Quantity Ordered`),
            total_yield = total_completed/total_ordered)

ggplot(BG_grpd, aes(x = reorder(Description, -total_completed), y = total_completed)) +
  geom_col()

ggplot(BG_grpd, aes(x = reorder(Description, -total_value), y = total_value)) +
  geom_col() +
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ","))

## Can see that 75%+ of orders and value come from 1 part.

ggplot(BG_grpd, aes(x = reorder(Description, -total_completed), y = total_yield)) +
  geom_col()

## Some yields are over 100% due to weird way of filling QAD. Main part is 80%.

# Yield per year
BG_grpd_year <- BG_data %>%
  group_by(Description, year_order = lubridate::floor_date(`Release Date`, "year")) %>%
  summarise(total_completed = sum(`Quantity Completed`),
            total_value = sum(`Total Value`),
            total_ordered = sum(`Quantity Ordered`),
            total_yield = total_completed/total_ordered) %>%
  filter(Description %in% c("BG-39 FILTER D1,75 L0,3", "BG 39 FILTER D4L1", "BMU BG39 900/D20L1"))

ggplot(BG_grpd_year, aes(x = year_order, y = total_completed, fill = reorder(Description, -total_completed))) +
  geom_col(position = "dodge")

ggplot(BG_grpd_year, aes(x = year_order, y = total_value, fill = reorder(Description, -total_value))) +
  geom_col(position = "dodge")

ggplot(BG_grpd_year, aes(x = year_order, y = total_yield, fill = reorder(Description, -total_completed))) +
  geom_col(position = "dodge")

## The three most ordered parts have yields below 100%. Some big dips here and there.

# Yield per order
BG_grpd_order <- BG_data %>%
  group_by(Description, date_order = lubridate::floor_date(`Release Date`, "month")) %>%
  summarise(total_completed = sum(`Quantity Completed`),
            total_value = sum(`Total Value`),
            total_ordered = sum(`Quantity Ordered`),
            total_yield = total_completed/total_ordered,
            t1 = total_ordered-total_completed,
            calc_yield = ifelse(t1 >= 0, total_completed/total_ordered, (total_completed+t1)/total_ordered)) %>%
  filter(Description %in% c("BG-39 FILTER D1,75 L0,3", "BG 39 FILTER D4L1", "BMU BG39 900/D20L1"))

ggplot(BG_grpd_order, aes(x = date_order, y = total_completed, col = reorder(Description, -total_completed))) +
  geom_point() +
  geom_line() 
  #geom_col(position = "dodge")

ggplot(BG_grpd_order, aes(x = date_order, y = total_yield, col = reorder(Description, -total_completed))) +
  geom_line() +
  geom_point()

## Per month yields are everywhere and very inconstant (0-1.4).

ggplot(BG_grpd_order, aes(x = date_order, y = calc_yield, col = reorder(Description, -total_completed))) +
  geom_line() +
  geom_point() ## corrected yields

BG_grpd_order %>%
  filter(t1 >= 0) %>%
  ggplot(aes(x = date_order, y = total_yield, col = reorder(Description, -total_completed))) +
    geom_line() +
    geom_point() ## without weird points
