# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# Types of Graphs: ggplot2 Geometries ----


library(tidyverse)
library(lubridate)
library(tidyquant)

bike_orderlines_tbl <- read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)


# 1.0 Point / Scatter Plots ----
# - Great for Continuous vs Continuous
# - Also good for Lollipop Charts (more on this in advanced plots)

# Goal: Explain relationship between order value and quantity of bikes sold

# Data Manipulation


# Scatter Plot




# 2.0 Line Plots ----
# - Great for time series

# Goal: Describe revenue by Month, expose cyclic nature

# Data Manipulation


# Line Plot




# 3.0 Bar / Column Plots ----
# - Great for categories

# Goal: Sales by Descriptive Category

# Data Manipulation


# Bar Plot






# 4.0 Histogram / Density Plots ----
# - Great for inspecting the distribution of a variable


# Goal: Unit price of bicycles
# Histogram


# Goal: Unit price of bicylce, segmenting by frame material
# Histogram


# Density




# 5.0 Box Plot / Violin Plot ----
# - Great for comparing distributions


# Goal: Unit price of models, segmenting by category 2

# Data Manipulation


# Box Plot


# Violin Plot & Jitter Plot







# 6.0 Adding Text & Labels ----

# Goal: Exposing sales over time, highlighting outlier

# Data Manipulation


# Adding text to bar chart


# Filtering labels to highlight a point





