
# Your Name: Jun Zou
# Course: PHB 228, Assignment 1
# Date: 2025-4-6
# Description: This script provides solutions for Homework 1, focusing on R vectorization.

# Load required packages

library(tidyverse)
library(bench)

# Set seed for reproducibility
#We set a seed so that random processes produce consistent results each time the code is run.
set.seed(123)

# Part 2: Vectorization Implementation
# 1. Function Creation
vectorized_zscore <- function(x) {
  (x-mean(x))/sd(x)
  return(NULL) # Placeholder
}

loop_zscore <- function(x) {
  n<-length(x)
  z_scores<-numeric(n)  # create an empty numeric vector of length x
  x_mean<-mean(x)
  x_sd<-sd(x)
  
  for (i in seq_len(n)) {
    z_scores[i]<-(x[i]-x_mean)/x_sd
  }
  z_scores
  return(NULL) # Placeholder
}
#Test both functions on iris$Sepal.Length
# We verify they produce the same results.
z_vec<-vectorized_zscore(iris$Sepal.Length)
z_loop<-loop_zscore(iris$Sepal.Length)

# Print logical check: are they the same?
cat("Are the results identical? ",
    all.equal(z_vec,z_loop), "\n")
# 2. Performance Comparison
benchmark_results<-bench::mark(
  vectorized=vectorized_zscore(iris$Sepal.Length),
  looped=loop_zscore(iris$Sepal.Length),
  iterations=50
)

# Print the benchmarking results
print(benchmark_results)


# Create a bar plot comparing the *median* execution time 
# We extract the median execution times from benchmark_results and plot them.
benchmark_summary <- benchmark_results %>%
  select(expression, median) %>%
  # expression is a language object; convert to string for a nice label
  mutate(Function = as.character(expression),
         Median_Time = as.numeric(median))

# Make a simple bar plot of median execution times
benchmark_plot <- ggplot(benchmark_summary, aes(x = Function, y = Median_Time)) +
  geom_col(fill = "steelblue") +
  labs(
    x = "Function",
    y = "Median Execution Time(seconds)"
  ) 
# Save the plot
ggsave("zscore_benchmark.png", plot = benchmark_plot)



# 3. Applied Vectorization
# Use vectorized operations to create new columns in iris:
iris$Sepal.Length.Centered<-iris$Sepal.Length - mean(iris$Sepal.Length)
iris$Petal.Area<-iris$Petal.Length*iris$Petal.Width

#Create a new logical column: Sepal.Length.Above.Species.Avg
#indicating if a flower's sepal length is above its species' average
iris$Sepal.Length.Above.Species.Avg <- iris$Sepal.Length > ave(iris$Sepal.Length, 
                                                               iris$Species)

# explain why vectorization is generally faster in R:
# Vectorized operations in R are implemented in highly optimized internal code, which allows them to operate over entire vectors in a single step.
# Looping in R, especially using explicit for-loops, carries more overhead at the R interpreter level. Hence, vectorized code is much more faster.


# Part 3: Data Analysis with Vectorization

# 3.1 Data Preparation
#Examine the ChickWeight dataset
# Print structure and/or first rows. We'll do both here for clarity.
str(ChickWeight)
head(ChickWeight)

#Create a new column: Growth.Rate
# Growth.Rate = daily weight change, NA on first measurement for each chick
ChickWeight <- ChickWeight %>%
  group_by(Chick) %>%
  mutate(Growth.Rate = c(NA, diff(weight))) %>%
  ungroup()

# Create a new column: Max.Weight
ChickWeight <- ChickWeight %>%
  group_by(Chick) %>%
  mutate(Max.Weight = max(weight)) %>%
  ungroup()

# Create Weight.Gain.Percent:
#  percent change from the first to the last measurement for each chick
ChickWeight <- ChickWeight %>%
  group_by(Chick) %>%
  mutate(
    Weight.Gain.Percent = (dplyr::last(weight)-dplyr::first(weight)) /
      dplyr::first(weight) *100
  ) %>%
  ungroup()

# 2. Group Analysis

#Calculate summary statistics by diet group
#Average Growth Rate
#Mean Max Weight
diet_summary <- ChickWeight %>%
  group_by(Diet) %>%
  summarize(
    Avg.Growth.Rate = mean(Growth.Rate, na.rm = TRUE),
    Mean.Max.Weight = mean(Max.Weight, na.rm = TRUE)
  )

#Display the diet_summary data frame
print(diet_summary)


# 3. Visualization

# Bar plot of mean growth rate by diet group
p_growth <- ggplot(diet_summary, aes(x = Diet, y = Avg.Growth.Rate)) +
  geom_col(fill = "tomato") +
  labs(x = "Diet",y = "Mean Growth Rate(g/day)"
  )

ggsave("growth_rate_plot.png", plot = p_growth)

#Line plot: average weight over time by diet group
# First summarize average weight by diet, time
avg_weight_data <- ChickWeight %>%
  group_by(Diet, Time) %>%
  summarize(
    Avg.Weight = mean(weight, na.rm = TRUE),
    .groups = "drop"
  )

p_weight <- ggplot(avg_weight_data, aes(x = Time, y = Avg.Weight, color = Diet)) +
  geom_line(size = 1) +
  labs(
    x = "Time(Days)",y = "Average Weight(g)",color = "Diet"
  )

ggsave("avg_weight_plot.png", plot = p_weight)




# 4. Interpretation
#From the line plot, the top‐most curve demonstrates that diet 3 achieves a notably higher final weight than the others by around day20.
#The bar chart of average daily growth rates shows that diet 3 leads to the greatest overall growth rate.
#The benchmarking plot comparing “looped” versus “vectorized” z‐score calculations reveals a stark difference in performance—vectorized operations are more efficient,
#taking only a fraction of the execution time required by the looped approach.
