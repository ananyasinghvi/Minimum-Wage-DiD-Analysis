
##################################################
# ECON 418-518 Exam 3
# Ananya Singhvi
# The University of Arizona
# ananyasinghvi@arizona.edu 
# 15 December 2024
###################################################


#####################
# Preliminaries
#####################

# Clear environment, plot pane, and console
rm(list = ls())
graphics.off()
cat("\014")

# Install pacman
if (!require(pacman)) install.packages("pacman")

# Load packages
pacman::p_load(data.table, plm, ggplot2, gridExtra, readxl)

# Set seed for reproducibility
set.seed(418518)

# Disable scientific notation
options(scipen = 999)

#loading dataset
dt <- data.table(read.csv("/Users/hps/Desktop/Econometrics/Exam3.csv"))

#################
# Part (ii)
#################

#Create indicator variable for Post, if time_period is November. then 1, otherwise 0.
dt[, Post := ifelse(time_period == "Nov", 1, 0)]

#Create indicator variable for New Jersey, if state is 1. then 1, otherwise 0.
dt[, NewJersey := ifelse(state == 1, 1, 0)]

#Mean total employment in New Jersey in November.
mean_emp_newjersey_november <- dt[Post == 1 & state == 1, mean(total_emp)]
print(mean_emp_newjersey_november)

#Mean total employment in New Jersey in February.
mean_emp_newjersey_february <- dt[Post == 0 & state == 1, mean(total_emp)]
print(mean_emp_newjersey_february)

#Mean total employment in Pennsylvania in November.
mean_emp_pennsylvania_november <- dt[Post == 1 & state == 0, mean(total_emp)]
print(mean_emp_pennsylvania_november)

#Mean total employment in Pennsylvania in February.
mean_emp_pennsylvania_februray <- dt[Post == 0 & state == 0, mean(total_emp)]
print(mean_emp_pennsylvania_februray)


#################
# Part (iii)
#################

# DiD estimate 
DiD <- (mean_emp_newjersey_november - mean_emp_newjersey_february) - (mean_emp_pennsylvania_november - mean_emp_pennsylvania_februray)

# Show DiD estimate
DiD

# Visualizing the effect

# Simulate data to mimic DiD outcomes
did_data <- data.frame(
  Group = rep(c("Treatment (New Jersey)", "Control (Pennsylvania)"), each = 2),
  Time = rep(c("Pre-intervention", "Post-intervention"), 2),
  Outcome = c(20.43, 25.90, 23.38, 21.10)
)

# Reorder Time to ensure Pre-intervention appears first
did_data$Time <- factor(did_data$Time, levels = c("Pre-intervention", "Post-intervention"))

# Counterfactual value for New Jersey post-intervention assuming parallel line trend.
counterfactual_nj_post <- 20.43 + (21.10 - 23.38) 

# Plot the DiD graph
ggplot(did_data, aes(x = Time, y = Outcome, group = Group, color = Group)) +

# Observed lines
geom_line(linewidth = 1.2) +        
geom_point(size = 3) +             
  
# Counterfactual line for New Jersey
geom_segment(aes(x = "Pre-intervention", xend = "Post-intervention",
                   y = 20.43, yend = counterfactual_nj_post), 
               linetype = "dotted", color = "red", size = 1) +
  
# Add intervention effect line
geom_segment(aes(x = "Post-intervention", xend = "Post-intervention",
                   y = counterfactual_nj_post, yend = 25.90), 
               linetype = "dashed", color = "black", size = 1) +
  
# Annotations
annotate("text", x = 2, y = 24, label = "Intervention Effect", size = 4, hjust = 0) +
annotate("text", x = 1.5, y = 21.5, label = "Counterfactual trend", size = 3.5, color = "red", hjust = 0) +
  
# Labels and formatting
ggtitle("Difference-in-Differences (DiD) with Counterfactual") +
xlab("Time") +
ylab("Outcome (Employment)") +
theme_minimal() +
scale_color_manual(values = c("red", "green")) +
theme(plot.title = element_text(hjust = 0.5, size = 16))


#################
# Part (iv)
#################

# Estimate model using lm
did_model <- lm(total_emp ~ state + Post + I(state*Post), data = dt)

# Summary of regression
summary(did_model)

# Show the 95% confidence interval for each parameter
confint(did_model, level = 0.95)

#################
# Part (vii)
#################
# Convert restaurant ID to a factor for fixed effects
dt$restaurant_id <- as.factor(dt$restaurant_id)

# Run the DiD model with restaurant fixed effects
did_fixed_effects_model <- lm(total_emp ~ state + Post + I (state*Post) + restaurant_id, data = dt)

# Display the summary
summary(did_fixed_effects_model)

