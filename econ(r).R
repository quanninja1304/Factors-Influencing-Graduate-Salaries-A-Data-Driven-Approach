# ====================================
# DESCRIPTIVE STATISTICS SECTION (R)
# ====================================

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(corrplot)
library(GGally)

# Set your working directory and load the data
# setwd("your_directory_here")
data <- read.csv("C:/hoc ki 4/econometrics/assignment/cleaned_dat_2(csv).csv")  # Change to your actual file name

# -------------------------------------
# 1. Summary Statistics Table
# -------------------------------------
summary_table <- data %>% 
  select(Starting_Salary, High_School_GPA, University_GPA, 
         SAT_Score, University_Ranking, Internships_Completed, Networking_Score) %>% 
  summary()
print(summary_table)

# -------------------------------------
# 2. Histograms with Visual Themes
# -------------------------------------
features <- c("Starting_Salary", "High_School_GPA", "University_GPA", 
              "SAT_Score", "University_Ranking", "Internships_Completed")

for (feature in features) {
  p <- ggplot(data, aes_string(x = feature)) +
    geom_histogram(bins = 30, fill = "#69b3a2", color = "white", alpha = 0.8) +
    ggtitle(paste("Distribution of", feature)) +
    xlab(feature) +
    ylab("Count") +
    theme_minimal(base_size = 14)
  print(p)
}

# -------------------------------------
# 3. Boxplots by Gender and Field of Study
# -------------------------------------
# Gender
data$Gender <- ifelse(data$Gender_Male == 1, "Male", 
                      ifelse(data$Gender_Female == 1, "Female", "Other"))

p_gender <- ggplot(data, aes(x = Gender, y = Starting_Salary, fill = Gender)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red") +
  theme_minimal(base_size = 14) +
  labs(title = "Starting Salary by Gender", x = "Gender", y = "Starting Salary") +
  scale_fill_brewer(palette = "Pastel1")
print(p_gender)

# Field of Study
fields <- c("Field_Business", "Field_Computer_Science", "Field_Engineering", 
            "Field_Law", "Field_Mathematics", "Field_Medicine")
field_names <- gsub("Field_", "", fields)
data$Field <- apply(data[fields], 1, function(x) field_names[which.max(x)])

data$Field <- factor(data$Field, levels = unique(data$Field))
p_field <- ggplot(data, aes(x = Field, y = Starting_Salary, fill = Field)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red") +
  theme_minimal(base_size = 14) +
  labs(title = "Starting Salary by Field of Study", x = "Field of Study", y = "Starting Salary") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")
print(p_field)

# -------------------------------------
# 4. Scatter Plots with Trend Lines
# -------------------------------------
scatter_vars <- c("University_GPA", "Internships_Completed", "SAT_Score", "Networking_Score")

for (var in scatter_vars) {
  p <- ggplot(data, aes_string(x = var, y = "Starting_Salary")) +
    geom_point(alpha = 0.5, color = "#0072B2") +
    geom_smooth(method = "lm", se = FALSE, color = "darkred") +
    theme_minimal(base_size = 14) +
    labs(title = paste("Starting Salary vs", var), x = var, y = "Starting Salary")
  print(p)
}

# -------------------------------------
# 5. Correlation Heatmap with Color Gradients
# -------------------------------------
cor_vars <- data %>% select(Age, High_School_GPA, SAT_Score, University_Ranking,
                            University_GPA, Internships_Completed, Projects_Completed,
                            Certifications, Soft_Skills_Score, Networking_Score, Starting_Salary)

corr_matrix <- cor(cor_vars, use = "complete.obs")

corrplot(corr_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black", 
         col = colorRampPalette(c("#6D9EC1", "white", "#E46726"))(200))

# ====================================
# END OF DESCRIPTIVE STATISTICS SECTION
# ====================================
