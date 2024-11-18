# Read data
setwd("D:/")
DataUTS=read.csv("UTS.csv", sep=",", header=TRUE)
DataUTS
# Check the packaging
nrow(DataUTS)
ncol(DataUTS)
str(DataUTS)
summary(DataUTS)
# Look at the top and bottom
head(DataUTS)
tail(DataUTS)
# Check n
dim(DataUTS)
# Validate
summary(DataUTS$TimeSpentOnCourse)
quantile(DataUTS$NumberOfVideosWatched)
# Periksa missing values
colSums(is.na(DataUTS))
# Hapus baris dengan NA jika diperlukan
DataUTS <- na.omit(DataUTS)
#Make a plot
library(ggplot2)

ggplot(DataUTS, aes(x = TimeSpentOnCourse, 
                    y = CompletionRate, 
                    color = CourseCategory, 
                    size = QuizScores)) +
  geom_point(alpha = 0.7) +
  labs(title = "Hubungan Waktu, Kategori Kursus, dan Penyelesaian Kursus",
       x = "Time Spent on Course",
       y = "Completion Rate",
       color = "Course Category",
       size = "Quiz Scores") +
  theme_minimal()
# Korelasi antar variabel numerik
correlation_matrix <- cor(DataUTS[, c("TimeSpentOnCourse", "QuizScores", "CompletionRate")])
print(correlation_matrix)

# Statistik deskriptif untuk dataset
summary(DataUTS)

# Deskripsi spesifik untuk variabel numerik
library(dplyr)
DataUTS %>%
  summarise(
    Avg_Time = mean(TimeSpentOnCourse),
    Avg_Quiz = mean(QuizScores),
    Avg_Completion = mean(CompletionRate),
    SD_Time = sd(TimeSpentOnCourse),
    SD_Quiz = sd(QuizScores),
    SD_Completion = sd(CompletionRate)
  )

# Model regresi linear awal
model <- lm(CompletionRate ~ CourseCategory + TimeSpentOnCourse + QuizScores, data = DataUTS)

# Ringkasan model
summary(model)

library(ggplot2)

# Simulasi data normal
normal_data <- rnorm(n = nrow(DataUTS), 
                     mean = mean(DataUTS$CompletionRate, na.rm = TRUE), 
                     sd = sd(DataUTS$CompletionRate, na.rm = TRUE))

# Membuat dataframe untuk normal_data agar bisa digabungkan
normal_data_df <- data.frame(CompletionRate = normal_data, Type = "Normal")

# Gabungkan data asli (CompletionRate) dan simulasi data normal
combined_data <- rbind(
  data.frame(CompletionRate = DataUTS$CompletionRate, Type = "Actual"),
  normal_data_df
)

# Plot histogram gabungan
ggplot(combined_data, aes(x = CompletionRate, fill = Type)) +
  geom_histogram(bins = 20, alpha = 0.7, position = "identity", color = "black") +
  scale_fill_manual(values = c("blue", "red")) +
  labs(title = "Perbandingan Distribusi Completion Rate dan Data Normal",
       x = "Completion Rate",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.title = element_blank())



