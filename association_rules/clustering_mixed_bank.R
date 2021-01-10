# Clustering mixed-type data
# https://towardsdatascience.com/clustering-on-mixed-type-data-8bbd0a2569c3

#' Load useful packages
library(cluster)
library(dplyr)
library(ggplot2)
library(readr)
#install.packages("Rtsne")
library(Rtsne)

################################################################################

# Load data
# source: https://archive.ics.uci.edu/ml/datasets/Bank+Marketing
df <- read.csv("./data/bank.csv", sep =";", stringsAsFactors = T)  # string as factors!

head(df)

names(df)

df <- df %>% select(age, job, marital, education, default, balance, housing)

################################################################################
# Compute Gower distance
gower_dist <- daisy(df, metric = "gower")

gower_mat <- as.matrix(gower_dist)


# Print most similar clients
df[which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]), arr.ind = TRUE)[1, ], ]

# Print most dissimilar
df[which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]), arr.ind = TRUE)[1, ], ]

################################################################################
# Clustering algorithm: Partitioning around medoids (PAM)

# Selecting the number of clusters: Silhouette coefficient

sil_width <- c(NA)

for(i in 2:10){  
  pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
  sil_width[i] <- pam_fit$silinfo$avg.width  
}

plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)


# Interpretation

k <- 9
pam_fit <- pam(gower_dist, diss = TRUE, k)
pam_results <- df %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results$the_summary


# medoids of each cluster

df[pam_fit$medoids, ]


################################################################################
# Visualization





