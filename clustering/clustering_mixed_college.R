# Clustering mixed-type data

# sources:
# https://dpmartin42.github.io/posts/r/cluster-mixed-types
# https://stats.stackexchange.com/questions/55798/what-is-the-optimal-distance-function-for-individuals-when-attributes-are-nomina/55802#55802


# Load packages
library(cluster)
library(dplyr)
library(ggplot2)
library(Rtsne)
library(ISLR)

set.seed(1234)
################################################################################
# Load data
data("College")
force(College)


# data description
help(data(College))

# exploration
summary(College)
head(College)

#
df <- College

################################################################################
# Data processing

# Creating new variable 'name' (college name) based on row names
df <- df %>% mutate(college_name = as.factor(row.names(.))) 

# Creating a new variable 'aceptance_rate' = 'Accept'/'Apps'
df <- df %>% mutate(acceptance_rate = Accept/Apps) 

# Creating a new variable 'isElite -->  Elite: more than 50% of students were in the top 10% of their high school class, Not Elite otherwise
df <- df %>% mutate(isElite = cut(Top10perc, breaks = c(0, 50, 100), labels = c("Not Elite", "Elite"), include.lowest =TRUE))

class(df$isElite)

# selecting variables
df <- df %>% select(college_name, acceptance_rate, Outstate, Enroll, Grad.Rate, Private, isElite)

#
head(df)
glimpse(df)

################################################################################
### Partioning around medoids (PAM)
################################################################################


# Compute Gower distance
gower_dist <- daisy(df, metric = "gower")

# gower_dist <- daisy(college_clean[, -1], metric = "gower", type = list(logratio = 3))

summary(gower_dist) # Metric :  mixed ;  Types = I: interval, N: nominal 

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

k <- 3
pam_fit <- pam(gower_dist, diss = TRUE, k)

pam_results <- df %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

pam_results$the_summary


# medoids of each cluster

df[pam_fit$medoids, ]


# add cluster number
df <- df %>% mutate(cluster = pam_fit$clustering)

################################################################################

################################################################################
# Visualization

tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)


tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering), college_name = df$college_name)


ggplot(aes(x = X, y = Y), data = tsne_data) + geom_point(aes(color = cluster)) + ggtitle(paste("Number of clusters (k):", k))


# Interpretation

tsne_data %>% filter(X > -10 & X < 0,
         Y > -30 & Y < -20) %>% left_join(df, by = "college_name") %>% collect %>%.[["college_name"]]


