# Create Sample Communication Research Datasets for Jumpstart 2025
# Run this code to generate the CSV files for your class

library(tidyverse)
library(here)

# Create data directory if it doesn't exist
if (!dir.exists(here("data"))) {
  dir.create(here("data"))
}

# Set seed for reproducible data
set.seed(2025)

# Dataset 1: Media Trust Survey (for Part 2 examples)
# Simulates a survey about trust in different news sources
# Creates realistic correlation between age and trust

n_participants <- 250

media_trust_survey <- tibble(
  participant_id = paste0("P", str_pad(1:n_participants, 3, pad = "0")),
  age = sample(18:75, n_participants, replace = TRUE),
  gender = sample(c("Male", "Female", "Non-binary", "Prefer not to say"), 
                  n_participants, replace = TRUE, prob = c(0.45, 0.45, 0.05, 0.05)),
  education = sample(c("High School", "Some College", "Bachelor's", "Master's", "PhD"), 
                     n_participants, replace = TRUE, prob = c(0.15, 0.25, 0.35, 0.20, 0.05)),
  news_source = sample(c("Television", "Newspapers", "Facebook", "Instagram", 
                         "Twitter", "YouTube", "Podcasts", "Online News Sites"), 
                       n_participants, replace = TRUE),
  daily_news_minutes = round(runif(n_participants, min = 5, max = 180)),
  political_interest = sample(1:7, n_participants, replace = TRUE, prob = c(0.05, 0.08, 0.12, 0.25, 0.25, 0.15, 0.10)),
  income_bracket = sample(c("Under $25k", "$25k-$50k", "$50k-$75k", "$75k-$100k", "Over $100k"), 
                          n_participants, replace = TRUE)
) %>%
  # Create trust_score that correlates with age (older = higher trust)
  # Using a moderate positive correlation (r ≈ 0.35)
  mutate(
    # Base trust score influenced by age
    trust_score_base = 2.5 + (age - 18) * 0.04 + rnorm(n_participants, 0, 1.2),
    trust_score = round(pmax(1, pmin(7, trust_score_base)), 1),
    
    # Credibility rating also slightly correlated with trust score
    credibility_rating_base = trust_score + rnorm(n_participants, 0.5, 1.0),
    credibility_rating = round(pmax(1, pmin(7, credibility_rating_base)), 1)
  ) %>%
  # Remove intermediate variables
  select(-trust_score_base, -credibility_rating_base)

# Write to CSV
write_csv(media_trust_survey, here("data", "media_trust_survey.csv"))

# Dataset 2: Social Media Engagement (for practice exercises)
# Simulates data about social media usage and political engagement

n_users <- 300

social_media_engagement <- tibble(
  user_id = paste0("U", str_pad(1:n_users, 3, pad = "0")),
  age = sample(16:65, n_users, replace = TRUE),
  platform = sample(c("Facebook", "Instagram", "Twitter", "TikTok", "YouTube", "LinkedIn"), 
                    n_users, replace = TRUE, prob = c(0.25, 0.25, 0.15, 0.20, 0.10, 0.05)),
  daily_hours = round(runif(n_users, min = 0.1, max = 8), 1),
  posts_per_week = rpois(n_users, lambda = 4),
  engagement_score = round(rnorm(n_users, mean = 65, sd = 20), 1),
  follower_count = round(10^runif(n_users, min = 1, max = 4.5)),
  political_posts_week = rpois(n_users, lambda = 1.5),
  news_sharing_frequency = sample(1:5, n_users, replace = TRUE, prob = c(0.3, 0.25, 0.25, 0.15, 0.05)),
  platform_satisfaction = sample(1:7, n_users, replace = TRUE, prob = c(0.05, 0.10, 0.15, 0.25, 0.25, 0.15, 0.05))
) %>%
  # Ensure engagement_score is realistic
  mutate(
    engagement_score = pmax(0, pmin(100, engagement_score))
  )

# Write to CSV
write_csv(social_media_engagement, here("data", "social_media_engagement.csv"))

# Dataset 3: Communication Survey (smaller dataset for basic examples)
# Simple dataset for initial demonstrations

n_basic <- 50

communication_survey <- tibble(
  id = paste0("P", str_pad(1:n_basic, 3, pad = "0")),
  age = sample(18:35, n_basic, replace = TRUE),
  media_trust = round(runif(n_basic, min = 1, max = 7), 1),
  platform = sample(c("Instagram", "Facebook", "TikTok", "Twitter"), n_basic, replace = TRUE),
  daily_use_hours = round(runif(n_basic, min = 0.5, max = 6), 1)
)

# Write to CSV
write_csv(communication_survey, here("data", "communication_survey.csv"))

# Dataset 4: News Consumption Patterns (for advanced exercises)
# More complex dataset with multiple measurements per participant

participants <- rep(paste0("P", str_pad(1:75, 3, pad = "0")), each = 4)
platforms <- rep(c("TV News", "Social Media", "Print Media", "Podcasts"), times = 75)

news_consumption <- tibble(
  participant_id = participants,
  platform_type = platforms,
  weekly_hours = round(runif(length(participants), min = 0, max = 15), 1),
  trust_level = sample(1:7, length(participants), replace = TRUE),
  age_group = rep(sample(c("18-29", "30-44", "45-59", "60+"), 75, replace = TRUE), each = 4),
  education_level = rep(sample(c("High School", "College", "Graduate"), 75, replace = TRUE), each = 4)
)

# Write to CSV
write_csv(news_consumption, here("data", "news_consumption_patterns.csv"))

# Print confirmation and basic info about each dataset
cat("✅ Sample datasets created successfully!\n\n")

cat("📊 Dataset Summary:\n")
cat("1. media_trust_survey.csv:", nrow(media_trust_survey), "participants,", ncol(media_trust_survey), "variables\n")
cat("2. social_media_engagement.csv:", nrow(social_media_engagement), "users,", ncol(social_media_engagement), "variables\n") 
cat("3. communication_survey.csv:", nrow(communication_survey), "participants,", ncol(communication_survey), "variables\n")
cat("4. news_consumption_patterns.csv:", nrow(news_consumption), "observations,", ncol(news_consumption), "variables\n\n")

cat("🗂️ All files saved to: data/\n")
cat("📝 Ready to use in your Jumpstart session!")