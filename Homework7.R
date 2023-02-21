library(tidyverse)
library(spotifyr)
library("ggplot2")
install.packages("ggthemes")                     # Install ggthemes package
library("ggthemes")

coachella2023 <- get_playlist_audio_features("", "2HfbF1Xx4RHhJI8jNvQAQb")

coachella20232 <- coachella2023 %>%                    # Start with awards.
  mutate(
    mode = ifelse(mode == 0, "Minor", "Major")
  )

ggplot(coachella20232, aes(x = valence, y = energy, size = danceability, color = mode)) +
  geom_point(position = "jitter", alpha = 0.4) +
  labs(x = "Valence", y = "Energy", color = "Mode", size = "Danceability", title = "Coachella 2023") +
  theme_fivethirtyeight()

coachella2022 <- get_playlist_audio_features("", "2tcwxeUSNT7gYMzidaf5W1")

coachella20222 <- coachella2022 %>%                    # Start with awards.
  mutate(
    mode = ifelse(mode == 0, "Minor", "Major")
  )

ggplot(coachella20222, aes(x = valence, y = energy, size = danceability, color = mode)) +
  geom_point(position = "jitter", alpha = 0.4) +
  labs(x = "Valence", y = "Energy", color = "Mode", size = "Danceability", title = "Coachella 2022") +
  theme_fivethirtyeight()



