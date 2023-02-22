library(tidyverse)
library(spotifyr)
library(compmus)

wood <-
  get_tidy_audio_analysis("6IQILcYkN2S2eSu5IHoPEH") |>
  select(segments) |>
  unnest(segments) |>
  select(start, duration, pitches)

wood |>
  mutate(pitches = map(pitches, compmus_normalise, "chebyshev")) |>
  compmus_gather_chroma() |> 
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = pitch_class,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
  theme_minimal() +
  scale_fill_viridis_c()

tame <-
  get_tidy_audio_analysis("0M9ydKzuF3oZTfYYPfaGX1?si=9351d1b959304e0c") |>
  select(segments) |>
  unnest(segments) |>
  select(start, duration, pitches)

tame |>
  mutate(pitches = map(pitches, compmus_normalise, "euclidean")) |>
  compmus_gather_chroma() |> 
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = pitch_class,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude", title = "Manhattan") +
  theme_minimal() +
  scale_fill_viridis_c()