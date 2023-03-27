library(spotifyr)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(plotly)
library(compmus)

coachella2023 <- get_playlist_audio_features("", "2HfbF1Xx4RHhJI8jNvQAQb")
saveRDS(object = coachella2023,file = "data/coachella2023.RDS")
coachella2022 <- get_playlist_audio_features("", "2tcwxeUSNT7gYMzidaf5W1")
saveRDS(object = coachella2022,file = "data/coachella2022.RDS")

sixteen <- 
  get_playlist_audio_features("coachella2016", "04ULDSJpTbZRvIJ6C2UGGp")

saveRDS(object = sixteen,file = "data/sixteen.RDS")

seventeen <- 
  get_playlist_audio_features("coachella2017", "1I1aCzte1UlC9ntZRF3Bnk")

saveRDS(object = seventeen,file = "data/seventeen.RDS")

eighteen <- 
  get_playlist_audio_features("coachella2018", "68qkJBTzGvjqASInTr1J2r")

saveRDS(object = eighteen,file = "data/eighteen.RDS")

nineteen <- 
  get_playlist_audio_features("coachella2019", "5PDX3yPg2bjM74Qlo1Afdn")

saveRDS(object = nineteen,file = "data/nineteen.RDS")

twentytwo <- 
  get_playlist_audio_features("coachella2022", "2tcwxeUSNT7gYMzidaf5W1")

saveRDS(object = twentytwo,file = "data/twentytwo.RDS")

twentythree <-
  get_playlist_audio_features("coachella2023", "2HfbF1Xx4RHhJI8jNvQAQb")

saveRDS(object = twentythree,file = "data/twentythree.RDS")

beyonce_live <-
  get_tidy_audio_analysis("6oHexA34fw1WfswjIEA1hd") |>
  select(segments) |>
  unnest(segments) |>
  select(start, duration, pitches)

saveRDS(object = beyonce_live,file = "data/beyonce_live.RDS")

beyonce_livegraph <- beyonce_live |>
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
  labs(x = "Time (s)", y = NULL, fill = "Magnitude", title = "'Formation' Live, 2019") +
  theme_minimal() +
  scale_fill_viridis_c()

saveRDS(object = beyonce_livegraph,file = "data/beyonce_livegraph.RDS")

beyonce_recording <-
  get_tidy_audio_analysis("6g0Orsxv6glTJCt4cHsRsQ") |>
  select(segments) |>
  unnest(segments) |>
  select(start, duration, pitches)

saveRDS(object = beyonce_recording,file = "data/beyonce_recording.RDS")

recording_graph <- beyonce_recording |>
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
  labs(x = "Time (s)", y = NULL, fill = "Magnitude", title = "'Formation' Recording, 2016") +
  theme_minimal() +
  scale_fill_viridis_c()

saveRDS(object = recording_graph,file = "data/recording_graph.RDS")

yoncedytw <- compmus_long_distance(
  beyonce_live |> mutate(pitches = map(pitches, compmus_normalise, "manhattan")),
  beyonce_recording |> mutate(pitches = map(pitches, compmus_normalise, "manhattan")),
  feature = pitches,
  method = "manhattan"
) |>
  ggplot(
    aes(
      x = xstart + xduration / 2,
      width = xduration,
      y = ystart + yduration / 2,
      height = yduration,
      fill = d
    )
  ) +
  geom_tile() +
  coord_equal() +
  labs(x = "Live Recording", y = "Album Recording") +
  theme_minimal() +
  scale_fill_viridis_c(guide = NULL)

saveRDS(object = yoncedytw,file = "data/yoncedytw.RDS")

bts <-
  get_tidy_audio_analysis("1mWdTewIgB3gtBM3TOSFhB?si=bf0f2f60b6604097") |> # Change URI.
  compmus_align(bars, segments) |>                     # Change `bars`
  select(bars) |>                                      #   in all three
  unnest(bars) |>                                      #   of these lines.
  mutate(
    pitches =
      map(segments,
          compmus_summarise, pitches,
          method = "mean", norm = "manhattan"              # Change summary & norm.
      )
  ) |>
  mutate(
    timbre =
      map(segments,
          compmus_summarise, timbre,
          method = "rms", norm = "euclidean"              # Change summary & norm.
      )
  )

saveRDS(object = bts,file = "data/bts.RDS")

btscepstro <- bts |>
  compmus_gather_timbre() |>
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = basis,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
  scale_fill_viridis_c() +                              
  theme_classic()

saveRDS(object = btscepstro,file = "data/btscepstro.RDS")

frank <-
  get_tidy_audio_analysis("5k8LB57xOq8UUNVaKWSqrf?si=55dd0703aa284181") |> # Change URI.
  compmus_align(bars, segments) |>                     # Change `bars`
  select(bars) |>                                      #   in all three
  unnest(bars) |>                                      #   of these lines.
  mutate(
    pitches =
      map(segments,
          compmus_summarise, pitches,
          method = "rms", norm = "euclidean"              # Change summary & norm.
      )
  ) |>
  mutate(
    timbre =
      map(segments,
          compmus_summarise, timbre,
          method = "rms", norm = "euclidean"              # Change summary & norm.
      )
  )

saveRDS(object = frank,file = "data/frank.RDS")

frankcepstro <- frank |>
  compmus_gather_timbre() |>
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = basis,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
  scale_fill_viridis_c() +                              
  theme_classic()

saveRDS(object = frankcepstro,file = "data/frankcepstro.RDS")

bts_ssm <-
  get_tidy_audio_analysis("1mWdTewIgB3gtBM3TOSFhB?si=675dfcb040174a12") |>
  compmus_align(bars, segments) |>
  select(bars) |>
  unnest(bars) |>
  mutate(
    pitches =
      map(segments,
          compmus_summarise, pitches,
          method = "acentre", norm = "manhattan"
      )
  ) |>
  mutate(
    timbre =
      map(segments,
          compmus_summarise, timbre,
          method = "mean"
      )
  )
bts_ssm <- bind_rows(
  bts_ssm |>
    compmus_self_similarity(pitches, "aitchison") |>
    mutate(d = d / max(d), type = "Chroma"),
  bts_ssm |>
    compmus_self_similarity(timbre, "euclidean") |>
    mutate(d = d / max(d), type = "Timbre")
) |>
  mutate() |>
  ggplot(
    aes(
      x = xstart + xduration / 2,
      width = xduration,
      y = ystart + yduration / 2,
      height = yduration,
      fill = d
    )
  ) +
  geom_tile() +
  coord_fixed() +
  facet_wrap(~type) +
  scale_fill_viridis_c(guide = "none") +
  theme_classic() +
  labs(x = "", y = "")

bts_ssm

saveRDS(object = bts_ssm,file = "data/bts_ssm.RDS")

frank_ssm <-
  get_tidy_audio_analysis("5k8LB57xOq8UUNVaKWSqrf?si=b9097b1e2bc94fc5") |>
  compmus_align(bars, segments) |>
  select(bars) |>
  unnest(bars) |>
  mutate(
    pitches =
      map(segments,
          compmus_summarise, pitches,
          method = "rms", norm = "euclidean"
      )
  ) |>
  mutate(
    timbre =
      map(segments,
          compmus_summarise, timbre,
          method = "mean"
      )
  )
frank_ssm <- bind_rows(
  frank_ssm |>
    compmus_self_similarity(pitches, "aitchison") |>
    mutate(d = d / max(d), type = "Chroma"),
  frank_ssm |>
    compmus_self_similarity(timbre, "euclidean") |>
    mutate(d = d / max(d), type = "Timbre")
) |>
  mutate() |>
  ggplot(
    aes(
      x = xstart + xduration / 2,
      width = xduration,
      y = ystart + yduration / 2,
      height = yduration,
      fill = d
    )
  ) +
  geom_tile() +
  coord_fixed() +
  facet_wrap(~type) +
  scale_fill_viridis_c(guide = "none") +
  theme_classic() +
  labs(x = "", y = "")

saveRDS(object = frank_ssm,file = "data/frank_ssm.RDS")

circshift <- function(v, n) {
  if (n == 0) v else c(tail(v, n), head(v, -n))
}

#      C     C#    D     Eb    E     F     F#    G     Ab    A     Bb    B
major_chord <-
  c(   1,    0,    0,    0,    1,    0,    0,    1,    0,    0,    0,    0)
minor_chord <-
  c(   1,    0,    0,    1,    0,    0,    0,    1,    0,    0,    0,    0)
seventh_chord <-
  c(   1,    0,    0,    0,    1,    0,    0,    1,    0,    0,    1,    0)

major_key <-
  c(5.0, 2.0, 3.5, 2.0, 4.5, 4.0, 2.0, 4.5, 2.0, 3.5, 1.5, 4.0)
minor_key <-
  c(5.0, 2.0, 3.5, 4.5, 2.0, 4.0, 2.0, 4.5, 3.5, 2.0, 1.5, 4.0)

chord_templates <-
  tribble(
    ~name, ~template,
    "Gb:7", circshift(seventh_chord, 6),
    "Gb:maj", circshift(major_chord, 6),
    "Bb:min", circshift(minor_chord, 10),
    "Db:maj", circshift(major_chord, 1),
    "F:min", circshift(minor_chord, 5),
    "Ab:7", circshift(seventh_chord, 8),
    "Ab:maj", circshift(major_chord, 8),
    "C:min", circshift(minor_chord, 0),
    "Eb:7", circshift(seventh_chord, 3),
    "Eb:maj", circshift(major_chord, 3),
    "G:min", circshift(minor_chord, 7),
    "Bb:7", circshift(seventh_chord, 10),
    "Bb:maj", circshift(major_chord, 10),
    "D:min", circshift(minor_chord, 2),
    "F:7", circshift(seventh_chord, 5),
    "F:maj", circshift(major_chord, 5),
    "A:min", circshift(minor_chord, 9),
    "C:7", circshift(seventh_chord, 0),
    "C:maj", circshift(major_chord, 0),
    "E:min", circshift(minor_chord, 4),
    "G:7", circshift(seventh_chord, 7),
    "G:maj", circshift(major_chord, 7),
    "B:min", circshift(minor_chord, 11),
    "D:7", circshift(seventh_chord, 2),
    "D:maj", circshift(major_chord, 2),
    "F#:min", circshift(minor_chord, 6),
    "A:7", circshift(seventh_chord, 9),
    "A:maj", circshift(major_chord, 9),
    "C#:min", circshift(minor_chord, 1),
    "E:7", circshift(seventh_chord, 4),
    "E:maj", circshift(major_chord, 4),
    "G#:min", circshift(minor_chord, 8),
    "B:7", circshift(seventh_chord, 11),
    "B:maj", circshift(major_chord, 11),
    "D#:min", circshift(minor_chord, 3)
  )

key_templates <-
  tribble(
    ~name, ~template,
    "Gb:maj", circshift(major_key, 6),
    "Bb:min", circshift(minor_key, 10),
    "Db:maj", circshift(major_key, 1),
    "F:min", circshift(minor_key, 5),
    "Ab:maj", circshift(major_key, 8),
    "C:min", circshift(minor_key, 0),
    "Eb:maj", circshift(major_key, 3),
    "G:min", circshift(minor_key, 7),
    "Bb:maj", circshift(major_key, 10),
    "D:min", circshift(minor_key, 2),
    "F:maj", circshift(major_key, 5),
    "A:min", circshift(minor_key, 9),
    "C:maj", circshift(major_key, 0),
    "E:min", circshift(minor_key, 4),
    "G:maj", circshift(major_key, 7),
    "B:min", circshift(minor_key, 11),
    "D:maj", circshift(major_key, 2),
    "F#:min", circshift(minor_key, 6),
    "A:maj", circshift(major_key, 9),
    "C#:min", circshift(minor_key, 1),
    "E:maj", circshift(major_key, 4),
    "G#:min", circshift(minor_key, 8),
    "B:maj", circshift(major_key, 11),
    "D#:min", circshift(minor_key, 3)
  )

prydz <-
  get_tidy_audio_analysis("3v2oAQomhOcYCPPHafS3KV?si=5728aa7d80bc4a4a") |>
  compmus_align(sections, segments) |>
  select(sections) |>
  unnest(sections) |>
  mutate(
    pitches =
      map(segments,
          compmus_summarise, pitches,
          method = "rms", norm = "euclidean"
      )
  )

prydz_graph <- prydz |> 
  compmus_match_pitch_template(
    chord_templates,
    method = "cosine",
    norm = "euclidean"
  ) |>
  ggplot(
    aes(x = start + duration / 2, width = duration, y = name, fill = d)
  ) +
  geom_tile() +
  scale_fill_viridis_c(guide = "none") +
  theme_minimal() +
  labs(x = "Time (s)", y = "")

saveRDS(object = prydz_graph,file = "data/prydz_graph.RDS")

get_conf_mat <- function(fit) {
  outcome <- .get_tune_outcome_names(fit)
  fit |> 
    collect_predictions() |> 
    conf_mat(truth = outcome, estimate = .pred_class)
}  

get_pr <- function(fit) {
  fit |> 
    conf_mat_resampled() |> 
    group_by(Prediction) |> mutate(precision = Freq / sum(Freq)) |> 
    group_by(Truth) |> mutate(recall = Freq / sum(Freq)) |> 
    ungroup() |> filter(Prediction == Truth) |> 
    select(class = Prediction, precision, recall)
}  

headliners <-
  get_playlist_audio_features("headliners", "7uPCMiwKhG0SSHO9Ivx5FL") |>
  add_audio_analysis() |>
  mutate(
    segments = map2(segments, key, compmus_c_transpose),
    pitches =
      map(segments,
          compmus_summarise, pitches,
          method = "mean", norm = "manhattan"
      ),
    timbre =
      map(
        segments,
        compmus_summarise, timbre,
        method = "mean"
      )
  ) |>
  mutate(pitches = map(pitches, compmus_normalise, "clr")) |>
  mutate_at(vars(pitches, timbre), map, bind_rows) |>
  unnest(cols = c(pitches, timbre))

headliners_juice <-
  recipe(
    track.name ~
      danceability +
      energy +
      loudness +
      speechiness +
      acousticness +
      instrumentalness +
      liveness +
      valence +
      tempo +
      duration +
      C + `C#|Db` + D + `D#|Eb` +
      E + `F` + `F#|Gb` + G +
      `G#|Ab` + A + `A#|Bb` + B +
      c01 + c02 + c03 + c04 + c05 + c06 +
      c07 + c08 + c09 + c10 + c11 + c12,
    data = headliners
  ) |>
  step_center(all_predictors()) |>
  step_scale(all_predictors()) |> 
  # step_range(all_predictors()) |> 
  prep(headliners |> mutate(track.name = str_trunc(track.name, 50))) |>
  juice() |>
  column_to_rownames("track.name")

headliners_dist <- dist(headliners_juice, method = "euclidean")

headliners_dendro <- headliners_dist |> 
  hclust(method = "complete") |> # Try single, average, and complete.
  dendro_data() |>
  ggdendrogram(rotate = TRUE, size = 1) + 
  labs(title = "Headliners Dendrogram")

saveRDS(object = headliners_dendro,file = "data/headliners_dendro.RDS")






