# ==============================================================================
# PROJECT: HIP-HOP/RAP AUTHENTICITY ANALYSIS (1990-2018)
# FOR VISUALISATIONS
# ==============================================================================

# Required Libraries ------------------------------------------------------
library(tidyverse)
library(tidytext)
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(grid)
library(gridExtra)
library(patchwork)
library(scales)

# 1. LOAD THE DATASETS ----------------------------------------------------
# https://marianaossilva.github.io/DSW2019/
charts_raw   <- read_delim("datasets/musicoset_popularity/song_chart.csv", delim = "\t", escape_double = FALSE, trim_ws = TRUE)
artists_raw  <- read_delim("datasets/musicoset_metadata/artists.csv", delim = "\t", escape_double = FALSE, trim_ws = TRUE)
songs_raw    <- read_delim("datasets/musicoset_metadata/songs.csv", delim = "\t", escape_double = FALSE, trim_ws = TRUE)
lyrics_raw   <- read_delim("datasets/musicoset_songfeatures/lyrics.csv", delim = "\t", escape_double = FALSE, trim_ws = TRUE)
pop_raw      <- read_delim("datasets/musicoset_popularity/song_pop.csv", delim = "\t", escape_double = FALSE, trim_ws = TRUE)
features_raw <- read_table("datasets/musicoset_songfeatures/acoustic_features.csv")
tracks_raw   <- read_table("datasets/musicoset_metadata/tracks.csv")

# https://github.com/MauriceButler/badwords
bad_words_raw <- read_file("datasets/profanity_lexicon/array.js")

# 2. DATA CLEANING & PRE-PROCESSING ---------------------------------------
## A. CLEAN LYRICS (Remove JSON artifacts & NOISE) ---------------------------
lyrics_clean <- lyrics_raw %>%
  filter(!is.na(lyrics)) %>%
  mutate(
    lyrics_text = str_remove_all(lyrics, "^\\['|'\\]$|^\\[\"|\"\\]$") %>%
      str_replace_all("\\\\n", "\n"),
    word_count = str_count(lyrics_text, "\\S+"),
    line_count = str_count(lyrics_text, "\n") + 1,
    avg_chars_per_line = nchar(lyrics_text) / line_count,
    has_tags = str_detect(lyrics_text, "\\[(Verse|Chorus|Hook|Intro)")
  ) %>%
  # Filter out "novels" (too long) or "scripts" (no structure)
  filter(
    word_count < 3000 & 
      (has_tags == TRUE | avg_chars_per_line < 80)
  ) %>%
  select(song_id, lyrics = lyrics_text)

## B. CREATE BAD WORDS LIST ------------------------------------
bad_words_list <- str_extract(bad_words_raw, "\\[.*\\]") %>%
  str_remove_all("^\\[|\\]$") %>%   # Remove brackets [ ]
  str_remove_all("[\"']") %>%       # Remove quotes " '
  str_split(",") %>%                # Split by comma
  unlist() %>%
  str_trim() %>%                    # Remove extra spaces
  str_to_lower() %>%                # Ensure lowercase for matching
  unique() %>%
  setdiff("bum")                    # False positive

profanity_per_song <- lyrics_clean %>%
  filter(!is.na(lyrics)) %>%
  select(song_id, lyrics) %>%
  unnest_tokens(word, lyrics) %>%
  filter(word %in% bad_words_list) %>%
  count(song_id, name = "profanity_count")

## C. Aggregate Data to avoid explosion ---------------------------------------
charts_aggregated <- charts_raw %>%
  group_by(song_id) %>%
  summarise(
    total_success_score = sum(rank_score, na.rm = TRUE),
    total_weeks = max(weeks_on_chart, na.rm = TRUE),
    best_peak_score = max(peak_position, na.rm = TRUE)
  )

pop_aggregated <- pop_raw %>%
  group_by(song_id) %>%
  summarise(
    is_pop = any(is_pop, na.rm = TRUE) 
  ) %>%
  ungroup()

## D. Prepare Release Year (Only Songs Released During 1990-2018) ------------
release_year_filtered <- tracks_raw %>%
  mutate(release_year = as.numeric(str_sub(release_date, 1, 4))) %>%
  filter(release_year >= 1990 & release_year <= 2018) %>%
  select(song_id, release_year)

songs_filtered <- songs_raw %>%
  inner_join(release_year_filtered, by = "song_id")

# 3. DEFINE HIP-HOP/RAP ARTISTS & SONGS -------------------------
pattern_strict  <- "\\b(rap|hip|drill|grime|urban)\\b"
pattern_loose   <- "hop|trap"

hiphop_artist_ids <- artists_raw %>%
  filter(
    str_detect(main_genre, pattern_strict) |
      str_detect(main_genre, pattern_loose) | 
      str_detect(genres, pattern_strict) | 
      str_detect(genres, pattern_loose)
  ) %>%
  pull(artist_id)

# Filter Songs (Match songs to Hip-Hop/Rap Artists)
song_hiphop_artist_match <- songs_filtered %>%
  select(song_id, artists) %>%
  mutate(artist_id_extracted = str_extract_all(artists, "'[a-zA-Z0-9]{22}':")) %>%
  unnest(artist_id_extracted) %>%
  mutate(artist_id = str_remove_all(artist_id_extracted, "[':]")) %>%
  filter(artist_id %in% hiphop_artist_ids) %>%
  distinct(song_id, artist_id) # Keep artist_id for later grouping

# Use unique() to ensure NO duplicates if a song has multiple artists
hiphop_ids <- unique(song_hiphop_artist_match$song_id)

# 4. CREATE MASTER DATASET -------------------------
songs_master <- songs_filtered %>%
  mutate(
    genre_group = if_else(song_id %in% hiphop_ids, "Hip-Hop/Rap", "Other")
  ) %>%
  left_join(pop_aggregated, by = "song_id") %>%
  left_join(charts_aggregated, by = "song_id") %>%
  left_join(features_raw, by = "song_id") %>%
  left_join(lyrics_clean, by = "song_id") %>%
  left_join(profanity_per_song, by = "song_id") %>%
  
  # Clean up Profanity Counts (Handle 0 vs NA)
  mutate(profanity_count = if_else(
    is.na(lyrics), 
    NA_real_,                       # No Lyrics = Unknown (NA)
    replace_na(profanity_count, 0)  # Lyrics exist but no match = Clean (0)
  ))

# PLOT --------------------------------------------------------------------
## (A) Temporal Trend -------------------------------------------------
# Data Prep
trend_hiphop <- songs_master %>%
  filter(!is.na(release_year) & genre_group == "Hip-Hop/Rap") %>%
  group_by(release_year) %>%
  summarise(avg_profanity = mean(profanity_count, na.rm = TRUE)) %>%
  mutate(category = "Hip-Hop/Rap")

peak_point <- trend_hiphop %>% 
  filter(avg_profanity == max(avg_profanity))

# --- Plot Chart ---
p1_trend <- 
  ggplot(trend_hiphop, aes(x = release_year, y = avg_profanity)) +
  geom_area(fill = "#E74C3C", alpha = 0.1) +
  geom_line(color = "#C0392B", size = 1) +
  geom_smooth(method = "loess", se = FALSE, color = "grey40", 
              linetype = "dashed", size = 0.7, alpha = 0.6) +
  geom_point(data = peak_point, size = 3, color = "#C0392B") +
  geom_text(data = peak_point, 
            aes(label = paste0(round(avg_profanity, 1), " words\n(", release_year, ")")),
            vjust = -0.5, hjust = 0.5, 
            size = 3.5, fontface = "bold", color = "#C0392B") +
  
  scale_x_continuous(breaks = seq(1990, 2018, by = 4)) + # Clean 4-year intervals
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(trend_hiphop$avg_profanity) * 1.2)) + # Remove gap at bottom
  
  labs(
    title = "The Rising Volume of Profanity",
    subtitle = "Average number of explicit words per Hip-Hop/Rap song (1990–2018)",
    x = NULL, # 'Year' is obvious from the numbers, simpler is better
    y = "Avg. Profanity Count"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#2c3e50"),
    plot.subtitle = element_text(size = 12, color = "grey50", margin = margin(b = 15)),
    panel.grid.minor = element_blank(),       # Remove minor gridlines (noise)
    panel.grid.major.x = element_blank(),     # Remove vertical lines (let time flow)
    axis.text = element_text(color = "grey40"),
    axis.title.y = element_text(margin = margin(r = 10), size = 11)
  )

## (B) Vocabulary -----------------------------------------
# --- 1. Data Prep: Filter, Group, THEN Dynamic Masking ---
source("visualisations/group_profanity_words.R")

profanity_freq <- songs_master %>%
  filter(genre_group == "Hip-Hop/Rap") %>% 
  select(song_id, lyrics) %>%
  unnest_tokens(word, lyrics) %>%
  filter(word %in% bad_words_list) %>%
  filter(word != "bum") %>%
  
  # A. Grouping Logic (Assign the 'Root' word, no suffixes)
  group_profanity_words() %>%
  
  # B. Dynamic Masking Step
  mutate(word_display = str_replace_all(word_root, "[aeiou]", "*")) %>%
  count(word_display, sort = TRUE) %>%
  slice_max(n, n = 10)

# --- 2. Plot Chart ---
p2_words <-
  ggplot(profanity_freq, aes(x = reorder(word_display, n), y = n)) +
  geom_col(fill = "#2C3E50", width = 0.7) + 
  geom_text(aes(label = n), hjust = -0.2, size = 3.5, color = "grey30") +
  
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) + 
 
   labs(
    title = "The Vocabulary of Explicit Content",
    subtitle = "Top 10 most frequently used profane words in Hip-Hop/Rap",
    x = NULL, 
    y = NULL 
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#2c3e50"),
    plot.subtitle = element_text(size = 12, color = "grey50", margin = margin(b = 15)),
    panel.grid.major.y = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(), 
    axis.text.x = element_blank(),        
    axis.text.y = element_text(color = "black", size = 11, face = "bold") 
  )

## (C) Genre Dominance ------------------------------------------
p3_genre <-
  songs_master %>%
  mutate(
    content_rating = if_else(explicit == TRUE, "Explicit Content", "Clean / Non-Explicit")
  ) %>%
  count(content_rating, genre_group) %>%
  group_by(content_rating) %>%
  mutate(percentage = n / sum(n)) %>% 
  
  ggplot(aes(x = content_rating, y = percentage, fill = genre_group)) +
  geom_col(position = "fill", width = 0.6) +
  geom_text(aes(label = percent(percentage, accuracy = 1)), 
            position = position_fill(vjust = 0.5), 
            color = "white", fontface = "bold", size = 5) +
  
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("Hip-Hop/Rap" = "firebrick", "Other" = "#4682B4")) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray30", size = 10),
    axis.text.x = element_text(face = "bold", size = 11),
    legend.position = "top",
    panel.grid.major.x = element_blank()
  ) +
  
  labs(
    title = "Genre Dominance: Explicit vs. Clean Music",
    subtitle = "Proportion of Hip-Hop/Rap within explicit and non-explicit tracks (Dataset: 1990-2018)",
    x = NULL,
    y = "Proportion",
    fill = "Genre"
  )

## (D) Correlation --------------------------------------------
# --- 1. Identify the Two Outliers ---

# Outlier A: The Most Explicit Song (Max Profanity)
top_profanity_outlier <- songs_master %>%
  filter(genre_group == "Hip-Hop/Rap") %>%
  slice_max(profanity_count, n = 1) # Safer than filter(x == max(x))

# Outlier B: The Most Popular Song (Max Success)
top_success_outlier <- songs_master %>% 
  filter(genre_group == "Hip-Hop/Rap") %>%
  slice_max(total_success_score, n = 1)

# --- 2. The Aesthetic Plot ---
p4_correlation <- 
  songs_master %>%
  filter(genre_group == "Hip-Hop/Rap") %>% 
  
  ggplot(aes(x = total_success_score, y = profanity_count)) +
  geom_rug(sides = "b", alpha = 0.1, color = "#34495E") +
  geom_jitter(aes(color = profanity_count), 
              alpha = 0.5, size = 1.8, width = 0.5, height = 0) +
  geom_smooth(method = "lm", color = "#2C3E50", fill = "grey80", alpha = 0.2, size = 0.8) +
  
  # 4a. Label: Most Explicit (RED)
  geom_label_repel(data = top_profanity_outlier, 
                   aes(label = paste0("Most Explicit:\n", song_name)),
                   size = 3, fontface = "bold", color = "#C0392B", 
                   box.padding = 0.5, point.padding = 0.3,
                   nudge_y = 10, nudge_x = -5) +
  
  # 4b. Label: Most Successful (GREEN/TEAL)
  geom_label_repel(data = top_success_outlier,
                   aes(label = paste0("Most Successful:\n", song_name)),
                   size = 3, fontface = "bold", color = "#117A65", # Professional Green
                   box.padding = 0.5, point.padding = 0.3,
                   nudge_y = 15, nudge_x = -10) +
  
  # 5. Annotation: Stats
  stat_cor(method = "pearson", 
           label.x.npc = "left", label.y.npc = "top",
           size = 4, fontface = "italic", color = "#2C3E50",
           p.accuracy = 0.01, r.accuracy = 0.001) +
  
  # 6. Colors & Scales
  scale_color_gradient(low = "#34495E", high = "#E74C3C", guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) + # More headroom for labels
  
  # 7. Styling
  labs(
    title = "Does Profanity Drive Commercial Success?",
    subtitle = "Correlation between commercial success and explicit content volume",
    x = "Total Success Score",
    y = "Profanity Count per Song"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#2c3e50"),
    plot.subtitle = element_text(size = 12, color = "grey50", margin = margin(b = 15)),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey92"),
    axis.title = element_text(size = 12, face = "bold", color = "#2c3e50"),
    axis.text = element_text(color = "grey40")
  )

# --- FINAL COMPOSITE ASSEMBLY ----
# Layout: 
# Top Row: Temporal Trend (A) | Genre Dominance (C)
# Bottom Row: Vocabulary (B)  | Correlation (D)
final_composite <- (p1_trend + p3_genre) / 
  (p2_words + p4_correlation) +
  
  plot_annotation(
    title = "The Normalisation of Explicit Content in Hip-Hop (1990-2018)",
    subtitle = "A multi-faceted analysis of volume, genre dominance, vocabulary, and commercial viability",
    caption = "Data Source: [Your Dataset] | Visualisation by Student [ID]",
    theme = theme(
      plot.title = element_text(size = 22, face = "bold", hjust = 0.5, color = "#2c3e50"),
      plot.subtitle = element_text(size = 14, hjust = 0.5, color = "grey50", margin = margin(b = 20))
    )
  )

print(final_composite)
