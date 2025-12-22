# Required Libraries ------------------------------------------------------
library(readr)
library(stringr)
library(tidyverse)
library(tidytext)
library(ggplot2)
library(gridExtra)
library(scales)

# 1. LOAD THE DATASETS ----------------------------------------------------
# Update paths if necessary based on your folder structure
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

## D. Prepare Release Year (Only Songs Released 2000 Onwards) ------------
release_year_2000 <- tracks_raw %>%
  mutate(release_year = as.numeric(str_sub(release_date, 1, 4))) %>%
  filter(release_year >= 2000) %>%
  select(song_id, release_year)

songs_2000 <- songs_raw %>%
  inner_join(release_year_2000, by = "song_id")

# 3. DEFINE HIP-HOP ARTISTS & SONGS -------------------------
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

# Filter Songs (Match songs to Hip-Hop Artists)
song_hiphop_artist_match <- songs_2000 %>%
  select(song_id, artists) %>%
  mutate(artist_id_extracted = str_extract_all(artists, "'[a-zA-Z0-9]{22}':")) %>%
  unnest(artist_id_extracted) %>%
  mutate(artist_id = str_remove_all(artist_id_extracted, "[':]")) %>%
  filter(artist_id %in% hiphop_artist_ids) %>%
  distinct(song_id, artist_id) # Keep artist_id for later grouping

# 4. CREATE MASTER DATASET -------------------------
hiphop_master <- songs_2000 %>%
  # inner_join duplicates rows when song contains multiple artists
  semi_join(song_hiphop_artist_match, by = "song_id") %>%
  left_join(features_raw, by = "song_id") %>%
  left_join(charts_aggregated, by = "song_id") %>%
  left_join(lyrics_clean, by = "song_id") %>%
  left_join(profanity_per_song, by = "song_id")

# A. Create a list of IDs that are definitely Hip-Hop
# We use unique() to ensure we don't have duplicates if a song has multiple rappers
hiphop_ids <- unique(song_hiphop_artist_match$song_id)

# B. Create Master Table
songs_master <- songs_2000 %>%
  # 1. CREATE GENRE COLUMN: Check if the song is in our Hip-Hop list
  mutate(
    genre_group = if_else(song_id %in% hiphop_ids, "Hip-Hop", "Other")
  ) %>%
  
  left_join(pop_aggregated, by = "song_id") %>%
  left_join(features_raw, by = "song_id") %>%
  left_join(charts_aggregated, by = "song_id") %>%
  left_join(lyrics_clean, by = "song_id") %>%
  left_join(profanity_per_song, by = "song_id") %>%
  
  # 3. Clean up Profanity Counts (Handle 0 vs NA)
  mutate(profanity_count = if_else(
    is.na(lyrics), 
    NA_real_,                       # No Lyrics = Unknown (NA)
    replace_na(profanity_count, 0)  # Lyrics exist but no match = Clean (0)
  ))

# Check the split
print(table(songs_master$genre_group))

# Charts Ideas ------------------------------------------------------------

# 1. Frequency of curse words
# 2. Song/Artist with most curse words
# 3. scatterplot (corelation: profanity vs popularity)
# 4. Acoustic features Hiphop vs other
# 5. Proportion of Hiphop songs (Side-by-side clean vs explicit)
# 6. Radar Chart for Hiphop vs other 3 top genres.
# 7. Usage of profanity over time (trendline)

# PLOT --------------------------------------------------------------------
## 1. Frequency of curse words (Bar chart) ----------
profanity_freq <- hiphop_master %>%
  select(song_id, lyrics) %>%
  unnest_tokens(word, lyrics) %>%
  filter(word %in% bad_words_list) %>%
  filter(word != "bum") %>%
  count(word, sort = TRUE) %>%
  slice_max(n, n = 20)

ggplot(profanity_freq, aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "firebrick", alpha = 0.85, width = 0.7) +
  coord_flip() +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray30", size = 10),
    axis.text.y = element_text(size = 11, face = "bold"),
    panel.grid.major.y = element_blank() # Clean horizontal lines
  ) +
  labs(
    title = "Top 20 Explicit Terms in Hip-Hop",
    subtitle = "Based on frequency count across all analysed songs",
    x = NULL,
    y = "Total Occurrences"
  ) +
  geom_text(aes(label = scales::comma(n)), hjust = -0.2, size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

## 2. Corelation: profanity vs popularity (Scatterplots) ------------------
hiphop_data <- songs_master %>% 
  filter(genre_group == "Hip-Hop") %>%
  filter(!is.na(total_success_score) & !is.na(profanity_count))

other_data <- songs_master %>% 
  filter(genre_group == "Other") %>%
  filter(!is.na(total_success_score) & !is.na(profanity_count))

cor_hiphop <- cor.test(hiphop_data$profanity_count, 
                       hiphop_data$total_success_score)

cor_other <- cor.test(other_data$profanity_count, 
                      other_data$total_success_score)

p1 <- ggplot(hiphop_data, aes(x = profanity_count, y = total_success_score)) +
  geom_jitter(alpha = 0.5, color = "midnightblue", width = 0.2) +
  geom_smooth(method = "lm", formula = 'y ~ x', color = "firebrick") +
  theme_minimal() +
  labs(
    title = "Hip-Hop: Profanity vs. Success",
    subtitle = paste0("Pearson's r = ", round(cor_hiphop$estimate, 3)),
    x = "Explicit Terms",
    y = "Popularity Score"
  )

p2 <- ggplot(other_data, aes(x = profanity_count, y = total_success_score)) +
  geom_jitter(alpha = 0.3, color = "darkgreen", width = 0.2) +
  geom_smooth(method = "lm", formula = 'y ~ x', color = "orange") +
  theme_minimal() +
  labs(
    title = "Other Genres: Profanity vs. Success",
    subtitle = paste0("Pearson's r = ", round(cor_other$estimate, 3)),
    x = "Explicit Terms",
    y = "Popularity Score"
  )

grid.arrange(p1, p2, ncol = 2)

## 3. Proportion of Hiphop songs to popular songs ------------------------------
### Donut Chart ####
# 1. Prepare Data
pop_market_share <- songs_master %>%
  filter(is_pop == TRUE) %>%
  count(genre_group) %>%
  mutate(
    percentage = n / sum(n),
    # Create nice labels
    label_text = paste0(genre_group, "\n", percent(percentage, accuracy = 0.1))
  )

# 2. Plot Donut
ggplot(pop_market_share, aes(x = 2, y = percentage, fill = genre_group)) +
  geom_bar(stat = "identity", color = "white") +
  
  # Turn Bar into Circle
  coord_polar(theta = "y", start = 0) +
  
  # Create the Hole (The "Donut" effect)
  xlim(0.5, 2.5) +
  
  # Colors (Matches your other charts)
  scale_fill_manual(values = c("Hip-Hop" = "firebrick", "Other" = "#4682B4")) +
  
  # Clean Theme (Remove axes/grid)
  theme_void() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  ) +
  
  # Add Text Labels
  geom_text(aes(label = label_text), 
            position = position_stack(vjust = 0.5), 
            color = "white", size = 5, fontface = "bold") +
  
  labs(title = "Share of Popular Music (2000–2019)")

### Stacked Bar Chart ####
ggplot(songs_master %>% filter(is_pop == TRUE), aes(x = "Popular Songs", fill = genre_group)) +
  
  # Draw the Bar
  geom_bar(position = "fill", width = 0.6) +
  
  # Add Labels inside the bar
  geom_text(aes(label = ..count..), stat = "count", position = position_fill(vjust = 0.5), 
            color = "white", fontface = "bold", size = 5) +
  
  # Formatting
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("Hip-Hop" = "firebrick", "Other" = "#4682B4")) +
  theme_minimal() +
  labs(title = "Market Share: Hip-Hop vs. Other Genres",
       y = "Percentage", x = NULL) +
  coord_flip() # Flips it to be horizontal (looks like a progress bar)

## 4. Proportion of Hiphop songs to Clean and Explicit -----------------------
explicit_composition <- songs_master %>%
  filter(!is.na(explicit)) %>%
  
  # Create readable labels from the TRUE/FALSE column
  mutate(
    content_rating = if_else(explicit == TRUE, "Explicit Content", "Clean / Non-Explicit")
  ) %>%
  count(content_rating, genre_group) %>%
  group_by(content_rating) %>%
  mutate(percentage = n / sum(n))

# 2. Create Stacked Bar Chart
ggplot(explicit_composition, aes(x = content_rating, y = percentage, fill = genre_group)) +
  
  # The Bars (Stacked)
  geom_col(position = "fill", width = 0.6) +
  
  # Add Labels inside the bars (e.g. "82%")
  geom_text(aes(label = percent(percentage, accuracy = 1)), 
            position = position_fill(vjust = 0.5), 
            color = "white", fontface = "bold", size = 5) +
  
  # Formatting
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("Hip-Hop" = "firebrick", "Other" = "#4682B4")) +
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
    subtitle = "Proportion of Hip-Hop within explicit and non-explicit tracks (Dataset: 2000–2019)",
    x = NULL,
    y = "Proportion",
    fill = "Genre"
  )
