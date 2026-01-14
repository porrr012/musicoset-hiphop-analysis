# ==============================================================================
# CONTENT WARNING: This script contains profanity filters required for data processing.
# DO NOT read below this line if you do not wish to be exposed to profane words
# ==============================================================================

# Function to group explicit terms by root word
group_profanity_words <- function(data) {
  data %>%
    mutate(word_root = case_when(
      str_detect(word, "^mother|^motha|^mutha") ~ "motherfucker",
      str_detect(word, "^fuck") ~ "fuck",
      str_detect(word, "^nigg") ~ "nigga",
      str_detect(word, "^bitch|^biat") ~ "bitch",
      str_detect(word, "^shit") ~ "shit",
      str_detect(word, "^ass") ~ "ass",
      str_detect(word, "damn") ~ "damn",
      
      # Manual mapping
      word %in% c("dick", "cock", "penis") ~ "dick/cock",
      word %in% c("tits", "titties", "boobs", "breasts") ~ "tits/boobs",
      word %in% c("hoe", "hoes", "whore") ~ "hoe/whore",
      str_detect(word, "^puss") ~ "pussy",
      TRUE ~ word
    ))
}
