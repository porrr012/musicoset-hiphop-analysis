# 🎵 Hip-Hop/Rap Songs Analysis (2000–2019)

## 📌 Project Overview
This project investigates the relationship between **explicit lyrics** and **commercial success** in Hip-Hop music during the digital era (2000–2019). Using the **Musicoset** dataset, we apply text mining and statistical regression to challenge the popular assumption that "vulgarity sells."

**Key Research Questions:**
1.  What are the most frequently used explicit terms in modern Hip-Hop?
2.  Is there a correlation between the density of profanity in a song and its chart popularity?

---

## 📊 Key Findings

* **Ubiquity of Explicit Content:** Hip-Hop accounts for the vast majority of "Explicit" tracks released since 2000, confirming profanity as a core stylistic identifier of the genre.
* **The "Authenticity" Paradox:** Despite the high volume of explicit content, **no positive correlation** was found between profanity count and chart success.
* **Negative Trend:** In fact, a weak **negative correlation** ($r = -0.12$) suggests that excessive profanity may act as a barrier to mainstream radio success, rather than a commercial driver.

---

### **Libraries Used**
* `tidyverse` (dplyr, readr, stringr) - Data manipulation
* `tidytext` - Tokenisation and sentiment analysis
* `ggplot2` - Data visualisation
* `gridExtra` - Arranging multiple plots on a single grid
* `scales` - Formatting chart axes and labels

## 📂 Repository Structure
```bash
├── datasets/
│   ├── musicoset_metadata/
│   │   ├── artists.csv
│   │   ├── songs.csv
│   │   └── tracks.csv
│   ├── musicoset_popularity/
│   │   ├── song_chart.csv
│   │   └── song_pop.csv
│   ├── musicoset_songfeatures/
│   │   ├── acoustic_features.csv
│   │   └── lyrics.csv
│   └── profanity_lexicon/
│       └── array.js
├── images/
├── main.R
├── musicoset-hiphop-analysis.Rproj
└── README.md
```

---

## 🚀 How to Run the Analysis

1.  **Clone the Repository**
    Open your terminal (or Git Bash) and run:
    ```bash
    git clone https://github.com/porrr012/musicoset-hiphop-analysis.git
    ```

2. Double-click the musicoset-hiphop-analysis.Rproj file to open RStudio.

3.  **Install Required R Packages**  
    ```r
    install.packages(c("tidyverse", "tidytext", "ggplot2", "gridExtra", "scales"))
    ```

3.   **Load the datasets** from [musicoset](https://marianaossilva.github.io/DSW2019/index.html#tables).  
    Ensure `datasets/` folder contains the Musicoset `.csv` files as structured [here](#-repository-structure).

5.  **Run `main.R` Script**

---

## 📈 Visualisations

*(Run `main.R` to generate these plots)*

1.  **Genre Dominance (Stacked Bar):** Visualises the proportion of Hip-Hop vs. Other genres in "Explicit" vs. "Clean" categories.
2.  **Profanity Distribution (Bar Chart):** The top 20 most frequent explicit terms in the dataset.
3.  **Regression Analysis (Scatterplot):** A plot of `Profanity Count` vs. `Success Score`, showing the negative trend line.

---

## 📄 References & Data Source

* **Dataset:** [Musicoset: Music Data for Text Mining](https://marianaossilva.github.io/DSW2019/) (Silva et al., 2019)
* **Profanity Lexicon:** [badwords](https://github.com/MauriceButler/badwords) (Butler, n.d.)

---
