## 🎵 Hip-Hop/Rap Songs Analysis (1990–2018)

### 📌 Project Overview
This project investigates the relationship between **explicit lyrics** and **commercial success** in Hip-Hop music during the digital era (1990–2018). Using the [**Musicoset**](https://marianaossilva.github.io/DSW2019/index.html#tables) dataset, we apply text mining and statistical regression to challenge the popular assumption that "vulgarity sells."

**Key Research Questions:**
1. How has profanity density in Hip-hop/Rap music evolved over time (1990–2018)?
2. How does profanity influence Hip-hop/Rap music in terms of its popularity?

---

### 📊 Key Findings  
* **The "Streaming Effect"**  
Profanity density in Hip-Hop spiked post-2010, directly correlating with the industry shift from radio censorship to on-demand streaming.  
* **Commercial Liability**  
Contrary to the "shock value" hypothesis, regression analysis reveals a negative correlation ($r = -0.134$) between explicit content and chart popularity.  
* **Genre Isolation**  
This phenomenon is unique to Hip-Hop; control groups (Pop, Rock) showed zero correlation ($r \approx 0$) between profanity and success.

---

### **Libraries Used**
* `tidyverse` (dplyr, readr, stringr) - Data manipulation
* `tidytext` - Tokenisation and sentiment analysis
* `ggplot2` - Data visualisation
* `grid` - Plot layouts and annotations
* `gridExtra` - Arranging multiple plots on a single grid
* `scales` - Formatting chart axes and labels
---
### 📂 Repository Structure
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
├── illustrations/
│       └── images/
│              └──screenshots/
├── main.R
├── musicoset-hiphop-analysis.Rproj
└── README.md
```

---

### 🚀 How to Run the Analysis

1.  **Clone the Repository**  
    Open your terminal (or Git Bash) and run:
    ```bash
    git clone https://github.com/porrr012/musicoset-hiphop-analysis.git
    ```

2. Double-click the `musicoset-hiphop-analysis.Rproj` to open RStudio.

3.  **Install Required R Packages**  
    ```r
    install.packages(c("tidyverse", "tidytext", "ggplot2", "gridExtra", "scales"))
    ```

3.   **Load the datasets** from [musicoset](https://marianaossilva.github.io/DSW2019/index.html#tables).  
    Ensure `datasets/` folder contains the Musicoset `.csv` files as structured [here](#-repository-structure).

5.  **Run `main.R` Script**

---

### 📄 References & Data Sources

* **Dataset:** [*MusicOSet*: An Enhanced Open Dataset for Music Data Mining](https://marianaossilva.github.io/DSW2019/) (Silva et al., 2019)
* **Profanity Lexicon:** [badwords](https://github.com/MauriceButler/badwords) (Butler, n.d.)

---
