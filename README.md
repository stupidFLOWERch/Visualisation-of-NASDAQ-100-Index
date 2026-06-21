# 📊 NASDAQ-100 Index Visualization

A data visualization project built using **R (RStudio)** to analyze and explore the NASDAQ-100 index through interactive charts and dashboards.

---

## ✨ Features

- 📈 Line Chart (Trend Analysis)
- 🕯️ Candlestick Chart (Stock Price Movement)
- 🥧 Pie Chart (Composition Analysis)
- 🌐 Sunburst Chart (Hierarchical Visualization)
- 📊 Bar Chart (Comparative Analysis)
- 🔵 Scatter Plot (Correlation Study)
- 🫧 Bubble Chart (Multi-variable Analysis)
- 📊 Interactive Dashboard (Shiny App)

---

## 🛠️ Tech Stack / Libraries Used

- `shiny` – Web application framework  
- `quantmod` – Financial data & time series analysis  
- `ggplot2` – Data visualization  
- `tidyverse` – Data manipulation & wrangling  
- `plotly` – Interactive charts  
- `dplyr` – Data transformation  
- `DT` – Interactive tables  
- `RColorBrewer` – Color palettes  
- `lubridate` – Date/time handling  

---

## 📁 Dataset Used

- `nasdaq100.csv` – Historical index data  
- `nasdaq100_company.csv` – Company listing & classification  
- `nasdaq100_metrics_ratios.csv` – Financial ratios & metrics  

---

## ⚙️ Setup & Installation

If required libraries are missing, run the following script in **RStudio**:

```r
packages <- c(
  "shiny",
  "quantmod",
  "ggplot2",
  "tidyverse",
  "plotly",
  "dplyr",
  "DT",
  "RColorBrewer",
  "lubridate"
)

install_if_missing <- function(p) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
    library(p, character.only = TRUE)
  }
}

sapply(packages, install_if_missing)
```

## How to run

1. Clone this repository
```bash
git clone https://github.com/stupidFLOWERch/Visualisation-of-NASDAQ-100-Index.git
```
2. Open project in RStudio
3. Open the main R script file (**20410848_ting_chunghieng_codes.R**)
4. Click Run / Source button in RStudio
