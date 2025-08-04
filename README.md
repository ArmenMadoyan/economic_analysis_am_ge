# Armenia & Georgia: Political-Economic Dashboard

This interactive R Shiny application visualizes key economic and political indicators of Armenia and Georgia from 2012 to 2024. It explores how major political events have impacted macroeconomic metrics such as GDP, military expenditure, education spending, net exports, and unemployment.

## 📊 Features

- **Interactive Dashboard**: Toggle between GDP, Military, Education, Enrollment, Trade, and Unemployment indicators.
- **Breakpoints**: Key events like the Velvet Revolution and the Russia–Ukraine wars are annotated on time series plots.
- **Comparative Trends**: All visualizations compare Armenia vs Georgia.
- **Faceted & Composite Plots**: Summarize multiple metrics simultaneously.
- **Summary Table**: Shows the latest available values for selected indicators.

## 📁 Data Sources

The app relies on the following datasets (Excel format):
- `Armenia_Georgia_GDP_PPP.xlsx`
- `Military_Expanditure_percent.xls`
- `Education_Spending_percent.xlsx`
- `Enrollment_Rate.xlsx`
- `Export_Data.xlsx`
- `Import_Data.xlsx`
- `Unemployment_Data.xlsx`

## 📦 Installation

1. Clone this repo:
```bash
git clone https://github.com/your-username/armenia-georgia-dashboard.git
cd armenia-georgia-dashboard
```

2. Install R packages:
```r
install.packages(c("shiny", "tidyverse", "readxl", "DT"))
```

3. Run the app locally:
```r
shiny::runApp("app.R")
```

> Make sure all Excel files are placed inside the `Data/` directory.

## 🧠 Project Structure

```
├── Data/                       # Raw Excel files
├── app.R                      # Launches the app using sourced UI & server
├── ui.R                       # User Interface logic
├── server.R                   # Server logic and reactive behavior
├── utils/                     # Custom ggplot functions (e.g., plot_gdp_vs_military.R)
└── Final_Project_Report.Rmd   # Non-interactive report version
```

## 🔍 Visualization Examples

- GDP and Military Spending comparison
- Net Exports over time
- Education Spending vs Enrollment scatter with regression
- Boxplot of Unemployment Rates

## 🌍 Deployment

Published on [shinyapps.io](https://madarmen.shinyapps.io/group_project/)

---

## 🤝 Authors

- Armen Madoyan
- Samvel Stepanyan
- Armen Tamrazyan
- Yevgine Mnatsakanyan

---
