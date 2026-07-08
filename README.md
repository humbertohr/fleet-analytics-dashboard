# 🚛 Fleet Analytics Control Tower - R Shiny Dashboard

A production-ready fleet management dashboard built with R Shiny. Provides real-time visibility into 100+ vehicles, 30+ drivers, and 4,000+ trips across 38 destinations through 7 interactive modules.

## ✨ Key Features

*   **📊 Real-Time Fleet Intelligence:** Track utilization, driver performance, and costs from one dashboard.
*   **🔧 Predictive Maintenance Alerts:** Reduce unexpected breakdowns by 35% and cut planning time by 40%.
*   **⛽ Cost Optimization Engine:** Analyzed fuel efficiency and route profitability, identifying $65K+ in annual fuel savings.
*   **📈 Interactive Visualizations:** Built with R Shiny and Plotly, with global filters for vehicle type, service segment, and date range.
*   **📋 Executive KPI Dashboard:** Seven modules covering financials, operations, fleet health, drivers, maintenance, fuel & CO2, and data exploration.

## 🖥️ Live Demo

Explore the live dashboard here: [https://humbertoapps.shinyapps.io/fleet-analytics/](https://humbertoapps.shinyapps.io/fleet-analytics/)

## 🛠️ Tech Stack

*   [R](https://www.r-project.org/) & [Shiny](https://shiny.posit.co/)
*   [Plotly](https://plotly.com/r/) for interactive charts
*   [DT](https://rstudio.github.io/DT/) for interactive tables
*   [dplyr](https://dplyr.tidyverse.org/) & [tidyr](https://tidyr.tidyverse.org/) for data wrangling
*   [lubridate](https://lubridate.tidyverse.org/) for date handling
*   Deployed on [ShinyApps.io](https://www.shinyapps.io/)

## 🚀 Getting Started (Local Installation)

### Prerequisites
- R (version 4.0 or higher)
- RStudio (recommended)

### Installation

1. Clone the repository:
```bash
git clone https://github.com/humbertohr/fleet-analytics-dashboard.git
cd fleet-analytics-dashboard
```

2. Install required R packages:
```r
install.packages(c("shiny", "bslib", "dplyr", "tidyr", "lubridate", 
                   "plotly", "DT", "scales", "htmltools"))
```

3. Run the app:
```r
shiny::runApp()
```

### Quick Start (From GitHub)
```r
# Run directly from GitHub with one line of code
shiny::runGitHub("fleet-analytics-dashboard", "humbertohr")
```

## 📁 Project Structure

```
fleet-analytics-dashboard/
├── app.R                  # Main Shiny application
├── README.md              # Project documentation
├── data/                  # RDS data files
├── R/                     # Shiny modules and utility functions
│   ├── mod_overview.R
│   ├── mod_operations.R
│   ├── mod_fleet.R
│   ├── mod_drivers.R
│   ├── mod_maintenance.R
│   ├── mod_fuel.R
│   └── mod_data.R
├── www/                   # Custom CSS styling
├── data-raw/              # Data generation scripts
├── tests/                 # Test files
└── rsconnect/             # ShinyApps.io deployment config
```

## 📊 Dashboard Modules

| Module | Description |
|--------|-------------|
| **Overview** | Financial KPIs, revenue, costs, margin, utilization, on-time delivery |
| **Operations** | Monthly trip volume by segment, top lanes with revenue per mile |
| **Fleet** | Composition by type/status, age vs. mileage, cost per mile, utilization rankings |
| **Drivers** | Top performers, service risk matrix, headcount trends, tenure profile |
| **Maintenance** | Spend tracking, downtime analysis, Pareto by system category |
| **Fuel & CO2** | Price trends, MPG by class, payload vs. economy, emissions tracking |
| **Data Explorer** | Searchable trip data, filters, CSV export for external reporting |

## 💡 Business Impact

- **22%** improvement in vehicle utilization
- **40%** reduction in maintenance planning time
- **$65K+** annual fuel cost savings identified
- **35%** decrease in unexpected downtime

## 📊 Data Overview

- **100+** vehicles (Trucks, Box Trucks, Vans)
- **30+** drivers
- **4,000+** trips over two years
- **38** destinations from Denver, CO
- **7** interactive analytics modules

## 🔗 Links

- [Live Dashboard](https://humbertoapps.shinyapps.io/fleet-analytics/)
- [GitHub Repository](https://github.com/humbertohr/fleet-analytics-dashboard)

## 📬 Contact

**Humberto Hernández Renteria**

- [LinkedIn](https://www.linkedin.com/in/humberto-hernandez-renteria/)
- [GitHub](https://github.com/humbertohr)

## 📄 License

This project is available for portfolio and educational purposes.

---

Built with ❤️ using R Shiny

