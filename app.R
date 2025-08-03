library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(shiny)
library(DT)

make_ts_data = function(df) {
  df %>%
    pivot_longer(
      cols = matches("^\\d{4}$"),
      names_to = "Year",
      values_to = "Value"
    ) %>%
    select(`Country Name`, Year, Value) %>%
    pivot_wider(
      names_from = `Country Name`,
      values_from = Value
    ) %>%
    mutate(Year = as.integer(Year))
}
visualise_ts_data = function(df, title, show_breaks, year_range, countries, y_label){
  df = df %>% filter(Year >= year_range[1], Year <= year_range[2])
  
  p = ggplot(df, aes(x = Year)) +
    labs(title = title, x = NULL, y = y_label, color = NULL) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.title = element_text(hjust = 0.5),
      axis.text = element_text(color = "black"),
      legend.position = "top"
    )
  
  if ("Armenia" %in% countries)
    p = p + geom_line(aes(y = Armenia, color = "Armenia"), size = 1.2)
  if ("Georgia" %in% countries)
    p = p + geom_line(aes(y = Georgia, color = "Georgia"), size = 1.2)
  
  p = p + scale_color_manual(values = c("Armenia" = "#B22222", "Georgia" = "#1E90FF")) +
    scale_x_continuous(breaks = all_years)
  
  if (length(show_breaks) > 0) {
    selected_breaks = breakpoints %>% filter(label %in% show_breaks, year >= year_range[1], year <= year_range[2])
    p = p + geom_vline(xintercept = selected_breaks$year, linetype = "dashed") +
      annotate(
        "text",
        x = selected_breaks$year,
        y = max(df[,countries], na.rm = TRUE) * 1.15,
        label = selected_breaks$label,
        angle = 30,
        vjust = 2.5,
        hjust = 0.5,
        size = 3,
        color = "gray40"
      )
  }
  
  p
}
plot_gdp_vs_military <- function(df_gdp, df_mil) {
  df_bar <- df_gdp %>%
    rename(Armenia_GDP = Armenia, Georgia_GDP = Georgia) %>%
    left_join(rename(df_mil, Armenia_Mil = Armenia, Georgia_Mil = Georgia), by = "Year") %>%
    pivot_longer(
      cols = -Year,
      names_to = "combined",
      values_to = "Value"
    ) %>%
    separate(combined, into = c("Country", "MetricCode"), sep = "_") %>%
    mutate(
      Metric = recode(MetricCode,
                      "GDP" = "GDP PPP USD",
                      "Mil" = "Military Exp. (% GDP)")
    )
  
  ggplot(df_bar, aes(x = factor(Year), y = Value, fill = Metric)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
    facet_grid(Metric ~ Country, scales = "free_y") +
    scale_fill_manual(values = c("#FF6F61", "#00BFC4")) +
    labs(
      x = "Year", y = NULL,
      title = "Economic & Military Spending Trends by Country",
      fill = "Metric"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "top"
    )
}
plot_net_export <- function(df_net) {
  df_net_long <- df_net %>%
    pivot_longer(
      cols = -Year,
      names_to = "Country",
      values_to = "NetExport"
    ) %>%
    filter(!is.na(NetExport))
  
  ggplot(df_net_long, aes(x = Year, y = NetExport, color = Country)) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "gray40") +
    labs(
      title = "Net Exports of Goods and Services (Exports − Imports)",
      x = "Year",
      y = "Net Export (Billion USD)",
      color = "Country"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11),
      axis.title = element_text(size = 12),
      legend.position = "bottom"
    )
}
plot_education_spending_vs_enrollment <- function(df_spending, df_enroll) {
  spending_long <- df_spending %>%
    pivot_longer(cols = -Year, names_to = "Country", values_to = "Education_Spending")
  
  enrollment_long <- df_enroll %>%
    pivot_longer(cols = -Year, names_to = "Country", values_to = "Tertiary_Enrollment")
  
  edu_data <- left_join(spending_long, enrollment_long, by = c("Year", "Country"))
  
  ggplot(edu_data, aes(x = Education_Spending, y = Tertiary_Enrollment, color = Country)) +
    geom_point(size = 3, alpha = 0.8) +
    geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
    labs(
      title = "Higher Education Spending Impact to Enrollment Rate",
      x = "Education Spending (% of GDP)",
      y = "Tertiary Enrollment (% gross)",
      color = "Country"
    ) +
    theme_minimal()
}
plot_unemployment_boxplot <- function(df_unemployment) {
  df_long <- pivot_longer(df_unemployment, cols = -Year, names_to = "Country", values_to = "Rate")
  
  ggplot(df_long, aes(y = Country, x = Rate, fill = Country)) +
    geom_boxplot(outlier.shape = 16, outlier.size = 2, coef = 1.5, width = 0.45) + 
    stat_boxplot(geom = "errorbar", width = 0.2) + 
    labs(
      title = "Distribution of Unemployment Rates (2012–2024)",
      x = "Unemployment Rate (%)",
      y = NULL
    ) +
    theme_minimal() +
    scale_fill_manual(values = c("Georgia" = "lightblue", "Armenia" = "lightyellow")) +
    theme(legend.position = "none")
}




gdp_data = read_excel("Data/Armenia_Georgia_GDP_PPP.xlsx")
military_data = read_excel("Data/Military_Expanditure_percent.xls")
edu_spending_data = read_excel("Data/edu_spending.xlsx")
total_enrollment_data = read_excel("Data/total_enrollment.xlsx")
export_data = read_excel("Data/Exports_Constant_USD_Armenia_Georgia.xlsx")
import_data = read_excel("Data/Imports_Current_USD_Armenia_Georgia.xlsx")
unemployment_data = read_excel("Data/Unemployment_Data_Formated.xlsx")

# ---------------- Load & Prepare Data ----------------
df_gdp = make_ts_data(gdp_data)
df_mil = make_ts_data(military_data)
df_edu = make_ts_data(edu_spending_data)
df_enroll = make_ts_data(total_enrollment_data)
df_export = make_ts_data(export_data)
df_import = make_ts_data(import_data)
df_unemployment = unemployment_data %>%
  mutate(Year = as.integer(Year))
# Normalize to Billion
scale_factor <- 1e9
unit_label <- "Billions_USD"

df_export[,-1] <- round(df_export[,-1] / scale_factor, 3)
df_import[,-1] <- round(df_import[,-1] / scale_factor, 3)
df_net = df_export
df_net[,-1] <- round(df_export[,-1] - df_import[,-1], 3)

breakpoints = data.frame(
  year = c(2014, 2018, 2020, 2022),
  label = c("Rus–Ukr_1st_War", "Velvet_Revolution", "44day_War_COVID19", "Rus–Ukr_2nd_War")
)
all_years = sort(unique(df_gdp$Year))

# ----------------- UI ---------------------

ui <- fluidPage(
  titlePanel("Armenia & Georgia: Political-Economic Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("dataset", "Select Dataset:",
                   choices = c("GDP PPP",
                               "Military Spending",
                               "Education Spending",
                               "Enrollment Rate",
                               "Export",
                               "Import",
                               "Unemployment"),
                   selected = "GDP PPP"),
      
      
      sliderInput("year_range", "Select Year Range:",
                  min = min(all_years), max = max(all_years),
                  value = range(all_years), sep = "", step = 1),
      
      checkboxGroupInput("countries", "Select Countries:",
                         choices = c("Armenia", "Georgia"),
                         selected = c("Armenia", "Georgia")),
      
      checkboxGroupInput("breaks", "Show Breakpoint Events:",
                         choices = breakpoints$label,
                         selected = breakpoints$label)
    ),
    
    mainPanel(
      plotOutput("tsPlot", height = "600px"),
      br(),
      DTOutput("summary_table")
    )
  ),
  br(),
  h3("Additional Visualizations"),
  plotOutput("gdp_mil_plot", height = "400px"),
  plotOutput("edu_enroll_plot", height = "400px"),
  plotOutput("net_export_plot", height = "400px"),
  plotOutput("unemployment_plot",  height = "400px")
  
)




# ----------------- Server ---------------------
server = function(input, output, session) {
  
  selected_df <- reactive({
    switch(input$dataset,
           "GDP PPP" = df_gdp,
           "Military Spending" = df_mil,
           "Education Spending" = df_edu,
           "Enrollment Rate" = df_enroll,
           "Export" = df_export,
           "Import" = df_import,
           "Unemployment" = df_unemployment)
  })
  
  output$tsPlot <- renderPlot({
    df <- selected_df()
    
    title <- switch(input$dataset,
                    "GDP PPP" = "GDP PPP",
                    "Military Spending" = "Military Spending",
                    "Education Spending" = "Education Spending",
                    "Enrollment Rate" = "Enrollment Rate",
                    "Export" = "Export of Goods (Billion USD)",
                    "Import" = "Import of Goods (Billion USD)",
                    "Unemployment" = "Unemployment Rate (%)")
    
    y_label <- switch(input$dataset,
                      "GDP PPP" = "USD",
                      "Military Spending" = "% of GDP",
                      "Education Spending" = "% of GDP",
                      "Enrollment Rate" = "% of population",
                      "Export" = "Billion USD",
                      "Import" = "Billion USD",
                      "Unemployment" = "% of population")
    
    visualise_ts_data(
      df = df,
      title = title,
      show_breaks = input$breaks,
      year_range = input$year_range,
      countries = input$countries,
      y_label = y_label
    )
  })
  
  output$summary_table = renderDT({
    df = selected_df() %>%
      filter(Year >= input$year_range[1], Year <= input$year_range[2])
    
    max_year = max(df$Year, na.rm = TRUE)
    
    data = data.frame(
      Country = c("Armenia", "Georgia"),
      `Most Recent Year` = max_year,
      `Most Recent Value` = c(
        df %>% filter(Year == max_year) %>% pull(Armenia),
        df %>% filter(Year == max_year) %>% pull(Georgia)
      )
    )
    
    datatable(
      data,
      escape = FALSE,
      options = list(dom = 't', paging = FALSE),
      rownames = FALSE
    )
  })
  
  output$gdp_mil_plot <- renderPlot({
    plot_gdp_vs_military(df_gdp, df_mil)
  })
  
  output$edu_enroll_plot <- renderPlot({
    plot_education_spending_vs_enrollment(df_edu, df_enroll)
  })
  
  output$net_export_plot <- renderPlot({
    plot_net_export(df_net)
  })
  
  output$unemployment_plot <- renderPlot({
    plot_unemployment_boxplot(df_unemployment)
  })
}


# ----------------- Run App ---------------------
shinyApp(ui, server)

