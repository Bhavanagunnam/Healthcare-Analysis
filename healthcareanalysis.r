# Generate synthetic healthcare dataset
set.seed(123) # For reproducibility

# Synthetic data creation
num_records <- 1000 # Number of rows in synthetic dataset

data <- data.frame(
  Age = sample(18:90, num_records, replace = TRUE),
  Gender = sample(c("Male", "Female"), num_records, replace = TRUE, prob = c(0.5, 0.5)),
  Medical_Condition = sample(
    c("Diabetes", "Hypertension", "Asthma", "Heart Disease", "Arthritis", "Cancer", "Depression"), 
    num_records, 
    replace = TRUE
  ),
  Billing_Amount = round(runif(num_records, min = 100, max = 5000), 2),
  Date_of_Admission = as.Date("2022-01-01") + sample(0:730, num_records, replace = TRUE),
  Discharge_Date = as.Date("2022-01-01") + sample(1:740, num_records, replace = TRUE)
)

# Ensure Discharge_Date is always after Date_of_Admission
data$Discharge_Date <- pmax(data$Discharge_Date, data$Date_of_Admission + 1)

 #Install necessary packages
 #install.packages("shiny")
 #install.packages("dplyr")
#install.packages("ggplot2")
 #install.packages("plotly")
 #install.packages("lubridate")
 #install.packages("rsconnect")

# Load necessary libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(lubridate)

# Define UI for the Shiny app
ui <- fluidPage(
  titlePanel("Healthcare Data Analysis Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("gender", "Select Gender:", choices = c("All", levels(as.factor(data$Gender))), selected = "All"),
      sliderInput("age", "Select Age Range:", min = min(data$Age), max = max(data$Age), 
                  value = range(data$Age), step = 1, round = 0),
      dateRangeInput("dateRange", "Select Date Range:", start = min(data$Date_of_Admission), end = max(data$Discharge_Date)),
      selectInput("condition", "Select Medical Condition:", choices = c("All", levels(as.factor(data$Medical_Condition))), selected = "All"),
      downloadButton("downloadData", "Download Data")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Age Distribution", plotlyOutput("ageDistPlot")),
        tabPanel("Gender Distribution", plotlyOutput("genderDistPlot")),
        tabPanel("Top Medical Conditions", plotlyOutput("topConditionsPlot")),
        tabPanel("Billing Amount by Condition", plotlyOutput("billingBoxPlot")),
        tabPanel("Age vs Billing Amount", plotlyOutput("ageBillingScatter")),
        tabPanel("Age Distribution by Gender", plotlyOutput("ageGenderDistPlot")),
        tabPanel("Billing Amount by Gender", plotlyOutput("billingGenderDistPlot")),
        tabPanel("Summary Statistics", verbatimTextOutput("summaryStats"))
      )
    )
  )
)

# Define server logic for the Shiny app
server <- function(input, output) {
  
  filtered_data <- reactive({
    data_filtered <- data
    if (input$gender != "All") {
      data_filtered <- data_filtered[data_filtered$Gender == input$gender, ]
    }
    if (input$condition != "All") {
      data_filtered <- data_filtered[data_filtered$Medical_Condition == input$condition, ]
    }
    data_filtered <- data_filtered[data_filtered$Age >= input$age[1] & data_filtered$Age <= input$age[2], ]
    data_filtered <- data_filtered[data_filtered$Date_of_Admission >= input$dateRange[1] & data_filtered$Discharge_Date <= input$dateRange[2], ]
    data_filtered
  })
  
  output$ageDistPlot <- renderPlotly({
    gg <- ggplot(filtered_data(), aes(x = Age)) +
      geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
      labs(title = "Distribution of Patient Age", x = "Age", y = "Count")
    ggplotly(gg)
  })
  
  output$genderDistPlot <- renderPlotly({
    gg <- ggplot(filtered_data(), aes(x = Gender)) +
      geom_bar(fill = "pink", color = "black") +
      labs(title = "Distribution of Patient Gender", x = "Gender", y = "Count")
    ggplotly(gg)
  })
  
  output$topConditionsPlot <- renderPlotly({
    condition_counts <- filtered_data() %>%
      group_by(Medical_Condition) %>%
      summarise(count = n()) %>%
      arrange(desc(count))
    
    gg <- ggplot(head(condition_counts, 10), aes(x = reorder(Medical_Condition, count), y = count)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      coord_flip() +
      labs(title = "Top 10 Medical Conditions", x = "Medical Condition", y = "Count")
    ggplotly(gg)
  })
  
  output$billingBoxPlot <- renderPlotly({
    gg <- ggplot(filtered_data(), aes(x = Medical_Condition, y = Billing_Amount, fill = Medical_Condition)) +
      geom_boxplot() +
      coord_flip() +
      labs(title = "Boxplot of Billing Amount by Medical Condition", x = "Medical Condition", y = "Billing Amount")
    ggplotly(gg)
  })
  
  output$ageBillingScatter <- renderPlotly({
    gg <- ggplot(filtered_data(), aes(x = Age, y = Billing_Amount, color = Medical_Condition)) +
      geom_point(alpha = 0.6) +
      labs(title = "Scatterplot of Age vs. Billing Amount", x = "Age", y = "Billing Amount")
    ggplotly(gg)
  })
  
  output$ageGenderDistPlot <- renderPlotly({
    gg <- ggplot(filtered_data(), aes(x = Age, fill = Gender)) +
      geom_histogram(binwidth = 5, position = "dodge", color = "black") +
      labs(title = "Age Distribution by Gender", x = "Age", y = "Count")
    ggplotly(gg)
  })
  
  output$billingGenderDistPlot <- renderPlotly({
    gg <- ggplot(filtered_data(), aes(x = Billing_Amount, fill = Gender)) +
      geom_histogram(binwidth = 50, position = "dodge", color = "black") +
      labs(title = "Billing Amount Distribution by Gender", x = "Billing Amount", y = "Count")
    ggplotly(gg)
  })
  
  output$trendPlot <- renderPlotly({
    data_trend <- filtered_data() %>%
      group_by(Date_of_Admission) %>%
      summarise(Billing_Amount = sum(Billing_Amount, na.rm = TRUE)) %>%
      arrange(Date_of_Admission)
    
    gg <- ggplot(data_trend, aes(x = Date_of_Admission, y = Billing_Amount)) +
      geom_line(color = "blue") +
      labs(title = "Trend of Billing Amount Over Time", x = "Date", y = "Billing Amount")
    ggplotly(gg)
  })
  
  output$summaryStats <- renderPrint({
    summary(filtered_data())
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("filtered_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file)
    }
  )
}

# Run the Shiny app
  shinyApp(ui = ui, server = server)
