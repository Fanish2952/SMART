library(shiny)
library(ggplot2)
library(dplyr)
library(corrplot)
library(shinythemes)
library(jsonlite)
library(httr)
library(chatgpt)
library(openai)
library(devtools)

# Load dataset
student_data <- read.csv("D:/SMART PROJECT/SMART DATASET.csv")

# Custom CSS
custom_css <- "
.irs-grid-text { display: none; }
.irs-min, .irs-max { display: none; }

#chat_input {
  width: 100%;
  border-radius: 5px;
  padding: 8px;
  border: none;
}

#chat_response {
  white-space: pre-wrap;
  margin-top: 10px;
  background-color: white;
  padding: 10px;
  border-radius: 8px;
  max-height: 200px;
  overflow-y: auto;
}
"

# Custom Slider
custom_slider <- function(id, label) {
  tagList(
    div(style = "position: relative; margin-bottom: 20px;",
        div(style = "position: absolute; top: 35px; left: 0%; width: 33%; text-align: left; color: #00ffcc;", "Low"),
        div(style = "position: absolute; top: 35px; left: 33%; width: 33%; text-align: center; color: #3498db;", "Moderate"),
        div(style = "position: absolute; top: 35px; left: 66%; width: 33%; text-align: right; color: #ff6666;", "High"),
        sliderInput(id, label, min = 1, max = 10, value = 5)
    )
  )
}

# UI
ui <- fluidPage(
  theme = shinytheme("cyborg"),
  tags$head(
    tags$style(HTML(custom_css)),
    tags$script(HTML("
      $(document).on('keydown', function(e) {
        if (e.key === 'Enter' && $('#chat_input').is(':focus')) {
          $('#send_chat').click();
        }
      });

      $(document).on('click', '#close_chat', function() {
        $('#chatbot').hide();
      });
    "))
  ),
  titlePanel("Student Mental Health Dashboard"),
  sidebarLayout(
    sidebarPanel(
      numericInput("Student_Id", "Student ID:", value = 1, min = 1),
      numericInput("Age", "Age:", value = 18, min = 15, max = 30),
      selectInput("Gender", "Gender:", choices = c("M", "F")),
      numericInput("Year_of_Study", "Year of Study:", value = 1, min = 1, max = 4),
      selectInput("Family_Support", "Family Support:", choices = c("Low", "Medium", "High")),
      selectInput("Student_Behaviour", "Student Behaviour:", choices = c("Reserved", "Reactive", "Aggressive", "Positive")),
      selectInput("Placement_Status", "Placement Status:", choices = c("Placed", "Not Placed", "Not Eligible")),
      custom_slider("Sleep_Hours", "Sleep Hours:"),
      custom_slider("Academic_Stress_Level", "Academic Stress Level:"),
      custom_slider("Financial_Stress_Level", "Financial Stress Level:"),
      custom_slider("Family_Stress_Level", "Family Stress Level:"),
      custom_slider("Emotional_Stress_Level", "Emotional Stress Level:"),
      numericInput("CGPA", "CGPA:", value = 7, min = 0, max = 10, step = 0.1),
      actionButton("submit_btn", "Submit", class = "btn btn-warning"),
      downloadButton("download_report", "Download Report", class = "btn btn-success"),
      actionButton("open_chat", "Open Chatbot", class = "btn btn-primary")
    ),
    mainPanel(
      conditionalPanel(
        condition = "input.submit_btn > 0",
        verbatimTextOutput("total_stress"),
        verbatimTextOutput("main_stress_source"),
        verbatimTextOutput("top3_stress"),
        verbatimTextOutput("counseling_need"),
        verbatimTextOutput("proposed_remedy"),
        verbatimTextOutput("coping_mechanism"),
        verbatimTextOutput("risky_behavior"),
        plotOutput("stress_correlation"),
        plotOutput("family_support_plot"),
        plotOutput("sleep_stress_plot"),
        plotOutput("cgpa_stress_plot"),
        plotOutput("placement_stress_plot")
      ),
      conditionalPanel(
        condition = "input.open_chat > 0",
        absolutePanel(
          id = "chatbot", class = "panel panel-default", fixed = TRUE, 
          draggable = TRUE, top = 100, right = 20, width = 350, height = "auto",
          style = "background: linear-gradient(45deg, #1a1a2e, #16213e, #0f3460); color: white; padding: 15px; border-radius: 10px; box-shadow: 0px 0px 15px rgba(0, 255, 255, 0.6);",
          div(style = "text-align: right;", actionButton("close_chat", "×", class = "btn btn-danger btn-sm")),
          textInput("chat_input", "Chat with AI:", ""),
          actionButton("send_chat", "Send", class = "btn btn-success btn-block"),
          verbatimTextOutput("chat_response")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  custom_theme <- theme_minimal() +
    theme(
      plot.background = element_rect(fill = "#1a1a2e", color = "#00ffcc"),
      panel.background = element_rect(fill = "#1a1a2e"),
      panel.grid.major = element_line(color = "#3498db"),
      panel.grid.minor = element_line(color = "#8e44ad"),
      text = element_text(color = "white")
    )
  
  stress_text <- function(value) {
    if (value <= 3) return("Low")
    else if (value <= 7) return("Moderate")
    else return("High")
  }
  
  report_data <- reactiveVal("")  # Store report text for download
  
  observeEvent(input$submit_btn, {
    avg_stress <- mean(c(input$Academic_Stress_Level,
                         input$Financial_Stress_Level,
                         input$Family_Stress_Level,
                         input$Emotional_Stress_Level), na.rm = TRUE)
    
    total_stress <- stress_text(avg_stress)
    output$total_stress <- renderText({
      paste("Total Stress Level:", total_stress)
    })
    
    stress_levels <- c(
      "Academic" = stress_text(input$Academic_Stress_Level),
      "Financial" = stress_text(input$Financial_Stress_Level),
      "Family" = stress_text(input$Family_Stress_Level),
      "Emotional" = stress_text(input$Emotional_Stress_Level)
    )
    
    sorted_stress <- sort(stress_levels, decreasing = TRUE)
    top3_names_levels <- sapply(names(sorted_stress)[1:3], function(factor) {
      paste(factor, "-", sorted_stress[factor])
    })
    
    output$top3_stress <- renderText({
      paste("Top 3 Stress Factors:\n1. ", top3_names_levels[1], 
            "\n2. ", top3_names_levels[2], 
            "\n3. ", top3_names_levels[3])
    })
    
    main_source <- names(which.max(sapply(stress_levels, function(x) match(x, c("Low", "Moderate", "High")))))
    output$main_stress_source <- renderText({
      paste("Main Stress Source:", main_source)
    })
    
    counseling <- ifelse(sum(input$Academic_Stress_Level, 
                             input$Financial_Stress_Level, 
                             input$Family_Stress_Level, 
                             input$Emotional_Stress_Level, na.rm = TRUE) > 20, "Yes", "No")
    output$counseling_need <- renderText({
      paste("Counseling Need:", counseling)
    })
    
    remedies <- c(
      "Academic Counseling – https://confidentcounselors.com/",
      "Financial Aid Consultation – https://www.nerdwallet.com/",
      "Peer Support – https://www.7cups.com/",
      "Emotional Support Workshop – https://yourdost.com/"
    )
    
    remedy <- sample(remedies, 1)
    output$proposed_remedy <- renderText({ paste("Proposed Remedy:", remedy) })
    
    mechanisms <- c(
      "Socializing", "Meditation – https://www.headspace.com/",
      "Journaling – https://penzu.com/", "Reading – https://www.goodreads.com/",
      "Exercise – https://www.fitnessblender.com/"
    )
    
    coping <- sample(mechanisms, 1)
    output$coping_mechanism <- renderText({ paste("Coping Mechanism Used:", coping) })
    
    risky <- ifelse(input$Emotional_Stress_Level > 7, "Yes", "No")
    output$risky_behavior <- renderText({ paste("Risky Behavior Engaged:", risky) })
    
    # Create report text
    full_report <- paste(
      "Student Mental Health Prediction Report\n\n",
      "Student ID: ", input$Student_Id,
      "\nAge: ", input$Age,
      "\nGender: ", input$Gender,
      "\nYear of Study: ", input$Year_of_Study,
      "\n\nTotal Stress Level: ", total_stress,
      "\nMain Stress Source: ", main_source,
      "\nTop 3 Stress Factors:\n  1. ", top3_names_levels[1],
      "\n  2. ", top3_names_levels[2],
      "\n  3. ", top3_names_levels[3],
      "\nCounseling Need: ", counseling,
      "\nProposed Remedy: ", remedy,
      "\nCoping Mechanism: ", coping,
      "\nRisky Behavior: ", risky,
      "\n\nReport generated by SMART Dashboard"
    )
    
    report_data(full_report)  # Save for download
  })
  
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("student_mental_health_report_", input$Student_Id, ".txt")
    },
    content = function(file) {
      writeLines(report_data(), file)
    }
  )
  
  output$stress_correlation <- renderPlot({
    cor_matrix <- cor(student_data %>% select(Total_Stress_Level, Academic_Stress_Level, Financial_Stress_Level, Emotional_Stress_Level), use = "complete.obs")
    corrplot(cor_matrix, method = "ellipse", col = colorRampPalette(c("#ffcc00", "#ff3300"))(200))
  })
  
  output$family_support_plot <- renderPlot({
    ggplot(student_data, aes(x = Family_Support, y = Total_Stress_Level)) +
      geom_bar(stat = "identity", fill = "#ffcc00") +
      labs(title = "Influence of Family Support and Parental Income on Stress") +
      custom_theme
  })
  
  output$sleep_stress_plot <- renderPlot({
    ggplot(student_data, aes(x = Sleep_Hours, y = Total_Stress_Level)) +
      geom_point(size = 3, color = "#ff6666") +
      labs(title = "Sleep Hours vs. Total Stress Level") +
      custom_theme
  })
  
  output$cgpa_stress_plot <- renderPlot({
    ggplot(student_data, aes(x = CGPA, y = Academic_Stress_Level)) +
      geom_point(size = 3, color = "#33ff99") +
      labs(title = "CGPA vs. Academic Stress") +
      custom_theme
  })
  
  output$placement_stress_plot <- renderPlot({
    ggplot(student_data, aes(x = Placement_Status, y = Financial_Stress_Level, fill = Placement_Status)) +
      geom_boxplot(color = "#ff6699") +
      labs(title = "Financial Stress by Placement Status") +
      custom_theme
  })
  
  # Chatbot
  # ----- put this near the top -----
  openai_api_key <- Sys.getenv("OPENAI_API_KEY")   # never hard-code
  
  # ... inside observeEvent(input$send_chat) -----
  observeEvent(input$send_chat, {
    req(input$chat_input)
    
    prompt <- input$chat_input
    
    res <- tryCatch({
      resp <- httr::POST(
        url   = "https://api.openai.com/v1/chat/completions",
        httr::add_headers(Authorization = paste("Bearer", "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")),
      httr::content_type_json(),
      encode = "json",              
      body = list(
        model = "gpt-3.5-turbo",
        temperature = 0.7,
        messages = list(list(
          role    = "user",
          content = prompt
        ))
      )
    )

    httr::stop_for_status(resp)              
    parsed <- httr::content(resp, as = "parsed", type = "application/json")
    parsed$choices[[1]]$message$content
  }, error = function(e) {
    "Sorry, I am unable to recall it."
  })

  output$chat_response <- renderText(res)
})

  
}

shinyApp(ui, server)
