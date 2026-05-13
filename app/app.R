#=======================
#SHINY APP CREATION
#=======================
# ============================================
# GLOBAL.R — Load everything once on startup
# Save this as global.R in your app folder
# ============================================

library(shiny)
library(shinydashboard)
library(randomForest)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(scales)



# Load all eight trained models
rf_shotgun_pass      <- readRDS("models/rf_shotgun_pass.rds")
rf_shotgun_run       <- readRDS("models/rf_shotgun_run.rds")
rf_uc_pass           <- readRDS("models/rf_uc_pass.rds")
rf_uc_run            <- readRDS("models/rf_uc_run.rds")
rf_yards_shotgun_pass <- readRDS("models/rf_yards_shotgun_pass.rds")
rf_yards_shotgun_run  <- readRDS("models/rf_yards_shotgun_run.rds")
rf_yards_uc_pass      <- readRDS("models/rf_yards_uc_pass.rds")
rf_yards_uc_run       <- readRDS("models/rf_yards_uc_run.rds")

# Load residual stats
residual_stats       <- readRDS("models/residual_stats.rds")
yards_residual_stats <- readRDS("models/yards_residual_stats.rds")

# Load reliability lookup
reliability_lookup   <- readRDS("models/reliability_lookup.rds")

# QB tier to score mapping
tier_to_score <- c(
  "Elite"             = 1.40,
  "Above Average"     = 0.72,
  "Average"           = 0.13,
  "Below Average"     = -0.36,
  "Replacement Level" = -1.20
)

# Formation colors used across all plots
formation_colors <- c(
  "Shotgun Pass"      = "#0072B2",
  "Shotgun Run"       = "#009E73",
  "Under Center Pass" = "#D55E00",
  "Under Center Run"  = "#CC79A7"
)

# ============================================
# SOURCE ALL HELPER FUNCTIONS
# ============================================

load("models/functions.RData")

# ============================================
# UI
# ============================================

ui <- dashboardPage(
  
  skin = "blue",
  
  dashboardHeader(
    title = "NFL Formation Recommender"
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Play Recommender", tabName = "recommender", 
               icon = icon("football-ball")),
      menuItem("About",            tabName = "about",
               icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # ============================================
      # RECOMMENDER TAB
      # ============================================
      tabItem(
        tabName = "recommender",
        
        fluidRow(
          
          # ---- INPUT PANEL ----
          box(
            title  = "Game Situation",
            status = "primary",
            solidHeader = TRUE,
            width  = 3,
            
            selectInput("down", "Down",
                        choices  = c("1st" = 1, "2nd" = 2,
                                     "3rd" = 3, "4th" = 4),
                        selected = 1),
            
            numericInput("ydstogo", "Yards to Go",
                         value = 10, min = 1, max = 99),
            
            numericInput("yardline_100",
                         "Yards from Opponent End Zone",
                         value = 75, min = 1, max = 99),
            
            numericInput("score_differential",
                         "Score Differential (negative = trailing)",
                         value = 0, min = -40, max = 40),
            
            numericInput("half_seconds_remaining",
                         "Seconds Remaining in Half",
                         value = 1800, min = 0, max = 1800),
            
            selectInput("qtr", "Quarter",
                        choices  = c("1st" = 1, "2nd" = 2,
                                     "3rd" = 3, "4th" = 4),
                        selected = 1),
            
            selectInput("qb_tier", "QB Quality Tier",
                        choices = c("Elite", "Above Average",
                                    "Average", "Below Average",
                                    "Replacement Level"),
                        selected = "Average"),
            
            numericInput("n_sims", "Simulations",
                         value = 10000, min = 1000, max = 10000,
                         step  = 1000),
            
            actionButton("run_sim", "Get Recommendation",
                         class = "btn-primary btn-lg btn-block")
          ),
          
          # ---- SITUATION SUMMARY ----
          box(
            title  = "Situation Summary",
            status = "info",
            solidHeader = TRUE,
            width  = 9,
            
            fluidRow(
              valueBoxOutput("vbox_down",    width = 3),
              valueBoxOutput("vbox_zone",    width = 3),
              valueBoxOutput("vbox_score",   width = 3),
              valueBoxOutput("vbox_time",    width = 3)
            ),
            
            # Primary recommendation banner
            fluidRow(
              box(
                width  = 12,
                status = "success",
                solidHeader = FALSE,
                uiOutput("primary_recommendation")
              )
            )
          )
        ),
        
        # ---- RELIABILITY WARNINGS ----
        fluidRow(
          box(
            title  = "Data Reliability",
            status = "warning",
            solidHeader = TRUE,
            width  = 12,
            uiOutput("reliability_warnings")
          )
        ),
        
        # ---- RESULTS TABLE ----
        fluidRow(
          box(
            title  = "Full Rankings",
            status = "primary",
            solidHeader = TRUE,
            width  = 12,
            tableOutput("summary_table")
          )
        ),
        
        # ---- PLOTS ----
        fluidRow(
          box(
            title  = "EPA Distributions",
            status = "primary",
            solidHeader = TRUE,
            width  = 6,
            plotOutput("plot_density", height = "350px")
          ),
          box(
            title  = "Mean EPA with 90% CI",
            status = "primary",
            solidHeader = TRUE,
            width  = 6,
            plotOutput("plot_ci", height = "350px")
          )
        ),
        
        fluidRow(
          box(
            title  = "Conversion Probability",
            status = "primary",
            solidHeader = TRUE,
            width  = 6,
            plotOutput("plot_conversion", height = "350px")
          ),
          box(
            title  = "Training Sample Size",
            status = "primary",
            solidHeader = TRUE,
            width  = 6,
            plotOutput("plot_sample", height = "350px")
          )
        )
      ),
      
      # ============================================
      # ABOUT TAB
      # ============================================
      tabItem(
        tabName = "about",
        box(
          title  = "About This Tool",
          status = "primary",
          solidHeader = TRUE,
          width  = 12,
          p("This tool uses eight Random Forest models trained on NFL 
             play-by-play data from 2016-2023 to recommend optimal 
             formation-play type combinations for any game situation."),
          p("Four models predict Expected Points Added (EPA) and four 
             parallel models predict yards gained, one for each of: 
             Shotgun Pass, Shotgun Run, Under Center Pass, 
             Under Center Run."),
          p("Conversion probability is derived from 10,000 stochastic 
             simulations of yards gained rather than a separate 
             classification model, making the uncertainty quantification 
             transparent and interpretable."),
          p("Reliability classifications flag situations where training 
             data is insufficient to support confident recommendations, 
             preventing the system from presenting false precision in 
             data-sparse scenarios."),
          hr(),
          p(strong("Data Source:"), "nflfastR play-by-play, 2016-2023"),
          p(strong("Author:"), "Your Name"),
          p(strong("Institution:"), "Syracuse University")
        )
      )
    )
  )
)

# ============================================
# SERVER
# ============================================

server <- function(input, output, session) {
  
  # Run simulation on button click
  results <- eventReactive(input$run_sim, {
    
    # Map QB tier to numeric score
    qb_score <- tier_to_score[input$qb_tier]
    
    # Run full recommendation pipeline
    recommend_play(
      down                   = as.numeric(input$down),
      ydstogo                = input$ydstogo,
      yardline_100           = input$yardline_100,
      score_differential     = input$score_differential,
      half_seconds_remaining = input$half_seconds_remaining,
      qtr                    = as.numeric(input$qtr),
      qb_quality_score       = qb_score,
      n_sims                 = input$n_sims,
      show_plot              = FALSE  # plots handled separately in Shiny
    )
  })
  
  # ============================================
  # VALUE BOXES
  # ============================================
  
  output$vbox_down <- renderValueBox({
    down_label <- c("1st", "2nd", "3rd", "4th")[as.numeric(input$down)]
    valueBox(
      value    = paste0(down_label, " & ", input$ydstogo),
      subtitle = "Down & Distance",
      icon     = icon("flag"),
      color    = "blue"
    )
  })
  
  output$vbox_zone <- renderValueBox({
    zone <- classify_situation(
      as.numeric(input$down), input$ydstogo,
      input$yardline_100, input$score_differential,
      input$half_seconds_remaining, as.numeric(input$qtr)
    )$field_zone
    valueBox(
      value    = zone,
      subtitle = "Field Zone",
      icon     = icon("map-marker"),
      color    = "green"
    )
  })
  
  output$vbox_score <- renderValueBox({
    valueBox(
      value    = ifelse(input$score_differential >= 0,
                        paste0("+", input$score_differential),
                        input$score_differential),
      subtitle = "Score Differential",
      icon     = icon("trophy"),
      color    = ifelse(input$score_differential >= 0, "green", "red")
    )
  })
  
  output$vbox_time <- renderValueBox({
    mins <- floor(input$half_seconds_remaining / 60)
    secs <- input$half_seconds_remaining %% 60
    valueBox(
      value    = sprintf("%d:%02d", mins, secs),
      subtitle = "Time Remaining in Half",
      icon     = icon("clock"),
      color    = ifelse(input$half_seconds_remaining <= 120,
                        "red", "blue")
    )
  })
  
  # ============================================
  # PRIMARY RECOMMENDATION BANNER
  # ============================================
  
  output$primary_recommendation <- renderUI({
    req(results())
    res <- results()
    
    primary <- res$primary
    epa     <- round(res$full_results[[primary]]$mean_epa, 3)
    conv    <- round(res$full_results[[primary]]$prob_conversion * 100, 1)
    
    tagList(
      h2(style = "color: #27ae60; font-weight: bold;",
         paste0("PRIMARY: ", primary)),
      p(style = "font-size: 16px;",
        strong("Expected EPA: "), epa, " | ",
        strong("Conversion Probability: "), paste0(conv, "%")),
      h4(style = "color: #7f8c8d;",
         paste0("BACKUP: ", res$backup))
    )
  })
  
  # ============================================
  # RELIABILITY WARNINGS
  # ============================================
  
  output$reliability_warnings <- renderUI({
    req(results())
    res <- results()
    rel <- res$reliability
    
    warnings <- tagList()
    
    if (length(rel$insufficient) > 0) {
      warnings <- tagList(warnings,
                          div(class = "alert alert-danger",
                              icon("ban"),
                              strong(" EXCLUDED — insufficient data: "),
                              paste(rel$insufficient, collapse = ", ")))
    }
    if (length(rel$low) > 0) {
      warnings <- tagList(warnings,
                          div(class = "alert alert-warning",
                              icon("exclamation-triangle"),
                              strong(" LOW CONFIDENCE: "),
                              paste(rel$low, collapse = ", ")))
    }
    if (length(rel$moderate) > 0) {
      warnings <- tagList(warnings,
                          div(class = "alert alert-info",
                              icon("info-circle"),
                              strong(" MODERATE CONFIDENCE: "),
                              paste(rel$moderate, collapse = ", ")))
    }
    if (res$reliability$is_ambiguous) {
      warnings <- tagList(warnings,
                          div(class = "alert alert-info",
                              icon("random"),
                              strong(" Top two options are statistically similar")))
    }
    
    if (length(warnings) == 0) {
      div(class = "alert alert-success",
          icon("check-circle"),
          strong(" All formation options have High reliability 
                  in this situation"))
    } else {
      warnings
    }
  })
  
  # ============================================
  # SUMMARY TABLE
  # ============================================
  
  output$summary_table <- renderTable({
    req(results())
    results()$summary %>%
      select(Rank, Formation_Play, Mean_EPA,
             CI_Lower, CI_Upper, Prob_Conversion, Prob_Boom, Prob_Bust,
             Reliability, N_Plays) %>%
      rename(
        `Formation`        = Formation_Play,
        `Mean EPA`         = Mean_EPA,
        `90% CI Lower`     = CI_Lower,
        `90% CI Upper`     = CI_Upper,
        `Conv % `          = Prob_Conversion,
        `Boom % `          = Prob_Boom,
        `Bust % `          = Prob_Bust,
        `Reliability`      = Reliability,
        `Training Plays`   = N_Plays
      )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # ============================================
  # RENDER ALL FOUR PLOTS FROM plot_recommendation
  # ============================================
  
  plots <- eventReactive(input$run_sim, {
    req(results())
    res <- results()
    
    # Call your existing plot function which returns p1, p2, p3, p4
    plot_recommendation(
      all_results = res$full_results,
      summary_df  = res$summary,
      down        = as.numeric(input$down),
      ydstogo     = input$ydstogo,
      yardline_100 = input$yardline_100
    )
  })
  
  output$plot_density <- renderPlot({
    req(plots())
    plots()$p1
  })
  
  output$plot_ci <- renderPlot({
    req(plots())
    plots()$p2
  })
  
  output$plot_conversion <- renderPlot({
    req(plots())
    plots()$p3
  })
  
  output$plot_sample <- renderPlot({
    req(plots())
    plots()$p4
  })
}

#============================================
# RUN APP
# ============================================

shinyApp(ui = ui, server = server)