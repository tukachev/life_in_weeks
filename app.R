library(shiny)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(colourpicker)

ui <- function(request) {
  fluidPage(
    theme = shinytheme("cerulean"),
    titlePanel("My Life in Weeks"),
    
    sidebarLayout(
      sidebarPanel(
        dateInput("birthdate", "Enter Your Birthdate:"),
        colourInput(
          inputId =  "Col",
          closeOnClick = TRUE,
          label = "Select color:",
          value = "#7D92C5",
          palette = "limited",
          allowedCols = c(
            "#3F51B5",
            "#6A7CC8",
            # светло-синий
            "#7D92C5",
            # пудрово-голубой
            "#8E94B9",
            # лососевый
            "#A09CB3",
            # серебристо-фиолетовый
            "#B2ADAA" # бежевый
          )
        ),
        checkboxInput(
          inputId = "current",
          label = "Show current week",
          value = TRUE
        ),
        hr(),
        h5("Download image"),
        downloadButton("download_png", "Download as PNG"),
        br(),
        br(),
        downloadButton("download_pdf", "Download as PDF"),
        br(),
        hr(),
        h5(
          "This button stores the current state of this application and get a URL for sharing"
        ),
        bookmarkButton(),
        hr(),
        tags$div(
          tags$p("Yurij Tukachev, last update: july 2024"),
          tags$a(href = "https://t.me/weekly_charts",
                 "https://t.me/weekly_charts")
        )
      ),
      
      mainPanel(plotOutput("life_plot"))
    )
  )
}

server <- function(input, output) {
  output$life_plot <- renderPlot({
    birthdate <- ymd(input$birthdate)
    
    age_years <-
      floor(as.numeric(difftime(Sys.Date(), birthdate, units = "days")) / 365.25)
    age_in_weeks <- age_years * 52
    
    last_birthday <-
      paste0(year(birthdate) + age_years,
             "-",
             format(birthdate, "%m-%d"))
    
    weeks_since_last_birthday <-
      floor(as.numeric(difftime(Sys.Date(), last_birthday, units = "weeks")))
    
    age_all <- (age_in_weeks + weeks_since_last_birthday) / 52
    full_weeks <- age_in_weeks + weeks_since_last_birthday
    
    weeks <- rep(1:52, 90)
    age <- rep(90:1, each = 52)
    age_weeks <-
      ((90 - age) * 52) + weeks
    # #6A7CC8 (светло-синий)
    # #7D92C5 (пудрово-голубой)
    # #8E94B9 (лососевый)
    # #A09CB3 (серебристо-фиолетовый)
    # #B2ADAA (бежевый)
    color_list <-
      # rep(c("#3F51B5", "gray90"), c(full_weeks, length(weeks) - full_weeks))
      rep(c(input$Col, "gray90"), c(full_weeks, length(weeks) - full_weeks))
    ifelse(isTRUE(input$current),
           color_list[full_weeks] <- "red",
           color_list[full_weeks] <- input$Col)
    life_df <- data.frame(age = age,
                          age_weeks = age_weeks,
                          color = color_list)
    
    ggplot(life_df, aes(
      x = weeks,
      y = rev(age),
      fill = color
    )) +
      geom_tile(linetype = 1,
                color = "white",
                linewidth = 0.25) +
      scale_fill_identity() +
      labs(
        title = "My Life in Weeks",
        x = "Week of the Year →",
        y = "← Age",
        caption = "@weekly_charts"
      ) +
      scale_y_continuous(
        labels = c(0, 1, seq(5, 90, 5)),
        expand = c(0, 0),
        breaks = c(0, 1, seq(5, 90, 5)),
        trans = "reverse"
      ) +
      scale_x_continuous(
        labels = seq(0, 52, 5),
        expand = c(0, 0),
        breaks = seq(0, 50, by = 5),
        position = "top"
      ) +
      coord_fixed(ratio = 1) +
      theme(
        plot.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(size = 12, color = "gray60"),
        text = element_text(family = "Sans", size = 24),
        axis.text = element_text(size = 10, color = "gray40"),
        axis.title = element_text(size = 16),
        axis.title.y = element_text(hjust = 1),
        axis.title.x = element_text(hjust = 0),
        legend.position = "none",
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank()
      )
  },  width = 800, height = 1200, res = 120, alt = "My Life in Weeks")
  
  output$download_png <- downloadHandler(
    filename = function() {
      paste0("MyLifeInWeeks", ".png")
    },
    content = function(file) {
      ggsave(
        file,
        bg = "white",
        dpi = 600,
        width = 5,
        height = 6,
        scale = 1.5
      )
    }
  )
  
  output$download_pdf <- downloadHandler(
    filename = function() {
      paste0("MyLifeInWeeks", ".pdf")
    },
    content = function(file) {
      ggsave(
        filename = file,
        # plot = my_plot,
        device = cairo_pdf,
        width = 8.27,
        height = 11.69,
        units = "in",
        dpi = 600
      )
    }
  )
}

# enableBookmarking(store = "url")
shinyApp(ui, server, enableBookmarking = "url")
