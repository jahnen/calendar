library(shiny)
library(lubridate)
library(dplyr)
library(tidyr)
library(reactable)
library(shinyscreenshot)
library(shinyjs)
library(shinyWidgets)

calculate_days_and_weekdays <- function(year, month) {
  # Generate all dates in the given month
  start_date <- ymd(sprintf("%04d-%02d-01", year, month))
  end_date <- ceiling_date(start_date, "month") - days(1)
  all_dates <- seq(start_date, end_date, by = "day")
  
  # Determine the weekday of the first date
  first_weekday <- wday(start_date) # 1 = Sunday, ..., 7 = Saturday
  
  # Add leading NA values to align the first date with Sunday
  if (first_weekday != 1) { # If the first date is not Sunday
    leading_nas <- rep(NA, first_weekday - 1)
  } else {
    leading_nas <- c()
  }
  days <- c(leading_nas, day(all_dates))
  
  # Create a full sequence of weekdays
  total_days <- length(days)
  weekdays <- rep(c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"), length.out = total_days)
  
  # Calculate week numbers
  week_number <- floor((seq_along(days) - 1) / 7) + 1
  
  # Create a data frame
  result <- data.frame(
    Day = days,
    Weekday = weekdays,
    Week = week_number
  )
  
  return(result)
}

# Shiny UI
ui <- fluidPage(
  tags$head(
    tags$link(
      rel = "icon",
      type = "image/png",
      href = "logo.png" # Favicon 
    )
  ),
  tags$head(
    tags$title("Minimal Calendar")
  ),
  # open graph tags
  tags$head(
    tags$meta(property = "og:title", content = "Minimal Calendar"),
    tags$meta(property = "og:description", content = "Create your custom monthly calendar"),
    tags$meta(property = "og:type", content = "website"),
    tags$meta(property = "og:url", content = "https://jhkim.shinyapps.io/calendar/"),
    tags$meta(property = "og:image", content = "logo.png"),
  ),
  # CUSTOM CSSs
  tags$head(
    tags$style(HTML("
      .rt-thead{
          height: 9vh;
      }
      
      .rt-tbody{
        height: 89vh;
      }
      
      /* header v-align*/
      .rt-th.rt-align-right{
        align-items: center;
      }
      
      /* header align*/
      .rt-th.rt-align-right .rt-th-inner{
        text-align: center;
      }
      
      #file_progress{
        display: none;
      }
      
      #image{
        margin:auto;
      }
      
      /*# icon of size controller*/
      button.btn.radiobtn.btn-custom-class.disabled{
        cursor: initial;
        background: #337ab7;
        color: white;
        opacity: 1;
      }
      
      #swal2-html-container{
        text-align:left;
      }
      
      "))
  ),
  useShinyjs(),
  useSweetAlert(),
  style = "margin-top: 1em; margin-bottom: 1em;",
  div(
    id = "info",
    style = "display: flex",
    # DATE
    column(
      width = 3,
      airMonthpickerInput(
        inputId = "ym",
        label = NULL,
        value = Sys.Date(),
        addon = "left",
        dateFormat = "MMMM yyyy",
        addonAttributes = list(
          style = "
            background: #337ab7;
            color: white;
          "
        ),
        width = "100%"
      )
    ),
    # Calendar Size
    column(
      width = 3,
      fluidRow(
        column(
          width = 3,
          radioGroupButtons(
            inputId = "target",
            label = NULL,
            choices = c("H", "D"),
            status = "primary"
          )
        ),
        column(
          width = 9,
          sliderInput(
            inputId = "size",
            label = NULL,
            min = 5,
            max = 50,
            value = 20,
            ticks = FALSE,
            step = 1
          )
        )
      )
    ),
    # Image handler
    column(
      width = 3,
      fluidRow(
        column(
          width = 3,
          radioGroupButtons(
            inputId = "types",
            label = NULL,
            choices = c(
              `<i class='fa-regular fa-calendar'></i>` = "cal",
              `<i class='fa-regular fa-image'></i>` = "image"
            ),
            status = "primary"
          )
        ),
        column(
          width = 9,
          # IMAGE
          div(
            id = "fileUI",
            style = "display: none;",
            fileInput(
              "file",
              NULL,
              accept = "image/*",
              placeholder = "Upload Image",
              buttonLabel = icon("upload"),
            )
          ),
        )
      )
    ),
    column(
      width = 3,
      id = "imageopt",
      style = "display:none;",
      fluidRow(
        column( # width
          width = 6,
          fluidRow(
            column(
              width = 1,
              radioGroupButtons(
                inputId = "i1",
                status = "custom-class",
                label = NULL,
                disabled = TRUE,
                selected = character(0),
                choices = c(
                  `<i class='fa-solid fa-arrows-up-down'></i>` = "ii"
                )
              )
            ),
            column(
              width = 8,
              style = "margin-left:10%;",
              sliderInput(
                inputId = "image.location",
                label = NULL,
                min = 0,
                max = 100,
                value = 0,
                step = 1,
                ticks = FALSE,
                width = "100%"
              )
            )
          )
        ),
        column( # location
          width = 6,
          fluidRow(
            column(
              width = 1,
              radioGroupButtons(
                inputId = "i2",
                status = "custom-class",
                label = NULL,
                disabled = TRUE,
                selected = character(0),
                choices = c(
                  `<i class='fa-solid fa-arrows-left-right'></i>` = "ii"
                )
              )
            ),
            column(
              width = 8,
              style = "margin-left:10%;",
              sliderInput(
                inputId = "image.width",
                label = NULL,
                min = 0,
                max = 100,
                value = 100,
                step = 5,
                ticks = FALSE
              )
            )
          )
        )
      )
    ),
    # HIDDEN BUTTONS
    actionButton("screenshot", "NA", style = "display:none;"),
    actionButton("toggle", "NA", style = "display: none;")
  ),
  uiOutput("UIs")
)

# Shiny Server
server <- function(input, output, session) {
  # INIT VALUE
  year.val <- reactiveVal(value = NULL)
  month.val <- reactiveVal(value = NULL)
  
  size.header <- reactiveVal(value = 20)
  size.date <- reactiveVal(value = 18)
  
  image.width <- reactiveVal(value = 100)
  image.location <- reactiveVal(value = 0)
  
  # Initial Note
  sendSweetAlert(
    session = session,
    title = "Welcome !!",
    text = 
      tags$span(
        tags$h3("How to Use?"),
        "1. Click", tags$text("Sat", style = 'color: blue'), "will hide options",
        tags$br(),
        "2. Click", tags$text("Sun", style = 'color: red'), "will download as image",
        tags$br(),
        tags$br(),
        "Code in", tags$a(href = "https://github.com/jahnen/calendar", target = "_", "Here")
      )
  )
  
  observeEvent(input$size, {
    req(input$size)
    target <- input$target
    if (target == "H") {
      size.header(input$size)
    }
    if (target == "D") {
      size.date(input$size)
    }
  })
  
  observeEvent(input$ym, {
    vals <- as.character(input$ym)
    year.val(as.numeric(substr(vals, 1, 4)))
    month.val(as.numeric(substr(vals, 6, 7)))
  })
  
  observeEvent(input$image.width, {
    req(input$image.width)
    image.width(input$image.width)
  })
  
  observeEvent(input$image.location, {
    req(input$image.location)
    image.location(input$image.location)
  })
  
  output$UIs <- renderUI({
    # only calendar
    if (input$types == "cal") {
      shinyjs::hide("fileUI")
      shinyjs::hide("imageopt")
      list(
        column(
          width = 12,
          reactableOutput("calendar_table", width = "100%", height = "99%")
        )
      )
    }
    
    # image + calendar
    else if (input$types == "image") {
      shinyjs::show("fileUI")
      shinyjs::show("imageopt")
      list(
        column(
          width = 3,
          style = paste0("margin-top:", image.location(), "em;"),
          imageOutput(
            "image",
            width = paste0(image.width(), "%"),
            height = "100%"
          )
        ),
        column(
          width = 9,
          reactableOutput("calendar_table", width = "100%", height = "99%")
        )
      )
    }
  })
  
  output$calendar_table <- renderReactable({
    year <- year.val()
    month <- month.val()
    
    # Calculate calendar data
    calendar_data <- calculate_days_and_weekdays(year, month)
    
    # Reshape the data for gt table
    calendar_table <- calendar_data %>%
      pivot_wider(names_from = Weekday, values_from = Day, values_fill = list(Day = NA)) %>%
      arrange(Week) |>
      select(-Week)
    
    # Sunday Onclick
    onclick.sun <- "document.getElementById('screenshot').click();"
    
    # Saturday Onclick
    onclick.sat <- "document.getElementById('toggle').click();"
    
    calendar_table |>
      reactable(
        class = "height: 90vh;",
        sortable = FALSE,
        # bordered = TRUE,
        compact = TRUE,
        defaultColDef = colDef(
          headerStyle = list(
            fontSize = paste0(size.header(), "px")
          ),
          style = list(
            fontSize = paste0(size.date(), "px")
          )
        ),
        columns = list(
          Sun = colDef(
            html = TRUE,
            header = JS(
              paste0(
                'function(column) {
              return `<div style="cursor: pointer; color: #bf616a" onclick ="', onclick.sun, '">` + column.name + `</div>`
            }'
              )
            ),
            style = list(
              fontSize = paste0(size.date(), "px"),
              color = "#bf616a"
            )
          ),
          Sat = colDef(
            html = TRUE,
            header = JS('function(column) {
              return `<div style="cursor: pointer; color: #337ab7" onclick = "', onclick.sat, '">` + column.name + `</div>`
            }'),
            style = list(
              fontSize = paste0(size.date(), "px"),
              color = "#337ab7"
            )
          )
        )
      )
  })
  
  # image handler
  observeEvent(input$file, {
    req(input$file)
    output$image <- renderImage(
      {
        list(src = input$file$datapath, alt = "Image", width = "100%", height = "100%")
      },
      deleteFile = FALSE
    )
  })
  
  # saturday
  observeEvent(input$toggle, {
    shinyjs::toggle("info")
  })
  
  # sunday
  observeEvent(input$screenshot, {
    updateReactable("calendar_table")
    screenshot(scale = 2)
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
