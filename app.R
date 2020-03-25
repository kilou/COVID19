reqpack<-function(X){
  pak_failed<-which(!unlist(lapply(X,require,character.only=T)))
  if(length(pak_failed)>=1){
    install.packages(X[pak_failed],repos='https://stat.ethz.ch/CRAN/')
    lapply(X[pak_failed],require,character.only=T)
  }
}
reqpack(c("shiny",
          "readxl",
          "dplyr",
          "ggplot2",
          "snowfall",
          "writexl",
          "shinybusy"))

options(stringsAsFactors = FALSE)

rm(list = ls())

# Source necessary functions for covid-19 forecasts
source("functions.r")

# Load known parameters
pars0 <- as.data.frame(read_xlsx("params.xlsx"))
pars0$date <- conv(pars0$date)


# =========================================================================== #
#                                                                             #
#                             USER INTERFACE                                  #
#                                                                             #
# =========================================================================== #


ui <- shinyUI(fluidPage(

  titlePanel("Estimated number of beds needed in intensive care"),

  mainPanel(

    tabsetPanel(

      tabPanel("Data",

        fluidPage(

          br(),

          column(width = 2,
              # Input: Select a file ----
            fileInput(inputId = "file1",
                      label = "Choose data file\n(csv or xlsx)",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
      "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")),

            # Horizontal line ----
            tags$hr(),

            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),

            # Input: Select separator ----
            radioButtons(inputId = "sep",
                         label = "Separator (CSV)",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),

            # Input: Select quotes ----
            radioButtons(inputId = "quote",
                         label = "Quote (CSV)",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),

            # Date format
            textInput(inputId = "date_format",
                      label = "Date format",
                      # value = "%Y-%m-%d",
                      value = "%d.%m.%Y",
                      width = NULL,
                      placeholder = "%d: day; %m: month; %Y: year"),

            # Horizontal line ----
            tags$hr(),

            # Input: Select number of rows to display ----
            radioButtons(inputId = "disp",
                         label = "Display",
                         choices = c(Head = "head", All = "all"),
                         selected = "all")

          ),

          column(width = 4,

            # Output: Data file ----
            tableOutput("table_input_data")

          ),

          column(width = 6,

            # Output: Plot of the data
            plotOutput("plot_nhos"),

            plotOutput("plot_nicu")

          )

        )

      ),

      tabPanel("Parameters",

        fluidPage(

# -------------------------- PARAMETER : FIRST ROW -------------------------- #

          fluidRow(

# ------------------------------ LAM PARAMETER ------------------------------ #

            column(6,

              h3("Exponential growth parameter", align = "center"),

              fluidRow(

                column(3,

                  numericInput(inputId = "mlam",
                               label = "Value",
                               value = pars0$mlam[nrow(pars0)],
                               min = 1,
                               max = Inf,
                               step = 0.01)

                ),

                column(6,

                  sliderInput(inputId = "vlam",
                              label = "Variance",
                              value = pars0$vlam[nrow(pars0)],
                              min = 0,
                              max = max(0.1, pars0$vlam[nrow(pars0)]))

                ),

                column(3,

                  numericInput(inputId = "cilam",
                               label = "CI length",
                               value = 0.9,
                               min = 0,
                               max = 1,
                               step = 0.05)

                )

              ),

              fluidRow(

                plotOutput("plot_lam", height = "300px")

              ),

              style = "margin-bottom:30px; border:1px solid; padding:20px;"

            ),

# ------------------------------ PIC PARAMETER ------------------------------ #

            column(6,

              h3("Proportion of hospitalized patients that will require IC",
                 align = "center"),

              fluidRow(

                column(3,

                  numericInput(inputId = "mpic",
                               label = "Value",
                               value = pars0$mpic[nrow(pars0)],
                               min = 0,
                               max = 1,
                               step = 0.01)

                ),

                column(6,

                  sliderInput(inputId = "vpic",
                              label = "Variance",
                              value = pars0$vpic[nrow(pars0)],
                              min = 0,
                              max = max(0.1, pars0$vpic[nrow(pars0)]))

                ),

                column(3,

                  numericInput(inputId = "cipic",
                               label = "CI length",
                               value = 0.9,
                               min = 0,
                               max = 1,
                               step = 0.05)

                )

              ),

              fluidRow(

                plotOutput("plot_pic", height = "300px")

              ),

              style = "margin-bottom:30px; border:1px solid; padding:20px;"

            )

          ),

# ------------------------- PARAMETER : SECOND ROW -------------------------- #

          fluidRow(

# ------------------------------ LAG PARAMETER ------------------------------ #

            column(6,

              h3("Time lag between hospital admission and ICU transfer", align = "center"),

              fluidRow(

                column(3,

                  numericInput(inputId = "mlag",
                               label = "Value",
                               value = pars0$mlag[nrow(pars0)],
                               min = 0,
                               max = Inf,
                               step = 1)

                ),

                column(6,

                  uiOutput("vlag_ui")

                ),

                column(3,

                  numericInput(inputId = "cilag",
                               label = "CI length",
                               value = 0.9,
                               min = 0,
                               max = 1,
                               step = 0.05)

                )

              ),

              fluidRow(

                plotOutput("plot_lag", height = "300px")

              ),

              style = "margin-bottom:30px; border:1px solid; padding:20px;"

            ),

# ------------------------------ LOS PARAMETER ------------------------------ #

            column(6,

              h3("Length of stay in ICU", align = "center"),

              fluidRow(

                column(3,

                  numericInput(inputId = "mlos",
                               label = "Value",
                               value = pars0$mlos[nrow(pars0)],
                               min = 0,
                               max = Inf,
                               step = 1)

                ),

                column(6,

                  uiOutput("vlos_ui")

                ),

                column(3,

                  numericInput(inputId = "cilos",
                               label = "CI length",
                               value = 0.9,
                               min = 0,
                               max = 1,
                               step = 0.05)

                )

              ),

              fluidRow(

                plotOutput("plot_los", height = "300px")

              ),

              style = "margin-bottom:30px; border:1px solid; padding:20px;"

            )

          )

        )

      ),

# -------------------------------- FORECASTS -------------------------------- #

      tabPanel("Forecasts",

        fluidPage(

          br(),

          fluidRow(

            column(2,

              uiOutput("date_max_ui")

            ),

            column(2,

              strong("Compute forecasts"),

              br(),

              actionButton("submit", "Submit")

            ),

            column(2,

              strong("Download forecasts"),

              br(),

              uiOutput("dl_forcasts_ui")

            )

          ),

          br(),

          fluidRow(

          ##### DEBUG #####

          # uiOutput("pars"),
          # uiOutput("data"),
          # uiOutput("days"),
          # uiOutput("pred_nhos")

          #################

            column(3,

              numericInput(inputId = "cinhos",
                           label = "CI length for #cases",
                           value = 0.9,
                           min = 0,
                           max = 1,
                           step = 0.05)

            ),

            column(3,

              numericInput(inputId = "cinbed",
                           label = "CI length for #beds",
                           value = 0.9,
                           min = 0,
                           max = 1,
                           step = 0.05)
            )

          ),


          fluidRow(

            plotOutput("plot_fc_nhos")

          ),

          fluidRow(

            plotOutput("plot_fc_nbed")

          )

        )

      ),

# ---------------------------------- ABOUT ---------------------------------- #

      tabPanel("About",

        fluidPage(

          br(),

          div(p(strong("Creators:"), "Aziz Chaouch, Jérôme Pasquier,",
                "Valentin Rousson and Bastien Trächsel"), 
              p(strong("R Packages:"), "dplyr, ggplot2, readxl,
                shiny, shinybusy, snowfall, writexl"),
              style = "font-family: courier;")

        )

      )

    )

  ),

  add_busy_spinner(spin = "fading-circle")

))

# =========================================================================== #
#                                                                             #
#                               SERVER                                        #
#                                                                             #
# =========================================================================== #


server <- function(input, output, session) {

# ---------------------------------- DATA ----------------------------------- #

  input_data <- reactive({

    req(input$file1)

    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        if (input$file1$type %in%
              c("text/csv", "text/comma-separated-values,text/plain")) {
          df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
        } else {
          df <- read_xlsx(input$file1$datapath,
                          col_names = input$header)
          df <- as.data.frame(df)
        }
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )

    if (ncol(df) < 3) {
      stop("Data file must have at least three columns")
    } else {
      df <- df[, 1:3]
      names(df) <- c("date0", "nhos", "nicu")
    }

    df$date <- as.Date(df$date0, format = input$date_format)
    df$nhos <- as.integer(df$nhos)
    df$nicu <- as.integer(df$nicu)

    df <- df[c("date0", "date", "nhos", "nicu")]

    return(df)

  })

  output$table_input_data <- renderTable({

    tbl <- input_data()
    tbl$date <- as.character(tbl$date)
    names(tbl) <- c("Date (input)", "Date (formated)", "Hospital (cumul.)",
                    "ICU (cumul.)")

    if(input$disp == "head") {
      return(head(tbl))
    }
    else {
      return(tbl)
    }

  })

  output$plot_nhos <- renderPlot({

    input_data() %>%
      ggplot(aes(x = date, y = nhos)) +
      geom_col(fill = "#428bca") +
      theme_minimal() +
      labs(x = "Date", y = "Cumulative cases")

  })

  output$plot_nicu <- renderPlot({

    input_data() %>%
      ggplot(aes(x = date, y = nicu)) +
      geom_col(fill = "#428bca") +
      theme_minimal() +
      labs(x = "Date", y = "N ICU")

  })

# ------------------------------- PARAMETERS -------------------------------- #

  output$vlag_ui <- renderUI({

    sliderInput(inputId = "vlag",
                label = "Variance",
                value = max(input$mlag, pars0$vlag[nrow(pars0)]),
                min = input$mlag,
                max = max(3 * input$mlag, pars0$vlag[nrow(pars0)]))

  })

  output$vlos_ui <- renderUI({

    sliderInput(inputId = "vlos",
                label = "Variance",
                value = max(input$mlos, pars0$vlos[nrow(pars0)]),
                min = input$mlos,
                max = max(3 * input$mlos, pars0$vlos[nrow(pars0)]))

  })

  output$plot_lam <- renderPlot({

    lam <- rlam(1e06, input$mlam, input$vlam)
    p <- c(0, 0.5, 1) + c(1, 0, -1) * (1 - input$cilam) / 2
    validate(need(lam, ""))
    histo(lam, p)

  })

  output$plot_pic <- renderPlot({

    pic <- rpic(1e06, input$mpic, input$vpic)
    p <- c(0, 0.5, 1) + c(1, 0, -1) * (1 - input$cipic) / 2
    validate(need(pic, ""))
    histo(pic, p)

  })

  output$plot_lag <- renderPlot({

    lag <- rlag(1e06, input$mlag, input$vlag)
    p <- c(0, 0.5, 1) + c(1, 0, -1) * (1 - input$cilag) / 2
    validate(need(lag, ""))
    histo(lag, p)

  })

  output$plot_los <- renderPlot({

    los <- rlos(1e06, input$mlos, input$vlos)
    p <- c(0, 0.5, 1) + c(1, 0, -1) * (1 - input$cilos) / 2
    validate(need(los, ""))
    histo(los, p)

  })

  pars <- reactive({

    pars <- pars0
    i <- nrow(pars)
    pars$mlam[i] = input$mlam
    pars$vlam[i] = input$vlam
    pars$mpic[i] = input$mpic
    pars$vpic[i] = input$vpic
    pars$mlag[i] = input$mlag
    pars$vlag[i] = input$vlag
    pars$mlos[i] = input$mlos
    pars$vlos[i] = input$vlos

    return(pars)

  })

# -------------------------------- FORECASTS -------------------------------- #

  output$date_max_ui <- renderUI({
    dm <- max(input_data()$date)
    dateInput(inputId = "date_max",
                        label = "Prediction until ...",
                        value = dm + 7,
                        min = dm + 1,
                        max = NULL,
                        format = "yyyy-mm-dd")
  })


  rv <- reactiveValues()

  nday <- reactive({

    input$date_max - max(input_data()$date)

  })

  observeEvent(input$submit, {

    show_modal_spinner() # show the modal window

    rv$pred <- pred.covid(nday = nday(), nsim = 1000, pars(),
                          input_data(), ncpu = 8)

    rv$days <- as.Date(strptime(colnames(rv$pred$nbed), format = "%d.%m.%Y"))

    remove_modal_spinner()

  })

  ###### DEBUG #######

  # output$pars <- renderTable({ pars() })
  # output$data <- renderTable({ input_data() })
  # output$pred_nhos <- renderTable({ rv$pred$nhos })
  # output$days <- renderText({ paste(as.character(rv$days), collapse=" ") })

  #####################

  output$plot_fc_nhos <- renderPlot({

    validate(need(rv$pred, ""))
    p <- c(0, 0.5, 1) + c(1, 0, -1) * (1 - input$cinhos) / 2
    pred <- rv$pred
    plot.covid(pred, what = "nhos", prob = p)

  })

  output$plot_fc_nbed <- renderPlot({

    validate(need(rv$pred, ""))
    p <- c(0, 0.5, 1) + c(1, 0, -1) * (1 - input$cinbed) / 2
    pred <- rv$pred
    plot.covid(pred, what = "nbed", prob = p)

  })

  fc_table <- reactive({

    p <- c(0.5, 0, 1) + c(0, 1, -1) * (1 - input$cinhos) / 2
    nhos <- t(apply(rv$pred$nhos, 2, quantile, prob = p))
    colnames(nhos) <- c("nhos", colnames(nhos)[2:3])
    nhos <- cbind(date = rownames(nhos),
                  as.data.frame(nhos, check.names = FALSE))
    p <- c(0.5, 0, 1) + c(0, 1, -1) * (1 - input$cinbed) / 2
    nbed <- t(apply(rv$pred$nbed, 2, quantile, prob = p))
    colnames(nbed) <- c("nbed", colnames(nbed)[2:3])
    nbed <- cbind(date = rownames(nbed),
                  as.data.frame(nbed, check.names = FALSE))
    list(nhos = nhos, nbed = nbed)

  })

  output$dl_forcasts <- downloadHandler(

    filename = function() {
      paste0("forcasts_", format(Sys.time(), "%Y%m%d%H%M%S"), ".xlsx")
    },
    content = function(file) {write_xlsx(fc_table(), path = file)}

  )

  output$dl_forcasts_ui <- renderUI({

    req(rv$pred)

    downloadButton("dl_forcasts", "Download")

  })

  fc_table <- reactive({

    p <- c(0.5, 0, 1) + c(0, 1, -1) * (1 - input$cinhos) / 2
    nhos <- t(apply(rv$pred$nhos, 2, quantile, prob = p))
    colnames(nhos) <- c("nhos", colnames(nhos)[2:3])
    nhos <- cbind(date = rownames(nhos),
                  as.data.frame(nhos, check.names = FALSE))

    p <- c(0.5, 0, 1) + c(0, 1, -1) * (1 - input$cinbed) / 2
    nbed <- t(apply(rv$pred$nbed, 2, quantile, prob = p))
    colnames(nbed) <- c("nbed", colnames(nbed)[2:3])
    nbed <- cbind(date = rownames(nbed),
                  as.data.frame(nbed, check.names = FALSE))

    list(nhos = nhos, nbed = nbed)

  })

}

# =========================================================================== #
#                                                                             #
#                               RUN THE APP                                   #
#                                                                             #
# =========================================================================== #

shinyApp(ui, server)
