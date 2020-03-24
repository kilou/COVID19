
library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(snowfall)

options(stringsAsFactors = FALSE)

rm(list = ls())

# Source necessary functions for covid-19 forcasts
source("functions.r")

# Load know parameters
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
            plotOutput("plot_ntot"),

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
                               value = pars0$mlam[3],
                               min = 1,
                               max = Inf,
                               step = 0.01)

                ),

                column(6,

                  sliderInput(inputId = "vlam",
                              label = "Variance",
                              value = pars0$vlam[3],
                              min = 0,
                              max = max(0.1, pars0$vlam[3]))

                ),

                column(3,

                  numericInput(inputId = "cilam",
                               label = "CI length",
                               value = 0.5,
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

              h3("Proportion of incident cases that will require IC",
                 align = "center"),

              fluidRow(

                column(3,

                  numericInput(inputId = "mpic",
                               label = "Value",
                               value = pars0$mpic[3],
                               min = 0,
                               max = 1,
                               step = 0.01)

                ),

                column(6,

                  sliderInput(inputId = "vpic",
                              label = "Variance",
                              value = pars0$vpic[3],
                              min = 0,
                              max = max(0.1, pars0$vpic[3]))

                ),

                column(3,

                  numericInput(inputId = "cipic",
                               label = "CI length",
                               value = 0.5,
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

              h3("Time between detection and admission", align = "center"),

              fluidRow(

                column(3,

                  numericInput(inputId = "mlag",
                               label = "Value",
                               value = pars0$mlag[3],
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
                               value = 0.5,
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

              h3("Length of stay", align = "center"),

              fluidRow(

                column(3,

                  numericInput(inputId = "mlos",
                               label = "Value",
                               value = pars0$mlos[3],
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
                               value = 0.5,
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

      tabPanel("Forcasts",

        fluidPage(

          br(),

          fluidRow(

            column(6,

              uiOutput("date_max_ui")

            ),

            column(6,

              strong("Compute forecasts"),

              br(),

              actionButton("submit", "Submit")

            )

          ),

          br(),

          fluidRow(

          ##### DEBUG #####

          #uiOutput("pars"),
          #uiOutput("data"),
          #uiOutput("days"),
          #uiOutput("pred_ntot")

          #################

            column(6,

              numericInput(inputId = "cintot",
                           label = "CI length for #cases",
                           value = 0.5,
                           min = 0,
                           max = 1,
                           step = 0.05)

            ),

            column(6,

              numericInput(inputId = "cinbed",
                           label = "CI length for #beds",
                           value = 0.5,
                           min = 0,
                           max = 1,
                           step = 0.05)
            )

          ),


          fluidRow(

            plotOutput("plot_fc_ntot")

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
              p(strong("R Packages:"), "shiny, dplyr, ggplot2"),
              style = "font-family: courier;")

        )

      )

    )

  )

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
      names(df) <- c("date0", "ntot", "nicu")
    }

    df$date <- as.Date(df$date0, format = input$date_format)
    df$ntot <- as.integer(df$ntot)
    df$nicu <- as.integer(df$nicu)

    df <- df[c("date0", "date", "ntot", "nicu")]

    return(df)

  })

  output$table_input_data <- renderTable({

    tbl <- input_data()
    tbl$date <- as.character(tbl$date)
    names(tbl) <- c("Date (input)", "Date (formated)", "Cumulative cases",
                    "N ICU")

    if(input$disp == "head") {
      return(head(tbl))
    }
    else {
      return(tbl)
    }

  })

  output$plot_ntot <- renderPlot({

    input_data() %>%
      ggplot(aes(x = date, y = ntot)) +
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
                value = max(input$mlag, pars0$vlag[3]),
                min = input$mlag,
                max = max(3 * input$mlag, pars0$vlag[3]))

  })

  output$vlos_ui <- renderUI({

    sliderInput(inputId = "vlos",
                label = "Variance",
                value = max(input$mlos, pars0$vlos[3]),
                min = input$mlos,
                max = max(3 * input$mlos, pars0$vlos[3]))

  })

  output$plot_lam <- renderPlot({

    lam <- rlam(1e06, input$mlam, input$vlam)
    p <- c(0, 0.5, 1) + c(1, 0, -1) * (1 - input$cilam) / 2
    # qlam <- quantile(lam, probs = p)
    # hist(lam, xlab = "", ylab = "", yaxt = "n", main = "")
    # abline(v = qlam, lty = 2)
    histo(lam, p)

  })

  output$plot_pic <- renderPlot({

    pic <- rpic(1e06, input$mpic, input$vpic)
    p <- c(0, 0.5, 1) + c(1, 0, -1) * (1 - input$cipic) / 2
    # qpic <- quantile(pic, probs = p)
    # hist(pic, xlab = "", ylab = "", yaxt = "n", main = "")
    # abline(v = qpic, lty = 2)
    histo(pic, p)

  })

  output$plot_lag <- renderPlot({

    lag <- rlag(1e06, input$mlag, input$vlag)
    p <- c(0, 0.5, 1) + c(1, 0, -1) * (1 - input$cilag) / 2
    # qlag <- quantile(lag, probs = p)
    # hist(lag, xlab = "", ylab = "", yaxt = "n", main = "")
    # abline(v = qlag, lty = 2)
    histo(lag, p)

  })

  output$plot_los <- renderPlot({

    los <- rlos(1e06, input$mlos, input$vlos)
    p <- c(0, 0.5, 1) + c(1, 0, -1) * (1 - input$cilos) / 2
    # qlos <- quantile(los, probs = p)
    # hist(los, xlab = "", ylab = "", yaxt = "n", main = "")
    # abline(v = qlos, lty = 2)
    histo(los, p)

  })

  pars <- reactive({

    pars1 <- data.frame(
      date = input_data()$date[nrow(input_data())],
      mlam = input$mlam,
      vlam = input$vlam,
      mpic = input$mpic,
      vpic = input$vpic,
      mlag = input$mlag,
      vlag = input$vlag,
      mlos = input$mlos,
      vlos = input$vlos
    )

    rbind(pars0[1:2, ], pars1)

  })

# -------------------------------- FORECASTS -------------------------------- #

  # FORECASTS USING PRED.COVID() FUNCTION

  # Forecasts ICU beds requirements

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

    rv$pred <- pred.covid(nday = nday(), nsim = 2000, pars(),
                          input_data(), ncpu = 4)

    rv$days <- as.Date(strptime(colnames(rv$pred$nbed), format = "%d.%m.%Y"))

  })

  ###### DEBUG #######

  # output$pars <- renderTable({ pars() })
  # output$data <- renderTable({ input_data() })
  # output$pred_ntot <- renderTable({ rv$pred$ntot })
  # output$days <- renderText({ paste(as.character(rv$days), collapse=" ") })

  #####################

  output$plot_fc_ntot <- renderPlot({

    validate(need(rv$pred, ""))

    p <- c(0, 0.5, 1) + c(1, 0, -1) * (1 - input$cintot) / 2
    # days <- rv$days
    pred <- rv$pred
    # data <- input_data()
    # today <- data$date[nrow(data)]
    # qntot <- t(apply(pred$ntot,2,quantile,probs=p))
    # plot(range(days),range(qntot),type="n",xlab="",ylab="Nombre de cas")
    # title("Nombre cumulatif de cas confirmés COVID-19 dans le canton de Vaud")
    # polygon(x=c(days,rev(days)),y=c(qntot[,1],rev(qntot[,3])),col="grey",border=NA)
    # lines(days,qntot[,2],lwd=2)
    # points(data$date,data$ntot,pch=19)
    # abline(v=today,lty=2)

    plot.covid(pred, what = "ntot", prob = p)

  })

  output$plot_fc_nbed <- renderPlot({

    validate(need(rv$pred, ""))

    p <- c(0, 0.5, 1) + c(1, 0, -1) * (1 - input$cinbed) / 2
    # days <- rv$days
    pred <- rv$pred
    # data <- input_data()
    # today <- data$date[nrow(data)]
    # qnbed <- t(apply(pred$nbed,2,quantile,probs=p))
    # plot(range(days),range(qnbed),type="n",xlab="",ylab="Nombre de lits")
    # title("Nombre de lits occupés aux soins intensifs dans le canton de Vaud")
    # polygon(x=c(days,rev(days)),y=c(qnbed[,1],rev(qnbed[,3])),col="grey",border=NA)
    # lines(days,qnbed[,2],lwd=2)
    # points(data$date,data$nicu,pch=19)
    # abline(v=today,lty=2)

    plot.covid(pred, what = "nbed", prob = p)

  })

}

## RUN THE APP

shinyApp(ui, server)
