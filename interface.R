
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
pars0 <- read.csv("params.csv")
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

      # tabPanel("Estimation",

        # fluidPage(

          # fluidRow(

            # column(width = 3, 

              # radioButtons(inputId = "predint",
                           # label = "",
                           # choices = paste(c("without", "with"),
                                           # "prediction interval"),
                           # selected = "without prediction interval"),


              # numericInput(inputId = "predint_length",
                           # label = "Prediction interval length",
                           # value = 0.5,
                           # min = 0,
                           # max = 1,
                           # step = 0.05)
            # ),

            # column(width = 3,

              # br(),

              # uiOutput("date_max_ui")

            # )
 
          # ),

          # fluidRow(

            # plotOutput("plot_predicted_new_cases",
                       # width = "100%", height = "350px")

          # )

        # )

      # ),

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
                               value = 1.25,
                               min = 1,
                               max = Inf,
                               step = 0.01)

                ),

                column(6,

                  sliderInput(inputId = "vlam",
                              label = "Variance",
                              value = 0.05,
                              min = 0,
                              max = 0.1)

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

                plotOutput("plot_lam")

              )

            ),

# ------------------------------ PIC PARAMETER ------------------------------ #

            column(6,

              h3("Proportion of incident cases that will require IC",
                 align = "center"),

              fluidRow(

                column(3,

                  numericInput(inputId = "mpic",
                               label = "Value",
                               value = 0.15,
                               min = 0,
                               max = 1,
                               step = 0.01)

                ),

                column(6,

                  sliderInput(inputId = "vpic",
                              label = "Variance",
                              value = 0.05,
                              min = 0,
                              max = 0.1)

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

                plotOutput("plot_pic")

              )

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
                               value = 8,
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

                plotOutput("plot_lag")

              )

            ),

# ------------------------------ LOS PARAMETER ------------------------------ #

            column(6,

              h3("Length of stay", align = "center"),

              fluidRow(

                column(3,

                  numericInput(inputId = "mlos",
                               label = "Value",
                               value = 9,
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

                plotOutput("plot_los")

              )

            )

          )

        )

      ),

# -------------------------------- FORECASTS -------------------------------- #

      tabPanel("Forcasts",

        fluidPage(

          br(),

          fluidRow(

            actionButton("submit", "Compute forcasts")

          ),

          br(),

          fluidRow(

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


          #uiOutput("pars"),
          #uiOutput("data"),
          #uiOutput("days"),
          #uiOutput("pred_ntot")


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
        if (input$file1$type %in% c("text/csv",
                                    "text/comma-separated-values,text/plain")) {
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
      geom_col(alpha = 0.9) +
      theme_minimal() +
      labs(x = "Date", y = "Cumulative cases")

  })

  output$plot_nicu <- renderPlot({

    input_data() %>%
      ggplot(aes(x = date, y = nicu)) +
      geom_col(alpha = 0.9) +
      theme_minimal() +
      labs(x = "Date", y = "N ICU")

  })

# ------------------------------- PARAMETERS -------------------------------- #

  output$vlag_ui <- renderUI({

    sliderInput(inputId = "vlag",
                label = "Variance",
                value = 2 * input$mlag,
                min = input$mlag,
                max = 3 * input$mlag)

  })

  output$vlos_ui <- renderUI({

    sliderInput(inputId = "vlos",
                label = "Variance",
                value = 2 * input$mlos,
                min = input$mlos,
                max = 3 * input$mlos)

  })

  output$plot_lam <- renderPlot({

    lam <- rlam(1e06, input$mlam, input$vlam)
    p <- 0:1 + c(1, -1) * (1 - input$cilam) / 2
    qlam <- quantile(lam, probs = p)
    hist(lam, xlab = "Value", ylab = "", yaxt = "n",
         main = "Distribution of the exponential growth parameter")
    abline(v = qlam, lty = 2)

  })

  output$plot_pic <- renderPlot({

    pic <- rpic(1e06, input$mpic, input$vpic)
    p <- 0:1 + c(1, -1) * (1 - input$cipic) / 2
    qpic <- quantile(pic, probs = p)
    hist(pic, xlab = "Value", ylab = "", yaxt = "n",
         main = "Distribution of the proportion...")
    abline(v = qpic, lty = 2)

  })

  output$plot_lag <- renderPlot({

    lag <- rlag(1e06, input$mlag, input$vlag)
    p <- 0:1 + c(1, -1) * (1 - input$cilag) / 2
    qlag <- quantile(lag, probs = p)
    hist(lag, xlab = "Value", ylab = "", yaxt = "n",
         main = "Distribution of the proportion...")
    abline(v = qlag, lty = 2)

  })

  output$plot_los <- renderPlot({

    los <- rlos(1e06, input$mlos, input$vlos)
    p <- 0:1 + c(1, -1) * (1 - input$cilos) / 2
    qlos <- quantile(los, probs = p)
    hist(los, xlab = "Value", ylab = "", yaxt = "n",
         main = "Distribution of the proportion...")
    abline(v = qlos, lty = 2)

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

    rbind(pars0, pars1)

  })

# -------------------------------- FORECASTS -------------------------------- #

  # FORECASTS USING PRED.COVID() FUNCTION

  # Forecasts ICU beds requirements

  rv <- reactiveValues(
   # pred=list(
     # ntot=matrix(1, nrow = 3, ncol = 3),
     # ninc=matrix(1, nrow = 3, ncol = 3),
     # nicu=matrix(1, nrow = 3, ncol = 3),
     # nbed=matrix(1, nrow = 3, ncol = 3)
   # )
  )

  observeEvent(input$submit, {

    rv$pred <- pred.covid(nday = 7, nsim = 2000, pars(), input_data(), ncpu=4)

    rv$days <- as.Date(strptime(colnames(rv$pred$nbed), format = "%d.%m.%Y"))

  })

  output$pars <- renderTable({ pars() })

  output$data <- renderTable({ input_data() })

  output$pred_ntot <- renderTable({ rv$pred$ntot })

  output$days <- renderText({ paste(as.character(rv$days), collapse=" ") })

  output$plot_fc_ntot <- renderPlot({

    p <- c(0, 0.5, 1) + c(1, 0, -1) * (1 - input$cintot) / 2
    days <- rv$days
    pred <- rv$pred
    data <- input_data()
    today <- data$date[nrow(data)]
    qntot <- t(apply(pred$ntot,2,quantile,probs=p))
    plot(range(days),range(qntot),type="n",xlab="",ylab="Nombre de cas")
    title("Nombre cumulatif de cas confirmés COVID-19 dans le canton de Vaud")
    polygon(x=c(days,rev(days)),y=c(qntot[,1],rev(qntot[,3])),col="grey",border=NA)
    lines(days,qntot[,2],lwd=2)
    points(data$date,data$ntot,pch=19)
    abline(v=today,lty=2)

  })

  output$plot_fc_nbed <- renderPlot({

    p <- c(0, 0.5, 1) + c(1, 0, -1) * (1 - input$cinbed) / 2
    days <- rv$days
    pred <- rv$pred
    data <- input_data()
    today <- data$date[nrow(data)]
    qnbed <- t(apply(pred$nbed,2,quantile,probs=p))
    plot(range(days),range(qnbed),type="n",xlab="",ylab="Nombre de lits")
    title("Nombre de lits occupés aux soins intensifs dans le canton de Vaud")
    polygon(x=c(days,rev(days)),y=c(qnbed[,1],rev(qnbed[,3])),col="grey",border=NA)
    lines(days,qnbed[,2],lwd=2)
    points(data$date,data$nicu,pch=19)
    abline(v=today,lty=2)

  })



# # Plot nb ICU beds required
# qnbed <- t(apply(pred$nbed,2,quantile,probs=p))
# plot(range(days),range(qnbed),type="n",xlab="",ylab="Nombre de lits")
# title("Nombre de lits occupés aux soins intensifs dans le canton de Vaud")
# polygon(x=c(days,rev(days)),y=c(qnbed[,1],rev(qnbed[,3])),col="grey",border=NA)
# lines(days,qnbed[,2],lwd=2)
# points(data$date,data$nicu,pch=19)
# abline(v=today,lty=2)



  # output$date_max_ui <- renderUI({
    # dm <- max(input_data()$date) + 1
    # dateInput(inputId = "date_max",
                        # label = "Prediction until ...",
                        # value = dm,
                        # min = dm,
                        # max = NULL,
                        # format = "yyyy-mm-dd")
  # })

  # predicted_new_cases_matrix <- reactive({

    # nc <- input_data()
    # fit <- lm(log(new_cases) ~ date, data = nc)
    # dm <- input$date_max
    # new_dates <- seq(max(nc$date) + 1, by = 1, len = dm - max(nc$date))
    # n <- predict(fit, data.frame(date = new_dates))
    # nsim = 100
    # M <- exp(t(sapply(n, function(z)
      # rnorm(n = nsim, mean = z, sd = abs(z)/10)
    # )))
    # attr(M, "dates") <- new_dates
    # return(M)

  # })

  # output$plot_predicted_new_cases <- renderPlot({

    # PI_length <- input$predint_length
    # M <- predicted_new_cases_matrix()

    # PI <- t(apply(M, 1, quantile, 0:1 + c(1, -1) * (1 - PI_length) / 2))
    # colnames(PI) <- c("lwr", "upr")

    # predicted_new_cases <- data.frame(
      # date = attr(M, "dates"),
      # new_cases = round(apply(M, 1, median))
    # )
    # predicted_new_cases <- cbind(predicted_new_cases, PI)

    # all_new_cases <- bind_rows(
      # cbind(input_data(), value_type = "observed value"),
      # cbind(predicted_new_cases, value_type = "predicted value")
    # )

    # plt <- all_new_cases %>%
      # ggplot(aes(x = date, y = new_cases, fill = value_type)) +
      # geom_col(alpha = 2/3) +
      # scale_fill_manual(values = c("dodgerblue", "red")) +
      # theme_minimal() +
      # theme(legend.position = "top", legend.title=element_blank()) +
      # labs(x = "Date", y = "New cases")

    # if (input$predint == "with prediction interval") {
      # plt <- plt +
        # geom_errorbar(aes(ymin=lwr, ymax=upr), width=.2,
                      # position=position_dodge(.9))
    # }

    # return(plt)

  # })

}

## RUN THE APP

shinyApp(ui, server)
