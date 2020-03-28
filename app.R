rm(list = ls())

reqpack <- function(X) {
  pak_failed <- which(!unlist(lapply(X, require, character.only = TRUE)))
  if(length(pak_failed)>=1){
    install.packages(X[pak_failed], repos = "https://stat.ethz.ch/CRAN/")
    lapply(X[pak_failed], require, character.only = TRUE)
  }
}
pkg_list <- c("dplyr",
              "ggplot2",
              "plotly",
              "readxl",
              "rhandsontable",
              "shiny",
              "shinybusy",
              "snowfall",
              "tippy",
              "writexl")
reqpack(pkg_list)

options(stringsAsFactors = FALSE)

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

            ##### DEBUG #####

            uiOutput("data_exists"),

            #################

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

# -------------------------------- PARMETERS -------------------------------- #

      tabPanel("Parameters",

        fluidPage(

          h3("Parameters",
            span(icon("question-circle"), id = "pars_info"),
            tippy_this(
              elementId = "pars_info",
              tooltip = "pars_info.html" %>% {readChar(., file.info(.)$size)},
              allowHTML = TRUE,
              placement = "right",
              maxWidth = "none",
              theme = "light"
            )
          ),

          rHandsontableOutput("pars_tbl"),

          h3("Visualization"),

          fluidRow(

            column(2,

              selectInput(inputId = "spar",
                          label = "Parameter",
                          choices = list(lam = "lam",
                                         pic = "pic",
                                         lag = "lag",
                                         los = "los")),

              uiOutput("sdate_ui"),

              numericInput(inputId = "cipar",
                                   label = "Interval length",
                                   value = 0.9,
                                   min = 0,
                                   max = 1,
                                   step = 0.05)

            ),

            column(5,

              plotOutput("par_hist")

            ),

            column(5,

              plotlyOutput("par_evol")

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
          # uiOutput("pred_nhos"),

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
              p(strong("R Packages:"), paste(pkg_list, collapse = ", ")),
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

  # Reactive values
  rv <- reactiveValues()

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

  # Is data ready?
  rv$is_data_ready <- FALSE
  observe({
    req(input_data())
    rv$is_data_ready <- TRUE
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

  pars <- reactive({

    if (is.null(input$pars_tbl)) {
      DF = pars0
    } else {
      DF = hot_to_r(input$pars_tbl)
    }
    if (all(!is.na(DF$date))) {
      DF <- DF %>% arrange(date)
    }
    DF

  })

  output$pars_tbl <- renderRHandsontable({

      if (!is.null(pars())) {

        col_err_lag <- which(names(pars()) %in% c("mlag", "vlag")) - 1
        row_err_lag <- which(pars()$vlag < pars()$mlag) - 1
        col_err_los <- which(names(pars()) %in% c("mlos", "vlos")) - 1
        row_err_los <- which(pars()$vlos < pars()$mlos) - 1
        col_dis_lam <- which(names(pars()) %in% c("mlam", "vlam")) - 1
        row_dis_lam <- as.integer(c())
        if (rv$is_data_ready) {
          if (nrow(input_data()) > 1) {
            row_dis_lam <-
              0:(max(which(pars()$date <= max(input_data()$date))) - 2)
          }
        }
        rhandsontable(pars(), useTypes = TRUE, stretchH = "all",
                      col_err_lag = col_err_lag, 
                      row_err_lag = row_err_lag,
                      col_err_los = col_err_los, 
                      row_err_los = row_err_los,
                      col_dis_lam = col_dis_lam, 
                      row_dis_lam = row_dis_lam) %>%
          hot_validate_numeric(cols = "mlam", min = 1) %>%
          hot_validate_numeric(cols = "vlam", min = 0) %>%
          hot_validate_numeric(cols = "mpic", min = 0, max = 1) %>%
          hot_validate_numeric(cols = "vpic", min = 0, max = 1) %>%
          hot_validate_numeric(cols = "mlag", min = 0) %>%
          hot_validate_numeric(cols = "vlag", min = 0) %>%
          hot_validate_numeric(cols = "mlos", min = 0) %>%
          hot_validate_numeric(cols = "vlos", min = 0) %>%
          hot_cols(renderer = "
            function(instance, td, row, col, prop, value, cellProperties) {
              Handsontable.renderers.NumericRenderer.apply(this, arguments);
              if (instance.params) {
                  hclag = instance.params.col_err_lag
                  hclag = hclag instanceof Array ? hclag : [hclag]
                  hrlag = instance.params.row_err_lag
                  hrlag = hrlag instanceof Array ? hrlag : [hrlag]
                  hclos = instance.params.col_err_los
                  hclos = hclos instanceof Array ? hclos : [hclos]
                  hrlos = instance.params.row_err_los
                  hrlos = hrlos instanceof Array ? hrlos : [hrlos]
                  hclam = instance.params.col_dis_lam
                  hclam = hclam instanceof Array ? hclam : [hclam]
                  hrlam = instance.params.row_dis_lam
                  hrlam = hrlam instanceof Array ? hrlam : [hrlam]
              }
              if (instance.params && hclag.includes(col) &&
                    hrlag.includes(row)) {
                td.style.background = 'red';
              }
              if (instance.params && hclos.includes(col) &&
                    hrlos.includes(row)) {
                td.style.background = 'red';
              }
              if (instance.params && hclam.includes(col) &&
                    hrlam.includes(row)) {
                td.style.background = 'lightgrey';
              }
            }")

      }

  })

  observe({

    if(any(pars()$vlag < pars()$mlag, na.rm = TRUE)) {

      showModal(modalDialog(
        title = "Error in lag parameter",
        "The variance has to be equal or greater to the parmeter",
        easyClose = TRUE,
        footer = NULL
      ))

    }

  })

  observe({

    if(any(pars()$vlos < pars()$mlos, na.rm = TRUE)) {

      showModal(modalDialog(
        title = "Error in los parameter",
        "The variance has to be equal or greater to the parmeter",
        easyClose = TRUE,
        footer = NULL
      ))

    }

  })

  output$sdate_ui <- renderUI({

    s <- NULL
    if (rv$is_data_ready) {
      b <- pars()$date <= max(input_data()$date)
      if (any(b)) s <- pars()$date[max(which(b))]
    }

    selectInput(inputId = "sdate",
                label = "Date",
                choices = pars()$date,
                selected = s)

  })

  output$par_hist <- renderPlot({

    p <- input$spar
    d <- input$sdate
    i <- pars()$date %>% {!is.na(.) & . == d}
    mv <- pars()[i, paste0(c("m", "v"), p)]
    v <- get(paste0("r", p))(1e06, mv[, 1], mv[, 2])
    q <- c(0, 0.5, 1) + c(1, 0, -1) * (1 - input$cipar) / 2
    validate(need(v, ""))
    histo(v, q)

  })

  output$par_evol <- renderPlotly({

    validate(need(pars(), ""))
    p <- paste0("m", input$spar)
    i <- 1
    if (rv$is_data_ready & p == "mlam") {
      b <- pars()$date <= max(input_data()$date)
      if (any(b)) i <- max(which(b))
    }
    plt <- ggplot(pars()[i:nrow(pars()), ], aes_string(x = "date", y = p)) +
      geom_point() +
      geom_step()
    ggplotly(plt)

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

  nday <- reactive({

    input$date_max - max(input_data()$date)

  })

  observeEvent(input$submit, {

    miss_param_0  <- any(is.na(pars()[c("date", "mpic", "vpic", "mlag",
                                        "vlag", "mlos", "vlos")]))
    i <- 1
    if (rv$is_data_ready) {
      b <- pars()$date <= max(input_data()$date)
      if (any(b)) i <- max(which(b))
    }
    miss_param_1 <- any(is.na(pars()[i:nrow(pars()), c("mlam", "vlam")]))

    if (miss_param_0 | miss_param_1) {

      showModal(modalDialog(
        title = "Error in parameter matrix",
        "The parameter matrix contains missing values",
        easyClose = TRUE,
        footer = NULL
      ))

      return()

    }

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
    list(nhos = nhos, nbed = nbed, pars = pars())

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

}

# =========================================================================== #
#                                                                             #
#                               RUN THE APP                                   #
#                                                                             #
# =========================================================================== #

shinyApp(ui, server)
