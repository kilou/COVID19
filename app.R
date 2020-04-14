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
pars0$date <- conv(pars0$date, format = "%d.%m.%Y")
pars_surv <- as.data.frame(read_xlsx("params_surv.xlsx"))


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

            h3("Import data",
              span(icon("question-circle"), id = "data_info"),
              tippy_this(
                elementId = "data_info",
                tooltip = "data_info.html" %>%
                  {readChar(., file.info(.)$size)},
                allowHTML = TRUE,
                placement = "right",
                maxWidth = "600px",
                theme = "light"
              )
            ),

            br(),

            # Input: Select a file ----
            fileInput(inputId = "file1",
                      label = "Choose data file (xlsx)",
                      multiple = FALSE,
                      accept = 
         "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"),

            ##### DEBUG #####

            # uiOutput("data_exists"),

            #################

            # Display date since...
            uiOutput("start_date_ui"),

            # Date format
            selectInput(inputId = "date_format",
                        label = "Date format",
                        choices = list(`DD.MM.YYYY` = "%d.%m.%Y",
                                       `MM.DD.YYYY` = "%m.%d.%Y",
                                       `YYYY.MM.DD` = "%Y.%m.%d",
                                       `DD-MM-YYYY` = "%d-%m-%Y",
                                       `MM-DD-YYYY` = "%m-%d-%Y",
                                       `YYYY-MM-DD` = "%Y-%m-%d",
                                       `DD/MM/YYYY` = "%d/%m/%Y",
                                       `MM/DD/YYYY` = "%m/%d/%Y",
                                       `YYYY/MM/DD` = "%Y/%m/%d"),
                        selected = "%Y-%m-%d"),

            # Input: Select number of rows to display ----
            radioButtons(inputId = "disp",
                         label = "Display",
                         choices = c(Head = "head", All = "all"),
                         selected = "all")

          ),

          column(width = 4,

            # Output: Data file ----
            tableOutput("table_data")

          ),

          column(width = 6,

            # Output: Plot of the data
            plotOutput("plot_nhos"),

            plotOutput("plot_nicu"),

            plotOutput("plot_ndead")

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
                          choices = list(egp = "egp",
                                         icp = "icp",
                                         adp = "adp",
                                         lag = "lag",
                                         los = "los")),

              uiOutput("sdate_ui"),

              numericInput(inputId = "pipar",
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

              uiOutput("sim_start_date_ui")

            ),

            column(2,

              uiOutput("sim_end_date_ui")

            ),

            column(1, ),

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
          # uiOutput("pred_ndead_cumul"),
          # uiOutput("is_fc_ndead"),

          #################

            column(3,

              uiOutput("pinhos_ui")

            ),

            column(3,

              uiOutput("pinbed_ui")

            ),

            column(3,

              uiOutput("pindead_ui")

            )

          ),

          fluidRow(

            plotOutput("plot_fc_nhos")

          ),

          fluidRow(

            plotOutput("plot_fc_nbed")

          ),

          fluidRow(

            plotOutput("plot_fc_ndead")

          )

        )

      ),

# ---------------------------------- ABOUT ---------------------------------- #

      tabPanel("About",

        fluidPage(

          br(),

          div(p(strong("Creators:"), "Aziz Chaouch, Yves Eggli,",
                "Jérôme Pasquier,", "Valentin Rousson and Bastien Trächsel"), 
              p(strong("R Packages:"), paste(pkg_list, collapse = ", ")),
              p(strong("Soure code:"),
                a("https://github.com/kilou/COVID19",
                  href="https://github.com/kilou/COVID19")),
              p(strong("Contact address:"),
                a("jerome.pasquier@unisante.ch",
                  href="mailto:jerome.pasquier@unisante.ch")),
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

    tryCatch(
      {
        df <- import.covid(
          input.file = input$file1$datapath,
          date.format = input$date_format
        )
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )

    return(df)

  })

  # Is data ready?
  rv$is_data <- FALSE
  observe({
    req(input_data())
    rv$is_data <- TRUE
  })

  # Check if data contain a `ndead` column
  rv$is_ndead <- FALSE
  observe({
    req(input_data())
    rv$is_ndead <- any(names(input_data()) == "ndead")
  })

  # Correct date format
  observe(

    if (rv$is_data) {

      if (any(is.na(input_data()$date))) {

        showModal(modalDialog(
          title = "Fail to read the date",
          "Check the date format.",
          easyClose = TRUE,
          footer = NULL
        ))

      }

    }

  )

  # Start date
  output$start_date_ui <- renderUI({

    val <- as.Date("2020-02-25")
    if  (val < min(input_data()$date) | val > max(input_data()$date)) {
      val <-  min(input_data()$date)
    }
    dateInput(inputId = "start_date",
                        label = "Display data since...",
                        value = val,
                        min = min(input_data()$date),
                        max = max(input_data()$date),
                        format = "yyyy-mm-dd")

  })

  data <- reactive({

    subset(input_data(), date >= input$start_date)

  })

  output$table_data <- renderTable({

    tbl <- data()

    tbl$date <- as.character(tbl$date)

    if (rv$is_ndead) {
      names(tbl) <- c("Date", "Hospital (cumul.)", "ICU (current)",
                      "Deaths (daily)")
    } else {
      names(tbl) <- c("Date", "Hospital (cumul.)", "ICU (current)")
    }

    if(input$disp == "head") {
      return(head(tbl))
    }
    else {
      return(tbl)
    }

  })

  output$plot_nhos <- renderPlot({

    data() %>%
      ggplot(aes(x = date, y = nhos)) +
      geom_col(fill = "#428bca") +
      theme_minimal() +
      labs(x = "Date", y = "Cumulative cases")

  })

  output$plot_nicu <- renderPlot({

    data() %>%
      ggplot(aes(x = date, y = nicu)) +
      geom_col(fill = "#428bca") +
      theme_minimal() +
      labs(x = "Date", y = "N ICU")

  })

  output$plot_ndead <- renderPlot({

    if (rv$is_ndead) {
      data() %>%
        mutate(ndead = cumsum(ndead)) %>%
        ggplot(aes(x = date, y = ndead)) +
        geom_col(fill = "#428bca") +
        theme_minimal() +
        labs(x = "Date", y = "Cumulative deaths")
    }

  })

# ------------------------------- PARAMETERS -------------------------------- #

  pars <- reactive({

    if (is.null(input$pars_tbl)) {
      DF = pars0
    } else {
      DF = hot_to_r(input$pars_tbl)
      DF$date <- as.Date(DF$date, format = "%Y-%m-%d")
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
        rhandsontable(mutate(pars(), date = as.character(date)),
                      useTypes = TRUE, stretchH = "all",
                      col_err_lag = col_err_lag, 
                      row_err_lag = row_err_lag,
                      col_err_los = col_err_los, 
                      row_err_los = row_err_los) %>%
          hot_col(col = "date", dateFormat = "YYYY-MM-DD", type = "date") %>%
          hot_validate_numeric(cols = "megp", min = 1) %>%
          hot_validate_numeric(cols = "vegp", min = 0) %>%
          hot_validate_numeric(cols = "micp", min = 0, max = 1) %>%
          hot_validate_numeric(cols = "vicp", min = 0) %>%
          hot_validate_numeric(cols = "madp", min = 0, max = 1) %>%
          hot_validate_numeric(cols = "vadp", min = 0) %>%
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
              }
              if (instance.params && hclag.includes(col) &&
                    hrlag.includes(row)) {
                td.style.background = 'red';
              }
              if (instance.params && hclos.includes(col) &&
                    hrlos.includes(row)) {
                td.style.background = 'red';
              }
            }")

      }

  }) %>% debounce(1000)

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

  rv$sdate <- min(pars0$date)

  output$sdate_ui <- renderUI({

    selectInput(inputId = "sdate",
                label = "Date",
                choices = pars()$date,
                selected = rv$sdate)

  })

  observe({

    if (!(rv$sdate %in% pars()$date)) {
      rv$sdate <- min(pars()$date)
    }

  })

  output$par_hist <- renderPlot({

    req(input$sdate)

    p <- input$spar
    d <- input$sdate
    i <- pars()$date %>% {!is.na(.) & . == d}
    mv <- pars()[i, paste0(c("m", "v"), p)]
    v <- get(paste0("r", p))(1e06, mv[, 1], mv[, 2])
    q <- c(0, 0.5, 1) + c(1, 0, -1) * (1 - input$pipar) / 2
    validate(need(v, ""))
    histo(v, q)

  })

  output$par_evol <- renderPlotly({

    validate(need(pars(), ""))
    p <- paste0("m", input$spar)
    i <- 1
    if (rv$is_data & p == "megp") {
      b <- pars()$date <= max(data()$date)
      if (any(b)) i <- max(which(b))
    }
    plt <- ggplot(pars()[i:nrow(pars()), ], aes_string(x = "date", y = p)) +
      geom_point() +
      geom_step()
    ggplotly(plt)

  })

# -------------------------------- FORECASTS -------------------------------- #

  output$sim_start_date_ui <- renderUI({

    d1 <- min(data()$date)
    d2 <- max(data()$date)
    dateInput(inputId = "sim_start_date",
                        label = "Prediction from...",
                        value = d2 + 1,
                        min = d1 + 1,
                        max = d2 + 1,
                        format = "yyyy-mm-dd")

  })

  # data used for prediction
  data_p <- reactive({

    subset(data(), date < input$sim_start_date)

  })

  output$sim_end_date_ui <- renderUI({

    dm <- max(data()$date)
    dateInput(inputId = "sim_end_date",
                        label = "Prediction until...",
                        value = dm + 7,
                        min = dm + 1,
                        max = NULL,
                        format = "yyyy-mm-dd")

  })

  nday <- reactive({

    input$sim_end_date - max(data_p()$date)

  })

  observeEvent(input$submit, {

    miss_param_0  <- any(is.na(pars()[c("date", "micp", "vicp", "madp", "vadp",
                                        "mlag", "vlag", "mlos", "vlos")]))

    i <- 1
    if (rv$is_data) {
      b <- pars()$date <= max(data_p()$date)
      if (any(b)) i <- max(which(b))
    }
    miss_param_1 <- any(is.na(pars()[i:nrow(pars()), c("megp", "vegp")]))

    if (!rv$is_data) {

      showModal(modalDialog(
        title = "Data not ready",
        "",
        easyClose = TRUE,
        footer = NULL
      ))

      return()

    }

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

    rv$pred <- pred.covid(nday = nday(), nsim = 1000, pars(), pars_surv,
                          data_p(), ncpu = 8)

    rv$days <- as.Date(strptime(colnames(rv$pred$nbed), format = "%d.%m.%Y"))

    remove_modal_spinner()

  })

  # Check if pred.covid outputs a `ndead` column
  rv$is_fc_ndead <- FALSE
  observe({
    req(rv$pred)
    rv$is_fc_ndead <- !is.null(rv$pred$ndead_cumul)
  })


  ###### DEBUG #######

  # output$pars <- renderTable({ pars() })
  # output$data <- renderTable({ data() })
  # output$days <- renderText({ paste(as.character(rv$days), collapse=" ") })
  # output$pred_nhos <- renderTable({ rv$pred$nhos })
  # output$pred_ndead_cumul <- renderTable({ rv$pred$ndead_cumul })
  # output$is_fc_ndead <- renderText({ as.character(rv$is_fc_ndead) })

  #####################

  output$pinhos_ui <- renderUI({

    req(rv$pred)

    numericInput(inputId = "pinhos",
                 label = paste("PI length for cumulative counts of",
                               "hospitalized patients"),
                 value = 0.9,
                 min = 0,
                 max = 1,
                 step = 0.05)

  })

  output$pinbed_ui <- renderUI({

    req(rv$pred)

    numericInput(inputId = "pinbed",
                 label = "PI length for number of occupied beds in ICUs",
                 value = 0.9,
                 min = 0,
                 max = 1,
                 step = 0.05)

  })

  output$pindead_ui <- renderUI({

    if (rv$is_fc_ndead) {

      req(rv$pred)

      numericInput(inputId = "pindead",
                   label = "PI length for cumulative counts of deaths",
                   value = 0.9,
                   min = 0,
                   max = 1,
                   step = 0.05)

    }

  })

  output$plot_fc_nhos <- renderPlot({

    validate(need(rv$pred, ""), need(input$pinhos, ""))
    p <- c(0, 0.5, 1) + c(1, 0, -1) * (1 - input$pinhos) / 2
    pred2 <- rv$pred
    plot.covid(pred2, what = "nhos", prob = p)

  })

  output$plot_fc_nbed <- renderPlot({

    validate(need(rv$pred, ""), need(input$pinbed, ""))
    p <- c(0, 0.5, 1) + c(1, 0, -1) * (1 - input$pinbed) / 2
    pred <- rv$pred
    plot.covid(pred, what = "nbed", prob = p)

  })

  output$plot_fc_ndead <- renderPlot({

    if (rv$is_fc_ndead) {

      validate(need(rv$pred, ""), need(input$pindead, ""))
      p <- c(0, 0.5, 1) + c(1, 0, -1) * (1 - input$pindead) / 2
      pred <- rv$pred
      plot.covid(pred, what = "ndead_cumul", prob = p)

    } else {

      return(NULL)

    }

  })

  fc_table <- reactive({

    p <- c(0.5, 0, 1) + c(0, 1, -1) * (1 - input$pinhos) / 2
    nhos <- round(t(apply(rv$pred$nhos, 2, quantile, prob = p)))
    colnames(nhos) <- c("nhos", colnames(nhos)[2:3])
    nhos <- cbind(date = as.Date(rownames(nhos), format = "%d.%m.%Y"),
                  as.data.frame(nhos, check.names = FALSE))

    p <- c(0.5, 0, 1) + c(0, 1, -1) * (1 - input$pinbed) / 2
    nbed <- round(t(apply(rv$pred$nbed, 2, quantile, prob = p)))
    colnames(nbed) <- c("nbed", colnames(nbed)[2:3])
    nbed <- cbind(date = as.Date(rownames(nbed), format = "%d.%m.%Y"),
                  as.data.frame(nbed, check.names = FALSE))

    if (rv$is_fc_ndead) {

      p <- c(0.5, 0, 1) + c(0, 1, -1) * (1 - input$pindead) / 2
      ndead <- round(t(apply(rv$pred$ndead_cumul, 2, quantile, prob = p)))
      colnames(ndead) <- c("ndead", colnames(ndead)[2:3])
      ndead <- cbind(date = as.Date(rownames(ndead), format = "%d.%m.%Y"),
                    as.data.frame(ndead, check.names = FALSE))

      r <- list(nhos = nhos, nbed = nbed, ndead = ndead,
                pars = pars(), data = data_p())

    } else {

      r <- list(nhos = nhos, nbed = nbed, pars = pars(), data = data_p())

    }

    return(r)

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
