metarShinyApp <- function() {

  if (!requireNamespace("shiny", quietly = T) | !requireNamespace("shinyFiles", quietly = T)) {
    stop("Packages \"shiny\" and \"shinyFiles\" are not installed.", call. = F)
  }

  require(data.table)
  require(shiny)
  require(shinyjs)
  require(shinyWidgets)
  require(shinyFiles)
  require(DT)

  input_type_options <- c("CSV", "Text", "Raw")

  read_file <- function(input_file, file_type) {

    if (file_type == "CSV") {

      # Try to find a METAR column, otherwise combine all columns
      raw_metar <- fread(file = input_file)
      found_metar_column <- F
      for (col in names(raw_metar)) {
        if (is.character(raw_metar[[col]][1])) {
          identify_tokens <- table(sapply(unlist(strsplit(raw_metar[[col]][1], " ")), identify_METAR_token))
          if (length(identify_tokens) > 1) {
            raw_metar <- raw_metar[[col]]
            found_metar_column <- T
            break
          }
        }
      }
      if (!found_metar_column) {
        raw_metar <- apply(raw_metar, 1, function(x) paste(x, collapse = " "))
      }

    } else if (file_type == "Text") {
      raw_metar <- readLines(input_file)
    } else if (file_type == "Raw") {
      raw_metar <- unlist(strsplit(input_file, "\n"))
    }

    return(parse_METAR(raw_metar))

  }

  datatable_customised <- function(
    data,
    rownames = F,
    selection = "none",
    style = "bootstrap4",
    options = list(
      pageLength = 15,
      lengthMenu = seq(5, 100, 5),
      columnDefs = list(list(className = 'dt-center', targets = "_all")),
      scrollX = T,
      dom = '<"dataTables_row"lBf>rt<"dataTables_row"ip>',
      buttons = c('copy', 'csv', 'excel')
    ),
    extensions = c("Buttons"),
    ...
  ){
    datatable(
      data = data,
      rownames = rownames,
      selection = selection,
      style = style,
      options = options,
      extensions = extensions,
      ...
    )
  }

  ui <- fluidPage(

    useShinyjs(),

    tags$head(
      tags$style(HTML("
        body {
          color: #333;
          background-color: #FFF;
        }
        body.dark-theme {
          color: #eee;
          background-color: #121212;
        }
        .modal-content {
          color: #333;
          background-color: #FFF;
        }
        .modal-content.dark-theme {
          color: #eee;
          background-color: #121212;
        }
        #output_table {
          background-color: #f9f9f9;
        }
        #output_table.dark-theme {
          background-color: #121212;
        }
        .spinner {
          color: #ffffff;
          font-size: 90px;
          text-indent: -9999em;
          overflow: hidden;
          width: 1em;
          height: 1em;
          margin-left: calc(50vw - 0.5em);
          margin-top: calc(10vh - 0.5em);
          border-radius: 50%;
          position: relative;
          -webkit-transform: translateZ(0);
          -ms-transform: translateZ(0);
          transform: translateZ(0);
          -webkit-animation: load6 1.7s infinite ease, round 1.7s infinite ease;
          animation: load6 1.7s infinite ease, round 1.7s infinite ease;
        }
        @-webkit-keyframes load6 {
          0% {
            box-shadow: 0 -0.83em 0 -0.4em, 0 -0.83em 0 -0.42em, 0 -0.83em 0 -0.44em, 0 -0.83em 0 -0.46em, 0 -0.83em 0 -0.477em;
          }
          5%,
          95% {
            box-shadow: 0 -0.83em 0 -0.4em, 0 -0.83em 0 -0.42em, 0 -0.83em 0 -0.44em, 0 -0.83em 0 -0.46em, 0 -0.83em 0 -0.477em;
          }
          10%,
          59% {
            box-shadow: 0 -0.83em 0 -0.4em, -0.087em -0.825em 0 -0.42em, -0.173em -0.812em 0 -0.44em, -0.256em -0.789em 0 -0.46em, -0.297em -0.775em 0 -0.477em;
          }
          20% {
            box-shadow: 0 -0.83em 0 -0.4em, -0.338em -0.758em 0 -0.42em, -0.555em -0.617em 0 -0.44em, -0.671em -0.488em 0 -0.46em, -0.749em -0.34em 0 -0.477em;
          }
          38% {
            box-shadow: 0 -0.83em 0 -0.4em, -0.377em -0.74em 0 -0.42em, -0.645em -0.522em 0 -0.44em, -0.775em -0.297em 0 -0.46em, -0.82em -0.09em 0 -0.477em;
          }
          100% {
            box-shadow: 0 -0.83em 0 -0.4em, 0 -0.83em 0 -0.42em, 0 -0.83em 0 -0.44em, 0 -0.83em 0 -0.46em, 0 -0.83em 0 -0.477em;
          }
        }
        @keyframes load6 {
          0% {
            box-shadow: 0 -0.83em 0 -0.4em, 0 -0.83em 0 -0.42em, 0 -0.83em 0 -0.44em, 0 -0.83em 0 -0.46em, 0 -0.83em 0 -0.477em;
          }
          5%,
          95% {
            box-shadow: 0 -0.83em 0 -0.4em, 0 -0.83em 0 -0.42em, 0 -0.83em 0 -0.44em, 0 -0.83em 0 -0.46em, 0 -0.83em 0 -0.477em;
          }
          10%,
          59% {
            box-shadow: 0 -0.83em 0 -0.4em, -0.087em -0.825em 0 -0.42em, -0.173em -0.812em 0 -0.44em, -0.256em -0.789em 0 -0.46em, -0.297em -0.775em 0 -0.477em;
          }
          20% {
            box-shadow: 0 -0.83em 0 -0.4em, -0.338em -0.758em 0 -0.42em, -0.555em -0.617em 0 -0.44em, -0.671em -0.488em 0 -0.46em, -0.749em -0.34em 0 -0.477em;
          }
          38% {
            box-shadow: 0 -0.83em 0 -0.4em, -0.377em -0.74em 0 -0.42em, -0.645em -0.522em 0 -0.44em, -0.775em -0.297em 0 -0.46em, -0.82em -0.09em 0 -0.477em;
          }
          100% {
            box-shadow: 0 -0.83em 0 -0.4em, 0 -0.83em 0 -0.42em, 0 -0.83em 0 -0.44em, 0 -0.83em 0 -0.46em, 0 -0.83em 0 -0.477em;
          }
        }
        @-webkit-keyframes round {
          0% {
            -webkit-transform: rotate(0deg);
            transform: rotate(0deg);
          }
          100% {
            -webkit-transform: rotate(360deg);
            transform: rotate(360deg);
          }
        }
        @keyframes round {
          0% {
            -webkit-transform: rotate(0deg);
            transform: rotate(0deg);
          }
          100% {
            -webkit-transform: rotate(360deg);
            transform: rotate(360deg);
          }
        }
        #spinner_wrapper {
          position: fixed;
          left: 0;
          top: 0;
          width: 100vw;
          height: 100vh;
          display: flex;
          justify-content: center;
          flex-direction: column;
          text-align: center;
        	color: #FFFFFF;
        	background-color: #555759;
        	animation: fadeIn 0.5s linear 0.5s forwards alternate;
          z-index: 10000;
          visibility: hidden;
        }
        @keyframes fadeIn {
          0% {
            visibility: visible;
            opacity: 0;
          }
          100% {
            visibility: visible;
            opacity: 0.5;
          }
        }
        .dataTables_row {
          display: flex;
          justify-content: space-between;
        }
      "))
    ),
    tags$body(
      class = "dark-theme || light-theme",
      div(
        style = "position: absolute; padding-top: 12px;",
        materialSwitch("toggle_theme", "Dark Mode", T, status = "primary")
      ),
      div(
        style = "text-align: center;",
        h2("METAR Shiny App"),
        radioGroupButtons("input_type", "Input Type", input_type_options, selected = "CSV"),
        uiOutput("input_ui"),
        actionButton("metar_process", "Process METAR")
      ),
      div(style = "height: 30px;"),
      uiOutput("output_ui"),
      conditionalPanel(
        condition = "$('html').hasClass('shiny-busy')",
        id = "spinner_wrapper",
        htmltools::HTML('<div class="spinner"></div>')
      )
    )
  )

  server <- function(input, output, session) {

    output$input_ui <- renderUI({
      if (input$input_type == "Raw") {
        div(
          style = "display: flex; justify-content: center",
          textAreaInput("input_raw", NULL, width = "90%", resize = "vertical")
        )
      } else if (input$input_type %in% c("CSV", "Text")) {
        div(
          style = "padding-bottom: 15px;",
          shinyFilesButton("metar_file", label = "Choose File", title = "Select METAR file", multiple = F),
          textOutput("file_name")
        )
      }
    })

    shinyFileChoose(input, "metar_file", roots = getVolumes())

    metar_file <- reactive({
      as.vector(parseFilePaths(roots = getVolumes(), input$metar_file)$datapath)
    })

    output$file_name <- renderText(metar_file())

    processed_metar <- eventReactive(input$metar_process, {
      if (input$input_type == "Raw") {
        read_file(input$input_raw, file_type = input$input_type)
      } else if (input$input_type %in% c("CSV", "Text") & length(metar_file()) > 0) {
        read_file(metar_file(), file_type = input$input_type)
      }
    })

    output$output_ui <- renderUI({
      req(processed_metar())
      div(
        DT::dataTableOutput(outputId = "output_table")
      )
    })

    output$output_table <- DT::renderDataTable({
      datatable_customised(processed_metar())
    }, server = T)

    onclick("toggle_theme", runjs("
      $('body').toggle('dark-theme');
      $('.model-content').toggle('dark-theme');
      $('#output_table').toggle('dark-theme');
    "))

    session$onSessionEnded(function() {
      stopApp()
    })

  }

  shinyApp(ui, server)

}
