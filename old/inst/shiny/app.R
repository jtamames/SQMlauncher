library(shiny)
library(shinyjs)
library(shinyFiles)

ui <- fluidPage(

  useShinyjs(),

  titlePanel("squeezeMetaR"),

  sidebarLayout(

    sidebarPanel(

      textInput("project_name", "Project name"),

      selectInput(
        "mode",
        "Execution mode",
        choices = c("coassembly", "sequential"),
        selected = "coassembly"
      ),

      numericInput(
        "numthreads",
        "Number of threads",
        value = 8,
        min = 1
      ),

      shinyFiles::shinyFilesButton(
        id = "samples_file",
        label = "Select samples file (-s)",
        title = "Choose file",
        multiple = FALSE
      ),
      verbatimTextOutput("samples_path"),

      shinyFiles::shinyDirButton(
        id = "input_dir",
        label = "Select input directory (-f)",
        title = "Choose directory"
      ),
      verbatimTextOutput("input_path"),

      shinyFiles::shinyDirButton(
        id = "workdir",
        label = "Select working directory",
        title = "Choose directory"
      ),
      verbatimTextOutput("workdir_path"),

      br(),

      actionButton("run", "Run", class = "btn-success"),
      actionButton("stop", "Abort", class = "btn-danger")

    ),

    mainPanel(

      strong("Status: "),
      textOutput("status", inline = TRUE),

      hr(),

      tags$h4("Log"),
      verbatimTextOutput("log", placeholder = TRUE)

    )
  )
)

server <- function(input, output, session) {

  roots <- c(home = normalizePath("~"))

  shinyFiles::shinyFileChoose(input, "samples_file", roots = roots)
  shinyFiles::shinyDirChoose(input, "input_dir", roots = roots)
  shinyFiles::shinyDirChoose(input, "workdir", roots = roots)

  samples_path <- reactive({
    req(input$samples_file)
    shinyFiles::parseFilePaths(roots, input$samples_file)$datapath
  })

  input_path <- reactive({
    req(input$input_dir)
    shinyFiles::parseDirPath(roots, input$input_dir)
  })

  workdir_path <- reactive({
    req(input$workdir)
    shinyFiles::parseDirPath(roots, input$workdir)
  })

  output$samples_path <- renderText({ samples_path() })
  output$input_path <- renderText({ input_path() })
  output$workdir_path <- renderText({ workdir_path() })

  proc <- reactiveVal(NULL)
  current_log_file <- reactiveVal(NULL)
  log_buffer <- reactiveVal("")
  status <- reactiveVal("Idle")

  output$status <- renderText({ status() })

  observe({
    if (status() == "Running") {
      shinyjs::disable("run")
      shinyjs::enable("stop")
    } else {
      shinyjs::enable("run")
      shinyjs::disable("stop")
    }
  })

  observeEvent(input$run, {

    req(samples_path(), input_path(), workdir_path(), input$project_name)

    project_dir <- file.path(workdir_path(), input$project_name)

    if (dir.exists(project_dir)) {
      showNotification("Project directory already exists", type = "error")
      return()
    }

    log_buffer("")
    status("Running")

    res <- run_squeezemeta(
      samples_file = samples_path(),
      input_dir = input_path(),
      project_name = input$project_name,
      workdir = workdir_path(),
      mode = input$mode,
      threads = input$numthreads
    )

    proc(res$process)
    current_log_file(res$log_file)

    showNotification("Process started", type = "message")
  })

  observe({

    p <- proc()
    log_file <- current_log_file()

    req(p, log_file)

    invalidateLater(2000, session)

    if (file.exists(log_file)) {

   log_content <- tryCatch({

    lines <- readLines(log_file, warn = FALSE)

    # Eliminar mensajes Broken pipe
    lines <- lines[!grepl("Broken pipe", lines)]

    # Eliminar códigos ANSI (colores)
    lines <- gsub("\033\\[[0-9;]*m", "", lines)

    paste(lines, collapse = "\n")

  }, error = function(e) log_buffer())
 
  

      log_buffer(log_content)
    }

    if (!p$is_alive()) {

      exit_status <- p$get_exit_status()

      if (!is.null(exit_status)) {
        if (exit_status == 0) {
          status("Finished")
        } else {
          status("Error")
        }

        showNotification(
          paste("Process finished with status:", exit_status),
          type = ifelse(exit_status == 0, "message", "error")
        )
      }

      proc(NULL)
    }
  })

  observeEvent(input$stop, {

    showModal(
      modalDialog(
        title = "Confirm Abort",
        "Are you sure you want to abort the running process?",
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_abort", "Yes, abort", class = "btn-danger")
        )
      )
    )
  })

  observeEvent(input$confirm_abort, {

    removeModal()

    p <- proc()

    if (!is.null(p) && p$is_alive()) {

      p$kill_tree()

      log_buffer(
        paste(
          log_buffer(),
          "\n--- PROCESS ABORTED BY USER ---\n",
          sep = "\n"
        )
      )

      status("Aborted")

      showNotification("Process aborted", type = "warning")

      proc(NULL)
    }
  })

  output$log <- renderText({
    log_buffer()
  })
}

shinyApp(ui, server)
