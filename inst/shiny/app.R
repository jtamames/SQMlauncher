library(shiny)
library(shinyjs)
library(shinyFiles)
library(bslib)

if (interactive()) {
  pkg_root <- tryCatch(
    {
      # Intenta encontrar la raiz del paquete de forma robusta
      path <- dirname(sys.frame(1)$ofile)
      while (!file.exists(file.path(path, "DESCRIPTION")) && path != dirname(path)) {
        path <- dirname(path)
      }
      path
    },
    error = function(e) "."
  )
  pkgload::load_all(pkg_root)
}

perfiles_nombres <- sapply(get_builtin_profiles(), function(x) x$name)

ui <- page_navbar(

  title = "SQMLauncher",
  useShinyjs(),

  navbar_options = navbar_options(
    bg = "#1f4e79",
    fg = "#1f4e79",
    theme = "dark"
  ),

  theme = bs_theme(
    version = 5,
    bg = "#f4f6f9",
    fg = "#1f4e79",
    primary = "#1f4e79"
  ),

  header = tagList(
    tags$style(HTML("

      .sidebar .form-control,
      .sidebar .form-select {
       height: 32px !important;
       padding: 4px 8px !important;
       font-size: 0.9rem !important;
     }

     .sidebar .btn {
       height: 32px !important;
       padding: 4px 12px !important;
       font-size: 0.9rem !important;
       border-radius: 4px !important;
     }

    .bslib-sidebar-layout > .sidebar {
      overflow: visible !important;
    }

    .bslib-sidebar-layout {
      overflow: visible !important;
    }

    .sidebar label {
       font-size: 0.9rem !important;
       margin-bottom: 3px !important;
     }
        .project-setup-compact .shiny-input-container {
         margin-bottom: 8px !important;
       }

       .project-setup-compact .form-control {
         font-size: 0.95rem !important;
       }

       .project-setup-compact label {
         margin-bottom: 3px !important;
       }

       .project-setup-compact .form-control {
       height: 32px !important;
       padding: 4px 8px !important;
       font-size: 0.9rem !important;
     }

     .project-setup-compact .shiny-input-container {
       margin-bottom: 6px !important;
     }

     .project-setup-compact label {
       font-size: 0.9rem !important;
       margin-bottom: 3px !important;
     }

      .input-files-compact .btn-default {
        background-color: transparent !important;
        color: var(--bs-primary) !important;
        border: 1px solid var(--bs-primary) !important;
        font-weight: 400 !important;
      }

      .input-files-compact .btn-default:hover,
      .input-files-compact .btn-default:focus {
        background-color: var(--bs-primary) !important;
        color: #ffffff !important;
        border-color: var(--bs-primary) !important;
      }

     .input-files-compact .shiny-input-container {
        margin-bottom: 6px !important;
      }

      .input-files-compact .file-path {
        font-size: 0.8rem;
        color: #6c757d;
        margin: 2px 0 8px 0 !important;
        word-break: break-all;
      }

      .input-files-compact .btn {
        padding: 4px 10px !important;
        font-size: 0.9rem;
      }

      .annotation-compact .form-check {
      margin-bottom: 2px !important;
     }

     .annotation-compact .shiny-input-container {
       margin-bottom: 4px !important;
     }

     .annotation-box {
       border: 1px solid #dee2e6;
       border-radius: 6px;
       padding: 6px 8px;
       margin-bottom: 6px;
       background-color: #f8f9fa;
     }

     .annotation-title {
       font-weight: 500;
       font-size: 0.9rem;
       margin-bottom: 4px;
     }

      .binning-compact .form-check {
      margin-bottom: 2px !important;
      }

      .binning-compact .form-check-label {
      font-size: 0.9rem !important;
     }

      .binning-compact .shiny-input-container {
      margin-bottom: 4px !important;
    }

     .advanced-compact .accordion-button {
       padding: 4px 8px !important;
       font-size: 0.9rem !important;
     }

     .advanced-compact .accordion-body {
       padding: 6px 8px !important;
     }

     .advanced-compact .form-check {
       margin-bottom: 4px !important;
     }

     .advanced-compact .form-check-label {
       font-size: 0.9rem !important;
     }

     .advanced-compact .form-control {
       font-size: 0.9rem !important;
       padding: 4px 6px !important;
    }
    "))
  ),

  # ======================== TAB: RUN ========================
  nav_panel(
    "Run",

    layout_sidebar(

      sidebar = sidebar(
        width = 300,

        # ---------------- Project Setup ----------------
        card(
          card_header("Project Setup"),
          div(class = "project-setup-compact",
            textInput("project_name", "Project name"),

            selectInput(
              "program",
              "Program",
              choices = c(
                "SqueezeMeta"   = "SqueezeMeta.pl",
                "sqm_reads"     = "sqm_reads.pl",
                "sqm_longreads" = "sqm_longreads.pl"
              )
            ),

            conditionalPanel(
              condition = "input.program == 'SqueezeMeta.pl'",
              selectInput(
                "mode",
                "Execution mode",
                choices  = c("coassembly", "sequential", "merged", "seqmerge"),
                selectize = FALSE
              )
            )
          )
        ),

        # ---------------- Input Files ----------------
        card(
          card_header("Input Files"),
          div(class = "input-files-compact",
            shinyFilesButton(
              id       = "samples_file",
              label    = "Samples file (-s)",
              title    = "Choose file",
              multiple = FALSE
            ),
            div(class = "file-path", textOutput("samples_path")),

            shinyDirButton(
              id       = "input_dir",
              label    = "Input directory (-f)",
              title    = "Choose directory",
              multiple = FALSE
            ),
            div(class = "file-path", textOutput("input_path")),

            shinyDirButton(
              id       = "workdir",
              label    = "Working directory",
              title    = "Choose directory",
              multiple = FALSE
            ),
            div(class = "file-path", textOutput("workdir_path"))
          )
        ),

        conditionalPanel(
          condition = "input.program == 'SqueezeMeta.pl'",
          selectInput(
            "profile_selection",
            "Load Profile",
            choices  = perfiles_nombres,
            selected = "Standard Metagenome"
          )
        ),

        # ---------------- Advanced Settings ----------------
        card(
          card_header("Advanced Settings"),
          div(class = "advanced-compact",

            accordion(
              open     = FALSE,
              multiple = TRUE,

              # ---------- Filtering (siempre visible) ----------
              accordion_panel(
                "Filtering",
                checkboxInput("run_trimmomatic", "Run Trimmomatic", FALSE),
                conditionalPanel(
                  condition = "input.run_trimmomatic == true",
                  textInput(
                    "cleaning_parameters",
                    "Parameters",
                    value = "LEADING:8 TRAILING:8 SLIDINGWINDOW:10:15 MINLEN:30"
                  )
                )
              ),

              # ---------- Assembly (solo SqueezeMeta) ----------
              accordion_panel(
                "Assembly",
                conditionalPanel(
                  condition = "input.program == 'SqueezeMeta.pl'",
                  selectInput(
                    "assembler",
                    "Assembler",
                    choices   = c("megahit", "spades", "rnaspades", "canu", "flye"),
                    selectize = FALSE
                  ),
                  textInput(
                    "assembly_options",
                    "Assembly options (optional)",
                    placeholder = ""
                  ),
                  numericInput(
                    "min_contig_length",
                    "Min contig length",
                    value = 200,
                    min   = 0
                  ),
                  checkboxInput("use_singletons", "Use singletons", FALSE)
                ),
                conditionalPanel(
                  condition = "input.program != 'SqueezeMeta.pl'",
                  p("Assembly options are only available for SqueezeMeta.", style = "color:#6c757d; font-size:0.85rem;")
                )
              ),

              # ---------- Annotation (siempre visible) ----------
              accordion_panel(
                "Annotation",
                div(class = "annotation-compact",
                  div(
                    class = "annotation-box",
                    tags$div(class = "annotation-title", "Disable annotations"),
                    checkboxInput("no_cog",  "No COG",  FALSE),
                    checkboxInput("no_kegg", "No KEGG", FALSE),
                    checkboxInput("no_pfam", "No PFAM", TRUE)
                  ),
                  checkboxInput("eukaryotes",  "Eukaryotes",  FALSE),
                  checkboxInput("doublepass",  "Doublepass",  FALSE),
                  shinyFilesButton(
                    id       = "external_dbs",
                    label    = "External DBs",
                    title    = "Select file",
                    multiple = FALSE
                  ),
                  div(class = "file-path", textOutput("extdb_path"))
                )
              ),

              # ---------- Mapping (solo SqueezeMeta) ----------
              accordion_panel(
                "Mapping",
                conditionalPanel(
                  condition = "input.program == 'SqueezeMeta.pl'",
                  selectInput(
                    "mapper",
                    "Mapper",
                    choices   = c("bowtie", "bwa", "minimap2-ont", "minimap2-pb", "minimap2-sr"),
                    selectize = FALSE
                  ),
                  textInput(
                    "mapping_options",
                    "Mapping options (optional)",
                    placeholder = ""
                  )
                ),
                conditionalPanel(
                  condition = "input.program != 'SqueezeMeta.pl'",
                  p("Mapping options are only available for SqueezeMeta.", style = "color:#6c757d; font-size:0.85rem;")
                )
              ),

              # ---------- Binning (solo SqueezeMeta) ----------
              accordion_panel(
                "Binning",
                conditionalPanel(
                  condition = "input.program == 'SqueezeMeta.pl'",
                  div(class = "binning-compact",
                    checkboxInput("no_bins", "No bins", FALSE),
                    conditionalPanel(
                      condition = "input.no_bins == false",
                      checkboxInput("only_bins", "Only bins", FALSE),
                      div(
                        style = "border:1px solid #dee2e6; border-radius:6px; padding:6px 8px; margin-top:4px; background-color:#f8f9fa;",
                        tags$div(style = "font-weight:500; font-size:0.9rem; margin-bottom:4px;", "Binners"),
                        checkboxGroupInput(
                          "binners",
                          NULL,
                          choices  = c("Concoct" = "concoct", "Metabat2" = "metabat2", "MaxBin" = "maxbin"),
                          selected = c("concoct", "metabat2")
                        )
                      )
                    )
                  )
                ),
                conditionalPanel(
                  condition = "input.program != 'SqueezeMeta.pl'",
                  p("Binning options are only available for SqueezeMeta.", style = "color:#6c757d; font-size:0.85rem;")
                )
              ),

              # ---------- Performance (siempre visible) ----------
              accordion_panel(
                "Performance",
                numericInput("numthreads", "Threads (-t)", 8, min = 1)
              )
            ) # fin accordion
          )
        ), # fin card Advanced Settings

        div(
          style = "display:flex; gap:6px; margin-top:6px;",
          actionButton("run",  "Run",   class = "btn-primary"),
          actionButton("stop", "Abort", class = "btn-danger")
        )
      ), # fin sidebar

      # ---------------- Main Panel ----------------
      card(
        card_header(
          div(
            style = "display:flex; justify-content:space-between; align-items:center;",
            span("Execution Status"),
            uiOutput("status_badge")
          )
        ),
        card_body(
          div(
            style = "
              background-color:#f1f3f5;
              padding:12px;
              font-family: monospace;
              font-size:13px;
              height:600px;
              overflow-y:auto;
              border-radius:6px;
              border:1px solid #dee2e6;
            ",
            uiOutput("log")
          )
        )
      )
    )
  ), # fin nav_panel Run

  nav_panel("Results", h4("Results viewer coming soon")),
  nav_panel("About",   p("SQMLauncher — Advanced GUI for SqueezeMeta — v0.1"))
)


# ======================== SERVER ========================

server <- function(input, output, session) {

  roots             <- c(home = normalizePath("~"))
  last_line_read    <- reactiveVal(0)
  current_consensus <- reactiveVal(50)

  shinyFiles::shinyFileChoose(input, "samples_file",  roots = roots)
  shinyFiles::shinyDirChoose(input,  "input_dir",     roots = roots)
  shinyFiles::shinyDirChoose(input,  "workdir",       roots = roots)
  shinyFiles::shinyFileChoose(input, "external_dbs",  roots = roots)

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

  extdb_path <- reactive({
    req(input$external_dbs)
    shinyFiles::parseFilePaths(roots, input$external_dbs)$datapath
  })

  output$extdb_path    <- renderText({ extdb_path() })
  output$samples_path  <- renderText({ samples_path() })
  output$input_path    <- renderText({ input_path() })
  output$workdir_path  <- renderText({ workdir_path() })

  proc             <- reactiveVal(NULL)
  current_log_file <- reactiveVal(NULL)
  log_buffer       <- reactiveVal("")
  status           <- reactiveVal("Idle")

  output$status <- renderText({ status() })

  # Badge de estado
  output$status_badge <- renderUI({
    s <- status()
    color <- switch(s,
      "Idle"     = "secondary",
      "Running"  = "primary",
      "Finished" = "success",
      "Error"    = "danger",
      "Aborted"  = "warning",
      "secondary"
    )
    tags$span(class = paste0("badge bg-", color), s)
  })

  observe({
    if (status() == "Running") {
      shinyjs::disable("run")
      shinyjs::enable("stop")
    } else {
      shinyjs::enable("run")
      shinyjs::disable("stop")
    }
  })

  observeEvent(input$program, {
    if (input$program != "SqueezeMeta.pl") {
      updateSelectInput(session, "profile_selection", selected = "custom")
    }
  })

  observeEvent(input$profile_selection, {
    req(input$profile_selection)

    if (input$program == "SqueezeMeta.pl") {
      profile <- get_profile_by_name(input$profile_selection)

      if (!is.null(profile)) {
        params <- profile$parameters

        updateNumericInput(session, "numthreads",       value = params$threads)
        updateSelectInput(session,  "mode",             selected = params$mode)
        updateSelectInput(session,  "assembler",        selected = params$assembler)
        updateSelectInput(session,  "mapper",           selected = params$mapper)
        updateTextInput(session,    "assembly_options", value = params$assembly_options)
        updateCheckboxInput(session,"no_bins",          value = params$skip_binning)

        current_consensus(if (!is.null(params$consensus)) params$consensus else 50)

        showNotification(paste("Profile applied:", profile$name), type = "message")
      }
    }
  })

  observeEvent(input$run, {
    req(samples_path(), input_path(), workdir_path(), input$project_name)

    project_dir <- file.path(workdir_path(), input$project_name)
    if (dir.exists(project_dir)) {
      showNotification("Project directory already exists", type = "error")
      return()
    }

    extdb_val <- NULL
    if (!is.null(input$external_dbs)) {
      df_file <- shinyFiles::parseFilePaths(roots, input$external_dbs)
      if (nrow(df_file) > 0) extdb_val <- df_file$datapath
    }

    log_buffer("")

    res <- tryCatch({
      run_squeezemeta(
        program            = input$program,
        samples_file       = samples_path(),
        input_dir          = input_path(),
        project_name       = input$project_name,
        workdir            = workdir_path(),
        mode               = input$mode,
        threads            = input$numthreads,
        run_trimmomatic    = input$run_trimmomatic,
        cleaning_parameters= input$cleaning_parameters,
        assembler          = input$assembler,
        assembly_options   = input$assembly_options,
        min_contig_length  = input$min_contig_length,
        use_singletons     = input$use_singletons,
        no_cog             = input$no_cog,
        no_kegg            = input$no_kegg,
        no_pfam            = input$no_pfam,
        eukaryotes         = input$eukaryotes,
        doublepass         = input$doublepass,
        extdb              = extdb_val,
        consensus          = current_consensus(),
        mapper             = input$mapper,
        mapping_options    = input$mapping_options,
        no_bins            = input$no_bins,
        only_bins          = input$only_bins,
        binners            = input$binners
      )
    }, error = function(e) {
      showNotification(
        paste("Error al lanzar SqueezeMeta:", e$message),
        type     = "error",
        duration = NULL
      )
      message("ERROR EN run_squeezemeta: ", e$message)
      NULL
    })

    if (!is.null(res)) {
      status("Running")
      proc(res$process)
      current_log_file(res$log_file)
      showNotification("Process started successfully", type = "message")
    }
  })

  # Leer stdout/stderr del proceso en tiempo real
  observe({
    p <- proc()
    req(p)
    invalidateLater(1000, session)

    out <- p$read_output_lines()
    if (length(out) > 0) {
      out <- out[!grepl("Broken pipe", out)]
      out <- gsub("\033\\[[0-9;]*m", "", out)
      log_buffer(paste(log_buffer(), paste(out, collapse = "\n"), sep = "\n"))
    }

    err <- p$read_error_lines()
    if (length(err) > 0) {
      err <- gsub("\033\\[[0-9;]*m", "", err)
      log_buffer(paste(log_buffer(), paste(err, collapse = "\n"), sep = "\n"))
    }

    if (!p$is_alive()) {
      exit_status <- p$get_exit_status()
      status(if (exit_status == 0) "Finished" else "Error")
      proc(NULL)
    }
  })

  observeEvent(input$stop, {
    showModal(
      modalDialog(
        title  = "Confirm Abort",
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
      log_buffer(paste(log_buffer(), "\n--- PROCESS ABORTED BY USER ---\n", sep = "\n"))
      status("Aborted")
      showNotification("Process aborted", type = "warning")
      proc(NULL)
    }
  })

  observeEvent(input$no_bins, {
    if (input$no_bins) {
      updateCheckboxInput(session,    "only_bins", value = FALSE)
      updateCheckboxGroupInput(session, "binners", selected = character(0))
    } else {
      updateCheckboxGroupInput(session, "binners", selected = c("concoct", "metabat2"))
    }
  })

  observeEvent(input$only_bins, {
    if (input$only_bins) updateCheckboxInput(session, "no_bins", value = FALSE)
  })

  output$log <- renderUI({
    tags$pre(style = "margin:0;", log_buffer())
  })
}

shinyApp(ui, server)
