library(shiny)
library(shinyjs)
library(shinyFiles)
library(bslib)

ui <- page_navbar(
  
  title = "SQMLauncher",
  
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
              "SqueezeMeta" = "SqueezeMeta.pl",
              "sqm_reads" = "sqm_reads.pl",
              "sqm_longreads" = "sqm_longreads.pl"
            )
          ),
          
          selectInput(
            "mode",
            "Execution mode",
            choices = c("coassembly", "sequential")
          )
         )
        ),
        
        # ---------------- Input Files ----------------
        card(
          card_header("Input Files"),
          div(class = "input-files-compact",
          shinyFilesButton(
            id = "samples_file",
            label = "Samples file (-s)",
            title = "Choose file",
            multiple = FALSE
          ),
         div(class = "file-path", textOutput("samples_path")),
          
          shinyDirButton(
            id = "input_dir",
            label = "Input directory (-f)",
            title = "Choose directory",
            multiple = FALSE
          ),
          div(class = "file-path", textOutput("input_path")),
         
          shinyDirButton(
            id = "workdir",
            label = "Working directory",
            title = "Choose directory",
            multiple = FALSE
          ),
         div(class = "file-path", textOutput("workdir_path")),
          )
        ),
        
        # ---------------- Advanced Settings ----------------
        card(
          card_header("Advanced Settings"),
           div(class = "advanced-compact",
          accordion(
            open = FALSE,
            multiple = TRUE,
            
            accordion_panel(
              "Filtering",
              checkboxInput("trim_reads", "Enable trimming", FALSE),
              checkboxInput("remove_low_complexity", "Remove low complexity", FALSE)
            ),
            
            accordion_panel(
              "Assembly",
              selectInput("assembler", "Assembler",
                          choices = c("megahit", "spades"))
            ),
            
            accordion_panel(
              "Mapping",
              selectInput("mapper", "Mapper",
                          choices = c("bowtie2", "minimap2"))
            ),
            
            accordion_panel(
              "Annotation",
              checkboxInput("use_kegg", "KEGG", TRUE),
              checkboxInput("use_cog", "COG", TRUE)
            ),
            
            accordion_panel(
              "Binning",
              checkboxInput("run_metabat", "MetaBAT2", TRUE),
              checkboxInput("run_maxbin", "MaxBin2", FALSE)
            ),
            
            accordion_panel(
              "Performance",
              numericInput("numthreads", "Threads (-t)", 8, min = 1)
            )
          )
        ),
        
        div(
          style = "display:flex; gap:6px; margin-top:6px;",
          actionButton("run", "Run", class = "btn-primary"),
          actionButton("stop", "Abort", class = "btn-danger")
        )
        )
      ),
      
      # ---------------- Main Panel ----------------
      card(
        card_header(
          div(
            style="display:flex; justify-content:space-between; align-items:center;",
            span("Execution Status"),
            uiOutput("status_badge")
          )
        ),
        
        card_body(
          div(
            style="
              background-color:#f1f3f5;
              padding:12px;
              font-family: monospace;
              font-size:13px;
              height:600px;
              overflow-y:auto;
              border-radius:6px;
              border:1px solid #dee2e6;
            ",
            verbatimTextOutput("log")
          )
        )
      )
    )
  ),
  
  nav_panel("Results", h4("Results viewer coming soon...")),
  nav_panel("About", p("SQMLauncher — Advanced GUI for SqueezeMeta"))
)

server <- function(input, output, session) {
  
  useShinyjs()
  
  roots <- c(home = normalizePath("~"))
  
  shinyFileChoose(input, "samples_file", roots = roots)
  shinyDirChoose(input, "input_dir", roots = roots)
  shinyDirChoose(input, "workdir", roots = roots)
  
  samples_path <- reactive({
    req(input$samples_file)
    parseFilePaths(roots, input$samples_file)$datapath
  })
  
  input_path <- reactive({
    req(input$input_dir)
    parseDirPath(roots, input$input_dir)
  })
  
  workdir_path <- reactive({
    req(input$workdir)
    parseDirPath(roots, input$workdir)
  })
  
  output$samples_path <- renderText({ samples_path() })
  output$input_path   <- renderText({ input_path() })
  output$workdir_path <- renderText({ workdir_path() })
  
  output$status_badge <- renderUI({
    span(class = "badge bg-secondary", "Idle")
  })
  
  output$log <- renderText({ "" })
}

shinyApp(ui, server)
