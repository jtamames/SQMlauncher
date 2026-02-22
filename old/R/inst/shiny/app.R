library(shiny)
library(squeezeMetaR)

ui <- fluidPage(
  titlePanel("squeezeMetaR"),

  sidebarLayout(
    sidebarPanel(
      textInput("project", "Project name"),
      selectInput("mode", "Mode",
                  choices = c("sequential", "coassembly")),
      numericInput("threads", "Threads", value = 8, min = 1),
      actionButton("run", "Run SqueezeMeta")
    ),

    mainPanel(
      verbatimTextOutput("status"),
      verbatimTextOutput("log")
    )
  )
)

server <- function(input, output, session) {

  proc <- reactiveVal(NULL)

  observeEvent(input$run, {
    p <- run_squeezemeta(
      project = input$project,
      mode = input$mode,
      threads = input$threads
    )
    proc(p)
  })

  output$status <- renderText({
    if (is.null(proc())) return("Idle")
    check_status(proc())
  })

  output$log <- renderText({
    if (is.null(proc())) return("")
    paste(proc()$read_output_lines(), collapse = "\n")
  })
}

shinyApp(ui, server)

