library(shiny)
library(DT)

dbtn <- function(...) {
  btn <- downloadButton(...)
  btn$attribs$download <- NULL
  return(btn)
}

ui <- fluidPage(
  fluidRow(
        column(
          width = 8,
          fileInput("file", "Upload a .parquet file", accept = ".parquet", width = "80%")
        ),
        column(
          width = 4,
          div(style = "height: 30px;"), # empty space above button
          uiOutput("download_placeholder")
        )
  ),
  hr(),
  fluidRow(
    column(
      width = 12,
      DTOutput("table")
    )
  )
)

server <- function(input, output, session) {
  data <- reactiveVal(NULL)
  
  # Place a disabled action button as download button initially
  output$download_placeholder <- renderUI(actionButton("no_download", "Download Edited Parquet", disabled=TRUE))

  observeEvent(input$file, {
    req(input$file)
    df <- nanoparquet::read_parquet(input$file$datapath)  # read parquet file as dataframe
    data(df)  # update DT table with df data
    output$download_placeholder <- renderUI(dbtn("download", "Download Edited Parquet"))  # replace the disabled action btn w/ a download btn
  })

  output$table <- renderDT({
    req(data())
    datatable(
      data(),
      editable = TRUE,
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv'),
        pageLength = 20,
        scrollX = TRUE
      )
    )
  }, server = FALSE)

  # Allow editing
  observeEvent(input$table_cell_edit, {
    info <- input$table_cell_edit
    df <- data()
    df[info$row, info$col] <- DT::coerceValue(info$value, df[info$row, info$col])
    data(df)
  })

  # Download handler
  output$download <- downloadHandler(
    filename = function() {
      paste0(tools::file_path_sans_ext(input$file$name), "_edited.parquet")
    },
    content = function(file) {
      nanoparquet::write_parquet(data(), file)
    },
    contentType = 'parquet'
  )
}

shinyApp(ui, server)
