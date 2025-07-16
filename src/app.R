library(shiny)
library(bslib)
library(DT)

dbtn <- function(...) {
  btn <- downloadButton(...)
  btn$attribs$download <- NULL
  return(btn)
}

ui <- page_sidebar(
  sidebar = sidebar(open = 'closed',
                    actionButton("add_row", "Add Row", disabled=TRUE),
                    actionButton("del_row", "Delete Row", disabled=TRUE),
                    actionButton("show_add_col", "Add Col", disabled=TRUE),  uiOutput("add_col_inputs"),
                    uiOutput("col_order_ui")
  ),
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
      DTOutput("table"),
      helpText("TESTFor datetime columns, enter values in the format %Y-%m-%dT%H:%M:%SZ (like `2025-12-31T23:59:59Z`)")
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
    # Enable the action buttons
    updateActionButton(session, "add_row", disabled = FALSE)
    updateActionButton(session, "del_row", disabled = FALSE)
    updateActionButton(session, "show_add_col", disabled = FALSE)
    updateActionButton(session, "add_col", disabled = FALSE)
  })
  
  output$table <- renderDT({
    req(data())
    datatable(
      data(),
      editable = TRUE,
      selection = 'multiple',
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv'),
        pageLength = 20,
        scrollX = TRUE
      )
    )
  }, server = FALSE)
  
  # Edit cell
  observeEvent(input$table_cell_edit, {
    info <- input$table_cell_edit
    df <- data()
    df[info$row, info$col] <- DT::coerceValue(info$value, df[info$row, info$col])
    data(df)
  })
  
  # Add row
  observeEvent(input$add_row, {
    df <- data()
    df[nrow(df) + 1, ] <- NA
    data(df)
  })
  
  # Delete row
  observeEvent(input$del_row, {
    req(input$table_rows_selected)
    df <- data()
    df <- df[-input$table_rows_selected, , drop = FALSE]
    data(df)
  })
  
  # Show add col inputs
  observeEvent(input$show_add_col, {
    toggle <- input$show_add_col
    if (toggle %% 2 == 1) {
      output$add_col_inputs <- renderUI({
        div(
          style = "border: 1px solid #ddd; padding: 15px; background-color: #f9f9f9; border-radius: 5px;",
          textInput("new_col_name", "New Column Name", placeholder='col1'),
          selectInput("new_col_type", "New Column Type",
                      choices = c("character", "float", "integer", "logical", "date", "datetime"),
                      selected = "character"),
          actionButton("add_col", "Add", class = "btn btn-success")
        )
      })
    } else {
      output$add_col_inputs <- renderUI(NULL)
    }
  })
  
  # Add col
  observeEvent(input$add_col, {
    req(input$new_col_name)
    df <- data()
    new_col <- make.names(input$new_col_name)
    if (!(new_col %in% colnames(df))) {
      df[[new_col]] <-   switch(
        input$new_col_type,
        "character" = as.character(NA),
        "float" = as.numeric(NA),
        "integer" = as.integer(NA),
        "logical" = as.logical(NA),
        "date" = as.Date(NA),
        "datetime" = Sys.time(),
        NA
      )
      data(df)
    } else {
      showNotification("Column already exists", type = "error")
    }
  })
  
  # Reorder cols
  output$col_order_ui <- renderUI({
    req(data())
    selectizeInput(
      "col_order",
      "Drop/Reorder Cols:",
      choices = colnames(data()),
      selected = colnames(data()),
      multiple = TRUE,
      options = list(plugins = list('drag_drop', 'remove_button'))
    )
  })
  
  # Reorder cols action
  observeEvent(input$col_order, {
    req(input$col_order)
    df <- data()
    if (all(input$col_order %in% colnames(df))) {
      data(df[, input$col_order, drop = FALSE])
    }
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
