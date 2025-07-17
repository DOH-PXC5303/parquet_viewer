library(shiny)
library(bslib)
library(DT)

dbtn <- function(...) {
  btn <- downloadButton(...)
  btn$attribs$download <- NULL
  return(btn)
}

ui <- page_sidebar(
  sidebar = sidebar(open = 'open',
                    actionButton("add_row", "Add Row", disabled=TRUE),
                    actionButton("del_row", "Delete Row", disabled=TRUE),
                    actionButton("show_add_col", "Add Col", disabled=TRUE),  uiOutput("add_col_inputs"),
                    uiOutput("col_order_ui"),
                    uiOutput("download_parquet_placeholder"),
                    uiOutput("download_csv_placeholder"),
                    fileInput("file", "Upload a .parquet file", accept = ".parquet", width = "100%")
  ),
  fluidRow(DTOutput("table")),
  
)

server <- function(input, output, session) {
  data <- reactiveVal(NULL)
  
  # Place a disabled action button as download button initially
  output$download_parquet_placeholder <- renderUI(actionButton("no_download_parquet", "Parquet", disabled=TRUE))
  output$download_csv_placeholder <- renderUI(actionButton("no_download_csv", "CSV", disabled=TRUE))
  
  
  observeEvent(input$file, {
    req(input$file)
    df <- nanoparquet::read_parquet(input$file$datapath)  # read parquet file as dataframe
    data(df)  # update DT table with df data
    # replace disabled actions buttons w/download buttons
    output$download_parquet_placeholder <- renderUI(dbtn("download_parquet", "Parquet"))  
    output$download_csv_placeholder <- renderUI(dbtn("download_csv", "CSV"))
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
      selection = list(mode = 'single', selected = NULL, target = 'row+column', selectable = NULL),
      options = list(
        dom = 'Bfrtip',
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
                      choices = c("string", "float", "integer", "logical", "date", "datetime"),
                      selected = "string"),
          uiOutput("def_val_ui"),
          actionButton("add_col", "Add", class = "btn btn-success")
        )
      })
    } else {
      output$add_col_inputs <- renderUI(NULL)
    }
  })
  
  # Set default value ui type
  output$def_val_ui <- renderUI({
    req(input$new_col_type)
    
    div(
      radioButtons(
        "default_val_type",
        "Default Value:",
        choices = c("NA", "Other"),
        selected = "NA",
        inline = TRUE
      ),
      conditionalPanel(
        condition = "input.default_val_type == 'Other'",
        switch(
          input$new_col_type,
          "string" = textInput("def_val", NULL, placeholder="string", value=""),
          "float" = numericInput("def_val", NULL, value=0.0),
          "integer" = numericInput("def_val", NULL, value=0),
          "logical" = selectInput("def_val", NULL, choices=c("True"=TRUE, "False"=FALSE), selected=TRUE),
          "date" = dateInput("def_val", NULL, value=Sys.Date()),
          "datetime" = div(
            textInput("def_val", "Default Value (UTC)", value=format(Sys.time(), '%Y-%m-%dT%H:%M:%SZ'), placeholder=format(Sys.time(), '%Y-%m-%dT%H:%M:%SZ')),
            helpText("Enter values in the format %Y-%m-%dT%H:%M:%SZ (like 2025-12-31T23:59:59Z)")
          ),
          NULL
        )
      )
    )
  })
  
  # Add col
  observeEvent(input$add_col, {
    req(input$new_col_name)
    
    df <- data()
    new_col <- make.names(input$new_col_name)
    if (!(new_col %in% colnames(df))) {
      df[[new_col]] <- switch(
        input$new_col_type,
        "string" = as.character(input$def_val),
        "float" = as.numeric(input$def_val),
        "integer" = as.integer(input$def_val),
        "logical" = as.logical(input$def_val),
        "date" = if (is.null(input$def_val)) as.Date(NA) else as.Date(input$def_val),
        "datetime" = as.POSIXct(input$def_val),
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
  
  
  
  # Parquet download handler
  output$download_parquet <- downloadHandler(
    filename = function() {
      paste0(tools::file_path_sans_ext(input$file$name), "_edited.parquet")
    },
    content = function(file) {
      nanoparquet::write_parquet(data(), file)
    },
    contentType = 'parquet'
  )
  
  # CSV download handler
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0(tools::file_path_sans_ext(input$file$name), "_edited.csv")
    },
    content = function(file) {
      write.csv(data(), file, na="", row.names=FALSE, )
    },
    contentType = 'csv'
  )
}

shinyApp(ui, server)
