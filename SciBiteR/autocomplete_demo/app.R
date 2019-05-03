library(shiny)

# UI
ui <- shiny::fluidPage(
  titlePanel("SciBite Autocomplete Demo"),
  mainPanel("This is SciBite's autocomplete demo, using the \"SPECIES\" VOCab. Please ensure that the SPECIES VOCab is
            loaded into your TERMite server before continuing. Start typing into the text box to see the autocomplete
            options appear in the dropdown (e.g. \"mous\")."),
  textInput(inputId = 'text', label = 'text'),
  uiOutput("autobox"),
  textOutput("autoid")
)

# Server
server <- function(input, output, session) {
  autotext <- shiny::reactive({ input$text })
  output$autobox <- shiny::renderUI(
    if (nchar(autotext()) < 3) {
      shiny::selectInput(inputId = 'autobox', label = 'Autocomplete', choices = autotext())
    }
    else {
      auto <- SciBiteR::autocomplete(endpoint <- "http://localhost:9090/termite/toolkit/autocomplete.api",
                           input <- autotext(),
                           VOCab = "SPECIES",
                           taxon = '')
      reactiveauto <- auto$id
      names(reactiveauto) <- auto$label
      shiny::selectInput(inputId = 'autobox', label = 'Autocomplete', choices = reactiveauto)
    }

  )
  output$autoid <- shiny::renderText({ input$autobox })
}

# Run app
shiny::shinyApp(ui = ui, server = server)
