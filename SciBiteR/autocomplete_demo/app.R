library(shiny)

appDesc <- "This is SciBite's autocomplete demo. Please ensure that the SPECIES VOCab is
loaded into your TERMite server before continuing. Start typing into the text box to see the
autocomplete options appear in the dropdown (e.g. \"mous\")."

# UI
ui <- shiny::fluidPage(
  titlePanel("SciBite Autocomplete Demo"),
  sidebarLayout(position = "right",
                sidebarPanel(uiOutput("autobox"), textOutput("autoid")),
                mainPanel(p("To use this demo please type a VOCab code into the",
                            strong("VOCab"),
                            "textbox, then start typing in the",
                            strong("Text"),
                            "textbox."),
                          em("As an example, use \"SPECIES\" as the VOCab and start typing \"macro\"."),
                          div("Only VOCabs preloaded into your TERMite server can be used.", style = "color:red"),
                          textInput(inputId = 'VOCabInput', label = 'VOCab'),
                          textInput(inputId = 'text', label = 'Text'))
  )
)

# Server
server <- function(input, VOCab, output, session) {
  autotext <- shiny::reactive({ input$text })
  VOCabtext <- shiny::reactive({ input$VOCabInput })
  output$autobox <- shiny::renderUI(
    if (nchar(autotext()) < 3) {
      shiny::selectInput(inputId = 'autobox', label = 'Autocomplete', choices = autotext())
    } else {
      auto <- SciBiteR::autocomplete(endpoint <- "http://localhost:9090/termite/toolkit/autocomplete.api",
                                     input <- autotext(),
                                     VOCab = toupper(VOCabtext()),
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
