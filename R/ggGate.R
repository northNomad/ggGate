#' Create gates from ggplot2 object
#'
#' Create gates by clicking on ggplot2
#'
#' @param p ggplot2 object.
#' @param write_gate_to String. This is the variable name of the \code{data.frame} that stores the gating coordinates.
ggGate <- function(p,
                   write_gate_to = "df_gate") {
  #
  require(shiny)

  shinyApp(
    ######################
    ui = basicPage(

      #NAME GATES
      textInput("name_polygon", "Name of gate", "cluster 1"),

      #CLICK ON PLOTS
      plotOutput("plot",
                 click = "plot_click",
                 dblclick = "plot_dblclick"),

      #DISPLAYS GATE COORDINATES
      tableOutput("df_coordinate"),

      #SAVE COORDINATES
      actionButton('save_to_global', "Save coordinates to global environment")
    ),
    ####################
    server = function(input, output) {

      #RENDERS IMPORTED GGPLOT2 OBJECT
      p <- reactiveVal(p)
      output$plot <- renderPlot({p()})

      #CREATE AND UPDATES GATE COORDINATES
      df <- reactiveVal(NULL)
      observeEvent(input$plot_click, {
        if (is.null(df())) {
          df(tibble(x = input$plot_click$x,
                    y = input$plot_click$y,
                    group = input$name_polygon)
          )
        } else {
          df(df() %>%
               add_row(x = input$plot_click$x,
                       y = input$plot_click$y,
                       group = input$name_polygon)
          )
        }
      })

      #DOUBLECLICK
      observeEvent(input$plot_dblclick, {
        p(p() +
            geom_polygon(data = df(),
                         aes(x, y, fill = group),
                         alpha = .2)
        )
      })

      #RENDERS GATE COORDINATES AS TABLE
      output$df_coordinate <- renderTable({df()})


      observeEvent(input$save_to_global, {
        assign(write_gate_to, df(), envir = .GlobalEnv)}
      )}
  )
}
