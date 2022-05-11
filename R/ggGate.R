#' Create gates from ggplot2 object
#'
#' Create gates by clicking on ggplot2.
#' A single click adds coordinates to the active polygon gate.
#' A double click draws the active polygon gate, and annotates the raw ggplot2 data with new columns.
#'
#' @param p ggplot2 object.
#' @param write_data_to String. Sets the variable name of the \code{data.frame} that stores the annotated data underlying the ggplot2 object.
#' @param write_gate_to String. Sets the variable name of the \code{data.frame} that stores the gating coordinates.
ggGate <- function(p,
                   write_data_to = "df_new",
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

      #SAVE COORDINATES
      actionButton('save_coord_to_global', "Save coordinates to global environment"),

      #SAVE APPENDED RAW DATA
      actionButton('save_annotated.raw_to_global', "Save annotated raw data to global environment"),

      #DISPLAYS GATE COORDINATES
      tableOutput("df_coordinate")


    ),
    ####################
    server = function(input, output) {
      #EXTRACT X, Y COLUMNS IN PLOT
      point.x <- dplyr::select(p$data, as_label(p$mapping$x)) %>% .[, 1]
      point.y <- dplyr::select(p$data, as_label(p$mapping$y)) %>% .[, 1]

      #MAKE RAW DATA REACTIVE
      raw <- reactiveVal(p$data)

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

      #DOUBLECLICK -
      observeEvent(input$plot_dblclick, {
        ##ADDS POLYGON
        p(p() +
            geom_polygon(data = df(),
                         aes(x, y, fill = group),
                         alpha = .2)
        )

        ##ANNOTATES RAW DF
        pol.x <- subset(df(), group == input$name_polygon) %>% .$x
        pol.y <- subset(df(), group == input$name_polygon) %>% .$y
        is.gated <- sp::point.in.polygon(point.x, point.y, pol.x, pol.y)
        #
        raw(
          raw() %>%
            mutate(!! input$name_polygon := is.gated)

        )
      })

      #RENDERS GATE COORDINATES AS TABLE
      output$df_coordinate <- renderTable({df()})

      #WRITE COORDINATES OF GATES TO GLOBAL ENV
      observeEvent(input$save_coord_to_global, {
        assign(write_gate_to, df(), envir = .GlobalEnv)
        })

      #WRITE ANNOTATED RAW DATA.FRAME TO GLOBAL ENV
      observeEvent(input$save_annotated.raw_to_global, {
        assign(write_data_to, raw(), envir = .GlobalEnv)
        })
    }
  )
}
