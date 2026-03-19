#' Create gates from ggplot2 object
#'
#' Create gates by clicking on ggplot2.
#' A single click adds coordinates to the active polygon gate.
#' A double click draws the active polygon gate, and annotates the raw ggplot2 data with new columns.
#'
#' @param p ggplot2 object.
#' @param write_data_to String. Sets the variable name of the \code{data.frame} that stores the annotated data underlying the ggplot2 object.
#' @param write_gate_to String. Sets the variable name of the \code{data.frame} that stores the gating coordinates.
ggGate <- function(p, write_data_to = "df_new", write_gate_to = "df_gate") {

  shinyApp(
    ui = basicPage(
      textInput("name_polygon", "Name of gate", "cluster 1"),
      plotOutput("plot", click = "plot_click", dblclick = "plot_dblclick"),
      actionButton('save_coord_to_global', "Save coordinates to global environment"),
      actionButton('save_annotated.raw_to_global', "Save annotated raw data to global environment"),
      tableOutput("df_coordinate")
    ),

    server = function(input, output) {

      point.x <- dplyr::select(p$data, rlang::as_label(p$mapping$x)) %>% .[, 1]
      point.y <- dplyr::select(p$data, rlang::as_label(p$mapping$y)) %>% .[, 1]

      raw <- reactiveVal(p$data)
      p <- reactiveVal(p)
      output$plot <- renderPlot({ p() })

      df <- reactiveVal(NULL)

      # A simple named color palette for gates
      gate_colors <- c(
        "cluster 1" = "#E41A1C", "cluster 2" = "#377EB8",
        "cluster 3" = "#4DAF4A", "cluster 4" = "#984EA3",
        "cluster 5" = "#FF7F00"
      )
      get_color <- function(name) {
        if (name %in% names(gate_colors)) gate_colors[[name]] else "black"
      }

      observeEvent(input$plot_click, {
        new_row <- tibble::tibble(
          x = input$plot_click$x,
          y = input$plot_click$y,
          group = input$name_polygon
        )
        df(if (is.null(df())) new_row else dplyr::bind_rows(df(), new_row))

        col <- get_color(input$name_polygon)

        # Use fixed color aesthetics to avoid scale conflicts
        p(p() +
          ggplot2::geom_path(
            data = dplyr::filter(df(), group == input$name_polygon),
            ggplot2::aes(x, y), color = col, show.legend = FALSE
          ) +
          ggplot2::geom_point(
            data = dplyr::filter(df(), group == input$name_polygon),
            ggplot2::aes(x, y), color = col, shape = 20, show.legend = FALSE
          )
        )
      })

      observeEvent(input$plot_dblclick, {
        df2 <- df() %>%
          dplyr::mutate(group_dup = group) %>%
          dplyr::group_by(group_dup) %>%
          dplyr::group_map(function(x, y) dplyr::bind_rows(x, x[1, ])) %>%
          do.call(rbind, .)

        col <- get_color(input$name_polygon)

        p(p() +
          ggplot2::geom_polygon(
            data = dplyr::filter(df(), group == input$name_polygon),
            ggplot2::aes(x, y), fill = col, alpha = .2
          ) +
          ggplot2::geom_path(
            data = dplyr::filter(df2, group == input$name_polygon),
            ggplot2::aes(x, y), color = col, show.legend = FALSE
          )
        )

        pol.x <- dplyr::filter(df(), group == input$name_polygon) %>% .$x
        pol.y <- dplyr::filter(df(), group == input$name_polygon) %>% .$y
        is.gated <- sp::point.in.polygon(point.x, point.y, pol.x, pol.y)

        raw(raw() %>% dplyr::mutate(!!input$name_polygon := is.gated))
      })

      output$df_coordinate <- renderTable({ df() })

      observeEvent(input$save_coord_to_global, {
        assign(write_gate_to, df(), envir = .GlobalEnv)
      })

      observeEvent(input$save_annotated.raw_to_global, {
        assign(write_data_to, raw(), envir = .GlobalEnv)
      })
    }
  )
}
