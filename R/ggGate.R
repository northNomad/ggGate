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

  used_cols <- unique(unlist(lapply(p$mapping, rlang::as_label)))
  for (layer in p$layers) {
    if (length(layer$mapping) > 0) {
      used_cols <- unique(c(used_cols, unlist(lapply(layer$mapping, rlang::as_label))))
    }
  }
  used_cols <- used_cols[used_cols %in% names(p$data)]
  p$data <- p$data[, used_cols, drop = FALSE]

  shinyApp(
    ui = basicPage(
      textInput("name_polygon", "Name of gate", "cluster 1"),
      plotOutput("plot", click = "plot_click", dblclick = "plot_dblclick"),
      actionButton('save_coord_to_global', "Save coordinates to global environment"),
      actionButton('save_annotated.raw_to_global', "Save annotated raw data to global environment"),
      tableOutput("df_coordinate")
    ),
    server = function(input, output) {
      point.x <- p$data[[rlang::as_label(p$mapping$x)]]
      point.y <- p$data[[rlang::as_label(p$mapping$y)]]

      raw <- reactiveVal(p$data)
      df  <- reactiveVal(NULL)

      built_plot <- reactive({
        current_df <- df()
        plt <- p

        if (!is.null(current_df)) {
          plt <- plt +
            ggplot2::geom_path(
              data = current_df,
              mapping = ggplot2::aes(x = x, y = y),
              color = "red",      # fixed, no aes mapping
              linewidth = 0.8,
              inherit.aes = FALSE
            ) +
            ggplot2::geom_point(
              data = current_df,
              mapping = ggplot2::aes(x = x, y = y),
              color = "red",      # fixed, no aes mapping
              shape = 20,
              size = 2,
              inherit.aes = FALSE
            ) +
            ggplot2::geom_polygon(
              data = current_df,
              mapping = ggplot2::aes(x = x, y = y),
              color = "red",      # fixed, no aes mapping
              fill = "red",       # fixed, no aes mapping
              alpha = 0.2,
              inherit.aes = FALSE
            )
        }
        plt
      })

      output$plot <- renderPlot({ built_plot() })

      observeEvent(input$plot_click, {
        new_row <- tibble::tibble(x = input$plot_click$x, y = input$plot_click$y, group = input$name_polygon)
        df(if (is.null(df())) new_row else dplyr::bind_rows(df(), new_row))
      })

      observeEvent(input$plot_dblclick, {
        closed <- df() %>%
          dplyr::group_by(group) %>%
          dplyr::group_map(function(x, y) dplyr::bind_rows(x, x[1, ])) %>%
          do.call(rbind, .)
        df(closed)

        pol.x    <- dplyr::filter(closed, group == input$name_polygon) %>% .$x
        pol.y    <- dplyr::filter(closed, group == input$name_polygon) %>% .$y
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
