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
  
  # Extract the original color scale from p to re-add it after new layers
  original_color_scale <- NULL
  for (s in p$scales$scales) {
    if ("colour" %in% s$aesthetics) {
      original_color_scale <- s
      break
    }
  }
  
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

      raw  <- reactiveVal(p$data)
      p_base <- reactiveVal(p)  # store base plot separately, never modify it
      df   <- reactiveVal(NULL)

      gate_colors <- c(
        "cluster 1" = "#E41A1C", "cluster 2" = "#377EB8",
        "cluster 3" = "#4DAF4A", "cluster 4" = "#984EA3",
        "cluster 5" = "#FF7F00"
      )
      get_color <- function(name) {
        if (name %in% names(gate_colors)) gate_colors[[name]] else "#000000"
      }

      # Build the display plot fresh each time from base + current df
      # This avoids layer accumulation conflicts
      built_plot <- reactive({
        current_df <- df()
        plt <- p_base()
        
        if (!is.null(current_df)) {
          groups <- unique(current_df$group)
          for (g in groups) {
            col <- get_color(g)
            gdf <- dplyr::filter(current_df, group == g)
            plt <- plt +
              ggplot2::geom_path(
                data = gdf,
                mapping = ggplot2::aes(x = x, y = y),
                color = col,
                linewidth = 0.8,
                inherit.aes = FALSE   # <-- KEY: don't inherit color aes from parent
              ) +
              ggplot2::geom_point(
                data = gdf,
                mapping = ggplot2::aes(x = x, y = y),
                color = col,
                shape = 20,
                size = 2,
                inherit.aes = FALSE   # <-- KEY
              )
          }
        }
        plt
      })

      output$plot <- renderPlot({ built_plot() })

      observeEvent(input$plot_click, {
        new_row <- tibble::tibble(
          x     = input$plot_click$x,
          y     = input$plot_click$y,
          group = input$name_polygon
        )
        df(if (is.null(df())) new_row else dplyr::bind_rows(df(), new_row))
      })

      observeEvent(input$plot_dblclick, {
        current_df <- df()
        
        # Close the polygon for the current gate
        closed <- current_df %>%
          dplyr::group_by(group) %>%
          dplyr::group_map(function(x, y) dplyr::bind_rows(x, x[1, ])) %>%
          do.call(rbind, .)
        
        df(closed)

        # Annotate raw data
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
