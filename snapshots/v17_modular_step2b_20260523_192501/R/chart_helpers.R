# Chart-related helpers extracted from v17 freeze.
# Do not translate function names or internal identifiers.

global_plot_values <- function(x) {
    if (identical(input$global_plot_scale, "percent")) x / WORLD_POPULATION * 100 else x
  }


global_axis_title <- function(metric_label) {
    if (identical(input$global_plot_scale, "percent")) paste0(metric_label, " (% of world population)") else paste0(metric_label, " (persons)")
  }


global_hover <- function(virus_label, metric_label) {
    if (identical(input$global_plot_scale, "percent")) {
      paste0("<b>", virus_label, "</b><br>Day: %{x}<br>", metric_label, ": %{y:.4f}%<extra></extra>")
    } else {
      paste0("<b>", virus_label, "</b><br>Day: %{x}<br>", metric_label, ": %{y:,.0f} persons<extra></extra>")
    }
  }


get_peak_info <- function(df) {
    idx <- which.max(df$I)
    list(day = df$time[idx], infected = df$I[idx])
  }


add_infection_peak_lines <- function(p, h_peak, c_peak, y_values) {
    ymax <- max(y_values, na.rm = TRUE)
    if (!is.finite(ymax) || ymax <= 0) ymax <- 1
    h_label <- if (identical(input$global_plot_scale, "percent")) sprintf("New virus peak | day: %s | %.4f%%", h_peak$day, h_peak$infected / WORLD_POPULATION * 100) else sprintf("New virus peak | day: %s | persons: %s", h_peak$day, format_big(h_peak$infected))
    c_label <- if (identical(input$global_plot_scale, "percent")) sprintf("Omicron reference peak | day: %s | %.4f%%", c_peak$day, c_peak$infected / WORLD_POPULATION * 100) else sprintf("Omicron reference peak | day: %s | persons: %s", c_peak$day, format_big(c_peak$infected))
    p %>% layout(
      shapes = list(
        list(type = "line", x0 = h_peak$day, x1 = h_peak$day, y0 = 0, y1 = ymax, line = list(color = "#E74C3C", width = 1, dash = "dash")),
        list(type = "line", x0 = c_peak$day, x1 = c_peak$day, y0 = 0, y1 = ymax, line = list(color = "#27AE60", width = 1, dash = "dot"))
      ),
      annotations = list(
        list(x = h_peak$day, y = ymax, text = h_label, showarrow = TRUE, ax = 40, ay = -30, font = list(size = 11)),
        list(x = c_peak$day, y = ymax * 0.90, text = c_label, showarrow = TRUE, ax = 40, ay = -30, font = list(size = 11))
      )
    )
  }

