# Chart-related helpers extracted from v17 freeze.
# Do not translate function names or internal identifiers.










get_peak_info <- function(df) {
    idx <- which.max(df$I)
    list(day = df$time[idx], infected = df$I[idx])
  }




