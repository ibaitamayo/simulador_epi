load_passenger_traffic_edges <- function(
    path = file.path("inst", "data", "passenger_traffic_edges.rds")) {

  if (!file.exists(path)) {
    stop("Missing passenger traffic edges file: ", path, call. = FALSE)
  }

  readRDS(path)
}
