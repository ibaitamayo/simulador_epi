
transmission_example_status_badge <- function(example_state) {

  if (!identical(example_state, "Custom")) {
    return(NULL)
  }

  tags$div(
    style = paste(
      "margin-top:6px;",
      "padding:4px 8px;",
      "border-radius:6px;",
      "background-color:#FDEBD0;",
      "color:#7E5109;",
      "font-weight:600;"
    ),
    "Status: Custom — pathogen-like parameters modified"
  )
}

transmission_example_info_panel <- function() {

  tags$details(
    style = "margin-top: 10px;",
    tags$summary(
      style = paste(
        "cursor:pointer;",
        "color:#2E86C1;",
        "font-weight:700;",
        "background-color:#EBF5FB;",
        "padding:6px 8px;",
        "border-radius:6px;"
      ),
      "About this Example"
    ),
    tags$div(
      style = paste(
        "background-color:#F8FBFD;",
        "border-left:3px solid #2E86C1;",
        "padding:8px 10px;",
        "margin-top:6px;",
        "border-radius:4px;"
      ),
      uiOutput("transmission_example_info")
    )
  )
}

