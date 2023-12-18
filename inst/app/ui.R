dashboardPage(
  dashboardHeader(
    title = "AquaMaps & Sanctuaries"),
  dashboardSidebar(
    selectInput(
      "sel_sanct", "Select Sanctuary:",
      lst_sancts),
    hr(),
    helpText("Draw polygon (toolbar on left of map) to find species there. Defaults to global."),
    actionButton(
      "btn_spp",
      glue("Show species (n={format(nrow(d_spp_g), big.mark = ',')})")),
    collapsed = F),
  dashboardBody(
    tags$head(tags$link(rel="stylesheet", type="text/css", href="styles.css")),
    leafletOutput("map") ) )
