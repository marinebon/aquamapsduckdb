page_sidebar(
  title = "Species Explorer",

  sidebar = sidebar(
    selectInput(
      "sel_rgn", "Region",
      lst_rgns),
    textOutput("txt_status"),
    hr(),
    helpText(
      "Select a Region or Draw a polygon (toolbar on left of Map)
      to filter to species and view as Table or treemap Plot.")),

  tags$head(tags$link(rel="stylesheet", type="text/css", href="styles.css")),

  navset_card_pill(
    placement = "above",
    nav_panel(
      title = "Map",
      leafletOutput("map") ),
    nav_panel(
      title = "Table",
      dataTableOutput("tbl_spp"),
      helpText(
        "amt = n_cells * avg_pct * avg_suit"),
      helpText(
        "Amount (amt) is the multiplication of the number of cells (n_cells),
         average percent (avg_pct) of a cell's contents within the selected polygon,
         and the average Suitability (avg_suit; 0 to 100%) of the species given by AquaMaps.")),
    nav_panel(
      title = "Plot",
      plotlyOutput("plt_spp"),
      helpText("Note: The rendering of this treemap plot is SLOW if # species > 1,000. Use toolbar on left of map to draw a smaller area with fewer species.")) ) )
