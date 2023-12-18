shinyServer(function(input, output, session) {

  # rxvals ----
  rxvals <- reactiveValues(
    ply   = ply_g,
    d_spp = d_spp_g)

  # map ----
  output$map <-  renderLeaflet({

    # pal for raster
    pal <- colorNumeric(
      "Spectral",
      values(r_nspp_3857, na.rm = T),
      na.color = "transparent",
      reverse = T)

    ms_basemap(base_opacity = 0.7) |>
      addRasterImage(
        r_nspp_3857,
        project = F,
        colors  = pal,
        opacity = 0.9) |>
      addPolygons(
        data        = sanctuaries,
        layerId     = ~nms,
        color       = "purple",
        opacity     = 1,
        weight      = 2,
        fillOpacity = 0,
        label       = ~sanctuary,
        highlightOptions = highlightOptions(
          weight       = 3,
          color        = "yellow",
          fillOpacity  = 0,
          opacity      = 1,
          bringToFront = T)) |>
      addLegend(
        pal      = pal,
        values   = values(r_nspp_3857),
        title    = "# species",
        position = "bottomright") |>
      addDrawToolbar(
        targetGroup         = 'draw',
        polylineOptions     = F,
        circleOptions       = F,
        circleMarkerOptions = F,
        markerOptions       = F,
        editOptions         = editToolbarOptions(
          selectedPathOptions = selectedPathOptions()),
        singleFeature = T) |>
      fitBounds(b[1], b[2], b[3], b[4])
  })

  # * select sanctuary ----
  observe({
    req(input$sel_sanct != "_NA_")

    rxvals$ply <- filter(sanctuaries, nms == input$sel_sanct)
  })

  # * click sanctuary ----
  observe({
    req(input$map_shape_click$id)

    updateSelectInput(
      session,
      "sel_sanct",
      selected = input$map_shape_click$id)
  })

  # * drawn ply -> rxvals$ply ----
  observeEvent(input$map_draw_new_feature, {
    feature <- input$map_draw_new_feature

    stopifnot(feature$geometry$type == "Polygon")

    updateSelectInput(
      session,
      "sel_sanct",
      selected = "_NA_")

    ply <- st_read(as.json(feature$geometry), quiet=T)
    st_geometry(ply_g) = "geom"
    rxvals$ply <- ply
  })

  # * rx.ply -> rx.d_spp ----
  observeEvent(rxvals$ply, {

    # browser()
    if (rxvals$ply$geom == ply_g$geom) {

      leafletProxy("map") |>
        clearGroup("selected")

    } else {

      lbls <- ifelse(
        "sanctuary" %in% names(rxvals$ply),
        rxvals$ply$sanctuary,
        "Drawn polygon")

      # ply add and zoom
      b   <- st_bbox(rxvals$ply) |> as.numeric()

      leafletProxy("map") |>
        clearGroup("selected") |>
        addPolygons(
          data        = rxvals$ply,
          group       = "selected",
          opacity     = 1,
          weight      = 5,
          color       = "yellow",
          fillOpacity = 0,
          label       = lbls) |>
        flyToBounds(b[1], b[2], b[3], b[4])
    }

    # ply to update d_spp
    rxvals$d_spp <- am_spp_in_ply(rxvals$ply)
    updateActionButton(
      session,
      "btn_spp",
      glue("Show species (n={format(nrow(rxvals$d_spp), big.mark = ',')})"))
  })

  # spp modal ----
  observeEvent(input$btn_spp, {
    showModal(modalDialog(
      title = "Species",
      size = "xl",
      tabsetPanel(
        id = "tabs_spp",
        tabPanel(
          "Table",
          helpText("amt = n_cells *  avg_pct * avg_prob"),
          dataTableOutput("tbl_spp")),
        tabPanel(
          "Treemap",
          helpText("Note: Treeamp rendering is SLOW if species n > 1,000. Use toolbar on left of map to draw a smaller area with fewer species."),
          plotlyOutput("plt_spp")) )))})

  # * tbl_spp ----
  output$tbl_spp <- DT::renderDataTable({
    rxvals$d_spp |>
      rename(
        n_cells  = n_cells,
        avg_pct  = avg_weight,
        avg_prob = avg_probability,
        amt      = n) |>
      datatable(
        extensions = c("Buttons", "FixedColumns"),
        options = list(
          scrollX      = T,
          pageLength   = 10,
          dom          = "Blfrtip",
          buttons      = c("copy", "csv", "excel", "pdf", "print"),
          fixedColumns = T ),
        rownames = F)  |>
      formatRound(
        columns = c("n_cells", "avg_pct", "avg_prob", "amt"),
        digits  = 2) },
    server = F)

  # * plt_spp ----
  output$plt_spp <- renderPlotly({

    rxvals$d_spp |>
      select(phylum, class, order, family, genus, species, n) |>
      count_to_treemap() })

})
