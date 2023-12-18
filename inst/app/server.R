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
    req(input$sel_sanct != "_NA_") # !rxvals$name %in% c("Globe", "Drawn polygon"))

    message("observe sanct select")
    #browser()

    ply <- filter(sanctuaries, nms == input$sel_sanct)
    attr(ply, "name") <- ply$sanctuary
    rxvals$ply <- ply
  })

  # * click sanctuary ----
  observe({
    req(input$map_shape_click$id %in% setdiff(sanctuaries$sanctuary, "_NA_"))

    message("observe sanct click")
    updateSelectInput(
      session,
      "sel_sanct",
      selected = input$map_shape_click$id)
  })

  # * draw -> rxply ----
  observeEvent(input$map_draw_new_feature, {
    feature <- input$map_draw_new_feature

    stopifnot(feature$geometry$type == "Polygon")

    updateSelectInput(
      session,
      "sel_sanct",
      selected = "_NA_")

    ply <- st_read(as.json(feature$geometry), quiet=T)
    st_geometry(ply) <- "geom"
    attr(ply, "name") <- "Drawn polygon"
    rxvals$ply <- ply

    message(isolate(glue("draw: set name: {attr(rxvals$ply, 'name')}")))
  })

  # * rxply -> rxspp ----
  observeEvent(rxvals$ply, {

    if (attr(rxvals$ply, 'name') == "Globe"){

      message("observe rxply - Globe")

      leafletProxy("map") |>
        clearGroup("selected")
    } else {

      if(attr(rxvals$ply, 'name') == "Drawn polygon"){
        message("observe rxply - Drawn:", attr(rxvals$ply, 'name'))
      } else {
        message("observe rxply - sanct:", attr(rxvals$ply, 'name'))
      }

      # browser()

      # ply add and zoom
      b   <- st_bbox(rxvals$ply) |> as.numeric()

      message(paste("add and zoom to b:", paste(b, collapse = ', ')))
      leafletProxy("map") |>
        clearGroup("selected") |>
        addPolygons(
          data        = rxvals$ply,
          group       = "selected",
          opacity     = 1,
          weight      = 5,
          color       = "yellow",
          fillOpacity = 0,
          label       = attr(rxvals$ply, 'name')) |>
        flyToBounds(b[1], b[2], b[3], b[4])
    }

    message("observe rxply finish")
    #browser()

    # ply to update d_spp
    rxvals$d_spp <- am_spp_in_ply(rxvals$ply)
  })

  # Text ----

  # * txt_status ----
  output$txt_status <- renderText({
    place <- attr(rxvals$ply, 'name')
    n_spp <- nrow(rxvals$d_spp)
    glue("# species for {place}: {format(nrow(rxvals$d_spp), big.mark = ',')}")
    })

  # Table: tbl_spp ----
  output$tbl_spp <- DT::renderDataTable({
    rxvals$d_spp |>
      rename(
        n_cells  = n_cells,
        avg_pct  = avg_weight,
        avg_suit = avg_probability,
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
      formatPercentage(
        columns = c("avg_pct", "avg_suit"),
        digits  = 0) |>
      formatRound(
        columns = c("n_cells"),
        digits  = 0) |>
      formatRound(
        columns = c("amt"),
        digits  = 3) },
    server = F)

  # Plot: plt_spp ----
  output$plt_spp <- renderPlotly({

    rxvals$d_spp |>
      select(phylum, class, order, family, genus, species, n) |>
      count_to_treemap() })

})
