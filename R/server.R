server = function(input, output) {

  output$tableContexts = renderTable(tbl_contexts)

  output$network_graph = renderVisNetwork({

    ## Network properties ##
    ## nodes ##
    nodes = data.frame(id = 1:length(pic),
                       shape = "image",
                       group = as.character(df_All$ContextLabel), # paste(as.character(df_All$ContextLabel), as.character(df_All$hard_cl_10), sep = ","),
                       title = paste0("<p>File: ", df_All$filenaam,
                                      "<br>Cluster: ", df_All$hard_cl_10, "<br>Cluster support: ", df_All$value_10, "<br>Opacity: ", df_All$code_10,
                                      "<br>FIS_1 Descriptor: ", df_All$final.descr.FIS_1, "<br>FIS_1 support: ", df_All$final.support.FIS_1,
                                      "<br>FIS_2 Descriptor: ", df_All$final.descr.FIS_2, "<br>FIS_2 support: ", df_All$final.support.FIS_2,
                                      "<br>FIS_3 Descriptor: ", df_All$final.descr.FIS_3, "<br>FIS_3 support: ", df_All$final.support.FIS_3,
                                      "<br>FIS_4_ext Descriptor: ", df_All$final.descr.ext.FIS_4, "<br>FIS_4_ext support: ", df_All$final.support.ext.FIS_4,
                                      "<br>FIS_4_int Descriptor: ", df_All$final.descr.int.FIS_4, "<br>FIS_4_int support: ", df_All$final.support.int.FIS_4,
                                      # "<br>FIS_5 Descriptor: ", df_All$final.descr.FIS_5, "<br>FIS_5 support: ", df_All$final.support.FIS_5,
                                      "<br>FIS_6 Descriptor: ", df_All$final.descr.FIS_6, "<br>FIS_6 support: ", df_All$final.support.FIS_6,
                                      "<br>FIS-7 Descriptor: ", df_All$final.descr.FIS_7, "<br>FIS-1-support: ", df_All$final.support.FIS_7, "</p>"),
                       label = df_All$filenaam, # NA, # ("sherd", 1:length(pic)), # df_All$ContextLabel, #
                       color.background = "white",
                       color.border = as.character(df_All[, names(df_All) == input$nodecolor]),
                       color.highlight = "#000000", # "#15171a",
                       borderWidth = 4, # https://www.rdocumentation.org/packages/visNetwork/versions/2.0.9/topics/visNodes
                       value = sizeGroup, # df_nodesize$nodesize, #
                       image = paste('data:image/jpg;base64', txt[1:length(pic)], sep = ','),
                       stringsAsFactors = F)
    # include the hard cluster color with opacity 1 as an extra border around the image.

    ## edges ##
    edges = data.frame(from = df_links_Measures$Source, to = df_links_Measures$Target, width = df_links_Measures$Value2/10, color = "#8a8a8a", color.opacity = 0.6)


    visNetwork(nodes, edges, width = "100%", height = 1000) %>%
      visNodes(borderWidth = 3, shapeProperties = list(useBorderWithImage = TRUE)) %>% # color = input$color,
      # visGroups(groupname = "0", size = 20) %>% # color = "#5347d1",
      # visGroups(groupname = "1", size = 30) %>% # color = "#a63536",
      # visLegend(enabled = TRUE) %>%
      # visIgraphLayout(layout = "layout_with_mds", physics = TRUE) %>%
      visIgraphLayout(layout = "layout_with_kk", physics = FALSE) %>% # visLayout(randomSeed = 611) %>% hierarchical = TRUE visLayout(improvedLayout = TRUE) # randomSeed = 2
      # visPhysics(solver = "barnesHut") %>%
      visEvents(selectNode = "function(nodes) {
                Shiny.onInputChange('clicked_node_id', nodes.nodes);
                ;}", doubleClick = "function(nodes){
                Shiny.onInputChange('clicked_node_id', nodes.nodes);
                ;}") %>%
      # visExport() %>%
      visOptions(selectedBy = list(variable = "group", multiple = TRUE, hideColor = "rgba(200,200,200,0.8)"), # , hideColor = "rgba(51,51,47,0.3)"
                 highlightNearest = list(enabled = TRUE, degree = 1, hover = FALSE, hideColor = "rgba(200,200,200,0.8)"),
                 nodesIdSelection = list(enabled = TRUE, main = "Select node from drop down list",
                                         style = 'width: 250px; height: 26px;
                                         background: none;
                                         color: #080504;
                                         border: none;
                                         outline: none;')) %>%
      visInteraction(tooltipDelay = 0,
                     tooltipStyle = 'position:fixed; text-align:center; border-radius: 2px; padding:2px; font-family:cursive; font-size:10px; font-color:#78427a; background-color:#f4f5e6;',
                     multiselect = TRUE)

  })


  output$selectedId <- renderPrint({
    paste0("Selected nodes: ", list(input$clicked_node_id))
  })

  get_beedata = reactive({
    listnodes = input$clicked_node_id
    print(listnodes)
    selectedData = df_All$filenaam[listnodes]
    bee_melted$selected = 0
    bee_melted[bee_melted$filenaam %in% selectedData, "selected"] = 1
    bee_melted = bee_melted[order(bee_melted$selected),]
  })


  output$beeplot = renderPlotly({

    bee_melted_React = get_beedata()

    pp = ggplotly(ggplot(bee_melted_React, aes(x = value, y = as.factor(variable), color = as.factor(selected), customdata = filenaam)) + # fill = as.factor(brushed)
                    geom_point(alpha = 0.5, stroke = 1, position = jitterpos, aes(size = as.factor(selected))) +
                    ggtitle(label = "", subtitle = "") +
                    labs(x = "",
                         y = "") +
                    theme(legend.position = "none",
                          panel.background = element_rect(fill = "white",
                                                          colour = "white",
                                                          size = 0.5, linetype = "solid"),
                          panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                                          colour = "grey80"),
                          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                                          colour = "grey80"),
                          axis.text.x = element_text(color = "grey10", size = 8, face = "plain", angle = 90),
                          axis.text.y = element_text(color = "grey10", size = 8, face = "plain")) +
                    scale_color_manual(labels = c("not-selected", "selected"), values = c("#968c8c", "#900c3f")) +
                    scale_size_manual(labels = c("not-selected", "selected"), values = c(1, 2))
    )

    toWebGL(event_register(plotly::layout(pp, dragmode = "select"), 'plotly_selected'))

  })

  output$beeselected <- renderPrint({
    d = event_data("plotly_selected")
    if (is.null(d)) "Brushed points appear here (double-click to clear)" else d
  })


  output$imageColorLegend = renderImage({
    return(list(
      src = "C:/Users/u0112360/Documents/____/Sagalassos/__PhD/Articles/_Typology/Scripts_VesselMorphology/interactiveViz/colorlabel_annot_snip_2.JPG",
      contentType = "image/jpg",
      height = 100,
      alt = "Color Legend"
    ))
  }, deleteFile = FALSE)

  observeEvent(input$clicked_node_id, {
    output$selectedSherdImage = renderImage({
      return(list(
        src = paste0(data_df$filewd[input$clicked_node_id],"/", data_df$filename[input$clicked_node_id]),
        width = 550,
        contentType = "image/jpg",
        alt = "Full Profile Image"
      ))
    }, deleteFile = FALSE)

    output$sherdInfo = renderTable({
      t(data.frame(
        "Sherd" = data_df$filename[input$clicked_node_id],
        "Context" = data_df$ContextCompare[input$clicked_node_id],
        "Context_checked" = data_df$inERlist[input$clicked_node_id] # update
      ))
    })


    output$sherdClusterInfo = renderTable({t(cluster_df_selected[input$clicked_node_id, 3:(2+length(unique(cluster_df_selected$hard_cl)))])})

    dataplotCluster = as.data.frame(t(cluster_df_selected[input$clicked_node_id, 3:(2+length(unique(cluster_df_selected$hard_cl)))]))
    dataplotCluster$Var1 = rownames(dataplotCluster)
    colnames(dataplotCluster)[1] = "Memb"
    dataplotCluster$Memb = round(dataplotCluster$Memb*100,0)

    output$plotClusterInfo = renderPlot({
      ggplot(data = dataplotCluster, aes(x = Var1, y = Memb)) + #
        geom_bar(stat = "identity", fill = "black", color = "black", width = 0.4) + # tableFG$data$fillColor, tableFG$data$borderColor
        geom_text(aes(label = Memb), vjust = -0.2, hjust = 0.5, color = "#201513", size = 5) +
        labs(x = "",
             y = "") +
        theme(panel.background = element_rect(fill = "white",
                                              colour = "white",
                                              size = 0.25, linetype = "solid"),
              panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                              colour = "grey80"),
              panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                              colour = "grey80"),
              axis.text.x = element_text(color = "grey10", size = 14, face = "plain", angle = 45),
              axis.text.y = element_text(color = "grey10", size = 14, face = "plain"))
    })


  })


  output$plotSherdWTat2.3 = renderPlot({
    ggplot(data = dataplotMeasures, aes(x = sherd_WTat2.3, y = 1, color = is.full.profile)) +
      geom_point(alpha = 0.4, shape = 124, stroke = 16) +
      geom_vline(xintercept = dataplotMeasures$sherd_WTat2.3[input$clicked_node_id], size = 2) + # , aes(color = as.factor(dataplotMeasures$is.full.profile[input$clicked_node_id]))
      labs(x = "Sherd Wall Thickness 2/3",
           y = "") +
      theme(legend.position = "none",
            panel.background = element_rect(fill = "white",
                                            colour = "white",
                                            size = 0.25, linetype = "solid"),
            panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                            colour = "grey80"),
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                            colour = "grey80"),
            axis.text.x = element_text(color = "grey10", size = 12, face = "plain"),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()) +
      scale_color_manual(values = c("#900c3f", "#000000"))
  })

  output$plotSherdHeight = renderPlot({
    ggplot(data = dataplotMeasures, aes(x = sherd_height, y = 1, color = is.full.profile)) +
      geom_point(alpha = 0.4, shape = 124, stroke = 16) +
      geom_vline(xintercept = dataplotMeasures$sherd_height[input$clicked_node_id], size = 2) + # , aes(color = as.factor(dataplotMeasures$is.full.profile[input$clicked_node_id]))
      labs(x = "Sherd Height",
           y = "") +
      theme(legend.position = "none",
            panel.background = element_rect(fill = "white",
                                            colour = "white",
                                            size = 0.25, linetype = "solid"),
            panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                            colour = "grey80"),
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                            colour = "grey80"),
            axis.text.x = element_text(color = "grey10", size = 12, face = "plain"),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()) +
      scale_color_manual(values = c("#900c3f", "#000000"))
  })


  output$plotSherdRimDiam = renderPlot({
    ggplot(data = dataplotMeasures, aes(x = sherd_rim_diameter, y = 1, color = sherd_rim.diameter.check)) +
      geom_point(alpha = 0.4, shape = 124, stroke = 16) +
      geom_vline(xintercept = dataplotMeasures$sherd_rim_diameter[input$clicked_node_id], size = 2) + # , aes(color = as.factor(dataplotMeasures$sherd_rim.diameter.check[input$clicked_node_id]))
      labs(x = "Rim Diameter (in cm)",
           y = "") +
      theme(legend.position = "none",
            panel.background = element_rect(fill = "white",
                                            colour = "white",
                                            size = 0.25, linetype = "solid"),
            panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                            colour = "grey80"),
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                            colour = "grey80"),
            axis.text.x = element_text(color = "grey10", size = 12, face = "plain"),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()) +
      scale_color_manual(values = c("#000000", "#900c3f"))
  })

  output$plotSherdHeightWidthRatio = renderPlot({
    ggplot(data = dataplotMeasures, aes(x = sherd_HW, y = 1, color = is.full.profile)) +
      geom_point(alpha = 0.4, shape = 124, stroke = 16) +
      geom_vline(xintercept = dataplotMeasures$sherd_HW[input$clicked_node_id], size = 2) + # , aes(color = as.factor(dataplotMeasures$is.full.profile[input$clicked_node_id]))
      labs(x = "Rim / Sherd Height",
           y = "") +
      theme(legend.position = "none",
            panel.background = element_rect(fill = "white",
                                            colour = "white",
                                            size = 0.25, linetype = "solid"),
            panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                            colour = "grey80"),
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                            colour = "grey80"),
            axis.text.x = element_text(color = "grey10", size = 12, face = "plain"),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()) +
      scale_color_manual(values = c("#900c3f", "#000000"))
  })

  output$plotSherdIncl = renderPlot({
    ggplot(data = dataplotMeasures, aes(x = sherd_inclination, y = 1, color = is.full.profile)) +
      geom_point(alpha = 0.4, shape = 124, stroke = 16) +
      geom_vline(xintercept = dataplotMeasures$sherd_inclination[input$clicked_node_id], size = 2) + # , aes(color = as.factor(dataplotMeasures$is.full.profile[input$clicked_node_id]))
      labs(x = "Sherd Wall Inclination",
           y = "") +
      theme(legend.position = "none",
            panel.background = element_rect(fill = "white",
                                            colour = "white",
                                            size = 0.25, linetype = "solid"),
            panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                            colour = "grey80"),
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                            colour = "grey80"),
            axis.text.x = element_text(color = "grey10", size = 12, face = "plain"),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()) +
      scale_color_manual(values = c("#900c3f", "#000000"))
  })


  get_clusterData_FIS.1 = reactive({
    listnodes = input$clicked_node_id
    print(listnodes)
    selectedData_FIS_1 = dataplot_FIS_1_ncls$filenaam[listnodes]
    dataplot_FIS_1_ncls$selected = 0
    dataplot_FIS_1_ncls[dataplot_FIS_1_ncls$filenaam %in% selectedData_FIS_1, "selected"] = 1
    dataplot_FIS_1_ncls = dataplot_FIS_1_ncls[order(dataplot_FIS_1_ncls$selected),]
  })
  output$FISplot_by_cluster_1 = renderPlot({
    dataplot_FIS_1_React = get_clusterData_FIS.1()
    ggparcoord(data = dataplot_FIS_1_React,
               columns = 2:4,
               scale = "globalminmax",
               showPoints = TRUE,
               alphaLines = 0.4,
               mapping = aes(color = as.factor(selected), size = as.factor(selected))#,
               # groupColumn = "selected"
    ) + # scale_color_viridis(discrete=TRUE) +
      facet_wrap(~ hard_cl, ncol = 6) +
      theme(legend.position = "none",
            panel.background = element_rect(fill = "white",
                                            colour = "white",
                                            size = 0.25, linetype = "solid"),
            panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                            colour = "grey80"),
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                            colour = "grey80"),
            axis.text.x = element_text(color = "grey10", size = 12, face = "plain", angle = 25),
            axis.text.y = element_text(color = "grey10", size = 12, face = "plain"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank()) +
      scale_color_manual(values = c("#968c8c", "#900c3f")) +
      scale_size_manual(values = c(1, 3))
  })


  get_clusterData_FIS.2 = reactive({
    listnodes = input$clicked_node_id
    print(listnodes)
    selectedData_FIS_2 = dataplot_FIS_2_ncls$filenaam[listnodes]
    dataplot_FIS_2_ncls$selected = 0
    dataplot_FIS_2_ncls[dataplot_FIS_2_ncls$filenaam %in% selectedData_FIS_2, "selected"] = 1
    dataplot_FIS_2_ncls = dataplot_FIS_2_ncls[order(dataplot_FIS_2_ncls$selected),]
  })
  output$FISplot_by_cluster_2 = renderPlot({
    dataplot_FIS_2_React = get_clusterData_FIS.2()
    ggparcoord(data = dataplot_FIS_2_React,
               columns = 2:3,
               scale = "globalminmax",
               showPoints = TRUE,
               alphaLines = 0.4,
               mapping = aes(color = as.factor(selected), size = as.factor(selected))#,
               # groupColumn = "selected"
    ) + # scale_color_viridis(discrete=TRUE) +
      facet_wrap(~ hard_cl, ncol = 6) +
      theme(legend.position = "none",
            panel.background = element_rect(fill = "white",
                                            colour = "white",
                                            size = 0.25, linetype = "solid"),
            panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                            colour = "grey80"),
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                            colour = "grey80"),
            axis.text.x = element_text(color = "grey10", size = 12, face = "plain", angle = 25),
            axis.text.y = element_text(color = "grey10", size = 12, face = "plain"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank()) +
      scale_color_manual(values = c("#968c8c", "#900c3f")) +
      scale_size_manual(values = c(1, 3))
  })



  get_clusterData_FIS.3 = reactive({
    listnodes = input$clicked_node_id
    print(listnodes)
    selectedData_FIS_3 = dataplot_FIS_3_ncls$filenaam[listnodes]
    dataplot_FIS_3_ncls$selected = 0
    dataplot_FIS_3_ncls[dataplot_FIS_3_ncls$filenaam %in% selectedData_FIS_3, "selected"] = 1
    dataplot_FIS_3_ncls = dataplot_FIS_3_ncls[order(dataplot_FIS_3_ncls$selected),]
  })
  output$FISplot_by_cluster_3 = renderPlot({
    dataplot_FIS_3_React = get_clusterData_FIS.3()
    ggparcoord(data = dataplot_FIS_3_React,
               columns = 2:3,
               scale = "globalminmax",
               showPoints = TRUE,
               alphaLines = 0.4,
               mapping = aes(color = as.factor(selected), size = as.factor(selected))#,
               # groupColumn = "selected"
    ) + # scale_color_viridis(discrete=TRUE) +
      facet_wrap(~ hard_cl, ncol = 6) +
      theme(legend.position = "none",
            panel.background = element_rect(fill = "white",
                                            colour = "white",
                                            size = 0.25, linetype = "solid"),
            panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                            colour = "grey80"),
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                            colour = "grey80"),
            axis.text.x = element_text(color = "grey10", size = 12, face = "plain", angle = 25),
            axis.text.y = element_text(color = "grey10", size = 12, face = "plain"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank()) +
      scale_color_manual(values = c("#968c8c", "#900c3f")) +
      scale_size_manual(values = c(1, 3))
  })

  get_clusterData_FIS.4.ext = reactive({
    listnodes = input$clicked_node_id
    print(listnodes)
    selectedData_FIS_4.ext = dataplot_FIS_4.ext_ncls$filenaam[listnodes]
    dataplot_FIS_4.ext_ncls$selected = 0
    dataplot_FIS_4.ext_ncls[dataplot_FIS_4.ext_ncls$filenaam %in% selectedData_FIS_4.ext, "selected"] = 1
    dataplot_FIS_4.ext_ncls = dataplot_FIS_4.ext_ncls[order(dataplot_FIS_4.ext_ncls$selected),]
  })
  output$FISplot_by_cluster_4.ext = renderPlot({
    dataplot_FIS_4.ext_React = get_clusterData_FIS.4.ext()
    ggparcoord(data = dataplot_FIS_4.ext_React,
               columns = 2:3,
               scale = "globalminmax",
               showPoints = TRUE,
               alphaLines = 0.4,
               mapping = aes(color = as.factor(selected), size = as.factor(selected))#,
               # groupColumn = "selected"
    ) + # scale_color_viridis(discrete=TRUE) +
      facet_wrap(~ hard_cl, ncol = 6) +
      theme(legend.position = "none",
            panel.background = element_rect(fill = "white",
                                            colour = "white",
                                            size = 0.25, linetype = "solid"),
            panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                            colour = "grey80"),
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                            colour = "grey80"),
            axis.text.x = element_text(color = "grey10", size = 12, face = "plain", angle = 25),
            axis.text.y = element_text(color = "grey10", size = 12, face = "plain"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank()) +
      scale_color_manual(values = c("#968c8c", "#900c3f")) +
      scale_size_manual(values = c(1, 3))
  })

  get_clusterData_FIS.4.int = reactive({
    listnodes = input$clicked_node_id
    print(listnodes)
    selectedData_FIS_4.int = dataplot_FIS_4.int_ncls$filenaam[listnodes]
    dataplot_FIS_4.int_ncls$selected = 0
    dataplot_FIS_4.int_ncls[dataplot_FIS_4.int_ncls$filenaam %in% selectedData_FIS_4.int, "selected"] = 1
    dataplot_FIS_4.int_ncls = dataplot_FIS_4.int_ncls[order(dataplot_FIS_4.int_ncls$selected),]
  })
  output$FISplot_by_cluster_4.int = renderPlot({
    dataplot_FIS_4.int_React = get_clusterData_FIS.4.int()
    ggparcoord(data = dataplot_FIS_4.int_React,
               columns = 2:3,
               scale = "globalminmax",
               showPoints = TRUE,
               alphaLines = 0.4,
               mapping = aes(color = as.factor(selected), size = as.factor(selected))#,
               # groupColumn = "selected"
    ) + # scale_color_viridis(discrete=TRUE) +
      facet_wrap(~ hard_cl, ncol = 6) +
      theme(legend.position = "none",
            panel.background = element_rect(fill = "white",
                                            colour = "white",
                                            size = 0.25, linetype = "solid"),
            panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                            colour = "grey80"),
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                            colour = "grey80"),
            axis.text.x = element_text(color = "grey10", size = 12, face = "plain", angle = 25),
            axis.text.y = element_text(color = "grey10", size = 12, face = "plain"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank()) +
      scale_color_manual(values = c("#968c8c", "#900c3f")) +
      scale_size_manual(values = c(1, 3))
  })


  get_clusterData_FIS.6 = reactive({
    listnodes = input$clicked_node_id
    print(listnodes)
    selectedData_FIS_6 = dataplot_FIS_6_ncls$filenaam[listnodes]
    dataplot_FIS_6_ncls$selected = 0
    dataplot_FIS_6_ncls[dataplot_FIS_6_ncls$filenaam %in% selectedData_FIS_6, "selected"] = 1
    dataplot_FIS_6_ncls = dataplot_FIS_6_ncls[order(dataplot_FIS_6_ncls$selected),]
  })
  output$FISplot_by_cluster_6 = renderPlot({
    dataplot_FIS_6_React = get_clusterData_FIS.6()
    ggparcoord(data = dataplot_FIS_6_React,
               columns = 2:3,
               scale = "globalminmax",
               showPoints = TRUE,
               alphaLines = 0.4,
               mapping = aes(color = as.factor(selected), size = as.factor(selected))#,
               # groupColumn = "selected"
    ) + # scale_color_viridis(discrete=TRUE) +
      facet_wrap(~ hard_cl, ncol = 6) +
      theme(legend.position = "none",
            panel.background = element_rect(fill = "white",
                                            colour = "white",
                                            size = 0.25, linetype = "solid"),
            panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                            colour = "grey80"),
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                            colour = "grey80"),
            axis.text.x = element_text(color = "grey10", size = 12, face = "plain", angle = 25),
            axis.text.y = element_text(color = "grey10", size = 12, face = "plain"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank()) +
      scale_color_manual(values = c("#968c8c", "#900c3f")) +
      scale_size_manual(values = c(1, 3))
  })

  get_clusterData_FIS.7 = reactive({
    listnodes = input$clicked_node_id
    print(listnodes)
    selectedData_FIS_7 = dataplot_FIS_7_ncls$filenaam[listnodes]
    dataplot_FIS_7_ncls$selected = 0
    dataplot_FIS_7_ncls[dataplot_FIS_7_ncls$filenaam %in% selectedData_FIS_7, "selected"] = 1
    dataplot_FIS_7_ncls = dataplot_FIS_7_ncls[order(dataplot_FIS_7_ncls$selected),]
    # dataplot_FIS_7_ncls$selected = as.factor(dataplot_FIS_7_ncls$selected)
  })
  output$FISplot_by_cluster_7 = renderPlot({
    dataplot_FIS_7_React = get_clusterData_FIS.7()
    ggparcoord(data = dataplot_FIS_7_React,
               columns = 2:3,
               scale = "globalminmax",
               showPoints = TRUE,
               alphaLines = 0.4,
               mapping = aes(color = as.factor(selected), size = as.factor(selected))#,
               # groupColumn = "selected"
    ) + # scale_color_viridis(discrete=TRUE) +
      facet_wrap(~ hard_cl, ncol = 6) +
      theme(legend.position = "none",
            panel.background = element_rect(fill = "white",
                                            colour = "white",
                                            size = 0.25, linetype = "solid"),
            panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                            colour = "grey80"),
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                            colour = "grey80"),
            axis.text.x = element_text(color = "grey10", size = 12, face = "plain", angle = 25),
            axis.text.y = element_text(color = "grey10", size = 12, face = "plain"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank()) +
      scale_color_manual(values = c("#968c8c", "#900c3f")) +
      scale_size_manual(values = c(1, 3))
  })




  output$distM_plot = renderHighchart({
    hchart(distM # ,  # last plot in: https://jkunst.com/highcharter/articles/showcase.html # https://jkunst.com/highcharter/articles/highcharter.html
           # "heatmap" #,
           # hcaes(x = Var1, y = Var2, value = value)
    ) %>%
      # hc_colorAxis(
      #    stops = color_stops(10, viridisLite::inferno(10, direction = -1)),
      #    type = "logarithmic"
      # ) %>%
      hc_yAxis(
        title = list(text = ""),
        labels = FALSE
      ) %>%
      hc_xAxis(
        title = list(text = ""),
        labels = FALSE
      ) %>%
      # hc_tooltip(
      #   formatter = fntltp
      # ) %>%
      # hc_size(height = 800
      # ) %>%
      hc_plotOptions(
        series = list(
          boderWidth = 1,
          dataLabels = list(enabled = FALSE),
          events = list(
            click = JS("function(event) {

                       var xstr = event.point.series.xAxis.categories[event.point.x].toString();
                       var ystr = event.point.series.yAxis.categories[event.point.y].toString();

                       var data = {
                       x: xstr,
                       y: ystr,
                       nonce: Math.random()
                       };

                       Shiny.onInputChange('matrix_click', data);

  }"))
        )
      )
  })

  #  event listener
  observeEvent(input$matrix_click, {

    # retrieve x and y variables from matrix
    dfplot_clicked_i$x_val[1:dim(mat_circumference_scaled[[input$matrix_click$x]])[1]] <- mat_circumference_scaled[[input$matrix_click$x]][,1]
    dfplot_clicked_i$y_val[1:dim(mat_circumference_scaled[[input$matrix_click$x]])[1]] <- mat_circumference_scaled[[input$matrix_click$x]][,2]

    dfplot_clicked_j$x_val[1:dim(mat_circumference_scaled[[input$matrix_click$y]])[1]] <- mat_circumference_scaled[[input$matrix_click$y]][,1]
    dfplot_clicked_j$y_val[1:dim(mat_circumference_scaled[[input$matrix_click$y]])[1]] <- mat_circumference_scaled[[input$matrix_click$y]][,2]

    # one dfplot_clicked
    dfplot_clicked_i = na.omit(dfplot_clicked_i)
    dfplot_clicked_j = na.omit(dfplot_clicked_j)
    dfplot_clicked_i$factor[1:dim(mat_circumference_scaled[[input$matrix_click$x]])[1]] = "i"
    dfplot_clicked_j$factor[1:dim(mat_circumference_scaled[[input$matrix_click$y]])[1]] = "j"
    dfplot_clicked = rbind.data.frame(dfplot_clicked_i, dfplot_clicked_j)

    # overlay selected sherd plots
    output$plotBoth = renderPlot({
      ggplot(dfplot_clicked, aes(x = x_val, y = y_val, color = factor)) + geom_path(linemitre = 20) + geom_point() +
        labs(x = "",
             y = "") +
        theme(legend.position = "bottom",
              legend.direction = "horizontal",
              legend.text=element_text(size = 20, face = "italic"),
              legend.title = element_blank(),
              legend.background = element_blank(),
              legend.box.background = element_rect(colour = "grey10"),
              legend.key.width = unit(2.2,"cm"),
              panel.background = element_rect(fill = "white",
                                              colour = "white",
                                              size = 0.5, linetype = "solid"),
              panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                              colour = "grey80"),
              panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                              colour = "grey80"),
              axis.text.x = element_text(color = "grey10", size = 16, face = "plain"),
              axis.text.y = element_text(color = "grey10", size = 16, face = "plain"),
              axis.title.x = element_text(color = "grey10", size = 20, face = "plain"),
              axis.title.y = element_text(color = "grey10", size = 20, face = "plain")) # +
      #   scale_linetype_manual(labels = c("convex", "log-normal", "primate"), values = c("dotted", "solid", "dashed"))
    })

  })


  output$imageLogo1 = renderImage({
    return(list(
      src = paste0(logowd, "/logo_kuleuven.png"),
      contentType = "image/jpg",
      alt = "KUL Logo"
    ))
  }, deleteFile = FALSE)

  output$imageLogo2 = renderImage({
    return(list(
      src = paste0(logowd, "/logo_uhasselt_small.png"),
      contentType = "image/jpg",
      alt = "UHasselt Logo"
    ))
  }, deleteFile = FALSE)


  # Table to get infor for FIS
  output$FIStable = renderDT(df_FIStable, filter = "top", options = list(pageLength = 5))


  observe({
    print(input$selectData)
  })


}



