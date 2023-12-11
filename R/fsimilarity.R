library(shiny)
library(highcharter)
library(ggplot2)

# read distance matrix
distM = read.csv(paste0(readwd, "distM_Eucl_FG_EFA_20231211.csv"), row.names = 1)

# circumference data
load(paste0(readwd, "circumference_scaled_FG_20231101.RData"))
rowN = max(unlist(lapply(mat_circumference_scaled, dim)))
names(mat_circumference_scaled) = names(distM)

# order data according to FG
data_df = read.csv(paste0(readwd, "df_pointMeasures_realD_FG_20230209.csv"))
seqID = order(data_df$FG_Final)
distM = distM[seqID, seqID]
mat_circumference_scaled = mat_circumference_scaled[seqID]

# filter data according to FG
selectFG = "A"
keepidx = which(data_df$FG_Final == selectFG)
distM = distM[keepidx, keepidx]
mat_circumference_scaled = mat_circumference_scaled[keepidx]
rm(data_df)

# process matrix data
distM = as.matrix(distM)
distM = distM/max(distM)

# data clicked
dfplot_clicked_i = data.frame(matrix(ncol = 2, nrow = rowN))
colnames(dfplot_clicked_i) = c("x_val",
                               "y_val")
dfplot_clicked_j = dfplot_clicked_i

# added
options(highcharter.rjson = FALSE)

# shiny ui
ui = fluidPage(
  
  titlePanel("Interactive distance matrix to inspect form similarity")
  
  , fluidRow(column(8
                    , highchartOutput('distM_plot', height = "1000px", width = "1000px")
  ), column(2, 
            br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
            plotOutput("plotRow", height = "200px"), br(),
            plotOutput("plotCol", height = "200px"), br(),
            plotOutput("plotBoth", height = "200px"))
  )
)

# shiny server
server = function(input, output, session) {
  
  output$distM_plot = renderHighchart({
    hchart(distM
    ) %>%
    hc_boost(
      enabled = TRUE
    ) %>%
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
    
    # retrieve i and j profiles from matrix
    dfplot_clicked_i$x_val[1:dim(mat_circumference_scaled[[input$matrix_click$x]])[1]] = mat_circumference_scaled[[input$matrix_click$x]][,1]
    dfplot_clicked_i$y_val[1:dim(mat_circumference_scaled[[input$matrix_click$x]])[1]] = mat_circumference_scaled[[input$matrix_click$x]][,2]
    
    dfplot_clicked_j$x_val[1:dim(mat_circumference_scaled[[input$matrix_click$y]])[1]] = mat_circumference_scaled[[input$matrix_click$y]][,1]
    dfplot_clicked_j$y_val[1:dim(mat_circumference_scaled[[input$matrix_click$y]])[1]] = mat_circumference_scaled[[input$matrix_click$y]][,2]
    
    # process dfplot_clicked
    dfplot_clicked_i = na.omit(dfplot_clicked_i)
    dfplot_clicked_j = na.omit(dfplot_clicked_j)
    dfplot_clicked_i$factor[1:dim(mat_circumference_scaled[[input$matrix_click$x]])[1]] = "i"
    dfplot_clicked_j$factor[1:dim(mat_circumference_scaled[[input$matrix_click$y]])[1]] = "j"
    dfplot_clicked = rbind.data.frame(dfplot_clicked_i, dfplot_clicked_j)
    
    # plot profile of selected row
    output$plotRow = renderPlot({
      ggplot(dfplot_clicked_i, aes(x = x_val, y = y_val)) + geom_path(linemitre = 20, color = "#606060") + geom_point() +
        theme(legend.position = "none",
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
              axis.title.y = element_text(color = "grey10", size = 20, face = "plain"))
    })
    
    # plot profile of selected column
    output$plotCol = renderPlot({
      ggplot(dfplot_clicked_j, aes(x = x_val, y = y_val)) + geom_path(linemitre = 20, color = "#606060") + geom_point() +
        theme(legend.position = "none",
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
              axis.title.y = element_text(color = "grey10", size = 20, face = "plain"))
    })
    
    # plot superimposed selected profiles
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
              axis.title.y = element_text(color = "grey10", size = 20, face = "plain"))
    })
    
  })
  
}
shinyApp(ui = ui, server = server)