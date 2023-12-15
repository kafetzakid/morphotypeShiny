library(shiny)
library(shinythemes)
library(altair) # altair installation: https://vegawidget.github.io/altair/articles/installation.html
library(readxl)
library(Rtsne)
library(umap)
library(stad)
library(igraph)

# read datasets
data_df = as.data.frame(read.csv("df_pointMeasures_realD_FG_20230209.csv"))
distM = read.csv("distM_Eucl_FG_EFA_20231211.csv", row.names = 1)
# create input for stad
stadM = distM; rownames(stadM) = colnames(stadM) = NULL; stadM = as.dist(stadM)

# compute embedings
mds_eucl = cmdscale(distM, k = 2)
set.seed(54321); tsne_eucl = Rtsne(distM, is_distance = TRUE, pca = FALSE, perplexity = floor((dim(distM)[1]-1)/3), theta = 0.0)
set.seed(54321); umap_eucl = umap(distM)
set.seed(54321); stad_eucl = stad(stadM, filter_values = data_df$sherd_HWratio, num_intervals = 10)
stad_coord = layout_nicely(stad_eucl$graph)

# combinme data
plot_embed = cbind.data.frame(mds_eucl, tsne_eucl$Y, umap_eucl$layout, stad_coord)
colnames(plot_embed) = c("MDS_d1", "MDS_d2", "tSNE_d1", "tSNE_d2", "UMAP_d1", "UMAP_d2", "STAD_d1", "STAD_d2")
plot_embed$filename = rownames(plot_embed)
plot_embed_labeled = merge(plot_embed, data_df, by.x = "filename")
plot_embed_labeled$Type = plot_embed_labeled$TV

# set data types
plot_embed$filename = as.character(plot_embed$filename)
plot_embed_labeled$Type = as.character(plot_embed_labeled$Type)
plot_embed_labeled$FG = as.character(plot_embed_labeled$FG_Final)

# order FG alphabetically
plot_embed_labeled = plot_embed_labeled[order(plot_embed_labeled$FG),]

# define colors per FG category
colorset = c("#E69F00","#56B4E9","#009E73","#F0E442","#0072B2")
domain_var = plot_embed_labeled$FG
range_color = colorset

# create brush
brush = alt$selection(type = "interval", resolve = "global")

# create base plot
base = alt$Chart(plot_embed_labeled)$
  mark_circle(fillOpacity = 0.9, size = 80)$
  encode(
    y = "MDS_d2",
    x = "MDS_d1",
    color = alt$condition(brush, alt$Color('FG:N', scale = alt$Scale(scheme = 'blueorange', domain = domain_var, range = range_color)), alt$ColorValue("#d6d6d6")),
    tooltip = c("FG:N", "filename:N")
  )$
  add_selection(brush)$
  properties(width = 400, height = 400)

# create plot matrix
map_matrix = (base | base$encode(y = "tSNE_d2", x = "tSNE_d1")) & (base$encode(y = "UMAP_d2", x = "UMAP_d1") | base$encode(y = "STAD_d2", x = "STAD_d1"))

# create ui
ui = tags$div(fluidPage(theme = shinytheme("journal"), 
                        headerPanel("MDS of pairwise distances"),
                        fluidRow(
                            column(width = 12, vegawidget::vegawidgetOutput("heatmap", width = "100%", height="600px"))
                          )
                        )
)

# create server
server <- function(input, output) {
  output$heatmap <- vegawidget::renderVegawidget({vegawidget(map_matrix)})
}

# run shiny app
shinyApp(ui = ui, server = server)