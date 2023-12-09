library(factoextra)
library(ggpattern)

# read measures data
data_df = read.csv("df_pointMeasures_realD_FG_20230209.csv")

# make ID
data_df$filename = substr(data_df$filename, 1, nchar(data_df$filename) - 4)

# normalize the data
df_norm = as.data.frame(data_df, stringsAsFactors = FALSE)
exclV = c("filename", "scale", "scale_check",  "sherd_rimD_certain", 
          "has.fullbase", "fullbase", "base.height", "is.full.profile",
          "max.WT", "FG_Final", "ContextCompare", "filewd", "TV_Final",
          "Type")
df_norm = df_norm[,!(names(df_norm) %in% exclV)]
vmin = lapply(df_norm, min)
vmax = lapply(df_norm, max)
for (i in 1:dim(df_norm)[2]) {
  if ((vmax[[i]] - vmin[[i]]) > 0) {
    df_norm[,i] = (df_norm[,i] - vmin[[i]]) / (vmax[[i]] - vmin[[i]])
  } else {
    df_norm[,i] = df_norm[,i]
  }
}
summary(df_norm)

# plot all data in violin plots (per FG).
# descrive for overall data, TO DO

# apply PCA
pca_vars = prcomp(df_norm, scale = F, center = T)
eig.val = get_eigenvalue(pca_vars)
eig.val$PC = 1:length(rownames(eig.val))

# get principal components rotation angle
pcord = abs(pca_vars$rotation[,1]/pca_vars$rotation[,2])*sign(pca_vars$rotation[,1])

# color background based on FG category
opacity = FALSE # opacity included after consistency is calculated
catlabel = FALSE

cluster_df = data.frame("hard_cl" = data_df$FG_Final, "filenaam" = data_df$filename)
head(cluster_df)

# initialize dataset
colorset_clusters = coloropacity_clusters = rep(NA, dim(cluster_df)[1])

if (catlabel) {
  cols_clusters = c("#1e60c9", "#b02c2c", "#c9b81e", "#754a31", "#000000") # TO DO: change colors
  colorset_clusters = cluster_df$hard_cl
  for (i in 1:length(cluster_df$hard_cl)) {
    if(cluster_df$hard_cl[i] == "A") {
      colorset_clusters[i] = cols_clusters[1]; coloropacity_clusters[i] = cluster_df$dist_from1[i]
    } else if(cluster_df$hard_cl[i] == "B") {
      colorset_clusters[i] = cols_clusters[2]; coloropacity_clusters[i] = cluster_df$dist_from2[i]
    } else if(cluster_df$hard_cl[i] == "C") {
      colorset_clusters[i] = cols_clusters[3]; coloropacity_clusters[i] = cluster_df$dist_from3[i]
    } else if(cluster_df$hard_cl[i] == "D") {
      colorset_clusters[i] = cols_clusters[4]; coloropacity_clusters[i] = cluster_df$dist_from4[i] 
    } else {
      colorset_clusters[i] = cols_clusters[5]; coloropacity_clusters[i] = cluster_df$dist_from5[i]
    }
  }
} else {
  colorset_clusters = rep("#f7d6b0", dim(cluster_df)[1])
}
colorset_clusters

if (opacity) {
  range_min = 0.2; range_max = 1
  coloropacity_clusters_min = min(coloropacity_clusters); coloropacity_clusters_max = max(coloropacity_clusters)
  coloropacity_clusters_norm = range_min + ((coloropacity_clusters - coloropacity_clusters_min) / (coloropacity_clusters_max - coloropacity_clusters_min))*(range_max - range_min)
} else {
  coloropacity_clusters_norm = 1
}

# choose profile
id = "SADR011208.jpg"
gli = which(data_df$filename == id)

data = data.frame(
  id = 1:dim(df_norm)[2],
  value = as.numeric(df_norm[gli,]+0.01)*1000,
  pattern.type = c(rep(c("a","b","c","d"), round(dim(df_norm)[2]/4)), "a")
)
# create plot
plot_rim_glyph = ggplot(data, aes(x = as.factor(id), y = value)) +
  geom_bar(stat = "identity") +
  geom_col_pattern(
    aes(pattern_type = as.factor(pattern.type)),
     pattern = 'magick', fill = 'grey20', colour = "black") +
    ylim(-100,1015) +
    theme_minimal() +
    theme(legend.position = "none", plot.background = element_rect(fill = alpha(colorset_clusters[gli], coloropacity_clusters_norm[gli]), colour = alpha(colorset_clusters[gli], coloropacity_clusters_norm[gli])), 
        axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(),
        plot.margin = unit(rep(-2,4), "cm")
) +
  coord_polar(start = 0) +
  scale_pattern_type_discrete(choices = c('gray100', 'bricks', 'gray20', 'checkerboard'))

# display plot
plot_rim_glyph
