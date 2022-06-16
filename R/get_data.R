# distance matrix
distM = read.csv(paste0(readwd,"distM_2Doutlines_dtw_1B150_20210802.csv"), row.names = 1)
# circumference data
load(paste0(readwd, "circumference_scaled_Rim_1B150_selectedERlist_20210802.RData"))
rowN = max(unlist(lapply(mat_circumference_scaled, dim)))
names(mat_circumference_scaled) = names(distM)

# process matrix data
distM = as.matrix(distM)
distM = distM/max(distM)

# data clicked
dfplot_clicked_i = data.frame(matrix(ncol = 2, nrow = rowN))
colnames(dfplot_clicked_i) = c("x_val",
                               "y_val")
dfplot_clicked_j = dfplot_clicked_i


# Tab Network #####

# high and low memberhsip, marginal cases for 1B150 clustering
special_set_1B150 = data.frame(
  "filename" = c("SADR021655.jpg", "SADR021680.jpg", "SADR020634.jpg", "SADR011104.jpg", "SADR020954.jpg", "SADR021208.jpg", "SADR010608.jpg", "SADR011077.jpg", "SADR011076.jpg"),
  "label" = c("central", "central", "central", "peipheral", "peipheral & marginal", "peipheral & marginal", "marginal", "marginal", "marginal")
)



# measures data
data_df = as.data.frame(read.csv("C:/Users/u0112360/Documents/____/Sagalassos/__PhD/Articles/_Typology/Results_VM_paper1/20210802_VMpaper/df_pointMeasures_Rim_1B150_selectedERlist_20210928.csv"))
data_df$filename = as.character(data_df$filename)
data_df$Type = as.character(data_df$Type)
rownames(data_df) = data_df$filename

# obs order initial
obs_order_initial = as.character(data_df$filename)

# # Include the data from the Fuzzy Rules #####
# Flattness
data_df$Rim_flattness_med = data_df$Rim_Diam_extra_half/data_df$Rim_median.WT
data_df$Rim_flattness_max = data_df$Rim_Diam_extra_half/data_df$Rim_max.WT
data_df$WT_LipBot = data_df$Rim_lip_width_scaled/data_df$Rim_bottom_width_scaled # included in FIS

# From FIS data created
data_df_FIS_created = as.data.frame(read.csv("C:/Users/u0112360/Documents/____/Sagalassos/__PhD/Articles/_Typology/Results_VM_paper1/20210802_VMpaper/df_extracted_FIS_1B150_1B170_1A100_1C100_20210802.csv"))
data_df_FIS_created$filename = as.character(data_df_FIS_created$filename)
# as.data.frame(t(data_df_FIS_created[180,]))
data_df_FIS_created[,(colnames(data_df_FIS_created) %in% c("VC.lengths", "VC.sides", "VC.Height.Ratio"))] = NULL # use later for vertical cut
data_df_FIS_created$distTop.Out = as.numeric(as.character(data_df_FIS_created$distTop.Out))
data_df_FIS_created$distTop.In = as.numeric(as.character(data_df_FIS_created$distTop.In))
data_df_FIS_created$CutsSymmetric = as.numeric(as.character(data_df_FIS_created$CutsSymmetric))
data_df_FIS_created$VC.Outline.Ratio = as.numeric(as.character(data_df_FIS_created$VC.Outline.Ratio))
# impute values
data_df_FIS_created$CutsSymmetric[is.na(data_df_FIS_created$CutsSymmetric)] = -0.1
data_df_FIS_created$distTop.In[is.na(data_df_FIS_created$distTop.In)] = -1
data_df_FIS_created$distTop.Out[is.na(data_df_FIS_created$distTop.Out)] = -1

data_df = merge(data_df, data_df_FIS_created, by = "filename")

# data FIS for ellipse variable
data_df_FIS_ellipse = as.data.frame(read.csv("C:/Users/u0112360/Documents/____/Sagalassos/__PhD/Articles/_Typology/Results_VM_paper1/20210802_VMpaper/df_extracted_FIS_1B150_1B170_1A100_1C100_20210920.csv"))
data_df_FIS_ellipse$filename = as.character(data_df_FIS_ellipse$filename)
data_df = merge(data_df, data_df_FIS_ellipse, by = "filename")


# From FIS-1
data_df_FIS_1 = as.data.frame(read.csv("C:/Users/u0112360/Documents/____/Sagalassos/__PhD/Articles/_Typology/Results_VM_paper1/20210802_VMpaper/df_extracted_FIS_1_1B150_1B170_1A100_1C100_20210802.csv"))
data_df_FIS_1$filename = as.character(data_df_FIS_1$filename)
data_df_FIS_1 = data_df_FIS_1[data_df_FIS_1$filename %in% data_df$filename, ]
#
# data for FIS plots by cluster
dataplot_FIS_1 = data.frame("filenaam" = data_df_FIS_1$filename, "everted.rim" = data_df_FIS_1$everted.rim, "straight.rim" = data_df_FIS_1$straight.rim, "inverted.rim" = data_df_FIS_1$inverted.rim) # , "hard_cl" = 0
dataplot_FIS_1$selected = 0
#
data_df_FIS_1 = data_df_FIS_1[, which(colnames(data_df_FIS_1) %in% c("filename", "final.support", "final.descr"))]
data_df_FIS_1$final.descr = as.character(data_df_FIS_1$final.descr)
colnames(data_df_FIS_1)[2:3] = c("final.support.FIS_1", "final.descr.FIS_1")
data_df = merge(data_df, data_df_FIS_1, by = "filename")

# From FIS-2
data_df_FIS_2 = as.data.frame(read.csv("C:/Users/u0112360/Documents/____/Sagalassos/__PhD/Articles/_Typology/Results_VM_paper1/20210802_VMpaper/df_extracted_FIS_2_1B150_1B170_1A100_1C100_20210802.csv"))
data_df_FIS_2$filename = as.character(data_df_FIS_2$filename)
data_df_FIS_2 = data_df_FIS_2[data_df_FIS_2$filename %in% data_df$filename, ]
#
# data for FIS plots by cluster
dataplot_FIS_2 = data.frame("filenaam" = data_df_FIS_2$filename, "horizontally.flattened.rim" = data_df_FIS_2$horizontally.flattened.rim, "not.horizontally.flattened.rim" = data_df_FIS_2$not.horizontally.flattened.rim) # , "hard_cl" = 0
dataplot_FIS_2$selected = 0
#
data_df_FIS_2 = data_df_FIS_2[, which(colnames(data_df_FIS_2) %in% c("filename", "final.support", "final.descr"))]
data_df_FIS_2$final.descr = as.character(data_df_FIS_2$final.descr)
colnames(data_df_FIS_2)[2:3] = c("final.support.FIS_2", "final.descr.FIS_2")
data_df = merge(data_df, data_df_FIS_2, by = "filename")

# From FIS-3
data_df_FIS_3 = as.data.frame(read.csv("C:/Users/u0112360/Documents/____/Sagalassos/__PhD/Articles/_Typology/Results_VM_paper1/20210802_VMpaper/df_extracted_FIS_3_1B150_1B170_1A100_1C100_20210802.csv"))
data_df_FIS_3$filename = as.character(data_df_FIS_3$filename)
data_df_FIS_3 = data_df_FIS_3[data_df_FIS_3$filename %in% data_df$filename, ]
#
# data for FIS plots by cluster
dataplot_FIS_3 = data.frame("filenaam" = data_df_FIS_3$filename, "rounded.rim" = data_df_FIS_3$rounded.rim, "not.rounded.rim" = data_df_FIS_3$not.rounded.rim) # , "hard_cl" = 0
dataplot_FIS_3$selected = 0
#
data_df_FIS_3 = data_df_FIS_3[, which(colnames(data_df_FIS_3) %in% c("filename", "final.support", "final.descr"))]
data_df_FIS_3$final.descr = as.character(data_df_FIS_3$final.descr)
colnames(data_df_FIS_3)[2:3] = c("final.support.FIS_3", "final.descr.FIS_3")
data_df = merge(data_df, data_df_FIS_3, by = "filename")

# From FIS-4
data_df_FIS_4 = as.data.frame(read.csv("C:/Users/u0112360/Documents/____/Sagalassos/__PhD/Articles/_Typology/Results_VM_paper1/20210802_VMpaper/df_extracted_FIS_4_1B150_1B170_1A100_1C100_20210802.csv"))
data_df_FIS_4$filename = as.character(data_df_FIS_4$filename)
data_df_FIS_4 = data_df_FIS_4[data_df_FIS_4$filename %in% data_df$filename, ]
#
# data for FIS plots by cluster
dataplot_FIS_4.ext = data.frame("filenaam" = data_df_FIS_4$filename, "vertically.flattened.exterior" = data_df_FIS_4$vertically.flattened.exterior, "not.vertically.flattened.exterior" = data_df_FIS_4$not.vertically.flattened.exterior) # , "hard_cl" = 0
dataplot_FIS_4.ext$selected = 0
#
# data for FIS plots by cluster
dataplot_FIS_4.int = data.frame("filenaam" = data_df_FIS_4$filename, "vertically.flattened.interior" = data_df_FIS_4$vertically.flattened.interior, "not.vertically.flattened.interior" = data_df_FIS_4$not.vertically.flattened.interior) # , "hard_cl" = 0
dataplot_FIS_4.int$selected = 0
#
data_df_FIS_4 = data_df_FIS_4[, which(colnames(data_df_FIS_4) %in% c("filename", "final.support.ext", "final.descr.ext", "final.support.int", "final.descr.int", "HC.int.ext"))]
data_df_FIS_4$final.descr.ext = as.character(data_df_FIS_4$final.descr.ext)
data_df_FIS_4$final.descr.int = as.character(data_df_FIS_4$final.descr.int)
colnames(data_df_FIS_4)[2:6] = c("final.support.ext.FIS_4", "final.descr.ext.FIS_4", "final.support.int.FIS_4", "final.descr.int.FIS_4", "final.descr.both.sides.FIS_4")
data_df = merge(data_df, data_df_FIS_4, by = "filename")

# # From FIS-5
# data_df_FIS_5 = as.data.frame(read.csv("C:/Users/u0112360/Documents/____/Sagalassos/__PhD/Articles/_Typology/Results_VM_paper1/20210802_VMpaper/df_extracted_FIS_5_1B150_1B170_1A100_1C100_20210802.csv"))
# data_df_FIS_5$filename = as.character(data_df_FIS_5$filename)
# data_df_FIS_5 = data_df_FIS_5[data_df_FIS_5$filename %in% data_df$filename, ]
# #
# # data for FIS plots by cluster
# dataplot_FIS_5 = data.frame("filenaam" = data_df_FIS_5$filename, "cut.rim" = data_df_FIS_5$cut.rim, "not.cut.rim" = data_df_FIS_5$not.cut.rim) # , "hard_cl" = 0
# dataplot_FIS_5$selected = 0
# #
# data_df_FIS_5 = data_df_FIS_5[, which(colnames(data_df_FIS_5) %in% c("filename", "final.support", "final.descr"))]
# data_df_FIS_5$final.descr = as.character(data_df_FIS_5$final.descr)
# colnames(data_df_FIS_5)[2:3] = c("final.support.FIS_5", "final.descr.FIS_5")
# data_df = merge(data_df, data_df_FIS_5, by = "filename")

# From FIS-6
data_df_FIS_6 = as.data.frame(read.csv("C:/Users/u0112360/Documents/____/Sagalassos/__PhD/Articles/_Typology/Results_VM_paper1/20210802_VMpaper/df_extracted_FIS_6_1B150_1B170_1A100_1C100_20210802.csv"))
data_df_FIS_6$filename = as.character(data_df_FIS_6$filename)
data_df_FIS_6 = data_df_FIS_6[data_df_FIS_6$filename %in% data_df$filename, ]
#
# data for FIS plots by cluster
dataplot_FIS_6 = data.frame("filenaam" = data_df_FIS_6$filename, "thickened.rim" = data_df_FIS_6$thickened.rim, "non.thickened.rim" = data_df_FIS_6$non.thickened.rim) # , "hard_cl" = 0
dataplot_FIS_6$selected = 0
#
data_df_FIS_6 = data_df_FIS_6[, which(colnames(data_df_FIS_6) %in% c("filename", "final.support", "final.descr"))]
data_df_FIS_6$final.descr = as.character(data_df_FIS_6$final.descr)
colnames(data_df_FIS_6)[2:3] = c("final.support.FIS_6", "final.descr.FIS_6")
data_df = merge(data_df, data_df_FIS_6, by = "filename")

# extra from FIS-6
data_df$rim.out.prot.length.selected = ifelse(data_df$rim.out.prot.length > data_df$rim.out.prot.length.2, data_df$rim.out.prot.length, data_df$rim.out.prot.length.2)
data_df$rim.inn.prot.length.selected = ifelse(data_df$Rim_incl_min_sign > data_df$rim.inn.prot.length.2, data_df$rim.inn.prot.length, data_df$rim.inn.prot.length.2)
data_df$rim.out.prot.length.selected_scaled = data_df$rim.out.prot.length.selected/data_df$Rim_height_manual
data_df$rim.inn.prot.length.selected_scaled = data_df$rim.inn.prot.length.selected/data_df$Rim_height_manual

# From FIS-7
data_df_FIS_7 = as.data.frame(read.csv("C:/Users/u0112360/Documents/____/Sagalassos/__PhD/Articles/_Typology/Results_VM_paper1/20210802_VMpaper/df_extracted_FIS_7_471-sherds_20210928.csv"))
data_df_FIS_7$filename = as.character(data_df_FIS_7$filename)
data_df_FIS_7 = data_df_FIS_7[data_df_FIS_7$filename %in% data_df$filename, ]
#
# data for FIS plots by cluster
dataplot_FIS_7 = data.frame("filenaam" = data_df_FIS_7$filename, "complicated.rim" = data_df_FIS_7$complicated.rim, "simple.rim" = data_df_FIS_7$simple.rim) # , "hard_cl" = 0
dataplot_FIS_7$selected = 0
#
data_df_FIS_7 = data_df_FIS_7[, which(colnames(data_df_FIS_7) %in% c("filename", "final.support", "final.descr"))]
data_df_FIS_7$final.descr = as.character(data_df_FIS_7$final.descr)
colnames(data_df_FIS_7)[2:3] = c("final.support.FIS_7", "final.descr.FIS_7")
data_df = merge(data_df, data_df_FIS_7, by = "filename")

# order data
obs_order_initial_id = match(obs_order_initial, data_df$filename) # added for the expected ordering
data_df = data_df[obs_order_initial_id, ]


# get plot data
df_labels = as.data.frame(as.character(data_df$TV), stringsAsFactors = FALSE)
data_df$filename = as.character(data_df$filename)
colnames(df_labels) = "TV"
rownames(df_labels) = data_df$filename
df_labels$TV = as.character(df_labels$TV)
df_labels$binaryLabel = data_df$is.full.profile
df_labels$binaryLabel = replace(df_labels$binaryLabel, df_labels$binaryLabel == TRUE, 1)
df_labels$binaryLabel = replace(df_labels$binaryLabel, df_labels$binaryLabel != 1, 0)

# colum = c("Rim_incl_sin_mean","rim.out.prot.length","rim.inn.prot.length", "rim.out.diff.length","rim.inn.diff.length","block.out.perC","block.perC", "trapez.perC","form.factor","aspect.ratio","roundness",
#           "Rim_HorizCut_WT","Rim_mean.WT","Rim_median.WT","Rim_sd.WT","Rim_min.WT","Rim_max.WT","RimWT_margin","Rim_Diam_extra_half","Rim_extent.1","Rim_curl",
#           "Rim_elongation.min","Rim_elongation.max","Rim_elongation.avg","Rim_radius.ratio","Rim_eccentricity","Rim_incl_sin_min","Rim_incl_sin_max","Rim_height_manual",
#           "WT.mean_norm", "WT.med_norm", "WT.var_norm", "WT.skew_norm", "WT.kurt_norm", "WT.BotMed_norm", "Rim_flattness_med", "Rim_flattness_max")
columCateg = c("filename", "TV", "Type", "filewd", "set", "is.full.profile", "sherd_rim.diameter.check", "Rim_HorizCut.Inn_Cont",
               "inERlist", "ContextCompare", "Year", "Site", "sherd_listlines.out", "sherd_listlines.inn", "Rim_HorizCut.Out_Cont",
               "Rim_HorizCut.Inn_Width", "Rim_HorizCut.Out_Width")
columFISdescr = c("final.descr.FIS_1", "final.descr.FIS_2", "final.descr.FIS_3", "final.descr.ext.FIS_4", "final.descr.int.FIS_4", "final.descr.both.sides.FIS_4", "final.descr.FIS_6", "final.descr.FIS_7") # "final.descr.FIS_5",

df_dataplot = data_df[, !(colnames(data_df) %in% columCateg)]
df_dataplot = df_dataplot[, !(colnames(df_dataplot) %in% columFISdescr)]
rownames(df_dataplot) = data_df$filename


## Beeplot Data ##
bee_dataplot = df_dataplot
# do not normalise pv, sherd_inclination, roundness, form.factor, slope.WT, MDLHratio, Rim_incl_sin
exclV = c("Rim_incl_sin_mean", "form.factor", "roundness", "form.factor", "Rim_extent.1", "Rim_eccentricity", "Rim_incl_sin_min", "Rim_incl_sin_max")
# exclV = c("roundness", "form.factor", "Rim_extent.1", "Rim_eccentricity",
#           "Rim_incl_sin_mean", "Rim_incl_sin_min", "Rim_incl_sin_max",
#           # "Rim_WT.mean.norm", "Rim_WT.med.norm", "Rim_WT.var.norm",
#           "Below.Rim_incl_sin_min", "Below.Rim_incl_sin_max", "Below.Rim_incl_sin_mean",
#           # 'FIS.1_mf.1.out','FIS.1_mf.1.avg','FIS.1_mf.1.inn','FIS.1_mf.2.out','FIS.1_mf.2.avg','FIS.1_mf.2.inn','FIS.1_mf.3.out','FIS.1_mf.3.avg','FIS.1_mf.3.inn','FIS.1_mf.4.out','FIS.1_mf.4.avg','FIS.1_mf.4.inn','straight.rim','slightly.bent.rim','quite.bent.rim','profoundly.bent.rim','straight.rim.sign','straight.rim.descr','slightly.bent.rim.sign','slightly.bent.rim.descr','quite.bent.rim.sign','quite.bent.rim.descr','profoundly.bent.rim.sign','profoundly.bent.rim.descr','final.support','interm.1.descr','interm.2.descr','final.descr',
#           'final.support.FIS_1', 'final.support.FIS_2', 'final.support.FIS_3', 'final.support.ext.FIS_4', 'final.support.int.FIS_4', 'final.support.FIS_5', 'final.support.FIS_6'
# ) # 'FIS6.var.1.inn','FIS6.var.1.out','FIS6.var.2.global','FIS6.var.3.global'
bee_dataplot = bee_dataplot[,!(names(bee_dataplot) %in% exclV)]
vmin = apply(bee_dataplot, 2, min, na.rm = TRUE)
vmax = apply(bee_dataplot, 2, max, na.rm = TRUE)
for (i in 1:dim(bee_dataplot)[2]) {
  if ((vmax[[i]] - vmin[[i]]) > 0) {
    bee_dataplot[,i] = (bee_dataplot[,i] - vmin[[i]]) / (vmax[[i]] - vmin[[i]])
  } else {
    bee_dataplot[,i] =  bee_dataplot[,i]
  }
}

# Merge bee_dataplot with non numeric data
bee_dataplot = cbind.data.frame(rownames(bee_dataplot), bee_dataplot, df_dataplot[,(names(df_dataplot) %in% exclV)])
names(bee_dataplot)[1] = "filenaam"
bee_dataplot$filenaam = as.character(bee_dataplot$filenaam)
bee_melted = melt(bee_dataplot, id = "filenaam")
bee_melted$selected = 0


# # Melt data
# bee_melted = melt(bee_dataplot, id = "filenaam")
# bee_melted$selected = 0

# order of observations
obs_order = as.character(bee_dataplot$filenaam)
# obs_order = unlist(strsplit(obs_order, ".jpg"))


# data for the Global plotMeasures
df_MuVo_all = as.data.frame(read.csv("C:/Users/u0112360/Documents/____/Sagalassos/__PhD/Articles/_Typology/Results_VM_paper1/20210612_multivocality/df_pointMeasures_multivocality_all.csv"))
df_MuVo_all = df_MuVo_all[df_MuVo_all$filename %in% data_df$filename, ]
dataplotMeasures = cbind.data.frame(df_MuVo_all$filename, df_MuVo_all$scale, df_MuVo_all$sherd_HWratio, df_MuVo_all$sherd_inclination, df_MuVo_all$sherd_rimDiam, df_MuVo_all$sherd_WThick1, df_MuVo_all$sherd_height)
colnames(dataplotMeasures) = c("filename", "scale", "sherd_HW", "sherd_inclination", "sherd_rim_diameter",  "sherd_WTat2.3", "sherd_height") # + "rim_diameter_check" + inclide WT just below rim => rim width?
dataplotMeasures$scaleCM = 3
dataplotMeasures$sherd_height = dataplotMeasures$scaleCM*dataplotMeasures$sherd_height/dataplotMeasures$scale
dataplotMeasures$sherd_rim_diameter = dataplotMeasures$scaleCM*dataplotMeasures$sherd_rim_diameter/dataplotMeasures$scale
dataplotMeasures$sherd_WTat2.3 = dataplotMeasures$scaleCM*dataplotMeasures$sherd_WTat2.3/dataplotMeasures$scale
dataplotMeasures_extra = data.frame("filename" = data_df$filename, "sherd_rim.diameter.check" = data_df$sherd_rim.diameter.check, "is.full.profile" = data_df$is.full.profile)
dataplotMeasures = merge(dataplotMeasures, dataplotMeasures_extra, by = "filename")
# order for the dataplotMeasures
obs_order_id = match(obs_order, dataplotMeasures$filename)
dataplotMeasures = dataplotMeasures[obs_order_id, ]


# jitter position
jitterpos = position_jitter(width = 0, height = 0, seed = 1)


# Data for Cluster Results
# Minkowski p = 4
# cluster_df_10 = as.data.frame(read.csv("C:/Users/u0112360/Documents/____/Sagalassos/__PhD/Articles/_Typology/Results_VM_paper1/20210802_VMpaper/df_cluster_results_Mink_v2_ncl-10_83of100-1B150_20210802.csv"), stingsAsFactors = FALSE)
cluster_df_10 = as.data.frame(read.csv("C:/Users/u0112360/Documents/____/Sagalassos/__PhD/Articles/_Typology/Results_VM_paper1/20210802_VMpaper/df_cluster_results_L4_RimDiam_inclBR_Mink_v2_ncl-3_83of100-1B150_20210802.csv"), stingsAsFactors = FALSE)
# cluster_df_10 = as.data.frame(read.csv("C:/Users/u0112360/Documents/____/Sagalassos/__PhD/Articles/_Typology/Results_VM_paper1/20210802_VMpaper/df_cluster_results_L4_RimDiam_inclBR_FIS_Mink_ncl-7_83of100-1B150_20210802.csv"), stingsAsFactors = FALSE)
cluster_df_10 = cluster_df_10[,-(which(colnames(cluster_df_10) %in% c("labelsTV","labelsType","input")))] # "dist_from1", "dist_from2", "input"
colnames(cluster_df_10)[1] = "filenaam"
# cluster_df_8 = as.data.frame(read.csv("C:/Users/u0112360/Documents/____/Sagalassos/__PhD/Articles/_Typology/Results_VM_paper1/20210802_VMpaper/df_cluster_results_Mink_v2_ncl-8_83of100-1B150_20210802.csv"), stingsAsFactors = FALSE)
# cluster_df_8 = as.data.frame(read.csv("C:/Users/u0112360/Documents/____/Sagalassos/__PhD/Articles/_Typology/Results_VM_paper1/20210802_VMpaper/df_cluster_results_L4_RimDiam_inclBR_Mink_ncl-4_83of100-1B150_20210802.csv"), stingsAsFactors = FALSE)
cluster_df_8 = as.data.frame(read.csv("C:/Users/u0112360/Documents/____/Sagalassos/__PhD/Articles/_Typology/Results_VM_paper1/20210802_VMpaper/df_cluster_results_L4_RimDiam_inclBR_Mink_v2_ncl-7_83of100-1B150_20210802.csv"), stingsAsFactors = FALSE)
# cluster_df_8 = as.data.frame(read.csv("C:/Users/u0112360/Documents/____/Sagalassos/__PhD/Articles/_Typology/Results_VM_paper1/20210802_VMpaper/df_cluster_results_L4_RimDiam_inclBR_FIS_Mink_ncl-5_83of100-1B150_20210802.csv"), stingsAsFactors = FALSE)
cluster_df_8 = cluster_df_8[,-(which(colnames(cluster_df_8) %in% c("labelsTV","labelsType","input")))] # "dist_from1", "dist_from2", "input"
colnames(cluster_df_8)[1] = "filenaam"
# cluster_df_6 = as.data.frame(read.csv("C:/Users/u0112360/Documents/____/Sagalassos/__PhD/Articles/_Typology/Results_VM_paper1/20210802_VMpaper/df_cluster_results_Mink_v2_ncl-6_83of100-1B150_20210802.csv"), stingsAsFactors = FALSE)
# cluster_df_6 = as.data.frame(read.csv("C:/Users/u0112360/Documents/____/Sagalassos/__PhD/Articles/_Typology/Results_VM_paper1/20210802_VMpaper/df_cluster_results_L4_RimDiam_Mink_ncl-7_83of100-1B150_20210802.csv"), stingsAsFactors = FALSE)
cluster_df_6 = as.data.frame(read.csv("C:/Users/u0112360/Documents/____/Sagalassos/__PhD/Articles/_Typology/Results_VM_paper1/20210802_VMpaper/df_cluster_results_L4_RimDiam_inclBR_Mink_v2_ncl-2_83of100-1B150_20210802.csv"), stingsAsFactors = FALSE)
# cluster_df_6 = as.data.frame(read.csv("C:/Users/u0112360/Documents/____/Sagalassos/__PhD/Articles/_Typology/Results_VM_paper1/20210802_VMpaper/df_cluster_results_L4_RimDiam_inclBR_FIS_Mink_ncl-4_83of100-1B150_20210802.csv"), stingsAsFactors = FALSE)
cluster_df_6 = cluster_df_6[,-(which(colnames(cluster_df_6) %in% c("labelsTV","labelsType","input")))] # "dist_from1", "dist_from2", "input"
colnames(cluster_df_6)[1] = "filenaam"
# cluster_df_4 = as.data.frame(read.csv("C:/Users/u0112360/Documents/____/Sagalassos/__PhD/Articles/_Typology/Results_VM_paper1/20210802_VMpaper/df_cluster_results_Mink_v2_ncl-4_83of100-1B150_20210802.csv"), stingsAsFactors = FALSE)
# cluster_df_4 = as.data.frame(read.csv("C:/Users/u0112360/Documents/____/Sagalassos/__PhD/Articles/_Typology/Results_VM_paper1/20210802_VMpaper/df_cluster_results_L4_RimDiam_Mink_ncl-5_83of100-1B150_20210802.csv"), stingsAsFactors = FALSE)
cluster_df_4 = as.data.frame(read.csv("C:/Users/u0112360/Documents/____/Sagalassos/__PhD/Articles/_Typology/Results_VM_paper1/20210802_VMpaper/df_cluster_results_L4_RimDiam_inclBR_Mink_v2_ncl-4_83of100-1B150_20210802.csv"), stingsAsFactors = FALSE)
# cluster_df_4 = as.data.frame(read.csv("C:/Users/u0112360/Documents/____/Sagalassos/__PhD/Articles/_Typology/Results_VM_paper1/20210802_VMpaper/df_cluster_results_L4_RimDiam_inclBR_FIS_Mink_ncl-3_83of100-1B150_20210802.csv"), stingsAsFactors = FALSE)
cluster_df_4 = cluster_df_4[,-(which(colnames(cluster_df_4) %in% c("labelsTV","labelsType","input")))] # "dist_from1", "dist_from2", "input"
colnames(cluster_df_4)[1] = "filenaam"

cluster_df_selected = cluster_df_10

# merge FIS and cluster data ## <<<<-----------
dataplot_FIS_1_ncls = merge(dataplot_FIS_1, cluster_df_selected, by = "filenaam")
dataplot_FIS_2_ncls = merge(dataplot_FIS_2, cluster_df_selected, by = "filenaam")
dataplot_FIS_3_ncls = merge(dataplot_FIS_3, cluster_df_selected, by = "filenaam")
dataplot_FIS_4.ext_ncls = merge(dataplot_FIS_4.ext, cluster_df_selected, by = "filenaam")
dataplot_FIS_4.int_ncls = merge(dataplot_FIS_4.int, cluster_df_selected, by = "filenaam")
# dataplot_FIS_5_ncls = merge(dataplot_FIS_5, cluster_df_selected, by = "filenaam")
dataplot_FIS_6_ncls = merge(dataplot_FIS_6, cluster_df_selected, by = "filenaam")
dataplot_FIS_7_ncls = merge(dataplot_FIS_7, cluster_df_selected, by = "filenaam")

# order obs dataplot_FIS_*_ncls
obs_order_FIS_id = match(obs_order, dataplot_FIS_1_ncls$filenaam)
dataplot_FIS_1_ncls = dataplot_FIS_1_ncls[obs_order_FIS_id, ]
obs_order_FIS_id = match(obs_order, dataplot_FIS_2_ncls$filenaam)
dataplot_FIS_2_ncls = dataplot_FIS_2_ncls[obs_order_FIS_id, ]
obs_order_FIS_id = match(obs_order, dataplot_FIS_3_ncls$filenaam)
dataplot_FIS_3_ncls = dataplot_FIS_3_ncls[obs_order_FIS_id, ]
obs_order_FIS_id = match(obs_order, dataplot_FIS_4.ext_ncls$filenaam)
dataplot_FIS_4.ext_ncls = dataplot_FIS_4.ext_ncls[obs_order_FIS_id, ]
obs_order_FIS_id = match(obs_order, dataplot_FIS_4.int_ncls$filenaam)
dataplot_FIS_4.int_ncls = dataplot_FIS_4.int_ncls[obs_order_FIS_id, ]
# obs_order_FIS_id = match(obs_order, dataplot_FIS_5_ncls$filenaam)
# dataplot_FIS_5_ncls = dataplot_FIS_5_ncls[obs_order_FIS_id, ]
# obs_order_FIS_id = match(obs_order, dataplot_FIS_6_ncls$filenaam)
dataplot_FIS_6_ncls = dataplot_FIS_6_ncls[obs_order_FIS_id, ]
obs_order_FIS_id = match(obs_order, dataplot_FIS_7_ncls$filenaam)
dataplot_FIS_7_ncls = dataplot_FIS_7_ncls[obs_order_FIS_id, ]

df_FIStable = cbind.data.frame(dataplot_FIS_1_ncls[,1:4], dataplot_FIS_2_ncls[,2:3], dataplot_FIS_3_ncls[,2:3], dataplot_FIS_4.ext_ncls[,2:3], dataplot_FIS_4.int_ncls[,2:3], dataplot_FIS_6_ncls[,2:3], dataplot_FIS_7_ncls[,2:3]) # dataplot_FIS_5_ncls[,2:3],


# STAD links
# # lines manhattan
# df_links_Measures = read.csv("C:/Users/u0112360/Documents/____/Sagalassos/__PhD/Articles/_Typology/Results_VM_paper1/20210802_VMpaper/df_links_STAD_ScaledPeriphery_Manh_83of100-1B150_20210802.csv")
# # lines chebyshev
# df_links_Measures = read.csv("C:/Users/u0112360/Documents/____/Sagalassos/__PhD/Articles/_Typology/Results_VM_paper1/20210802_VMpaper/df_links_STAD_ScaledPeriphery_Cheb_83of100-1B150_20210802.csv")
# # lines canberra
# df_links_Measures = read.csv("C:/Users/u0112360/Documents/____/Sagalassos/__PhD/Articles/_Typology/Results_VM_paper1/20210802_VMpaper/df_links_STAD_ScaledPeriphery_Canb_83of100-1B150_20210802.csv")
# # lines Minkowski p=4           <<---
# df_links_Measures = read.csv("C:/Users/u0112360/Documents/____/Sagalassos/__PhD/Articles/_Typology/Results_VM_paper1/20210802_VMpaper/df_links_STAD_ScaledPeriphery_Mink_83of100-1B150_20210802.csv")
# 2D DTW
# df_links_Measures = read.csv("C:/Users/u0112360/Documents/____/Sagalassos/__PhD/Articles/_Typology/Results_VM_paper1/20210802_VMpaper/df_links_STAD_ScaledHeight_dtw_1B150_20210802.csv")
# jaccard
#
# # efa
#
# # measures
#
# # descriptors
#
# combined
# df_links_Measures = read.csv("C:/Users/u0112360/Documents/____/Sagalassos/__PhD/Articles/_Typology/Results_VM_paper1/20210802_VMpaper/df_links_STAD_Combined_Rims_Manh_83of100-1B150_20210802.csv")
# df_links_Measures = read.csv("C:/Users/u0112360/Documents/____/Sagalassos/__PhD/Articles/_Typology/Results_VM_paper1/20210802_VMpaper/df_links_STAD_Combined_Rims_Mink_83of100-1B150_20210802.csv")
# df_links_Measures = read.csv("C:/Users/u0112360/Documents/____/Sagalassos/__PhD/Articles/_Typology/Results_VM_paper1/20210802_VMpaper/df_links_STAD_Combined_Rims_Eucl_83of100-1B150_20210802.csv")
# df_links_Measures = read.csv("C:/Users/u0112360/Documents/____/Sagalassos/__PhD/Articles/_Typology/Results_VM_paper1/20210802_VMpaper/df_links_STAD_Combined_L4_G3_Manh_83of100-1B150_20210802.csv")
# df_links_Measures = read.csv("C:/Users/u0112360/Documents/____/Sagalassos/__PhD/Articles/_Typology/Results_VM_paper1/20210802_VMpaper/df_links_STAD_Combined_L4_G4_Manh_83of100-1B150_20210802.csv")
# df_links_Measures = read.csv("C:/Users/u0112360/Documents/____/Sagalassos/__PhD/Articles/_Typology/Results_VM_paper1/20210802_VMpaper/df_links_STAD_Combined_L4_G7_Manh_83of100-1B150_20210802.csv")
# df_links_Measures = read.csv("C:/Users/u0112360/Documents/____/Sagalassos/__PhD/Articles/_Typology/Results_VM_paper1/20210802_VMpaper/df_links_STAD_Combined_L4_G3_Mink_83of100-1B150_20210802.csv")
# df_links_Measures = read.csv("C:/Users/u0112360/Documents/____/Sagalassos/__PhD/Articles/_Typology/Results_VM_paper1/20210802_VMpaper/df_links_STAD_Combined_L4_G4_Mink_83of100-1B150_20210802.csv")
# df_links_Measures = read.csv("C:/Users/u0112360/Documents/____/Sagalassos/__PhD/Articles/_Typology/Results_VM_paper1/20210802_VMpaper/df_links_STAD_Combined_L4_G7_Mink_83of100-1B150_20210802.csv")
# df_links_Measures = read.csv("C:/Users/u0112360/Documents/____/Sagalassos/__PhD/Articles/_Typology/Results_VM_paper1/20210802_VMpaper/df_links_STAD_Combined_L4_G3_Eucl_83of100-1B150_20210802.csv")
# df_links_Measures = read.csv("C:/Users/u0112360/Documents/____/Sagalassos/__PhD/Articles/_Typology/Results_VM_paper1/20210802_VMpaper/df_links_STAD_Combined_L4_G4_Eucl_83of100-1B150_20210802.csv")
# df_links_Measures = read.csv("C:/Users/u0112360/Documents/____/Sagalassos/__PhD/Articles/_Typology/Results_VM_paper1/20210802_VMpaper/df_links_STAD_Combined_L4_G7_Eucl_83of100-1B150_20210802.csv")
# df_links_Measures = read.csv("C:/Users/u0112360/Documents/____/Sagalassos/__PhD/Articles/_Typology/Results_VM_paper1/20210802_VMpaper/df_links_STAD_Combined_L4_RimDiam_inclBR_Mink_83of100-1B150_20210802.csv")
# df_links_Measures = read.csv("C:/Users/u0112360/Documents/____/Sagalassos/__PhD/Articles/_Typology/Results_VM_paper1/20210802_VMpaper/df_links_STAD_Combined_L4_RimDiam_Mink_83of100-1B150_20210802.csv")
df_links_Measures = read.csv("C:/Users/u0112360/Documents/____/Sagalassos/__PhD/Articles/_Typology/Results_VM_paper1/20210802_VMpaper/df_links_STAD_Combined_normAll01_L4_RimDiam_inclBR_Mink_83of100-1B150_20210802.csv")
# df_links_Measures = read.csv("C:/Users/u0112360/Documents/____/Sagalassos/__PhD/Articles/_Typology/Results_VM_paper1/20210802_VMpaper/df_links_STAD_Combined_normAll01_L4_RimDiam_Mink_83of100-1B150_20210802.csv")
# df_links_Measures = read.csv("C:/Users/u0112360/Documents/____/Sagalassos/__PhD/Articles/_Typology/Results_VM_paper1/20210802_VMpaper/df_links_STAD_Combined_normAll01_L4_RimDiam_inclBR_FIS_Mink_83of100-1B150_20210802.csv")


# UI Colours ###

df_colorAll = data.frame(bee_dataplot$filenaam)
names(df_colorAll) = "filenaam"
df_colorAll$filenaam = as.character(df_colorAll$filenaam)
df_colorAll = cbind.data.frame(df_colorAll, df_labels)
df_colorAll$binaryLabel = ifelse(df_colorAll$binaryLabel == '1', "#f28e2c", "#4e79a7")

groupLabels = c("binaryLabel", "TV", names(bee_dataplot))
# multiple colours palette display.brewer.all()
# brewerset = brewer.pal(11, name = "BrBG")[c(2,3,4,8,9,10)]
# c('#800000','#9A6324','#808000', '#469990','#000075','#000000',
#   '#e6194B','#f58231','#ffe119','#bfef45','#3cb44b','#42d4f4','#4363d8','#911eb4','#f032e6','#a9a9a9',
#   '#fabed4','#ffd8b1','#fffac8','#aaffc3','#dcbeff','#ffffff') # # https://sashamaps.net/docs/resources/20-colors/
# instead of the shiny yellow #ffe119 use #e5c700
# instead of the cyan #42d4f4 use #0db6db
alphaHEXcode = data.frame("value" = 100:0,
                          "code" = c("FF","FC","FA","F7","F5","F2","F0","ED","EB","E8","E6","E3","E0","DE","DB","D9","D6",
                                     "D4","D1","CF","CC","C9","C7","C4","C2","BF","BD","BA","B8","B5","B3","B0","AD","AB","A8",
                                     "A6","A3","A1","9E","9C","99","96","94","91","8F","8C","8A","87","85","82","80","7D","7A","78","75","73",
                                     "70","6E","6B","69","66","63","61","5E","5C","59","57","54","52","4F","4D","4A","47","45","42","40","3D","3B","38",
                                     "36","33","30","2E","2B","29","26","24","21","1F","1C","1A","17","14","12","0F","0D","0A","08","05","03","00"))
brewerset_Cat = c('#808000', '#469990','#dcbeff', '#9A6324','#000000',
                  '#e6194B','#f58231','#e5c700','#bfef45','#3cb44b','#0db6db','#4363d8','#911eb4','#f032e6','#a9a9a9',
                  '#fabed4','#ffd8b1','#fffac8','#aaffc3','#000075', '#ffffff','#800000')
# brewerset_Clusters = c('#000000', '#db6d00', '#006ddb', '#920000',
#                        '#8f4e00', '#ffdf4d', '#676767', '#009999',
#                        '#ff6db6', '#490092')
brewerset_Clusters = c('#000000', '#db6d00', '#006ddb', '#920000',
                       '#754a3c', '#ffdf4d', '#676767', '#009999',
                       '#ff6db6', '#490092')
newcol_Cat = colorRampPalette(brewerset_Cat)


colorset = newcol_Cat(length(unique(df_colorAll$TV))) # TV color categories
df_colorTV = cbind.data.frame(sort(unique(df_colorAll$TV)), colorset)
names(df_colorTV) = c("TV", "col")
df_colorTV$TV = as.character(df_colorTV$TV)
df_colorAll = merge(df_colorAll, df_colorTV, by = "TV")
names(df_colorAll)[4] = "TV_col"


brewerset_Con = brewer.pal(11, name = "BrBG")
newcol_Con = colorRampPalette(brewerset_Con)

for(i in 4:length(groupLabels)) {
  vari = bee_dataplot[, names(bee_dataplot) %in% groupLabels[i]]
  colorset =  newcol_Con(length(unique(vari)))
  df_colorVari = cbind.data.frame(sort(unique(vari)), colorset)
  names(df_colorVari)[1] = names(bee_dataplot)[(names(bee_dataplot) %in% groupLabels[i])]
  names(df_colorVari)[2] = "col"
  # head(df_colorVari)
  df_colorFili = bee_dataplot[ , c(1, which(names(bee_dataplot) %in% groupLabels[i]))] # 1=>bee_dataplot$filenaam
  # head(df_colorFili)
  df_colorI = merge(df_colorFili, df_colorVari, by = names(bee_dataplot)[(names(bee_dataplot) %in% groupLabels[i])])
  # head(df_colorI)
  df_colorI = df_colorI[,-1]
  names(df_colorI)[2] = names(bee_dataplot)[(names(bee_dataplot) %in% groupLabels[i])]
  # head(df_colorI)
  df_colorAll = merge(df_colorAll, df_colorI, by = "filenaam")
}
rm(df_colorI, df_colorFili, df_colorVari)

# All selected data enriched: variables Original, variables Scaled, variable Colors, filewd
# df_dataplot, bee_dataplot, df_colorAll
df_All = cbind.data.frame(rownames(df_dataplot), df_dataplot)
names(df_All)[1] = "filenaam"
names(bee_dataplot)[2:length(names(bee_dataplot))] = paste0(names(bee_dataplot)[2:length(names(bee_dataplot))], "_Sc")
df_All = merge(df_All, bee_dataplot, by = "filenaam")
names(df_colorAll)[5:length(names(df_colorAll))] = paste0(names(df_colorAll)[5:length(names(df_colorAll))], "_col")
df_colorAll = as.data.frame(lapply(df_colorAll, as.character))
df_All = merge(df_All, df_colorAll, by = "filenaam")
df_filewd = data_df[ ,c("filename","filewd")]
names(df_filewd)[1] = "filenaam"
df_All = merge(df_All, df_filewd, by = "filenaam")
df_All$filenaam = as.character(df_All$filenaam)
df_All$plain_col = ifelse(df_All$filenaam %in% special_set_1B150$filename, "#000000", "#d6d6d6")
df_All$TV = as.character(df_All$TV)

# clean some data from the environmnet
rm(df_colorAll, df_colorTV, vmin, vmax) # df_colorCentr,

## descriptors in FIS 1
df_descriptors = cbind.data.frame(unique(data_df_FIS_1$final.descr.FIS_1), brewerset_Cat[2:(1+length(unique(data_df_FIS_1$final.descr.FIS_1)))])
names(df_descriptors) = c("final.descr.FIS_1", "final.descr.FIS_1_col")
df_descriptors$final.descr.FIS_1 = as.character(df_descriptors$final.descr.FIS_1)
df_colorI = merge(data_df_FIS_1, df_descriptors, by = "final.descr.FIS_1")
df_colorI$value = round(df_colorI$final.support.FIS_1*100,0)
df_colorI = merge(df_colorI, alphaHEXcode, by = "value")
df_colorI$final.descr.FIS_1_col_alpha = paste0(df_colorI$final.descr.FIS_1_col, df_colorI$code)
df_colorI = df_colorI[,-which(colnames(df_colorI) %in% c("final.support.FIS_1", "value", "code"))]
names(df_colorI)[which(names(df_colorI) == "filename")] = "filenaam"
df_All = merge(df_All, df_colorI, by = "filenaam")

## descriptors in FIS 2
df_descriptors = cbind.data.frame(unique(data_df_FIS_2$final.descr.FIS_2), brewerset_Cat[2:(1+length(unique(data_df_FIS_2$final.descr.FIS_2)))])
names(df_descriptors) = c("final.descr.FIS_2", "final.descr.FIS_2_col")
df_descriptors$final.descr.FIS_2 = as.character(df_descriptors$final.descr.FIS_2)
df_colorI = merge(data_df_FIS_2, df_descriptors, by = "final.descr.FIS_2")
df_colorI$value = round(df_colorI$final.support.FIS_2*100,0)
df_colorI = merge(df_colorI, alphaHEXcode, by = "value")
df_colorI$final.descr.FIS_2_col_alpha = paste0(df_colorI$final.descr.FIS_2_col, df_colorI$code)
df_colorI = df_colorI[,-which(colnames(df_colorI) %in% c("final.support.FIS_2", "value", "code"))]
names(df_colorI)[which(names(df_colorI) == "filename")] = "filenaam"
df_All = merge(df_All, df_colorI, by = "filenaam")

## descriptors in FIS 3
df_descriptors = cbind.data.frame(unique(data_df_FIS_3$final.descr.FIS_3), brewerset_Cat[2:(1+length(unique(data_df_FIS_3$final.descr.FIS_3)))])
names(df_descriptors) = c("final.descr.FIS_3", "final.descr.FIS_3_col")
df_descriptors$final.descr.FIS_3 = as.character(df_descriptors$final.descr.FIS_3)
df_colorI = merge(data_df_FIS_3, df_descriptors, by = "final.descr.FIS_3")
df_colorI$value = round(df_colorI$final.support.FIS_3*100,0)
df_colorI = merge(df_colorI, alphaHEXcode, by = "value")
df_colorI$final.descr.FIS_3_col_alpha = paste0(df_colorI$final.descr.FIS_3_col, df_colorI$code)
df_colorI = df_colorI[,-which(colnames(df_colorI) %in% c("final.support.FIS_3", "value", "code"))]
names(df_colorI)[which(names(df_colorI) == "filename")] = "filenaam"
df_All = merge(df_All, df_colorI, by = "filenaam")

## descriptors in FIS 4
# exterior
df_descriptors = cbind.data.frame(unique(data_df_FIS_4$final.descr.ext.FIS_4), brewerset_Cat[2:(1+length(unique(data_df_FIS_4$final.descr.ext.FIS_4)))])
names(df_descriptors) = c("final.descr.ext.FIS_4", "final.descr.ext.FIS_4_col")
df_descriptors$final.descr.ext.FIS_4 = as.character(df_descriptors$final.descr.ext.FIS_4)
df_colorI = merge(data_df_FIS_4, df_descriptors, by = "final.descr.ext.FIS_4")
df_colorI$value = round(df_colorI$final.support.ext.FIS_4*100,0)
df_colorI = merge(df_colorI, alphaHEXcode, by = "value")
df_colorI$final.descr.ext.FIS_4_col_alpha = paste0(df_colorI$final.descr.ext.FIS_4_col, df_colorI$code)
df_colorI = df_colorI[,which(colnames(df_colorI) %in% c("filename", "final.descr.ext.FIS_4", "final.descr.ext.FIS_4_col"))]
names(df_colorI)[which(names(df_colorI) == "filename")] = "filenaam"
df_All = merge(df_All, df_colorI, by = "filenaam")
# interior
df_descriptors = cbind.data.frame(unique(data_df_FIS_4$final.descr.int.FIS_4), brewerset_Cat[2:(1+length(unique(data_df_FIS_4$final.descr.int.FIS_4)))])
names(df_descriptors) = c("final.descr.int.FIS_4", "final.descr.int.FIS_4_col")
df_descriptors$final.descr.int.FIS_4 = as.character(df_descriptors$final.descr.int.FIS_4)
df_colorI = merge(data_df_FIS_4, df_descriptors, by = "final.descr.int.FIS_4")
df_colorI$value = round(df_colorI$final.support.int.FIS_4*100,0)
df_colorI = merge(df_colorI, alphaHEXcode, by = "value")
df_colorI$final.descr.int.FIS_4_col_alpha = paste0(df_colorI$final.descr.int.FIS_4_col, df_colorI$code)
df_colorI = df_colorI[,which(colnames(df_colorI) %in% c("filename", "final.descr.int.FIS_4", "final.descr.int.FIS_4_col"))]
names(df_colorI)[which(names(df_colorI) == "filename")] = "filenaam"
df_All = merge(df_All, df_colorI, by = "filenaam")

# ## descriptors in FIS 5
# df_descriptors = cbind.data.frame(unique(data_df_FIS_5$final.descr.FIS_5), brewerset_Cat[2:(1+length(unique(data_df_FIS_5$final.descr.FIS_5)))])
# names(df_descriptors) = c("final.descr.FIS_5", "final.descr.FIS_5_col")
# df_descriptors$final.descr.FIS_5 = as.character(df_descriptors$final.descr.FIS_5)
# df_colorI = merge(data_df_FIS_5, df_descriptors, by = "final.descr.FIS_5")
# df_colorI$value = round(df_colorI$final.support.FIS_5*100,0)
# df_colorI = merge(df_colorI, alphaHEXcode, by = "value")
# df_colorI$final.descr.FIS_5_col_alpha = paste0(df_colorI$final.descr.FIS_5_col, df_colorI$code)
# df_colorI = df_colorI[,-which(colnames(df_colorI) %in% c("final.support.FIS_5", "value", "code"))]
# names(df_colorI)[which(names(df_colorI) == "filename")] = "filenaam"
# df_All = merge(df_All, df_colorI, by = "filenaam")

## descriptors in FIS 6
df_descriptors = cbind.data.frame(unique(data_df_FIS_6$final.descr.FIS_6), brewerset_Cat[2:(1+length(unique(data_df_FIS_6$final.descr.FIS_6)))])
names(df_descriptors) = c("final.descr.FIS_6", "final.descr.FIS_6_col")
df_descriptors$final.descr.FIS_6 = as.character(df_descriptors$final.descr.FIS_6)
df_colorI = merge(data_df_FIS_6, df_descriptors, by = "final.descr.FIS_6")
df_colorI$value = round(df_colorI$final.support.FIS_6*100,0)
df_colorI = merge(df_colorI, alphaHEXcode, by = "value")
df_colorI$final.descr.FIS_6_col_alpha = paste0(df_colorI$final.descr.FIS_6_col, df_colorI$code)
df_colorI = df_colorI[,-which(colnames(df_colorI) %in% c("final.support.FIS_6", "value", "code"))]
names(df_colorI)[which(names(df_colorI) == "filename")] = "filenaam"
df_All = merge(df_All, df_colorI, by = "filenaam")

## descriptors in FIS 7
df_descriptors = cbind.data.frame(unique(data_df_FIS_7$final.descr.FIS_7), brewerset_Cat[2:(1+length(unique(data_df_FIS_7$final.descr.FIS_7)))])
names(df_descriptors) = c("final.descr.FIS_7", "final.descr.FIS_7_col")
df_descriptors$final.descr.FIS_7 = as.character(df_descriptors$final.descr.FIS_7)
df_colorI = merge(data_df_FIS_7, df_descriptors, by = "final.descr.FIS_7")
df_colorI$value = round(df_colorI$final.support.FIS_7*100,0)
df_colorI = merge(df_colorI, alphaHEXcode, by = "value")
df_colorI$final.descr.FIS_7_col_alpha = paste0(df_colorI$final.descr.FIS_7_col, df_colorI$code)
df_colorI = df_colorI[,-which(colnames(df_colorI) %in% c("final.support.FIS_7", "value", "code"))]
names(df_colorI)[which(names(df_colorI) == "filename")] = "filenaam"
df_All = merge(df_All, df_colorI, by = "filenaam")


# hard cluster results coloring ##
## 10 clusters
# colorset = newcol_Cat(length(unique(cluster_df_10$hard_cl))) # Cluster color categories
df_colorCluster = cbind.data.frame(unique(cluster_df_10$hard_cl), brewerset_Clusters[1:length(unique(cluster_df_10$hard_cl))])
names(df_colorCluster) = c("hard_cl", "Cluster_10_col")
df_colorCluster$hard_cl = as.character(df_colorCluster$hard_cl)
df_colorI = merge(cluster_df_10, df_colorCluster, by = "hard_cl")
# adjust transparency
vecAlpha = data.frame("filenaam" = df_colorI$filenaam, "value" = rep(1, dim(df_colorI)[1]))
for (rown in 1:dim(vecAlpha)[1]) {
  vecAlpha[rown, "value"] = round(df_colorI[rown, as.numeric(df_colorI$hard_cl[rown])+2]*100,0)
}
# scale values for clearer visualisation: we scale in [20, 100]
minVal = min(vecAlpha$value); maxVal = max(vecAlpha$value)
vecAlpha$value = round(20 + (100 - 20)*(vecAlpha$value - minVal) / (maxVal - minVal),0)
vecAlpha = merge(vecAlpha, alphaHEXcode, by = "value")
colnames(vecAlpha)[which(colnames(vecAlpha) == "value")] = "value_10"
df_colorI = merge(df_colorI, vecAlpha, by = "filenaam")
colnames(df_colorI)[which(colnames(df_colorI) == "code")] = "code_10"
df_colorI$Cluster_10_Membership_col = paste0(df_colorI$Cluster_10_col, df_colorI$code_10) #, vecAlpha$code ??
colnames(df_colorI)[which(colnames(df_colorI) == "hard_cl")] = "hard_cl_10"
df_colorI = df_colorI[, c("filenaam", "hard_cl_10", "Cluster_10_col", "value_10", "code_10", "Cluster_10_Membership_col")]
df_All = merge(df_All, df_colorI, by = "filenaam")
## 8 clusters
# colorset = newcol_Cat(length(unique(cluster_df_8$hard_cl))) # TV color categories
df_colorCluster = cbind.data.frame(unique(cluster_df_8$hard_cl), brewerset_Clusters[1:length(unique(cluster_df_8$hard_cl))])
names(df_colorCluster) = c("hard_cl", "Cluster_8_col")
df_colorCluster$hard_cl = as.character(df_colorCluster$hard_cl)
df_colorI = merge(cluster_df_8, df_colorCluster, by = "hard_cl")
# adjust transparency
vecAlpha = data.frame("filenaam" = df_colorI$filenaam, "value" = rep(1,dim(df_colorI)[1]))
for (rown in 1:dim(vecAlpha)[1]) {
  vecAlpha[rown,"value"] = round(df_colorI[rown, as.numeric(df_colorI$hard_cl[rown])+2]*100,0)
}
# scale values for clearer visualisation: we scale in [20, 100]
minVal = min(vecAlpha$value); maxVal = max(vecAlpha$value)
vecAlpha$value = round(20 + (100 - 20)*(vecAlpha$value - minVal) / (maxVal - minVal),0)
vecAlpha = merge(vecAlpha, alphaHEXcode, by = "value")
colnames(vecAlpha)[which(colnames(vecAlpha) == "value")] = "value_8"
df_colorI = merge(df_colorI, vecAlpha, by = "filenaam")
colnames(df_colorI)[which(colnames(df_colorI) == "code")] = "code_8"
df_colorI$Cluster_8_Membership_col = paste0(df_colorI$Cluster_8_col, df_colorI$code_8)
colnames(df_colorI)[which(colnames(df_colorI) == "hard_cl")] = "hard_cl_8"
df_colorI = df_colorI[, c("filenaam", "hard_cl_8", "Cluster_8_col", "value_8", "code_8", "Cluster_8_Membership_col")]
df_All = merge(df_All, df_colorI, by = "filenaam")
## 6 clusters
# colorset = newcol_Cat(length(unique(cluster_df_6$hard_cl))) # TV color categories
df_colorCluster = cbind.data.frame(unique(cluster_df_6$hard_cl), brewerset_Clusters[1:length(unique(cluster_df_6$hard_cl))])
names(df_colorCluster) = c("hard_cl", "Cluster_6_col")
df_colorCluster$hard_cl = as.character(df_colorCluster$hard_cl)
df_colorI = merge(cluster_df_6, df_colorCluster, by = "hard_cl")
# adjust transparency
vecAlpha = data.frame("filenaam" = df_colorI$filenaam, "value" = rep(1,dim(df_colorI)[1]))
for (rown in 1:dim(vecAlpha)[1]) {
  vecAlpha[rown,"value"] = round(df_colorI[rown, as.numeric(df_colorI$hard_cl[rown])+2]*100,0)
}
# scale values for clearer visualisation: we scale in [20, 100]
minVal = min(vecAlpha$value); maxVal = max(vecAlpha$value)
vecAlpha$value = round(20 + (100 - 20)*(vecAlpha$value - minVal) / (maxVal - minVal),0)
vecAlpha = merge(vecAlpha, alphaHEXcode, by = "value")
colnames(vecAlpha)[which(colnames(vecAlpha) == "value")] = "value_6"
df_colorI = merge(df_colorI, vecAlpha, by = "filenaam")
colnames(df_colorI)[which(colnames(df_colorI) == "code")] = "code_6"
df_colorI$Cluster_6_Membership_col = paste0(df_colorI$Cluster_6_col, df_colorI$code_6)
colnames(df_colorI)[which(colnames(df_colorI) == "hard_cl")] = "hard_cl_6"
df_colorI = df_colorI[, c("filenaam", "hard_cl_6", "Cluster_6_col", "value_6", "code_6", "Cluster_6_Membership_col")]
df_All = merge(df_All, df_colorI, by = "filenaam")
## 4 clusters
# colorset = newcol_Cat(length(unique(cluster_df_4$hard_cl))) # TV color categories
df_colorCluster = cbind.data.frame(unique(cluster_df_4$hard_cl), brewerset_Clusters[1:length(unique(cluster_df_4$hard_cl))])
names(df_colorCluster) = c("hard_cl", "Cluster_4_col")
df_colorCluster$hard_cl = as.character(df_colorCluster$hard_cl)
df_colorI = merge(cluster_df_4, df_colorCluster, by = "hard_cl")
# adjust transparency
vecAlpha = data.frame("filenaam" = df_colorI$filenaam, "value" = rep(1,dim(df_colorI)[1]))
for (rown in 1:dim(vecAlpha)[1]) {
  vecAlpha[rown,"value"] = round(df_colorI[rown, as.numeric(df_colorI$hard_cl[rown])+2]*100,0)
}
# scale values for clearer visualisation: we scale in [20, 100]
minVal = min(vecAlpha$value); maxVal = max(vecAlpha$value)
vecAlpha$value = round(20 + (100 - 20)*(vecAlpha$value - minVal) / (maxVal - minVal),0)
vecAlpha = merge(vecAlpha, alphaHEXcode, by = "value")
colnames(vecAlpha)[which(colnames(vecAlpha) == "value")] = "value_4"
df_colorI = merge(df_colorI, vecAlpha, by = "filenaam")
colnames(df_colorI)[which(colnames(df_colorI) == "code")] = "code_4"
df_colorI$Cluster_4_Membership_col = paste0(df_colorI$Cluster_4_col, df_colorI$code_4)
colnames(df_colorI)[which(colnames(df_colorI) == "hard_cl")] = "hard_cl_4"
df_colorI = df_colorI[, c("filenaam", "hard_cl_4", "Cluster_4_col", "value_4", "code_4", "Cluster_4_Membership_col")]
df_All = merge(df_All, df_colorI, by = "filenaam")

# Add the columns that are not yet added in the df_All ###
data_df_categ = data_df[,which(colnames(data_df) %in% columCateg)]
colnames(data_df_categ)[1] = "filenaam"
data_df_categ$ContextLabel = as.character(data_df_categ$ContextCompare)
data_df_categ$ContextLabel = substring(data_df_categ$ContextLabel, 4)
df_All = merge(df_All, data_df_categ, by = "filenaam")


# reorder the dataframe
obs_order_id = match(obs_order, df_All$filenaam) # added for the expected ordering
df_All = df_All[obs_order_id, ] # added for the expected ordering
rownames(df_All) = NULL # added for the expected ordering



listcolors = list("plain" = "plain_col", "full profile" = "binaryLabel", "TV" = "TV_col",
                  "Cluster (10)" = "Cluster_10_col", "Cluster_Membership (10)" = "Cluster_10_Membership_col",
                  "Cluster (8)" = "Cluster_8_col", "Cluster_Membership (8)" = "Cluster_8_Membership_col",
                  "Cluster (6)" = "Cluster_6_col", "Cluster_Membership (6)" = "Cluster_6_Membership_col",
                  "Cluster (4)" = "Cluster_4_col", "Cluster_Membership (4)" = "Cluster_4_Membership_col",
                  "rim.out.prot.length" = "rim.out.prot.length_col",
                  "rim.inn.prot.length" = "rim.inn.prot.length_col", "rim.out.diff.length" = "rim.out.diff.length_col", "rim.inn.diff.length" = "rim.inn.diff.length_col",
                  "rim.inn.prot.length.selected" = "rim.inn.prot.length.selected_col", "rim.out.prot.length.selected" = "rim.out.prot.length.selected_col",
                  "rim.inn.prot.length.selected_scaled" = "rim.inn.prot.length.selected_scaled_col", "rim.out.prot.length.selected_scaled" = "rim.out.prot.length.selected_scaled_col",
                  "block.perC" = "block.perC_col", "trapez.perC" = "trapez.perC_col",
                  "Rim_HorizCut.Out_WT" = "Rim_HorizCut.Out_WT_col", "Rim_HorizCut.Inn_WT" = "Rim_HorizCut.Inn_WT_col",
                  "Rim_mean.WT" = "Rim_mean.WT_col", "Rim_median.WT" = "Rim_median.WT_col",
                  "Rim_sd.WT" = "Rim_sd.WT_col", "Rim_min.WT" = "Rim_min.WT_col",  "Rim_max.WT" = "Rim_max.WT_col",
                  "RimWT_margin" = "RimWT_margin_col", "Rim_Diam_extra_half" = "Rim_Diam_extra_half_col", "Rim_curl" = "Rim_curl_col",
                  "Rim_elongation.min" = "Rim_elongation.min_col", "Rim_elongation.max" = "Rim_elongation.max_col", "Rim_elongation.avg" = "Rim_elongation.avg_col",
                  "Rim_radius.ratio" = "Rim_radius.ratio_col",  "Rim_height_manual" = "Rim_height_manual_col", "Rim_incl_sin_mean" = "Rim_incl_sin_mean_col",
                  "form.factor" = "form.factor_col", "aspect.ratio" = "aspect.ratio_col", "roundness" = "roundness_col",
                  "Rim_extent.1" = "Rim_extent.1_col", "Rim_eccentricity" = "Rim_eccentricity_col", "Rim_incl_sin_min" = "Rim_incl_sin_min_col", "Rim_incl_sin_max" = "Rim_incl_sin_max_col",
                  "Rim_mass.centre.x" = "Rim_mass.centre.x_col", "Rim_mass.centre.y" =  "Rim_mass.centre.y_col",
                  "Rim_majoraxis" = "Rim_majoraxis_col", "Rim_WT.mean.norm" = "Rim_WT.mean.norm_col", "Rim_WT.med.norm" = "Rim_WT.med.norm_col",
                  "Rim_WT.var.norm" = "Rim_WT.var.norm_col", "Rim_WT.skew.norm" = "Rim_WT.skew.norm_col", "Rim_WT.kurt.norm" = "Rim_WT.kurt.norm_col",
                  "Rim_WT.BotMed.norm" = "Rim_WT.BotMed.norm_col", "Below.Rim_incl_sin_min" = "Below.Rim_incl_sin_min_col",
                  "Below.Rim_incl_sin_max" = "Below.Rim_incl_sin_max_col", "Below.Rim_incl_sin_mean" = "Below.Rim_incl_sin_mean_col",
                  "Below.Rim_incl_min_sign" = "Below.Rim_incl_min_sign_col", "Below.Rim_incl_max_sign" = "Below.Rim_incl_max_sign_col",
                  "Below.Rim_incl_mean_sign" = "Below.Rim_incl_mean_sign_col",
                  "Rim_flattness_med" = "Rim_flattness_med_col", "Rim_flattness_max" = "Rim_flattness_max_col", "CutsSymmetric" = "CutsSymmetric_col", "WT_LipBot" = "WT_LipBot_col",
                  "ellipse.perC" = "ellipse.perC_col",
                  # 'FIS.1_mf.1.out' = 'FIS.1_mf.1.out_col', 'FIS.1_mf.1.avg' = 'FIS.1_mf.1.avg_col', 'FIS.1_mf.1.inn' = 'FIS.1_mf.1.inn_col', 'FIS.1_mf.2.out' = 'FIS.1_mf.2.out_col',
                  # 'FIS.1_mf.2.avg' = 'FIS.1_mf.2.avg_col', 'FIS.1_mf.2.inn' = 'FIS.1_mf.2.inn_col', 'FIS.1_mf.3.out' = 'FIS.1_mf.3.out_col', 'FIS.1_mf.3.avg' = 'FIS.1_mf.3.avg_col',
                  # 'FIS.1_mf.3.inn' = 'FIS.1_mf.3.inn_col', 'FIS.1_mf.4.out' = 'FIS.1_mf.4.out_col', 'FIS.1_mf.4.avg' = 'FIS.1_mf.4.avg_col', 'FIS.1_mf.4.inn' = 'FIS.1_mf.4.inn_col',
                  # 'straight.rim' = 'straight.rim_col', 'slightly.bent.rim' = 'slightly.bent.rim_col', 'quite.bent.rim' = 'quite.bent.rim_col', 'profoundly.bent.rim' = 'profoundly.bent.rim_col',
                  # 'straight.rim.sign' = 'straight.rim.sign_col', 'straight.rim.descr' = 'straight.rim.descr_col', 'slightly.bent.rim.sign' = 'slightly.bent.rim.sign_col',
                  # 'slightly.bent.rim.descr' = 'slightly.bent.rim.descr_col', 'quite.bent.rim.sign' = 'quite.bent.rim.sign_col', 'quite.bent.rim.descr' = 'quite.bent.rim.descr_col',
                  # 'profoundly.bent.rim.sign' = 'profoundly.bent.rim.sign_col', 'profoundly.bent.rim.descr' = 'profoundly.bent.rim.descr_col', 'final.support' = 'final.support_col',
                  # 'interm.1.descr' = 'interm.1.descr_col', 'interm.2.descr' = 'interm.2.descr_col', 'final.descr' = 'final.descr_col',
                  'final.support.FIS_1' = 'final.support.FIS_1_col', 'final.support.FIS_2' = 'final.support.FIS_2_col', 'final.support.FIS_3' = 'final.support.FIS_3_col', 'final.support.ext.FIS_4' = 'final.support.ext.FIS_4_col', 'final.support.int.FIS_4' = 'final.support.int.FIS_4_col', 'final.support.FIS_6' = 'final.support.FIS_6_col', # 'final.support.FIS_5' = 'final.support.FIS_5_col',
                  'final.descr.FIS_1' = 'final.descr.FIS_1_col', 'final.descr.FIS_1_alpha' = 'final.descr.FIS_1_col_alpha',
                  'final.descr.FIS_2' = 'final.descr.FIS_2_col', 'final.descr.FIS_2_alpha' = 'final.descr.FIS_2_col_alpha',
                  'final.descr.FIS_3' = 'final.descr.FIS_3_col', 'final.descr.FIS_3_alpha' = 'final.descr.FIS_3_col_alpha',
                  'final.descr.ext.FIS_4' = 'final.descr.ext.FIS_4_col', 'final.descr.ext.FIS_4_alpha' = 'final.descr.ext.FIS_4_col_alpha',
                  'final.descr.int.FIS_4' = 'final.descr.int.FIS_4_col', 'final.descr.int.FIS_4_alpha' = 'final.descr.int.FIS_4_col_alpha',
                  # 'final.descr.FIS_5' = 'final.descr.FIS_5_col', 'final.descr.FIS_5_alpha' = 'final.descr.FIS_5_col_alpha',
                  'final.descr.FIS_6' = 'final.descr.FIS_6_col', 'final.descr.FIS_6_alpha' = 'final.descr.FIS_6_col_alpha',
                  'final.descr.FIS_7' = 'final.descr.FIS_7_col', 'final.descr.FIS_7_alpha' = 'final.descr.FIS_7_col_alpha'
)


# Context data
tbl_contexts = as.data.frame(table(df_All$ContextLabel)) # df_All$ContextCompare
tbl_contexts$Var1 = as.character(tbl_contexts$Var1)
tbl_contexts$Site = ""
for (tbli in 1:dim(tbl_contexts)[1]) {
  tbl_contexts$Site[tbli] = unlist(strsplit(tbl_contexts[tbli,"Var1"], "-"))[2]
}
colnames(tbl_contexts) = c("Context", "sherds", "Site")
tbl_inERlist = unique(cbind.data.frame(df_All$ContextLabel,df_All$inERlist))
colnames(tbl_inERlist) = c("Context", "Checked")
tbl_contexts = merge(tbl_contexts, tbl_inERlist, by = "Context")
tbl_contexts = tbl_contexts[order(tbl_contexts$Site),c("Site", "Context", "Checked", "sherds")]


# Size of node data as the certainty of belonging to the group
# add in df_All?
df_nodesize = data.frame("filenaam" = df_All$filenaam, "nodesize" = 0)
for (cl_i in unique(cluster_df_10$hard_cl)) {
  df_cl_i = subset(cluster_df_10, cluster_df_10$hard_cl == cl_i)
  # here we can normalise inside the cluster if we want ....
  df_nodesize[c(match(df_cl_i$filenaam, df_nodesize$filenaam)), "nodesize"] = round(df_cl_i[,cl_i+2]*20,0)
} # the order is correct because we have taken the filenaam data from 'df_All'


### Network Data ###
## wd ##
# wd = as.character(df_All$filewd.y) # original profile image
wd = rep("C:/Users/u0112360/Documents/____/Sagalassos/__PhD/Articles/_Typology/Scripts_VesselMorphology/img_rim_20201711", length(data_df$filename)) # rim image
# wd = rep("C:/Users/u0112360/Documents/____/Sagalassos/__PhD/Articles/_Typology/Scripts_VesselMorphology/img_cropped_Profile/img_cropped_Pr_1B150_20211112", length(data_df$filename)) # cropped profile image

## images ##
# pic = as.character(df_All$filenaam)  # original image
pic = paste0("img_rim_", as.character(data_df$filename)) # rim image
# pic = paste0("remade_", as.character(data_df$filename)) # cropped profile image

## base64 images ##
txt = NULL
for (i in 1:length(pic)) {
  txt[i] = RCurl::base64Encode(readBin(paste0(wd[i], "/", pic[i]), 'raw', file.info(paste0(wd[i], "/", pic[i]))[1, 'size']), 'txt')
}

# node attributes
sizeGroup = 20 # for all sherds having the same size



