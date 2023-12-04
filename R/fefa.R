library(Momocs)

# global values
selectedID = 1 # choose profile 
maxh = 12 # maximum number of harmonics
minthr = "90%" # threshold for minimum number of harmonics
thrh = "99%" # threshold for selected number of harmonics

# read measures data
data_df = as.data.frame(read.csv(paste0(readwd,"df_pointMeasures_realD_FG_20230209.csv")))
data_df$filename = as.factor(data_df$filename)
data_df$FG = as.factor(data_df$FG)

# read profile circumference data
load(paste0(readwd, "circumference_scaled_FG_20231101.RData"))
mat_circumference = mat_circumference_scaled

# get the out object
ldk_vec = rep(1, dim(data_df)[1])
shape_obj = Out(mat_circumference, fac = data_df, ldk = ldk_vec)

# order outlines according to factor (FG)
sortid = order(data_df$FG)
mat_circumference_sorted = mat_circumference[sortid]
data_df_TVsorted = data_df[sortid,]
shape_obj_sorted = Out(mat_circumference_sorted, fac = data_df_TVsorted)

# facet plot of all outlines
par(mfrow=c(1,1))
panel(shape_obj_sorted, fac = "FG", dim = c(16,18), main = "", pal_div_PiYG(length(levels(data_df$FG)), transp = 0), names = data_df_TVsorted$FG, cex.names = 0.6)

# plot one outline
coo_plot(shape_obj[selectedID], col = "grey", centroid = TRUE)

# center outlines on the common origin
shapenorm = coo_center(shape_obj)
shapenorm_sorted = coo_center(shape_obj_sorted)

# superimpose outlines
stack(shapenorm, ldk_confell = TRUE)

# calibrate Elliptic Fourier Analysis for the dataset
fp0 = calibrate_harmonicpower_efourier(shapenorm, nb.h = maxh, plot = TRUE)

# calibrate Elliptic Fourier Analysis for one profile
fp1 = calibrate_harmonicpower_efourier(shapenorm, nb.h = maxh, id = selectedID, plot = FALSE)

# facet plot of Elliptic Fourier Analysis calibration for one profile
calibrate_reconstructions_efourier(shapenorm, range = 1:maxh, id = selectedID)

# retrieve minimum number of harmonics
minh = fp1$minh[names(fp1$minh) == minthr]

# calculate reconstruction deviation from original for one profile
fp2 = calibrate_deviations_efourier(shapenorm, dist.nbpts = 100, range = minh:maxh, id = selectedID)

# choose number of harmonics
selectedh = fp0$minh[names(fp0$minh) == thrh]

# transform data with Elliptical Fourier Analysis
efashape = Momocs::efourier(shapenorm, nb.h = selectedh, smooth.it = 0, norm = TRUE)
df_efa = efashape$coe

# plot EFA transformation
library(reshape2)
library(ggplot2)
efashape_sorted = Momocs::efourier(shapenorm_sorted, nb.h = selectedh, smooth.it = 0, norm = TRUE)
df_efa_sorted = efashape_sorted$coe
dataplot = melt(df_efa_sorted)
ggplot(dataplot, aes(x = Var2, y = Var1)) + 
  geom_raster(aes(fill = log(value))) + 
  scale_fill_gradient(low = "#ECECEC", high = "#006CA5") +
  labs(x = "harmonic coefficients", y = "sorted profiles", title = "") +
  theme_bw() + theme(axis.text.x = element_text(size = 9, angle = 0, vjust = 0.5),
                     axis.text.y = element_blank(),
                     plot.title = element_text(size = 11))

