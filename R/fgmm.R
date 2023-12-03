# global values
hsc = 300 # profile height in pixels (strongly advised to scale all profiles to this constant value)
col_crc = "#80808080" # color of circumference
lwd_crc = 10 # line width of circumference
col_gmm = "#000000" # color of geometric measures representation
lwd_gmm = 4 # line width of geometric measures representation

# read measures data
data_df = read.csv("df_pointMeasures_realD_FG_20230209.csv")

# choose a profile
id = "SADR011208.jpg" # TO DO: based on UI user input
idx = which(data_df$filename == id)

# read profile circumference
load("circumference_scaled_FG_20231101.RData")
plot(mat_circumference_scaled[[idx]], type = "l", lwd = lwd_crc, col = col_crc)
mat_idx = mat_circumference_scaled[[idx]]

# re-scale circumference to actual scale
mat_idx = (data_df$sherd_height[idx]*mat_idx)/hsc
plot(mat_idx, type = "l", lwd = lwd_crc, col = col_crc, xlab = "profile x-axis (in cm)", ylab = "profile y-axis (in cm)") 

# get middle line
midl = median(mat_idx[,1])

# get index of baseline end 
idx_br = min(which(diff(mat_idx[,2]) > 0))

# get circumference index of wall angle
idx_wl = dim(mat_idx)[1] - round(data_df$wall_angle_pos[idx]*hsc, 0)
idx_wr = idx_br + round(data_df$wall_angle_pos[idx]*hsc, 0)

# get circumference index of wall thickness
idx_wt = dim(mat_idx)[1] - round(0.75*hsc, 0)

# plot primary GMM on top of profile
# # height
lines(x = rep(midl, 2), y = c(0, data_df$sherd_height[idx]), lwd = lwd_gmm, col = col_gmm)
# # base diameter
lines(x = c(mat_idx[1,1], mat_idx[1,1]+data_df$sherd_botDiam[idx]), 
      y = c(mat_idx[1,2], mat_idx[1,2]),
      lwd = lwd_gmm, col = col_gmm)
# # rim diameter
lines(x = c(mat_idx[dim(mat_idx)[1]-hsc,1], (mat_idx[dim(mat_idx)[1]-hsc,1] + data_df$sherd_rimDiam[idx])), 
      y = rep(data_df$sherd_height[idx], 2),
      lwd = lwd_gmm, col = col_gmm)
# # orientation of the wall
lines(x = c(mat_idx[dim(mat_idx)[1],1], mat_idx[dim(mat_idx)[1]-hsc,1]), 
      y = c(mat_idx[dim(mat_idx)[1],2], mat_idx[dim(mat_idx)[1]-hsc,2]), 
      lwd = lwd_gmm, col = col_gmm, lty = 2)
lines(x = c(mat_idx[idx_br,1], mat_idx[idx_br+hsc,1]),
      y = c(mat_idx[idx_br,2], mat_idx[idx_br+hsc,2]),
      lwd = lwd_gmm, col = col_gmm, lty = 2)
# # base height
lines(x = c(midl - data_df$sherd_botDiam[idx]/2, midl + data_df$sherd_botDiam[idx]/2),
      y = rep(data_df$base.height[idx], 2),
      lwd = lwd_gmm, col = col_gmm, lty = 2)
# # wall angle position (in function of the height)
lines(x = c(mat_idx[idx_wl,1], 0.2*data_df$sherd_rimDiam_inner[idx]), # TO DO: compute second value
      y = rep(data_df$wall_angle_pos[idx]*data_df$sherd_height[idx],2),
      lwd = lwd_gmm, col = col_gmm, lty = 3)
lines(x = c(mat_idx[idx_wr,1], max(mat_idx[,1])-0.2*data_df$sherd_rimDiam_inner[idx]), # TO DO: compute second value
      y = rep(data_df$wall_angle_pos[idx]*data_df$sherd_height[idx],2),
      lwd = lwd_gmm, col = col_gmm, lty = 3)
# # wall thickness at 2/3 of height
points(x = mat_idx[idx_wt,1], 
       y = 0.75*data_df$sherd_height[idx], 
       cex = 5*data_df$sherd_WThick1[idx], pch = 19, col = col_gmm)