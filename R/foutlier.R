readwd = ""
EFAinput = TRUE

# read data GMM
data_df = read.csv(paste0(readwd,"df_pointMeasures_realD_FG_20230209.csv"))
colnames(data_df)[which(colnames(data_df) == "FG_Final")] = "FG"
# selected columns (only for GMM)
selCol = c("sherd_HWratio",
           "sherd_inclination",
           "median.WT",
           "sherd_botDiam",
           "sherd_height",
           "sherd_rimDiam_inner")
# metadata columns
mdCol = c("filename", "FG")
# subset data
data_df = cbind.data.frame(data_df[, which(colnames(data_df) %in% mdCol)], data_df[, which(colnames(data_df) %in% selCol)])

if (EFAinput) {
  # read data EFA-PCA
  pca_df = read.csv(paste0(readwd,"df_efa_scaled_pca_FG_20231101.csv"), row.names = 1)
  md_df = data_df[, mdCol]
  sum(rownames(pca_df) == md_df$filename) == dim(md_df)[1]
  data_df = cbind.data.frame(data_df[, mdCol], pca_df) # designed as TRUE = filenames are in the same order
}

# index of selected columns
cols = (length(mdCol)+1):dim(data_df)[2]

# data per group
FG.A = data_df[data_df$FG == "A", ]
FG.B = data_df[data_df$FG == "B", ]
FG.C = data_df[data_df$FG == "C", ]
FG.D = data_df[data_df$FG == "D", ]
FG.F = data_df[data_df$FG == "F", ]

# normalise data per group
vmax = apply(FG.A[,cols], 2, max); vmin = apply(FG.A[,cols], 2, min); FG.A.norm = FG.A
for (i in 1:length(cols)) {
  FG.A.norm[, cols[i]] = (FG.A[, cols[i]] - vmin[i])/(vmax[i] - vmin[i])
}
vmax = apply(FG.B[,cols], 2, max); vmin = apply(FG.B[,cols], 2, min); FG.B.norm = FG.B
for (i in 1:length(cols)) {
  FG.B.norm[, cols[i]] = (FG.B[, cols[i]] - vmin[i])/(vmax[i] - vmin[i])
}
vmax = apply(FG.C[,cols], 2, max); vmin = apply(FG.C[,cols], 2, min); FG.C.norm = FG.C
for (i in 1:length(cols)) {
  FG.C.norm[, cols[i]] = (FG.C[, cols[i]] - vmin[i])/(vmax[i] - vmin[i])
}
vmax = apply(FG.D[,cols], 2, max); vmin = apply(FG.D[,cols], 2, min); FG.D.norm = FG.D
for (i in 1:length(cols)) {
  FG.D.norm[, cols[i]] = (FG.D[, cols[i]] - vmin[i])/(vmax[i] - vmin[i])
}
vmax = apply(FG.F[,cols], 2, max); vmin = apply(FG.F[,cols], 2, min); FG.F.norm = FG.F
for (i in 1:length(cols)) {
  FG.F.norm[, cols[i]] = (FG.F[, cols[i]] - vmin[i])/(vmax[i] - vmin[i])
}

# Mahalanobis distance (multivariate outlier detection - based on variables data)
multi.outliers.A = mahalanobis(FG.A.norm[,cols], colMeans(FG.A.norm[,cols]), cov(FG.A.norm[,cols]))
cv.A = qchisq(p = 0.001, df = dim(FG.A.norm[,cols])[2], lower.tail = FALSE)
plot(multi.outliers.A); lines(x = 1:length(multi.outliers.A), y = rep(cv.A, length(multi.outliers.A)))

multi.outliers.B = mahalanobis(FG.B.norm[,cols], colMeans(FG.B.norm[,cols]), cov(FG.B.norm[,cols]))
cv.B = qchisq(p = 0.001, df = dim(FG.B.norm[,cols])[2], lower.tail = FALSE)
plot(multi.outliers.B); lines(x = 1:length(multi.outliers.B), y = rep(cv.B, length(multi.outliers.B)))
# 3 outliers in GMM ; 0 in EFA

multi.outliers.C = mahalanobis(FG.C.norm[,cols], colMeans(FG.C.norm[,cols]), cov(FG.C.norm[,cols]))
cv.C = qchisq(p = 0.001, df = dim(FG.C.norm[,cols])[2], lower.tail = FALSE)
plot(multi.outliers.C); lines(x = 1:length(multi.outliers.C), y = rep(cv.C, length(multi.outliers.C)))
# 4 outliers in GMM ; 3 in EFA

multi.outliers.D = mahalanobis(FG.D.norm[,cols], colMeans(FG.D.norm[,cols]), cov(FG.D.norm[,cols]))
cv.D = qchisq(p = 0.001, df = dim(FG.D.norm[,cols])[2], lower.tail = FALSE)
plot(multi.outliers.D); lines(x = 1:length(multi.outliers.D), y = rep(cv.D, length(multi.outliers.D)))

multi.outliers.F = mahalanobis(FG.F.norm[,cols], colMeans(FG.F.norm[,cols]), cov(FG.F.norm[,cols]))
cv.F = qchisq(p = 0.001, df = dim(FG.F.norm[,cols])[2], lower.tail = FALSE)
plot(multi.outliers.F); lines(x = 1:length(multi.outliers.F), y = rep(cv.F, length(multi.outliers.F)))

# save data Mahananobis
df_outlies.A = data.frame("FG" = "A", "filename" = FG.A.norm$filename, "MD" = multi.outliers.A, "CV" = cv.A, "outlier" = multi.outliers.A > cv.A)
df_outlies.B = data.frame("FG" = "B", "filename" = FG.B.norm$filename, "MD" = multi.outliers.B, "CV" = cv.B, "outlier" = multi.outliers.B > cv.B)
df_outlies.C = data.frame("FG" = "C", "filename" = FG.C.norm$filename, "MD" = multi.outliers.C, "CV" = cv.C, "outlier" = multi.outliers.C > cv.C)
df_outlies.D = data.frame("FG" = "D", "filename" = FG.D.norm$filename, "MD" = multi.outliers.D, "CV" = cv.D, "outlier" = multi.outliers.D > cv.D)
df_outlies.F = data.frame("FG" = "F", "filename" = FG.F.norm$filename, "MD" = multi.outliers.F, "CV" = cv.F, "outlier" = multi.outliers.F > cv.F)

df_outlies.FG = rbind.data.frame(df_outlies.A, df_outlies.B, df_outlies.C, df_outlies.D, df_outlies.F)