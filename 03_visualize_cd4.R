source("02_summary_cd4.R")
source("sparse_fbplot.R")
source("intensity_sparse_fbplot.R")
packages = c("rainbow")

## Now load or install&load all
package_check <- lapply(
    packages,
    FUN = function(x) {
        if (!require(x, character.only = TRUE)) {
            install.packages(x, dependencies = TRUE)
            library(x, character.only = TRUE)
        }
    }
)


month <- c(-18:42)


########### 0. Before the functional boxplot, we apply the functional bagplot and functional HDR boxplot.
cd4_rainbow <- t(cd4_mfpca$bootstrap_fit[[1]])
colnames(cd4_rainbow) <- 1:366
pdf("./figures/fbagplot_cd4.pdf", width = 7.2, height = 6)
fbagplot_example <- fboxplot(data = fds(x = -18:42, y = cd4_rainbow, 
                    xname = "Months since Seroconversion", 
                    yname = "Total CD4 Cell Counts"), type = "bag", 
                     plot.type = "functional",
                    plotlegend = FALSE)
title(main = "(a) Functional Bagplot: Fitted CD4 Cell Counts",
      cex.main = 1.1, cex.lab = 1.3, cex.axis = 1.3)
dev.off()

pdf("./figures/fHDRplot_cd4.pdf", width = 7.2, height = 6)
fboxplot(data = fds(-18:42, cd4_rainbow, 
                    xname = "Months since Seroconversion", 
                    yname = "Total CD4 Cell Counts"), type = "hdr",
         plot.type = "functional",
         plotlegend = FALSE)
title(main = "(b) Functional HDR Boxplot: Fitted CD4 Cell Counts",
      cex.main = 1.1, cex.lab = 1.3, cex.axis = 1.3)
dev.off()

###### 1. We can apply directly functional boxplot to the fitted CD4 data
pdf("./figures/fbplot_cd4.pdf", width = 7.2, height = 6)
fbplot_cd4 <- sparse_fbplot(fit = cd4_mfpca$bootstrap_fit[[1]], 
                               sparse = cd4_mfpca$bootstrap_fit[[1]], 
                               time_index = month, 
                               depth = cd4_depth,
                               two_stage = FALSE, sq_vo = FALSE, plot = TRUE,
                               xlab = "Months since Seroconversion", 
                               ylab = "Total CD4 Cell Counts", 
                               title = "(c) Functional Boxplot: Fitted CD4 Cell Counts",
                               yrange = NULL,
                               cex.main = 1.1, cex.lab = 1.3, cex.axis = 1.3,
                               medlabel = FALSE, outlabel = FALSE,  
                               prob = 0.5, factor = 1.5,
                               color = 6, outliercolor.fb = 2, barcol = 4,
                               outliercolor.dir = 3, fullout = FALSE)
dev.off()


#### 2. We can apply two-stage functional boxplot to the fitted CD4 cell Data
pdf("./figures/twostage_fbplot_cd4.pdf", width = 7.2, height = 6)
twostage_fbplot_cd4 <- sparse_fbplot(fit = cd4_mfpca$bootstrap_fit[[1]], 
                            sparse = cd4_mfpca$bootstrap_fit[[1]], 
                            time_index = month, 
                            depth = cd4_depth,
                            two_stage = TRUE, sq_vo = FALSE, plot = TRUE,
                            xlab = "Months since Seroconversion", 
                            ylab = "Total CD4 Cell Counts", 
                            title = "(d) Two-Stage Functional Boxplot: Fitted CD4 Cell Counts",
                            yrange = NULL,
                            cex.main = 1.1, cex.lab = 1.3, cex.axis = 1.3,
                            medlabel = FALSE, outlabel = FALSE,  
                            prob = 0.5, factor = 1.5,
                            color = 6, outliercolor.fb = 2, barcol = 4,
                            outliercolor.dir = 3, fullout = FALSE)
dev.off()

##### However, the above two methods did not consider the sparse information.

### Hence, we consider the sparse functional boxplot and 

## the intensity sparse functional boxplot for the CD4 Data


pdf("./figures/sp_fbplot_cd4.pdf", width = 7.2, height = 6)
sp_fbplot_cd4 <- sparse_fbplot(fit = cd4_mfpca$bootstrap_fit[[1]], 
                                        sparse = cd4, 
                                        time_index = month, 
                                        depth = cd4_depth,
                                        two_stage = FALSE, sq_vo = FALSE, plot = TRUE,
                                        xlab = "Months since Seroconversion", 
                                        ylab = "Total CD4 Cell Counts", 
                                        title = "(a) Sparse Functional Boxplot: Fitted CD4 Cell Counts",
                                        yrange = NULL,
                                        cex.main = 1.1, cex.lab = 1.3, cex.axis = 1.3,
                                        medlabel = FALSE, outlabel = FALSE,  
                                        prob = 0.5, factor = 1.5,
                                        color = 6, outliercolor.fb = 2, barcol = 4,
                                        outliercolor.dir = 3, fullout = FALSE)
dev.off()

pdf("./figures/sp_twostage_fbplot_cd4.pdf", width = 7.2, height = 6)
sp_twostage_fbplot_cd4 <- sparse_fbplot(fit = cd4_mfpca$bootstrap_fit[[1]], 
                                        sparse = cd4, 
                                        time_index = month, 
                                        depth = cd4_depth,
                                        two_stage = TRUE, sq_vo = FALSE, plot = TRUE,
                                        xlab = "Months since Seroconversion", 
                                        ylab = "Total CD4 Cell Counts", 
                                        title = "(b) Sparse Two-Stage Functional Boxplot: Fitted CD4 Cell Counts",
                                        yrange = NULL,
                                        cex.main = 1.1, cex.lab = 1.3, cex.axis = 1.3,
                                        medlabel = FALSE, outlabel = FALSE,  
                                        prob = 0.5, factor = 1.5,
                                        color = 6, outliercolor.fb = 2, barcol = 4,
                                        outliercolor.dir = 3, fullout = FALSE)
dev.off()
sp_twostage_fbplot_cd4$fb_outlier_index
sp_twostage_fbplot_cd4$dir_outlier_index
sp_twostage_fbplot_cd4$med_index


pdf("./figures/it_fbplot_cd4.pdf", width = 7.7, height = 6)
it_twostage_fbplot_cd4 <- intensity_sparse_fbplot(fit = cd4_mfpca$bootstrap_fit[[1]], 
                                                  sparse = cd4, 
                                                  time_index = month, 
                                                  depth = cd4_depth,
                                                  two_stage = FALSE, sq_vo = FALSE, plot = TRUE, 
                                                  xlab = "Months since Seroconversion", 
                                                  ylab = "Total CD4 Cell Counts", 
                                                  title = "(c) Intensity Sparse Functional Boxplot: Fitted CD4 Cell Counts",
                                                  yrange = NULL,
                                                  cex.main = 1.1, cex.lab = 1.3, cex.axis = 1.3,
                                                  medlabel = FALSE, outlabel = FALSE,  
                                                  prob = 0.5, factor = 1.5,
                                                  color = 6, barcol = 4,
                                                  outliercolor.fb = 2, colorrange = NULL,
                                                  outliercolor.dir = 3, fullout = FALSE,
                                                  colorsplit = 40, 
                                                  showcontour = FALSE, showlegend = TRUE)
dev.off()


pdf("./figures/it_twostage_fbplot_cd4.pdf", width = 7.7, height = 6)
it_twostage_fbplot_cd4 <- intensity_sparse_fbplot(fit = cd4_mfpca$bootstrap_fit[[1]], 
                                                  sparse = cd4, 
                                                  time_index = month, 
                                                  depth = cd4_depth,
                                                  two_stage = TRUE, sq_vo = FALSE, plot = TRUE, 
                                                  xlab = "Months since Seroconversion", 
                                                  ylab = "Total CD4 Cell Counts", 
                                                  title = "(d) Intensity Sparse Two-Stage Functional Boxplot: Fitted CD4 Cell Counts",
                                                  yrange = NULL,
                                                  cex.main = 1, cex.lab = 1.3, cex.axis = 1.3,
                                                  medlabel = FALSE, outlabel = FALSE,  
                                                  prob = 0.5, factor = 1.5,
                                                  color = 6, barcol = 4,
                                                  outliercolor.fb = 2, colorrange = NULL,
                                                  outliercolor.dir = 3, fullout = FALSE,
                                                  colorsplit = 40, 
                                                  showcontour = FALSE, showlegend = TRUE)
dev.off()
it_twostage_fbplot_cd4$fb_outlier_index
it_twostage_fbplot_cd4$dir_outlier_index
it_twostage_fbplot_cd4$med_index

