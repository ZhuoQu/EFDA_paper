#### This file plots the simplified sparse functional boxplot of CD4 plots

## first we need to transform cd4 data to a list of three categories: argvals, subj and y
source("00_plot_cd4.R")
source("parameterization_PD.R")
source("multivariate_functional_depth.R")
cd4_clust <- list()
for (l in 1:nrow(CD4)) {
  observed_index <- which(!is.na(CD4[l, ]))
  cd4_clust[[l]] <- list(argvals = as.numeric(colnames(CD4)[observed_index]),
                         subj = rep(l, length(observed_index)),
                         y = CD4[l, observed_index])
}
param_cd4 <- parameterization_PD(cd4_clust)
cd4_global_extremal <- mulitvariate_functional_depth(nbvar = 1, 
                                                     param_PD = param_cd4,
                                                     point_depth_comp = "global",
                                                     type = "extremal depth", 
                                                     mt_depth = "halfspace depth", weight = "time")
source("boxplot_criterion.R")
source("boxplot_time_criterion.R")
source("simplified_sparse_boxplot.R")
source("simplified_sparse_intensity_boxplot.R")


extremal_boxplot <- boxplot_criterion(nbvar = 1, 
                                      param_PD = param_cd4, 
                                      Dmat = cd4_global_extremal, 
                                      plotboxplot = 1, 
                                      outlier_candidate = NULL,
                                      titles = "Simplified Sparse Functional Boxplot: Observed CD4 Cell Counts", 
                                      ylabs = "Total CD4 Cell Counts")

domain_outlier <- extremal_boxplot$domain_outlier
select_index[domain_outlier]
function_outlier <- unique(unlist(extremal_boxplot$functional_outlier))
extremal_intensity_boxplot <- boxplot_criterion(nbvar = 2, 
                                                param_PD = param_hurricane, 
                                                Dmat = hurricane_global_extremal, 
                                                plotboxplot = 2, 
                                                outlier_candidate = NULL,
                                                titles = c("Simplified Intensity Sparse Functional Boxplot: Longitude",
                                                           "Simplified Intensity Sparse Functional Boxplot: Latitude"), 
                                                ylabs = c("Longitude", "Latitude"))

