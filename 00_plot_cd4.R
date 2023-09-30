packages = c("refund", "fdaPOIFD")

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
list("cd4")
CD4 <- cd4 ## load the cd4 data which is a matrix of 366 * 61


pdf("./figures/observed_cd4.pdf", height = 4, width = 4)
par(mfrow = c(1, 1), mai = c(0.5, 0.55, 0.3, 0.07), 
    mar = c(3.5, 3.5, 2, 1), mgp = c(2, 1, 0))
plot(month, CD4[1, ], type = "n", 
     ylim = range(CD4, na.rm = TRUE), ylab ="CD4 Cell Counts",
     xlab = "Months since Seroconversion", main = "(a) Observed CD4 Cell Counts", cex.main = 1.2)
na_prop <- lapply(1:nrow(CD4), function(ij) {
  cd4_df <- data.frame(month, CD4[ij, ])
  prop_na <- length(which(is.na(CD4[ij, ]) == TRUE)) / ncol(CD4)
  lines(na.omit(cd4_df), col = "grey", lty = 2)
  #points(month, ij, col = "black", cex = 0.4)
  #lines(month, ij, col = "black", lty = 1)
  return(prop_na)
})

apply(CD4, 1, function(ij) {
  #cd4_df <- data.frame(month, ij)
  #prop_na<-length(which(is.na(ij)==TRUE))/61
  #lines(na.omit(cd4_df),col="grey",lty=2)
  #points(month,ij,col="black",cex=0.4)
  lines(month, ij, col = "black", lty = 1)
  #return(prop_na)
})

apply(CD4, 1, function(ij) {
  cd4_df <- data.frame(month, ij)
  #prop_na<-length(which(is.na(ij)==TRUE))/61
  #lines(na.omit(cd4_df),col="grey",lty=2)
  points(month, ij, col = "black", cex = 0.4)
  #lines(month,ij,col="black",lty=1)
  #return(prop_na)
})
dev.off()
