packages <- c("remotes")
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
remotes::install_github("CarolinaEuan/HMClust")
library(devtools)
install_github("CarolinaEuan/HMClust")
library(HMClust)

HMAlgo <- function(x, fx, parallel = FALSE, 
                   normalize = TRUE, tvd = TRUE) {
  n <- length(fx[, 1])
  k <- length(fx[1, ])
  if(normalize)fxN <- apply(fx, 2, normalize, 
                            x = x) else fxN <- fx
  if(parallel) {
    library("doParallel")
    cl <- min(k, detectCores() - 1)
    registerDoParallel(cl)
    if(tvd) {
      MatDiss <- foreach(i = 1:k, .combine = rbind) %dopar% {
        Aux.MatDiss <- rep(NA, k)
        for(j in i:k) Aux.MatDiss[j] <- TVD(x, fxN[, i], fxN[, j])
        Aux.MatDiss
      }
    } else {
      MatDiss <- foreach(i = 1:k, .combine = rbind) %dopar% {
        Aux.MatDiss <- rep(NA, k)
        for(j in i:k) Aux.MatDiss[j] <- sqrt(sum((fxN[, i] - fxN[, j]) ^ 2))
        Aux.MatDiss
      }
    }
  } else {
    MatDiss <- matrix(NA,k,k)
    if(tvd) {
      for(i in 1:k) for(j in i:k) MatDiss[i, j] <- TVD(x, fxN[, i],
                                                       fxN[, j])
    } else {
      for(i in 1:k) for(j in i:k) MatDiss[i, j] <- sqrt(sum((fxN[, i] - fxN[, j])^2))
    }
  }
  diag(MatDiss) <- 0
  MatDiss[lower.tri(MatDiss)] <- t(MatDiss)[lower.tri(MatDiss)]
  #Dinamic Diss Mat
  MD_Change <- MatDiss# initial MD
  diag(MD_Change) <- rep(Inf, k)
  #Dinamic estimated spectra
  fx_Change <- fxN
  # initial groups
  g <- as.list(1:k)
  #Save variables
  min.value <- numeric(k - 1)
  groups <- list()
  #Average version
  #Version 2
  if(tvd) {
    for(ite in 1:(k-1)) {
      #############
      # Identify the closest clusters and the minimun value
      min.value[ite] <- min(MD_Change)
      aux1 <- which(MD_Change == min(MD_Change), arr.ind = TRUE)[1, ]
      g <- c(g[-aux1], list(unlist(g[aux1])))# new groups
      groups[[ite]] <- g ##
      #############
      # Spectral Merge
      fx_Change <- fx_Change[, -aux1]
      fx_Change <- cbind(fx_Change, rowMeans(fxN[, g[[k - ite]]]))
      #############
      #Compute new TVD
      MD_Change <- MD_Change[-aux1, -aux1]
      if(ite < (k - 1)){
        aux2 <- numeric(k - ite - 1)
        for(i in 1:(k - ite - 1)){
          aux2[i] <- TVD(x, fx_Change[, i],
                         fx_Change[, k - ite])
        }
        MD_Change <- rbind(MD_Change, aux2)
        MD_Change <- cbind(MD_Change, c(aux2, Inf))# new MD
      }else{ MD_Change <- 0}
    }
  } else {
    for(ite in 1:(k-1)){
      #############
      # Identify the closest clusters and the minimun value
      min.value[ite] <- min(MD_Change)
      aux1 <- which(MD_Change == min(MD_Change), arr.ind = TRUE)[1,]
      g <- c(g[-aux1], list(unlist(g[aux1])))# new groups
      groups[[ite]] <- g ##
      #############
      # Spectral Merge
      fx_Change <- fx_Change[, -aux1]
      fx_Change <- cbind(fx_Change, rowMeans(fxN[, g[[k - ite]]]))
      #############
      #Compute new TVD
      MD_Change <- MD_Change[-aux1, -aux1]
      if(ite < (k - 1)) {
        aux2 <- numeric(k - ite - 1)
        for(i in 1:(k - ite - 1)){
          aux2[i] <- sqrt(sum((fx_Change[, i] - fx_Change[, k - ite])^2))
        }
        MD_Change <- rbind(MD_Change, aux2)
        MD_Change <- cbind(MD_Change, c(aux2, Inf))# new MD
      } else{ 
          MD_Change <- 0
         }
    }
  }
  out <- list(MatDiss, min.value, groups)
  names(out) <- c("Diss.Matrix", 
                  "min.value", 
                  "Groups")
  out
}

normalize <- function(x, fx, normfx = FALSE,
                      D = 1, y = NULL) {
  #normalization with trapezoidal rule
  if(D == 1){
    length.x <- length(x)
    inte <- ((x[2:length.x] - x[1:(length.x-1)]) %*% (fx[2:length.x] + fx[1:(length.x-1)]) / 2)
  }
  if(D == 2){
    length.x <- length(x)
    length.y <- length(y)
    We <- matrix(4, 
                 nrow = length.x, 
                 ncol=length.y)
    We[c(1, 1, length.x, length.x),
       c(1,length.y, 1, length.y)] <- 1
    We[2:(length.x - 1), 1] <- 2
    We[2:(length.x - 1), length.y] <- 2
    We[1,2:(length.y - 1)] <- 2
    We[length.x, 2:(length.y - 1)] <- 2
    inte <- sum(fx * We) * (x[2] - x[1]) * (y[2] - y[1]) * (1 / 4)
  }
  if(normfx) {
    return(inte)
  } else {
    if(inte == 0) {
      return(fx)
    } else{
      return(fx / inte)}
  }
}
