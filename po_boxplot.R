packages = c("ggplot2", "fdaPOIFD", "grid")

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



po_boxplot <- function (data, centralRegion = 0.5, fmag = 1.5, fdom = 0, 
                        depth.ordered,
                        xlab, ylab, title) {
  out <- outliergramPOFD(data, fshape = 1.5, p1 = 1, p2 = 0)
  shape.out <- out$shape
  N <- dim(data)[2]
  P <- dim(data)[1]
  w <- (N - rowSums(is.na(data)))/(N)
  if (is.null(rownames(data))) {
    rownames(data) <- x <- c(1:P)
  } else {
    x <- as.numeric(rownames(data))
  }
  if (is.null(colnames(data))) {
    colnames(data) <- ids <- c(1:N)
  } else {
    ids <- colnames(data)
  }
  if (is.null(depth.ordered)) {
    depth.ordered <- POIFD(data, type = "MBD")
  }
  mbd.ids <- depth.ordered[ids]
  order_remove_shape.out <- setdiff(as.numeric(names(depth.ordered)), shape.out)
  idCR <- as.numeric(names(depth.ordered[1:ceiling((N - length(shape.out)) * centralRegion)]))
  center <- data[, idCR]
  propCR <- rowMeans(!is.na(center))
  
  infCR <- apply(center, 1, function(x) suppressWarnings(min(x, na.rm = TRUE)))
  infCR[infCR == Inf | infCR == -Inf] = NA
  supCR <- apply(center, 1, function(x) suppressWarnings(max(x, na.rm = TRUE)))
  supCR[supCR == Inf | supCR == -Inf] = NA
  upperWhisker <- supCR + fmag * (supCR - infCR)
  lowerWhisker <- infCR - fmag * (supCR - infCR)

  # if (fdom == 0) {
  #   fdom = quantile(propCR, probs = 0.05)
  # }
  if (fdom < 1 && sum(propCR <= fdom) >= 1) {
    dom.out <-  names(which(colMeans(!is.na(center)) <= quantile(colMeans(!is.na(center)), 0.05)))
  }
  dom.out <- as.numeric(dom.out)
  median <- setdiff(as.numeric(names(depth.ordered)), union(dom.out, shape.out))[1]
  mag.out <- union(which(colSums(data <= lowerWhisker, na.rm = TRUE)!= 0),
                   which(colSums(data >= upperWhisker, na.rm = TRUE)!= 0))
  mag.out <- setdiff(mag.out, union(dom.out, shape.out))
  maximum_bound <- apply(CD4[, -c(shape.out, dom.out, mag.out)], 2, function(k)  {
      if (!is.null(k)) max(k, na.rm = TRUE)
      })
  maximum_bound[which(maximum_bound == -Inf)] = 
      (maximum_bound[which(maximum_bound == -Inf) - 1] + maximum_bound[which(maximum_bound == -Inf) + 1]) / 2
  
  minimum_bound <- apply(CD4[, -c(shape.out, dom.out, mag.out)], 2, function(k)  {
      min(k, na.rm = TRUE)
  })
  
  minimum_bound[which(minimum_bound == -Inf)] = 
      (minimum_bound[which(minimum_bound == -Inf) - 1] + minimum_bound[which(minimum_bound == -Inf) + 1]) / 2
  
  
  fdata <- data.frame(id = rep(ids, each = P), 
                      x = rep(x, N), 
                      y = c(data))
  fdataShape <- data.frame(id = rep(shape.out, each = P), 
                         x = rep(x, length(shape.out)), 
                         y = c(data[, shape.out]))
  fdataMag <- data.frame(id = rep(mag.out, each = P), 
                         x = rep(x, length(mag.out)), 
                         y = c(data[, mag.out]))
  fdataDom <- data.frame(id = rep(dom.out, each = P), 
                         x = rep(x, length(dom.out)), 
                         y = c(data[, dom.out]))
  dataWhisker <- data.frame(id = c(rep(1, P), rep(2, P)), 
                            x = c(x, rev(x)), 
                            y = c(upperWhisker, rev(lowerWhisker)), 
                            wcol = c(w, rev(w)))
  dataWhiskerBars <- data.frame(colBar = c(w[ceiling(P / 2)], w[ceiling(P / 2)]), 
                                x1 = c(x[ceiling(P / 2)], x[ceiling(P / 2)]), 
                                x2 = c(x[ceiling(P / 2)], x[ceiling(P / 2)]), 
                                y1 = c(supCR[ceiling(P / 2)], infCR[ceiling(P / 2)]), 
                                y2 = c(upperWhisker[ceiling(P / 2)], lowerWhisker[ceiling(P / 2)]))
  auxPolygonX <- c(matrix(rep(c(1, 2, 2, 1), P - 1), nrow = 4) + 
                     matrix(rep(seq(0:c(P - 2)) - 1, each = 4), nrow = 4))
  auxPolygonLowHigh <- c(matrix(rep(c(1, 2, P + 2, P + 1), P - 1), nrow = 4) + 
                           matrix(rep(seq(0:c(P - 2)) - 1, each = 4), nrow = 4))
  dataBand <- data.frame(id = rep(factor(1:c(P - 1)), each = 4), 
                         x = x[auxPolygonX], 
                         y = c(infCR, supCR)[auxPolygonLowHigh], 
                         wcol = rep(w[2:P], each = 4))
  boxplotPartially <- ggplot() + 
    geom_polygon(data = dataBand, aes(x = .data$x, y = .data$y, group = .data$id, fill = .data$wcol, color = .data$wcol)) + 
    geom_line(data = dataWhisker, aes(x = .data$x, y = .data$y, group = .data$id, color = .data$wcol), cex = 1) + 
    geom_segment(data = dataWhiskerBars, aes(x = .data$x1, y = .data$y1, xend = .data$x2, yend = .data$y2, color = .data$colBar), 
                 size = 0.75) +
    geom_line(data = fdataMag[!is.na(fdataMag$y), ], aes(x = .data$x, y = .data$y, group = .data$id), color = "yellow", 
              linetype = "dashed", cex = 0.75) + 
    geom_line(data = fdataDom[!is.na(fdataDom$y), ], aes(x = .data$x, y = .data$y, group = .data$id), color = "purple", 
              linetype = "dashed", cex = 0.75) + 
    geom_point(data = fdataDom[!is.na(fdataDom$y), ], aes(x = .data$x, y = .data$y, group = .data$id), color = "purple",
               cex = 0.75) +
    geom_line(data = fdataShape[!is.na(fdataShape$y), ], aes(x = .data$x, y = .data$y, group = .data$id), color = "green", 
              linetype = "dashed", cex = 0.75) + 
    geom_point(data = fdata[intersect(which(fdata$id == median), which(!is.na(fdata$y) == TRUE)), ], 
               aes(x = .data$x, y = .data$y), color = "white", cex = 0.75) +
    scale_fill_gradient(low = "white", high = "black", limits = c(0, 1), aesthetics = c("fill", "color")) + 
    labs(x = xlab, y = ylab, title = title) + 
    theme(legend.position = "none", legend.key.size = unit(0.75, "cm"), legend.title = element_blank(), 
          panel.background = element_blank(), plot.title = element_text(hjust = 0.5), 
          axis.line = element_line(colour = "black", size = rel(1)))
  pdf("partially_observed_fbplot.pdf", width = 4.2, height = 4)
  par(mfrow = c(1, 1), mai = c(0.5, 0.55, 0.3, 0.07), 
      mar = c(3.5, 3.5, 2, 1), mgp = c(2, 1, 0))
  boxplotPartially
  dev.off()
  
  pdf("outliergram_cd4.pdf", width = 4.2, height = 4)
  par(mfrow = c(1, 1), mai = c(0.5, 0.55, 0.3, 0.07), 
      mar = c(3.5, 3.5, 2, 1), mgp = c(2, 1, 0))
  outliergramPOFD(data, fshape = 1.5, p1 = 1, p2 = 0)
  dev.off()
  
 
  return(list(fboxplot = boxplotPartially, 
              all.out = unique(c(mag.out, dom.out)), 
              magnitude = mag.out, domain = dom.out))
}
