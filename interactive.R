packages <- c("refund.shiny", "refund")
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
data("CanadianWeather")
fpca_pro <- fpca.face(Y = t(CanadianWeather$monthlyTemp), p = 3,
          knots = 8,
          argvals = 1:12, pve = 0.95, var = TRUE)
mat <- t(CanadianWeather$monthlyTemp)
rownames(mat) <- 1:35
fpca2_pro <- fpca2s(Y = mat,
                      argvals = 1:12 / 12, npc = 4)
plot_shiny(fpca2_pro)
#library(rsconnect)
#rsconnect::setAccountInfo(name='zhuoqu',
#                          token='89B51C57EBB1678E1E0EB5B64F4192F0',
#                          secret='8ljF3yNHJAFTq/m5leP1Hp2NPR9OpQDu7N9or0bc')
#rsconnect::deployApp('path/to/your/app')
