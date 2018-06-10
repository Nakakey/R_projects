##読み込むデータのディレクトリ
## plotly version: ‘4.7.1.9000’
# devtools::install_github("ropensci/plotly")
library(shiny)
library(plotly)
library(MCMCpack)
library(dplyr)

num_grid <- 100
x <- seq(0, 1, length.out = num_grid)
y <- seq(0, 1, length.out = num_grid)
gri <- expand.grid(x, y)
colnames(gri) <- c("x", "y")
gri <- subset(gri, x + y <= 1)
gri <- mutate(gri, 1 - x - y)
colnames(gri)[3] <- "z"

## coordinate transformation
mat_ct = matrix(c(1,0, 1/2,sqrt(3)/2), 2, 2)
gri_xy_ct <- as.matrix(gri[,c("x","y")]) %*% t(mat_ct)

runApp("shiny_scripts")
