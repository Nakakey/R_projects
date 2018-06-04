##読み込むデータのディレクトリ
library(shiny)
library(plotly)
library(MCMCpack)
library(dplyr)

num_grid <- 100
x <- seq(0, 1, length.out = num_grid)
y <- seq(0, 1, length.out = num_grid)
gri <- expand.grid(x, y)
colnames(gri) <- c("x", "y")
gri <- gri %>%
  mutate(ifelse((1 - x - y ) >= 0, (1 - x - y), 0))
colnames(gri)[3] <- "z"

runApp("shiny_scripts")
