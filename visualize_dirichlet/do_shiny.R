##読み込むデータのディレクトリ
library(shiny)
library(rlist)
library(RColorBrewer)
library(rgl)
library(ggplot2)
library(reshape2)

print(dir("../",pattern="*.Rdata"))
ind <- as.numeric(readline("insert the file number"))
name_file <- dir("../",pattern="*.Rdata")[ind]
load(paste0("../",name_file))

make_grid <- function(border,domain,grid_num){
	x <- seq(-domain,domain,length=grid_num)
	y <- seq(-domain,domain,length=grid_num)
	x_y <- expand.grid(x,y)
	z <- apply(x_y,1,function(x)
		sqrt(border^2 - sum(x^2))
		)
	z[is.nan(z)] <- 0
	return( cbind(x_y,z) )
}
point_grid = make_grid(brain_size,brain_size,70)
grid_cortex = make_grid(brain_size-cortex.depth,brain_size-cortex.depth,70)
tseq <- nrow(eeg)
cols = brewer.pal(8,"Accent")
col_alpha = paste0(cols,"1A")

evaluate_result2 <- function(x){
  nums_pt <- list()
  dims <- list()
  for(i in 1:length(x)){
  nums_pt[[i]] <- sapply(x[[i]],length)
  dims[[i]] <-sapply(1:length(nums_pt[[i]]),function(y)nrow(x[[i]][[y]][[1]]$mean))
  }
  pt_num <- nums_pt[[1]]
  tmp <- sapply(1:length(dims),function(x) sapply(dims[[x]],function(y)c(x,y)) )
  tmp <- do.call(cbind,tmp)
  df <- as.data.frame(t(tmp))
  df <- cbind(df,unlist(nums_pt)/pt_num)
  colnames(df) <- c("time_step","result","weight")
  return(list(df=df,dims=dims,nums=nums_pt))
}

status_print <- c("ver_min","ver_map","both","simple")
#status <- as.numeric(readline("plot estimate result?(MIN:1)or(MAP:2)or(both:3) or not:"))


##make estimate result
res <- evaluate_result2(res_posterior)
df <- res$df
num_model <- res$dims
est <- sapply(num_model,min)
ind_MAP <- sapply(res$nums,which.max)
est_MAP <- sapply(1:length(res_posterior),function(x)res$dims[[x]][ind_MAP[x]])
name_gt <- ls(pattern="source*")
gts <- lapply(name_gt,function(x)eval(parse(text=x)))
gt <- apply(sapply(gts,colSums),1,function(x)sum(x!=0))

runApp("shiny_scripts")
