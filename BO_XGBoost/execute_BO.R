set.seed(123)

require(dplyr)
require(ggplot2)
require(caret)
library(bigrquery)
library(httpuv)
library(xgboost)
require(Matrix)
require(data.table)
library(ROCR)
library(rBayesianOptimization)

###########################################
##       make train data of XGboost        ###
###########################################
## converting to 2 class
## 2-class 化
convert_cnt2 <- function(x,threshold){
    if( x > threshold) { return(1)
        }else{return(0)}
}

##(目的変数作成)

# load asb data 
load("repeater_asb.Rdata")
## 外れ値除去
num_upperclass <- 3
dat_fil <- 
    dat_asb %>%
    filter(f_year <= 2016) %>%
    filter(age>=18 & age < 70) %>%
		#filter(cnt ==1 | cnt > 3) %>%
    filter(cnt <= 10) 
#cap_id 重複データのドロップ
dat_fil <- dat_fil[!duplicated(dat_fil$cap),]

##(説明変数作成)##

#load jln rsv dta
load(file = "asb_usr_jln_booking.Rdata")


#外れ値除去
# 30回の根拠としては、平均使用回数が8回、sdが12くらいだったので一応2シグマ基準ということで
dat_ovl_jln <- 
    dat_jln %>%
    filter(c_checkout_total < 30 & c_checkout_total>= 4) %>%
    filter(cap_member_id %in% dat_fil$cap) %>%
    arrange(cap_member_id)

dat_ovl_asb <-
    dat_fil %>%
    filter(cap %in% dat_ovl_jln$cap_member_id) %>%
    arrange(cap)

# カウントデータを割合変換
dat_ovl_jln[,2:62] <- dat_ovl_jln[2:62]/dat_ovl_jln$c_checkout_total
dat_set <-  cbind(dat_ovl_jln[ , -1], label = sapply(dat_ovl_asb$cnt, function(x)convert_cnt2(x,2)) )
dat_set[is.na(dat_set)] <- 0

## make data for XGBoost
dat <- dat_set
num_dat <- nrow(dat)
ind_train <- sample(num_dat, round(0.8 *num_dat) )
df_train <- dat[ind_train, ]
df_test <- dat[-ind_train, ]
train.mx <- sparse.model.matrix(label~. , df_train)
test.mx <- sparse.model.matrix(label~. , df_test)
dtrain <- xgb.DMatrix( train.mx, label = df_train$label )
dtest <- xgb.DMatrix( test.mx, label = df_test$label )
weight <- nrow(filter(dat_set,label==0)) / nrow(filter(dat_set,label==1))

## holdout function of optimizing
watchlist <- list( eval = dtest, train = dtrain ) 

xgb_holdout <- function( ex, xweight, xdepth, nr, xg, xcol ){
    model <- xgb.train( 
        params = list( eta = ex/100, min_child_weight = xweight, max_depth = xdepth, gamma = xg/10 ,##serch parameters 
                              colsample_bytree = xcol/50, ##serch parameters 
                              silent = 0, lambda = 2, objective = 'binary:logistic' ,
                              eval_metric = 'auc', scale_pos_weight = weight ,  subsample = 0.7 ), 
        data = dtrain, nrounds=1000, nthread = 2,
        early_stopping_rounds = 10, watchlist = watchlist)
        ##eval score
	  t.pred <- rbind (predict( model, newdata = dtest ) , df_test$label )
	  pred_roc <- prediction(t.pred[1,],t.pred[2,])
	  auc.tmp <- performance(pred_roc,"auc")
	  auc <- as.numeric(auc.tmp@y.values)
	  list( Score = auc , Pred = auc )
}

print("start optimization")
opt_xgb <- BayesianOptimization(xgb_holdout,
			  bounds = list(ex = c(1L,20L), xweight = c(3L, 10L), xdepth = c(3L , 10L), 
				xg = c(1L, 30L), xcol = c(35L, 50L)),
			  init_points =10, n_iter = 30, acq = "ei" , kappa = 2.576 , eps = 0.0 ,verbose = TRUE
			  )
