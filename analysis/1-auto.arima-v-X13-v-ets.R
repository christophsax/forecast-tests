# compares X13-SEATS, auto.arima, and ets against the M3 competition data

# original analysis by Peter Ellis, at http://ellisp.github.io/blog/2015/12/21/m3-and-x13/ 
# extended by Christoph Sax, using robust.seas and parallelization


# use new robust.seas function from seasonal dev
devtools::install_github("christophsax/seasonal")

library(Mcomp)     # competition data
library(seasonal)
library(forecast) 
library(parallel)  # part of R base, but needs to be loaded


# --- cluster parallelization, which also works on Windows ---------------------

# (on Linux and Mac, you can use mclapply, which is much simpler)

# set up cluster
cl <- makeCluster(detectCores())

# load 'seasonal' for each node
clusterEvalQ(cl, {library(seasonal); library(forecast)})

# export data to each node
clusterExport(cl, varlist = "M3")

# run in parallel (times on an older Macbook Pro with 8 cores)

system.time({
  lets1 <- parLapply(cl, M3, function(e) forecast(ets(e$x), h = 18, PI = FALSE)$mean)
})
#  user  system elapsed 
# 0.039   0.013 264.158 

system.time({
  laarima <- parLapply(cl, M3, function(e) forecast(auto.arima(e$x), h = 18)$mean)
})
#  user  system elapsed 
# 0.041   0.014 312.261 

# X-13 at least wins the speed contest!
system.time({
  lx13 <- parLapply(cl, M3, function(e) series(robust.seas(e$x, forecast.save = "fct", forecast.maxlead = 18, seats = NULL), "forecast.forecasts")[, 1])
})
#  user  system elapsed 
# 0.049   0.017  62.936

# finally, stop the cluster
stopCluster(cl)


# --- Compute accuracy ---------------------------------------------------------

# lists with acutals and scale
lact <- lapply(M3, function(e) e$xx)
lscl <- lapply(M3, function(e) mean(abs(diff(e$x, lag = frequency(e$x)))))

# to convert the forecasts in M3 from matrix to the same 'list style' as lact
# and lscl
toList <- function(m){
  m <- as.matrix(m)
  z <- list()
  for (i in 1:nrow(m)){
    zi <- m[i, ]
    z[[i]] <- zi[!is.na(zi)]
  }
  z
}

# collect all these lists in a big list
ll <- list()
ll$act <- lact
ll$scl <- lscl
ll$THETA <- toList(M3Forecast$THETA)
ll$ForecastPro <- toList(M3Forecast$ForecastPro)
ll$ForcX <- toList(M3Forecast$ForcX)
ll$`B-J auto` <- toList(M3Forecast$`B-J auto`)
ll$AutoBox1 <- toList(M3Forecast$AutoBox1)
ll$AutoBox2 <- toList(M3Forecast$AutoBox2)
ll$AutoBox3 <- toList(M3Forecast$AutoBox3)
ll$x13 <- lx13
ll$aarima <- laarima
ll$ets1 <- lets1

# subsets for monthly, quarterly and yearly data
ll.m <- lapply(ll, function(e) e[sapply(M3, function(e) e$period == "MONTHLY")])
ll.q <- lapply(ll, function(e) e[sapply(M3, function(e) e$period == "QUARTERLY")])
ll.y <- lapply(ll, function(e) e[sapply(M3, function(e) e$period == "YEARLY")])

# function to compute MASE on all components of the list (except the first 2)
lMASE <- function(ll){
  MASE <- function(lfct){
    ff <- function(act, fct, scl) mean(abs(c(act) - c(fct)[1:length(act)])) / scl
    mean(unlist(Map(ff, act = ll$act, fct = lfct, scl = ll$scl)))
  }
  sapply(ll[-c(1,2)], MASE)
}



out <- data.frame(all = lMASE(ll),
           monthly = lMASE(ll.m),
           quarterly = lMASE(ll.q),
           yearly = lMASE(ll.y))

#                  all   monthly quarterly   yearly
# THETA       1.394629 0.8578892  1.086772 2.806325
# ForecastPro 1.467114 0.8475165  1.203647 3.025574
# ForcX       1.422250 0.8942060  1.154643 2.769352
# B-J auto    1.544345 0.9136582  1.188486 3.164894
# AutoBox1    1.685426 0.9244682  1.330999 3.678540
# AutoBox2    1.512327 1.0824328  1.185067 2.753962
# AutoBox3    1.574326 0.9622224  1.272043 3.177214
# x13         1.557856 0.8757007  1.250895 3.241308
# aarima      1.460277 0.8806883  1.186706 2.963824
# ets1        1.426508 0.8638731  1.178036 2.864631

# knitr::kable(out)



