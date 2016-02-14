# compares X13-SEATS, auto.arima, and ets against the M3 competition data
# Peter Ellis, originally at http://ellisp.github.io/blog/2015/12/21/m3-and-x13/ 

library(Mcomp) # competition data
library(dplyr)
library(tidyr)
library(seasonal)
library(showtext)
library(ggplot2)
library(scales)
library(RColorBrewer)



checkX13()

nseries <- length(M3)


# forecasts from the original competition
theta <- as.matrix(M3Forecast$THETA)[1:nseries, ]
fpro <- as.matrix(M3Forecast$ForecastPro)[1:nseries, ]
fcx <- as.matrix(M3Forecast$ForcX)[1:nseries, ]
bjauto <- as.matrix(M3Forecast$`B-J auto`)[1:nseries, ]
ab1 <- as.matrix(M3Forecast$AutoBox1)[1:nseries, ]
ab2 <- as.matrix(M3Forecast$AutoBox2)[1:nseries, ]
ab3 <- as.matrix(M3Forecast$AutoBox3)[1:nseries, ]

# set up matrices to hold the forecasts from new methods
hybrid2 <- hybrid_x13 <- hybrid3 <- matrix(NA, nrow = nseries, ncol = 18)


### use new robust.seas function from seasonal dev

devtools::install_github("christophsax/seasonal", ref = "feature/robust.seas")


### cluster parallelization, which also works on Windows
# (on Linux and Mac, you can use mclapply, which is much easier)

library(parallel)

# set up cluster
cl <- makeCluster(detectCores())

# load 'seasonal' for each node
clusterEvalQ(cl, {library(seasonal); library(forecast)})

# export data to each node
clusterExport(cl, varlist = "M3")

# run in parallel (on an older Macbook Pro with 8 cores)

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

lact <- lapply(M3, function(e) e$xx)
lscl <- lapply(M3, function(e) mean(abs(diff(e$x, lag = frequency(e$x)))))

toList <- function(m){
  m <- as.matrix(m)
  z <- list()
  for (i in 1:nrow(m)){
    zi <- m[i, ]
    z[[i]] <- zi[!is.na(zi)]
  }
  z
}


# collect fct in a big list
ll <- list()
ll$act <- lact
ll$scl <- lscl
ll$theta <- toList(M3Forecast$THETA)
ll$x13 <- lx13
ll$aarima <- laarima
ll$ets1 <- lets1



lMASE <- function(ll){
  MASE <- function(lfct){
    ff <- function(act, fct, scl) mean(abs(c(act) - c(fct)[1:length(act)])) / scl
    mean(unlist(Map(ff, act = ll$act, fct = lfct, scl = ll$scl)))
  }

  lapply(ll[-c(1,2)], MASE)
}


ll.m <- lapply(ll, function(e) e[sapply(M3, function(e) e$period == "MONTHLY")])
ll.q <- lapply(ll, function(e) e[sapply(M3, function(e) e$period == "QUARTERLY")])
ll.y <- lapply(ll, function(e) e[sapply(M3, function(e) e$period == "YEARLY")])


lMASE(ll)
# $theta
# [1] 1.394629

# $x13
# [1] 1.557856

# $aarima
# [1] 1.460277

# $ets1
# [1] 1.426508


lMASE(ll.m)
# $theta
# [1] 0.8578892

# $x13
# [1] 0.8757007

# $aarima
# [1] 0.8806883

# $ets1
# [1] 0.8638731


lMASE(ll.q)
# $theta
# [1] 1.086772

# $x13
# [1] 1.250895

# $aarima
# [1] 1.186706

# $ets1
# [1] 1.178036


lMASE(ll.y)
# $theta
# [1] 2.806325

# $x13
# [1] 3.241308

# $aarima
# [1] 2.963824

# $ets1
# [1] 2.864631



# X-13 better for monthly data, but worse for annual and quarterly...



# looking, e.g at quarterly data does not reveal any obvious problem...

l <- ll.q

for (i in 1:length(l$act)){
  cat ("Press [enter] to continue")
  line <- readline()
  ts.plot(cbind(l$act[[i]], l$aarima[[i]], l$x13[[i]]), col = c(1, 3, 2))
}




# --- Compute accuracy ---------------------------------------------------------

number_methods <- 13
mase <- mape <- smape <- matrix(NA, nrow = number_methods, ncol = nseries)
f <- matrix(NA, nrow = number_methods, ncol = 18)
for(i in 1:nseries)
{
   x <- M3[[i]]$xx
   n <- length(x)
   f[1, 1:n] <- theta[i, 1:n]
   f[2, 1:n] <- fpro[i, 1:n]
   f[3, 1:n] <- fcx[i, 1:n]
   f[4, 1:n] <- bjauto[i, 1:n]
   f[5, 1:n] <- ab1[i, 1:n]
   f[6, 1:n] <- ab2[i, 1:n]
   f[7, 1:n] <- ab3[i, 1:n]
   f[8, 1:n] <- ets1[i, 1:n]
   f[9, 1:n] <- aarima[i, 1:n]
   f[10, 1:n] <- hybrid2[i, 1:n]
   f[11, 1:n] <- x13_2[i, 1:n]
   f[12, 1:n] <- hybrid_x13[i, 1:n]
   f[13, 1:n] <- hybrid3[i, 1:n]
   scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(x))))
   for(j in 1:number_methods)
   {
      mape[j, i] <- mean(abs((x - f[j, 1:n]) / x)) * 100
      smape[j, i] <- mean(abs(x - f[j, 1:n]) / (abs(x) + abs(f[j, 1:n]))) * 200
      mase[j, i] <- mean(abs(x - f[j, 1:n]) / scale)
   }
}

# All series
m3table <- matrix(NA, nrow = number_methods, ncol = 3)
m3table[,1] <- rowMeans(mape, na.rm = TRUE)
m3table[,2] <- rowMeans(smape)
m3table[,3] <- rowMeans(mase)
m3table <- as.data.frame(m3table)

names(m3table) <- c("MAPE", "sMAPE", "MASE")

m3table$method <- c("Theta", "ForecastPro", "ForecastX", "BJauto",
                       "Autobox1", "Autobox2", "Autobox3",
                       "ETS", "AutoARIMA", "Hybrid AutoARIMA ETS", 
                       "X13-SEATS-ARIMA", "Hybrid AutoARIMA X13", "Hybrid AA-X13-ETS")

m3table %>%
   arrange(MASE)

p1 <- m3table %>%
   arrange(-MASE) %>%
   mutate(method = factor(method, levels = method)) %>%
   mutate(hybrid = ifelse(grepl("Hybrid", method), "Hybrid", "Single")) %>%
   ggplot(aes(x = MASE, y = method, colour = hybrid)) +
   geom_point() +
   geom_segment(aes(yend = method), xend = 1) +
   labs(x = "Mean Absolute Scaled Error (smaller is better)", colour = "") +
   ggtitle("Comparison of selected forecasting methods on the M3 data")
svg("output/1-dots.svg", 6, 5)
print(p1)
dev.off()

png("output/1-dots.png", 600, 500, res = 100)
print(p1)
dev.off()

#=================further investigation===============

#-------------example series with unit root--------------
# Series Y434, Marital status (numbers in thousands), 1974 to 1988
svg("output/1-prob1.svg", 6, 4)
plot(M3[[434]]$x, bty = "l", main = "Y434 Marital status (numbers in thousands)")
grid()
dev.off()

# this works and decides it's ARIMA(0,1,0) with drift:
auto.arima(M3[[434]]$x)

# this fails
robust.seas(M3[[434]]$x)
seas(M3[[434]]$x, regression.aictest = NULL, x11 = "")

# but if we manually specify the model
m <- seas(M3[[434]]$x, 
          regression.aictest = NULL, 
          regression.variables = c("const"),
          arima.model = c(0,1,0), 
          forecast.maxlead = 18)
series(m, "forecast.forecasts")[1:18 , 1]    

# which is the same as auto.arima's original forecast:
aarima[434, ]

#-----------trading day problem--------------
# quarterly Denmark GDP by expenditure
svg("output/1-prob2.svg", 6, 4)
plot(M3[[1154]]$x, bty = "l", main = "Q509 Denmark GDP by expenditure")
grid()
dev.off()

# fails because of singularity to do with number of Saturdays:
seas(M3[[1154]]$x)


# if we specify manually which regression variables to choose 
# and we drop trading days, we get a result:
m <- seas(M3[[1154]]$x, 
          regression.variables = c("const", "easter[14]", "seasonal"), 
          forecast.maxlead = 18)
series(m, "forecast.forecasts")[1:18 , 1]    

aarima[1154, ]


#---------------runs but produces no data------
# quarterly cash flow
svg("output/1-prob3.svg", 6, 4)
plot(M3[[807]]$x, bty = "l", main = "Q162 Cash Flow" )
grid()
dev.off()

# following runs but produces no data
seas(M3[[807]]$x)

# this works and concludes MA(1):
auto.arima(M3[[807]]$x)

# but if we manually specify the model
m <- seas(M3[[807]]$x, 
          regression.variables = c("const"),
          arima.model = c(0,0,1), 
          forecast.maxlead = 18)
series(m, "forecast.forecasts")[1:18 , 1]  
aarima[807, ]


#---------cannot process spec file for an unknown reason---------
# Monthly shipments
svg("output/1-prob4.svg", 6, 4)
plot(M3[[1485]]$x, main = "M84, Shipments")
grid()
dev.off()

# runs fine:
auto.arima(M3[[1485]]$x)

# takes a long time and eventually fails for unknown reason:
robust.seas(M3[[1485]]$x)


# still fails:
m <- seas(M3[[1485]]$x, 
          regression.variables = c("const"),
          arima.model = c(0,1,1), 
          forecast.maxlead = 18)

# can't work this one out


#-----------can't handle phony date/times----------------
# Telecommunications data, start = "1"
svg("output/1-prob5.svg", 6, 4)
plot(M3[[3000]]$x, main = "O171 Telecommunication data")
grid()
dev.off()

# fails because X13 can't handle abstract times, or things 
# starting in year 1.  No obvious way around this.
seas(M3[[3000]]$x)


#=================when X13 runs, but does different than auto.arima=============
# 15th row of mase is for X13 and 9th is for auto arima


mases <- data_frame(
      x13 = mase[15, ],
      aa = mase[9, ],
      id = 1:ncol(mase)) %>%
   mutate(diff = x13 - aa) 


#------------eg when X13 much better than auto.arima---------------
mases %>% arrange(diff)
# stock shares sold on NYSE 1947 to 1987
# note that basically the crash of 1987 is what stuffed auto arima
# X13 seems to make an inspired guess!
m <- seas(M3[[335]]$x, regression.aictest = NULL, regression.variables = "const")
summary(m) # ARIMA(1,0,0)
auto.arima(M3[[335]]$x) # ARIMA(0,2,0)

comp_x13_aa <- function(s){
            
   orig <- M3[[s]]$x
   act  <- M3[[s]]$xx
   
   tmp1 <- rbind(
      data.frame(
         time = as.numeric(time(orig)),
         value = as.numeric(orig),
         method = "Actual result",
         stringsAsFactors = FALSE),
      data.frame(
         time = as.numeric(time(act)),
         value = as.numeric(act),
         method = "Actual result",
         stringsAsFactors = FALSE)
   )
   
   tmp2 <- data_frame(
      x13 = x13[s, 1:length(act)],
      aa  = aarima[s, 1:length(act)],
      time = time(act)
   ) %>%
      gather(method, value, -time) 
      
   tmp <- rbind(tmp1, tmp2) %>%
      mutate(method = gsub("aa", "auto.arima()", method),
             method = gsub("x13", "X13-SEATS", method))
   
   # we want a named vector of colours so each method gets the same colour each time
   pal <- brewer.pal(3, "Set1")
   names(pal) <- unique(tmp$method)
   
   # need the order to help order the legend
   latest <- tmp %>%
      filter(time == max(time)) %>%
      arrange(-value) 
   
   p1 <- tmp %>%
      mutate(method = factor(method, levels = latest$method)) %>%
      ggplot(aes (x = time, y = value, colour = method)) +
      geom_line() +
      geom_point() +
      scale_y_log10() +
      ggtitle(M3[[s]]$description) +
      scale_colour_manual(values = pal)
   
   svg(paste0("output/1-x13-comp-", s, ".svg"), 6, 3)
      print(p1)
   dev.off()
}

comp_x13_aa(335)
comp_x13_aa(334)
comp_x13_aa(332)
comp_x13_aa(49)
comp_x13_aa(171)


#-----------eg when X13 much worse than auto.arima eg 1------------
mases %>% arrange(-diff)
comp_x13_aa(73)
comp_x13_aa(64)
comp_x13_aa(1388)
comp_x13_aa(120)



plot(M3[[73]]$x)

aarima[73, ]
seats[73, ] # completely explosive

m <- seas(M3[[73]]$x, 
          regression.aictest = NULL, 
          regression.variables = c("const"),
          forecast.maxlead = 18)
# SEATS gives ARIMA(1,1,2), log transform


# auto.arima gives a simpler ARIMA(0,1,0) without the log transform
auto.arima(M3[[73]]$x)



#-----------------X13 much worse eg 2----------------
mases %>% arrange(-diff)

plot(M3[[1388]]$x)

aarima[1388, ]
x13[1388, ] # again, completely explosive

m <- seas(M3[[1388]]$x, forecast.maxlead = 18)
summary(m) # ARIMA(1,1,0)(1,1,0), log transform


# auto.arima gives ARIMA(1,1,0)(0,1,0) on original
auto.arima(M3[[1388]]$x)


