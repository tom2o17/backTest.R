## Environmental cleanup and loading required packages
rm(list=ls(all=T))
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#install.packages(tidyquant)
#install.packages(TTR)
#install.packages(ggplot2)

library(tidyquant)
library(TTR)
library(ggplot2)


## Overview:
# The following script backtests different trading methods there are three
# different trading methods which are backtested. 
# 1) RSI strategy
# 2) SMA strategy 
# 3) MACD strategy

# Values within each of the blocks of code relating to the above strategy
# can be altered to modify the strategy. 



# Setting up the table to store our results in
gross = data.frame()



## RSI strategy backtest with profit calculation loop. 

name = 'MKR-USD'   # The name of the asset that you would like to test 


## Setting up the price action data frame for analysis
tick = getSymbols(name, src = 'yahoo', auto.assign = F)

colnames(tick) = c('Open', 'High', 'Low', 'Close', 'Volume', 'adj')

summary(tick)
tick = na.omit(tick)



## Computing the various indicators based on the price action. 

# RSI
rsi = RSI(tick$Close)

# Macd value for said security 
value = MACD(tick$Close)
histogram = value$macd - value$signal
value = cbind(value, histogram)

summary(histogram)

# Computing the simple moving averages 
sma30 = SMA(tick$Close, 30)
sma100 = SMA(tick$Close, 100)


dates = index(tick)

tick = as.data.frame(tick)
tick = cbind(dates, tick)
tick = cbind(tick, rsi, value, sma30, sma100)

colnames(tick) = c('dates','Open', 'High', 'Low', 'Close',
                   'Volume', 'adj', 'rsi',
                   'macd', 'signal', 'histogram',
                   'sma30', 'sma100')


## Loop for testing the rsi strategy 30 ~ 70 
bought = F
sell = F

# This tests the RSI strategy for a give security
test = tick[100:nrow(tick),]
feed = data.frame()
results_rsi = data.frame()
feed = test

for (i in 1:nrow(test)) {
  if (feed[i,]$rsi <= 29 & bought != T) {
    print('buy signal triggered!')
    bought = T
    act = c('buy', feed[i,]$Close)
    results_rsi = rbind(results_rsi, act)
  }
  if (feed[i,]$rsi >= 71 & bought == T){
    print('sell signal triggered!')
    bought = F
    act = c('sell', feed[i,]$Close)
    results_rsi = rbind(results_rsi, act)
  }
  #print(i)
}


# This loop calculates the profit that would have been lost or made with the
# Strategy 

profit = data.frame()

hold = vector(length=0)
for (i in 1:nrow(results_rsi)) {
  if (results_rsi[i,1] == 'buy') {
    hold[1] = results_rsi[i,2]
  } else if (results_rsi[i,1]=='sell') {
    hold[2] = results_rsi[i,2]
  }
  if (length(hold) == 2) {
    pi = (as.numeric(hold[2]) - as.numeric(hold[1]))
    profit = rbind(profit, pi)
    hold = vector(length = 0)
  }
}
rsi = NA
rsi = sum(profit)


## --------------



##  Comparison to the SMA strategy 

feed = test[100:nrow(tick),]
results_sma = data.frame()
bought = F

# Notes: 
# Date not displaying as date

for (i in 2:nrow(test)) {
  
  if (bought != T) {
    #print(feed[i,]$sma30 >= feed[i,]$sma100 & bought != T)
    logic = feed[i,]$sma30 > feed[i,]$sma100 & feed[i-1,]$sma30 < feed[i-1,]$sma100
    if (is.na(logic) != T ){
      if (logic) {
        print('buy signal triggered')
        bought = T
        date = feed[i,]$dates
        act = c('buy', feed[i,]$Close)
        results_sma = rbind(results_sma, act)
      }
    }
    
  }
  if (bought) {
    logic2 = feed[i,]$sma30 < feed[i,]$sma100 & feed[i-1,]$sma30 > feed[i-1,]$sma100
    if (is.na(logic2) !=T) {
      if(logic2) {
        date = feed[i,]$dates
        print('Sell signal triggered')
        bought = F
        act = c('sell', feed[i,]$Close)
        results_sma = rbind(results_sma, act)
      }
    }
  }
  #print(tick[i,]$dates)
}



# Summing the profits from the SMA strategy 


profit = data.frame()
hold = vector(length=0)
for (i in 1:nrow(results_sma)) {
  if (results_sma[i,1] == 'buy') {
    hold[1] = results_sma[i,2]
  } else if (results_sma[i,1]=='sell') {
    hold[2] = results_sma[i,2]
  }
  if (length(hold) == 2) {
    pi = (as.numeric(hold[2]) - as.numeric(hold[1]))
    profit = rbind(profit, pi)
    hold = vector(length = 0)
  }
}
sma1 = NA
sma1 = sum(profit)






# Visualization tool
tick %>%
  ggplot(aes(x = dates)) +
  geom_line(aes(y = Close, color = 'Price')) +
  geom_line(aes(y = sma30, color = 'sma30'))+
  geom_line(aes(y = sma100, color = 'sma100'))

date(feed[i,]$dates)

view = tick[300:500,]

view %>%
  ggplot(aes(x = dates)) +
  geom_line(aes(y = macd, color = 'MACD')) +
  geom_line(aes(y = signal, color = 'Signal'))

plot(view$histogram)

#####

## MACD trading strategy testing

test = tick[40:nrow(tick),]

# Sell @ test$histogram[i-2] > test$histogram[i]

bought = F
results_mac = data.frame()
for (i in 2:nrow(test)) {
  if (test$histogram[i-1] < 0 & test$histogram[i] > 0) {
    if (bought == F) {
      bought = T
      print(paste('buy', test$Close[i]))
      act = c('buy', test$Close[i])
      results_mac = rbind(results_mac, act)
      }
  }
  if (bought) {
    if (test$histogram[i-1] > 0 & test$histogram[i] < 0) {
      bought = F
      print(paste('sell', test$Close[i]))
      act = c('sell', test$Close[i])
      results_mac = rbind(results_mac, act)
    }
  }
}



# Profit calculator

profit = data.frame()

hold = vector(length=0)
for (i in 1:nrow(results_mac)) {
  if (results_mac[i,1] == 'buy') {
    hold[1] = results_mac[i,2]
  } else if (results_mac[i,1]=='sell') {
    hold[2] = results_mac[i,2]
  }
  if (length(hold) == 2) {
    pi = (as.numeric(hold[2]) - as.numeric(hold[1]))
    profit = rbind(profit, pi)
    hold = vector(length = 0)
  }
}
mac = NA
mac = sum(profit)

rsi_profit = paste('rsi Strategy yeilded', rsi)
mac_profit = paste('macd strategy yeilded', mac)
start = tick$dates[1]
end = tick$dates[nrow(tick)]
print(rsi_profit)
print(mac_profit)
elapse =  end-start
info = c(name, rsi, mac, sma1, elapse)

gross = rbind(gross,info)
colnames(gross) = c('item', 'rsi yeild', 'macd yeild', 'sma yeild', 'elapsed')
gross












