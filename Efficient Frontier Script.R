# Set Up
library(dplyr)
library(TTR)
library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.quadprog)
library(ROI.plugin.glpk)
library(jsonlite)
library(curl)
library(psych)
library(corrplot) # For alternative correlation plotting
library(ggplot2)
library(googleVis)
options(scipen=999)

library(optimx)
library(stringr)
library(Hmisc) # For alternative correlation plotting
library(quadprog)




#Establish data range and asset names
fromDate <- as.Date("2014-09-22") #22-Sep is a Monday, to get weekly returns for week ending 26-Sep (in first row)
toDate <- as.Date("2016-02-19") #Original: 2016-02-19 (Friday)
assets <- c("URTH","EEM", "GOVT", "IGOV", "EMB", "GTIP", "CORP", "GLD", "USO", "RWO", "QAI")
fx <- c("USD/EUR","USD/JPY", "USD/BTC") #Currencies have to be stated in this format, with USD as the base


# Get weekly returns of the asset classes

# Get raw price data (Yahoo Finance)

# for (a in assets) {
#     prices <- getSymbols(a, src = "yahoo",from = fromDate, to = toDate, auto.assign = FALSE)
#     adj.prices <- prices[,6]
#     colnames(adj.prices)[1] <- a
#     
# }

for (a in assets) {
    getSymbols(a, src = "yahoo",from = fromDate, to = toDate)
}

# Remove necessary columns
URTH <- URTH$URTH.Adjusted
EEM <- EEM$EEM.Adjusted
GOVT <- GOVT$GOVT.Adjusted
IGOV <- IGOV$IGOV.Adjusted
EMB <- EMB$EMB.Adjusted
GTIP <- GTIP$GTIP.Adjusted
CORP <- CORP$CORP.Adjusted
GLD <- GLD$GLD.Adjusted
USO <- USO$USO.Adjusted
RWO <- RWO$RWO.Adjusted
QAI <- QAI$QAI.Adjusted

# Get FX price data (Onanda) REMEMBER: TO SWAP JPY CURRENCY
for (f in fx){
    getFX(f,from=fromDate, to=toDate)
}

# Remove weekends from FX data
USDEUR <- USDEUR[.indexwday(USDEUR) %in% 1:5]
USDJPY <- USDJPY[.indexwday(USDJPY) %in% 1:5]
USDBTC <- USDBTC[.indexwday(USDBTC) %in% 1:5]

# Invert values of the FX
EURUSD <- 1/USDEUR
JPYUSD <- 1/USDJPY
BTCUSD <- 1/USDBTC

#########################################################
# # FX Processing
# # Remove / from the name, so that you can call the variables
# fx <- str_replace_all(fx, "[^[:alnum:]]", "")
# 
# print(assets)
# print(fx)
# 
# # Invert the currency pair string
# tempNewCurr <- swapFxName("USDJPY")
# print(tempNewCurr)
# 
# # Invert the values of currency pair
# testV <- c(2,2,2)
# results<- 1 / testV
# print(results)
#########################################################


# Run weekly
URTH.week <- to.weekly(URTH)
EEM.week <- to.weekly(EEM)
GOVT.week <- to.weekly(GOVT)
IGOV.week <- to.weekly(IGOV)
EMB.week <- to.weekly(EMB)
GTIP.week <- to.weekly(GTIP)
CORP.week <- to.weekly(CORP)
GLD.week <- to.weekly(GLD)
USO.week <- to.weekly(USO)
RWO.week <- to.weekly(RWO)
QAI.week <- to.weekly(QAI)

EURUSD.week <- to.weekly(EURUSD)
JPYUSD.week <- to.weekly(JPYUSD)
BTCUSD.week <- to.weekly(BTCUSD)

# Rename headers
rename_headers <- function(table){
    colnames(table) <- c("Open","High","Low","Close")
    return(table)
}

URTH.week <- rename_headers(URTH.week)
EEM.week <- rename_headers(EEM.week)
GOVT.week <- rename_headers(GOVT.week)
IGOV.week <- rename_headers(IGOV.week)
EMB.week <- rename_headers(EMB.week)
GTIP.week <- rename_headers(GTIP.week)
CORP.week <- rename_headers(CORP.week)
GLD.week <- rename_headers(GLD.week)
USO.week <- rename_headers(USO.week)
RWO.week <- rename_headers(RWO.week)
QAI.week <- rename_headers(QAI.week)

EURUSD.week <- rename_headers(EURUSD.week)
JPYUSD.week <- rename_headers(JPYUSD.week)
BTCUSD.week <- rename_headers(BTCUSD.week)

#Just get standard date column
dateCol <- data.frame(week = index(URTH.week), row.names=NULL)

#function to add new column and calculate weekly return; returns xts object
calc_weekReturn <- function(table){
    table$weekReturn <- (table$Close - table$Open)/table$Open
    result <- table$weekReturn
    result <- data.frame(result, row.names=NULL)
    result <- cbind(dateCol, result)
    result <-  xts(result, order.by=result[,1])
    result$week <- NULL
    result
}


# Add new column for Weekly return 
URTH.week2 <- calc_weekReturn(URTH.week)
EEM.week2 <- calc_weekReturn(EEM.week)
GOVT.week2 <- calc_weekReturn(GOVT.week)
IGOV.week2 <- calc_weekReturn(IGOV.week)
EMB.week2 <- calc_weekReturn(EMB.week)
GTIP.week2 <- calc_weekReturn(GTIP.week)
CORP.week2 <- calc_weekReturn(CORP.week)
GLD.week2 <- calc_weekReturn(GLD.week)
USO.week2 <- calc_weekReturn(USO.week)
RWO.week2 <- calc_weekReturn(RWO.week)
QAI.week2 <- calc_weekReturn(QAI.week)

EURUSD.week2 <- calc_weekReturn(EURUSD.week)
JPYUSD.week2 <- calc_weekReturn(JPYUSD.week)
BTCUSD.week2 <- calc_weekReturn(BTCUSD.week)


#ISSUE: Need to find better way to combine multiple xts objects together
all.data <- cbind(cbind(cbind(cbind(cbind(cbind(cbind(cbind(cbind(cbind(cbind(cbind(cbind(URTH.week2, EEM.week2),GOVT.week2),IGOV.week2),EMB.week2),GTIP.week2),CORP.week2),GLD.week2),USO.week2),RWO.week2),QAI.week2),EURUSD.week2),JPYUSD.week2),BTCUSD.week2)
colnames(all.data) <- c("URTH","EEM","GOVT","IGOV","EMB","GTIP","CORP","GLD","USO","RWO","QAI","EURUSD","JPYUSD","BTCUSD")
anyNA(all.data) #To check if any missing values
#ISSUE: Need to figure out why it's not numeric in the first place; without converting, unable to use optimize.portfolio()
storage.mode(all.data) <- "numeric"

all.data_nobtc <- all.data[,1:13] #HARDCODED: Subset only first 13 columns, to exclude bitcoin timeseries

# -------- Time Series --------

#Bitcoin price trend
BTCUSD <- getFX("BTC/USD",from=fromDate, to=toDate, auto.assign = FALSE)
btcusd.df <- data.frame(Date=index(BTCUSD), coredata(BTCUSD)) 
ggplot(btcusd.df, aes(Date, BTC.USD)) + geom_line() +
    scale_x_date() + xlab("") + ylab("Price")

Line <- gvisLineChart(btcusd.df,options=list(height=600))
plot(Line)

# -------- Descriptive Stats --------

desStats <- tibble::rownames_to_column(describe(all.data), "Assets")

#Function to cross out insig correlations in corrplot
cor.mtest <- function(mat, conf.level = 0.95){
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
    diag(p.mat) <- 0
    diag(lowCI.mat) <- diag(uppCI.mat) <- 1
    for(i in 1:(n-1)){
        for(j in (i+1):n){
            tmp <- cor.test(mat[,i], mat[,j], conf.level = conf.level)
            p.mat[i,j] <- p.mat[j,i] <- tmp$p.value
            lowCI.mat[i,j] <- lowCI.mat[j,i] <- tmp$conf.int[1]
            uppCI.mat[i,j] <- uppCI.mat[j,i] <- tmp$conf.int[2]
        }
    }
    return(list(p.mat, lowCI.mat, uppCI.mat))
}

M <- cor(all.data)
res1 <- cor.mtest(all.data,0.95)
corrplot(M, method='number', type='lower', p.mat = res1[[1]], sig.level=0.05)


# -------- MAS APIs --------
fromMonth <- format(fromDate, "%Y-%m")
toMonth <- format(toDate, "%Y-%m")

url <- paste("https://eservices.mas.gov.sg/api/action/datastore/search.json?resource_id=5f2b18a8-0883-4769-a635-879c63d3caac&sort=end_of_month%20desc&between[end_of_month]=", fromMonth, ",", toMonth, sep="")
callResult <- fromJSON(url)
callResult.df <- data.frame(callResult$result$records)

avgRate <- mean(as.numeric(callResult.df$banks_fixed_deposits_3m)) / 100
riskFree <- ((1+avgRate)^(1/52))-1 #period rate (week)


# -------- Portfolio Analytics --------

init.portf_withbtc <- portfolio.spec(assets=colnames(all.data))
init.portf_withbtc <- add.objective(portfolio=init.portf_withbtc, type="return", name="mean")
init.portf_withbtc <- add.objective(portfolio=init.portf_withbtc, type="risk", name="StdDev")
init.portf_withbtc <- add.constraint(portfolio=init.portf_withbtc, type="full_investment")
init.portf_withbtc <- add.constraint(portfolio=init.portf_withbtc, type="long_only")
init.portf_withbtc

# Maximizing Sharpe Ratio can be formulated as a quadratic programming problem and solved very quickly using optimize_method="ROI". 
# Although "StdDev" was specified as an objective, the quadratic programming problem uses the variance-covariance matrix in the objective function.

# The default action if "mean" and "StdDev" are specified as objectives with optimize_method="ROI" is to maximize quadratic utility. 
# If we want to maximize Sharpe Ratio, we need to pass in maxSR=TRUE to optimize.portfolio.

optimized.result_withbtc <- optimize.portfolio(R=all.data, portfolio=init.portf_withbtc, optimize_method="ROI", maxSR=TRUE, trace=TRUE)

weights_withbtc <- round(optimized.result_withbtc$weights,digits=3)
weights_withbtc2 <- data.frame(Weight=coredata(weights_withbtc)) 
weights_withbtc3 <- tibble::rownames_to_column(weights_withbtc2, "Assets")

ggplot(data=weights_withbtc3, aes(x=Assets, y=Weight, label=sprintf("%.0f%%",100*Weight))) +
    geom_bar(stat="identity") +
    scale_y_continuous(labels = scales::percent) +
    geom_text(size = 3, position = position_stack(vjust = 0.5), color="white")

portMean_withbtc <- unname(optimized.result_withbtc$objective_measures[[2]]) 
portSD_withbtc <- unname(optimized.result_withbtc$objective_measures[[1]])
sharpe_withbtc <- (portMean_withbtc - riskFree)/ portSD_withbtc
portMean_withbtc
portSD_withbtc
sharpe_withbtc

#-----

init.portf_nobtc <- portfolio.spec(assets=colnames(all.data_nobtc)) #Different data set
init.portf_nobtc <- add.objective(portfolio=init.portf_nobtc, type="return", name="mean")
init.portf_nobtc <- add.objective(portfolio=init.portf_nobtc, type="risk", name="StdDev")
init.portf_nobtc <- add.constraint(portfolio=init.portf_nobtc, type="full_investment")
init.portf_nobtc <- add.constraint(portfolio=init.portf_nobtc, type="long_only")
init.portf_nobtc

optimized.result_nobtc <- optimize.portfolio(R=all.data_nobtc, portfolio=init.portf_nobtc, optimize_method="ROI", maxSR=TRUE, trace=TRUE)
weights_nobtc <- round(optimized.result_nobtc$weights,digits=3)
weights_nobtc2 <- data.frame(Weight=coredata(weights_nobtc)) 
weights_nobtc3 <- tibble::rownames_to_column(weights_nobtc2, "Assets")

ggplot(data=weights_nobtc3, aes(x=Assets, y=Weight, label=sprintf("%.0f%%",100*Weight))) +
    geom_bar(stat="identity") +
    scale_y_continuous(labels = scales::percent) +
    geom_text(size = 3, position = position_stack(vjust = 0.5), color="white")


portMean_nobtc <- unname(optimized.result_nobtc$objective_measures[[2]]) 
portSD_nobtc <- unname(optimized.result_nobtc$objective_measures[[1]])
sharpe_nobtc <- (portMean_nobtc - riskFree)/portSD_nobtc
portMean_nobtc
portSD_nobtc 
sharpe_nobtc 

##################################################

# ------------

# 2) Optimization
# Get mean and SD for each asset class

means <- colMeans(all.data)

URTH.sd <- sd(URTH.data)
EEM.sd <- sd(EEM.data)
EURUSD.sd <- sd(EURUSD.data)
JPYUSD.sd <- sd(JPYUSD.data)

# Generate covariance matrix; need to convert to df first

assetCor <- cor(all.data)
chart.Correlation(all.data, histogram=TRUE) 
#Alternative Correlation analyses 
#corrplot(assetCor, method="number", type="lower")
#rcorr(as.matrix(all.data))

#Order: URTH, EURUSD
assetCov <- cov(all.data)

#Initialize weights
#initWeight <- 1/ncol(all.data)
#weights <- vector(mode="numeric", length=ncol(all.data)) #IMPORTANT: Position has to correspond to the asset
weights <- c(1,0,0,0,0,0,0,0,0,0,0,0,0,0)
totalWeights <- sum(weights)

portMean <- function(assetReturns, weights){
    pm <- as.numeric(crossprod(colMeans(assetReturns), weights))
    pm
}

portSD <- function(assetReturns, weights){
    temp1 <- weights %*% cov(assetReturns)
    temp2 <- temp1 %*% weights
    result <- sqrt(temp2) 
    result
}


# #Calculate portfolio mean
# portMean <- as.numeric(crossprod(means, weights))
# 
# #Calculate portfolio SD
# temp1 <- weights %*% assetCov
# temp2 <- temp1 %*% weights
# portSD <- sqrt(temp2) 

#Sharpe Ratio
sharpe <- portMean / portSD





#Function to caculate optimal portfolio weights given asset returns. Permutate risk parameter to generate efficient frontier. Constraints: sum of weights = 1 and no shorting
#riskParam <- 1
optimPortWeights <- function(assetReturns, riskParam){
    Dmat <- cov(assetReturns)
    dvec <- colMeans(assetReturns) * riskParam
    Amat <- cbind(1, diag(nrow(Dmat))) 
    bvec <- c(1, rep(0, nrow(Dmat))) 
    meq <- 1
    qp <- solve.QP(Dmat, dvec, Amat, bvec, meq)
    qp$solution
}

testWeights <- optimPortWeights(all.data, 0)
testMean <- portMean(all.data,testWeights)
testSD <- portSD(all.data, testWeights)
testSharpe <- testMean / testSD


weights <- qp$solution
totalWeights <- sum(weights)



#Calculate portfolio SD
temp1 <- weights %*% assetCov
temp2 <- temp1 %*% weights
portSD <- sqrt(temp2) 

sharpe <- portMean / portSD

# Generate series of means above and below anchor

# Re-run optimizations, taking note of the weights, mean, SD

# Plot

############### Functions ###############

# Takes in currency pair string and returns inverse string. Example: "USDJPY" -> "JPYUSD"
swapFxName <- function(s){
    oldCurr <- s
    firstCurr <- substr(oldCurr,1,3)
    secondCurr <- substr(oldCurr,4,6)
    newCurr <- paste(secondCurr, firstCurr, sep="")
}


# Quadratic programming for portfolio optimization
#Resource: http://www.wdiam.com/2012/06/10/mean-variance-portfolio-optimization-with-r-and-quadratic-programming/

# risk.param <- 1 #permutate to generate efficient frontier
# Dmat <- assetCov
# dvec <- colMeans(all.data) * risk.param
# # Amat <- matrix(1, nrow=nrow(Dmat)) #Sum of Weights Equal to One
# # bvec <- 1 #Sum of Weights Equal to One
# Amat <- cbind(1, diag(nrow(Dmat))) #Sum of Weights Equal to One AND No Shorting (weight >= 0)
# bvec <- c(1, rep(0, nrow(Dmat))) #Sum of Weights Equal to One AND No Shorting (weight >= 0)
# # Amat <- cbind(1, diag(nrow(Dmat)), -1*diag(nrow(Dmat))) #Sum of Weights Equal to One AND No Shorting (weight >= 0) AND no heavy concentration (weight <= 0.15)
# # bvec <- c(1, rep(0, nrow(Dmat)), rep(-0.15, nrow(Dmat))) #Sum of Weights Equal to One AND No Shorting (weight >= 0) AND no heavy concentration (weight <= 0.15)
# meq <- 1
# qp <- solve.QP(Dmat, dvec, Amat, bvec, meq)


z <- read.zoo(text = testAPI.df$end_of_month, FUN = as.yearmon)
month <- as.Date(as.yearmon(testAPI.df$end_of_month))

as.POSIXlt(paste("2009-12", days(Sys.Date()), sep="-"))
