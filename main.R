require("derivmkts")
require("RQuantLib")
require("ggplot2")
require("quantmod")

#Get Data
stock_index <- "GOOG"
start_date <- Sys.Date() - 1
end_date <- Sys.Date()
getSymbols(stock_index, verbose = TRUE, src = "yahoo", 
           from=start_date,to=end_date)

#Variables

#Expiration
EXP = as.Date("2022-02-04")

#Today's date
TODAY = Sys.Date()

#Dist to expiration
days2exp = as.numeric(EXP - TODAY)

#Stock price
STK = tail(GOOG[, c(4)], n = 1)
STK = coredata(STK)

#Strike price - AUTOMATE
STRIKE = 2665

#Risk free rate of return
getSymbols.FRED("DGS10", env = .GlobalEnv)
DGS10 = last(DGS10)/10/360
Rf = sum(rep(DGS10, days2exp))

#Dividend yield - AUTOMATE
DIV = 0.00

#Volatility - AUTOMATE VALUE
VOL = as.numeric(AmericanOptionImpliedVolatility(type= "call", value= 120, strike= STRIKE,
                                                 underlying = STK, dividendYield = DIV, 
                                                 riskFreeRate = Rf, maturity = days2exp/365, volatility = 0.5))

binomopt(s= STK, k= STRIKE, v=VOL, r= Rf, tt= 10/365, d= DIV, nstep= 1,
             putopt= FALSE, american= TRUE, returntrees= TRUE)

binomplot(s= STK, k= STRIKE, v=VOL, r= Rf, tt= 10/365, d= DIV, nstep= 1,
          putopt= FALSE, american= TRUE, plotarrows= TRUE, plotvalues= TRUE)
