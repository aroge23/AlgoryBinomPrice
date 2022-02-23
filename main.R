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
EXP = as.Date("2022-03-18")

#Today's date
TODAY = Sys.Date()

#Dist to expiration
days2exp = as.numeric(EXP - TODAY)

#Stock price
STK = tail(GOOG[, c(4)], n = 1)
STK = coredata(STK)

#Strike price - AUTOMATE
STRIKE = 2605

#Risk free rate of return
getSymbols.FRED("DGS10", env = .GlobalEnv)
DGS10 = last(DGS10)/10/360
Rf = sum(rep(DGS10, days2exp))

#Dividend yield - AUTOMATE
DIV = 0.00

#Volatility - AUTOMATE VALUE
VOL = as.numeric(AmericanOptionImpliedVolatility(type= "call", value= 95.93, strike= STRIKE,
                                                 underlying = STK, dividendYield = DIV, 
                                                 riskFreeRate = 0.1, maturity = days2exp/365, volatility = 0.5))

#greeks
greeks(bscall(s=STK, k=STRIKE, v=VOL, r=Rf, tt= 30/365, d=DIV), complete= TRUE)

binomopt(s= STK, k= STRIKE, v=VOL, r= Rf, tt= days2exp/365, d= DIV, nstep= 1,
             putopt= FALSE, american= TRUE, returntrees= TRUE)

binomplot(s= STK, k= STRIKE, v=VOL, r= Rf, tt= days2exp/365, d= DIV, nstep= 1,
          putopt= FALSE, american= TRUE, plotarrows= TRUE, plotvalues= TRUE)


#------------------------------------------------------------
DATES = seq.Date(from=as.Date("2021-10-16"), to=Sys.Date() + 30, by= "1 day")
NAxts = xts(rep(NA, length(DATES)), order.by= DATES)
DATES = DATES[options.expiry(NAxts)]

callStrat = function(expiry_dates, symbol, strikesAboveATM) {
  #pass in the expirations and extract options for ticker
  ops = lapply(as.list(2:length(expiry_dates)), function(ii){
    # assign open_date & next_expiry (1-month expirations)
    open_date = expiry_dates[ii - 1]
    next_expiry = expiry_dates[ii]
    
    #read in options
    op = 1#FUNCTION FOR STRIKE VS PRICE DATA
    
    #get the closest strike to the last traded stock price
    op$stkPrc2strike = as.numeric(op$stkClose) - as.numeric(op$strike)
    if(strikesAboveATM == 0) {
      #eliminate over the money calls
      op = subset(op, op$stkPrc2strike > 0)
      #which has the least difference to the stock close
      ATM = op[which.min(op$stkPrc2strike),]
    } else {
      #if strikesAboveATM is above max strikes available, last strike will be selected:
      if (strikesAboveATM > length(op$stkPrc2strike)) {
        strikesAboveATM = length(op$stkPrc2strike)
      }
      #find the OTM calls and select
      ATM = op[which(op$stkPrc2strike < 0)[strikesAboveATM],]
    }
    #extract desired columns
    ATM = ATM[, c("Date", "expiry", "days2Exp", "stkClose", "strike", "Mid")]
    colnames(ATM) = c("openDate", "expiry", "days2exp", "open_stkPrc", "strike", "call_premium")
    ATM
  })
  #combine options
  ops = rbindlist(ops, use.names= TRUE, fill= TRUE)
  #get the latest quote for the last expiration
  lastPrc = getQuote(symbol)$Last
  #add expiration prices to ops
  ops$prcAtExp = c(ops$open_stkPrc[2:nrow(ops)], lastPrc)
  #calculate the net premium received
  ops$stk2strike = (as.numeric(ops$prcAtExp) - as.numeric(ops$strike))
  #ops$netPremium = ifelse(ops$stk2strike > 0, as.numeric(ops$call_premium), as.numeric(ops$call_premium) + ops$stk2strike)
  ops$netPremium = ifelse(ops$stk2strike > 0, (as.numeric(ops$strike) - as.numeric(ops$open_stkPrc))
                          + as.numeric(ops$call_premium), as.numeric(ops$call_premium) + ops$stk2strike)
  
  #calculate returns
  ops$ccRet = round(ops$netPremium / ops$open_stkPrc, 4)
  ops$stkRet = round(ops$prcAtexp / ops$open_stkPrc - 1, 4)
  
  #return the results
  ops
}
