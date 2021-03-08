#' ---
#' title: "Econ436 Project 3, Commodity Response to Exchange Rate"
#' author: "Eric Weber"
#' date: "May 1, 2019"
#' fontsize: 12pt
#' geometry: margin=0.1in
#' ---

# output:
#   pdf_document:
#     fig_width: 8
#     fig_height: 5

#Notes on the data frame I made:
#doesn't include "PALLFNF" which is an aggregate index of all commodities
#changed "PFSHMEAL" to "PFISH" to match the description
#there were two "POILAPSP"s in original data, I took the one with more data

#indice 1: Long description
#indice 2: short name
#indice 3: category name
#indices 4 to 474: data from Jan-1980 to March-2019

#start===============================================================================
library(openxlsx)
library(vars)
library(tseries)  #for ADF
library(urca)     #for unit root test

setwd("~/R")

#COMMODITY DATA
comData <- read.xlsx("commodities_all.xlsx")
comData[2,51] <- "Crude Oil, WTI" #shorter name for West Texas Intermediate
#get the code-names of all commodities, minus the description column and xchg rate column
codeList <- names(comData)[-c(1,2)]

#generate time series plots of random commodities 
# par(mfrow=c(2,2))
# for (code in codeList) {
#   if (rnorm(1,0,1) > 1.1) {
#     code_ts <- ts(as.double(comData[(4):474,code]), start=c(1980, 1), frequency=12)
#     plot(code_ts, main=comData[2,code],ylab="Price")
#     if (readline("Press enter to continue, type 'end' to exit loop...") == "end") break
#   }
# }


#IRF DATA (pre-made, because the vars package takes so long to compute)
dfirf <- read.csv("Commod_IRF_Vectors.csv",check.names=FALSE)
dfirf <- dfirf[,-1]


#function for getting the log-difference of a data column, given the column's name
get_log_diff <- function(code) {
  vect <- as.double(comData[(4):474,code])
  n <- length(vect)
  vect <- (log(vect[2:n])-log(vect[1:(n-1)]))*100
  return(vect)
}

#convert a short-code-name of a commodity to its more readable long name
getLongNames <- function(names) {
  n <- length(names)
  for (i in 1:n) names[i] <- comData[2,names[i]]
  return(names)
}

#convert short-code-names of commodities to their category
getCategory <- function(names) {
  n <- length(names)
  for (i in 1:n) names[i] <- comData[3,names[i]]
  return(names)
}

#DataFrame IRFs=======================================================================
#Generating a data Frame of all relevant IRF vectors (and 95% bounds of those vectors).

#creating the table
#for each commodity store:
# TWEX on TWEX: irf.11$irf$TWEX, irf.11$Lower$TWEX, irf.11$Upper$TWEX
# COMMOD on TWEX: irf.21$irf$TWEX, irf.21$Lower$TWEX, irf.21$Upper$TWEX

#Codes: Ti, TL, TU... Ci, CL, CU

#WE DON'T NEED THIS CODE BECAUSE WE LOADED THE IRF VECTORS FROM CSV FILE
#THIS IS THE CODE TO GENERATE THAT CSV FILE

# tableCols <- c("start")
# for (code in codeList) {
#   tableCols <- c(tableCols, paste(code,"Ti"),paste(code,"TL"),paste(code,"TU"))
#   tableCols <- c(tableCols, paste(code,"Ci"),paste(code,"CL"),paste(code,"CU"))
# }
# tableCols <- tableCols[-1] #remove dummy
# 
# dfirf <- data.frame(matrix(ncol=length(tableCols), nrow=13)) 
# colnames(dfirf) <- tableCols
# 
# #for looping
# for (code in codeList) {
#   TWEX <- get_log_diff("XCHG")
#   COMMOD <- get_log_diff(code)
#   z = as.matrix(cbind(TWEX,COMMOD))  #create two columns
#   p.est <- as.numeric(VARselect(z)$selection[1])  #gets AIC estimated p-lag-value
#   var.1c <- VAR(z, p=p.est, type = "none")
#   
#   irf.11 = irf(var.1c, impulse = "TWEX", n.ahead=12, response="TWEX", boot=TRUE, cumulative=TRUE)
#   irf.21 = irf(var.1c, impulse = "TWEX", n.ahead=12, response="COMMOD", boot=TRUE, cumulative=TRUE)
#   
#   # TWEX on TWEX: 
#   dfirf[,paste(code,"Ti")] <- irf.11$irf$TWEX
#   dfirf[,paste(code,"TL")] <- irf.11$Lower$TWEX
#   dfirf[,paste(code,"TU")] <- irf.11$Upper$TWEX
#   # COMMOD on TWEX: 
#   dfirf[,paste(code,"Ci")] <- irf.21$irf$TWEX
#   dfirf[,paste(code,"CL")] <- irf.21$Lower$TWEX
#   dfirf[,paste(code,"CU")] <- irf.21$Upper$TWEX
#   
#   #print(code)
#   # if (readline("Press enter to continue, type 'end' to exit loop...") == "end") break
# }
# 
# #write.csv(dfirf,"Commod_IRF_Vectors.csv")


#DataFrame Dynamic Elasticities==========================================================
#Generating a data Frame for the dynamic elasticity vectors of all commodities 

#creating the table
#for each commodity store:
# Dynamic Elasticity: DYNELAST, DYNELAST_LOWER, DYNELAST_UPPER
#Codes: D, DL, DU

tableCols <- c("start")
for (code in codeList) {
  tableCols <- c(tableCols, paste(code,"D"),paste(code,"DL"),paste(code,"DU"))
}
tableCols <- tableCols[-1] #remove dummy
dfde <- data.frame(matrix(ncol=length(tableCols), nrow=13)) 
colnames(dfde) <- tableCols
#View(dfde)  #test

#for looping
for (code in codeList) {
  
  irfcom <- dfirf[,paste(code,"Ci")]
  irftwex <- dfirf[,paste(code,"Ti")]
  dynelas <- ((-1)*irfcom)/irftwex
  DyneElastLower <- ((-1)*(dfirf[,paste(code,"CL")]))/(dfirf[,paste(code,"TL")])
  DyneElastUpper <- ((-1)*(dfirf[,paste(code,"CU")]))/(dfirf[,paste(code,"TU")])
  
  #SINCE COMMOD RESPONSE IS LOWER THAN TWEX RESPONSE ALWAYS, "UPPER" AND "LOWER" SWITCH
  dfde[,paste(code,"D")] <- dynelas
  dfde[,paste(code,"DL")] <- DyneElastUpper
  dfde[,paste(code,"DU")] <- DyneElastLower
  
  #print(code)
  #if (readline("Press enter to continue, type 'end' to exit loop...") == "end") break
}

#write.csv(dfde,"Commod_DE_Vectors.csv")

#Data Frame For Rankings =============================================================
#and classification of the dynamic elasticities 

tableCols <- c("Avg DE","Avg Lower","Avg Upper","Classify","Test Result")
dfrnk <- data.frame(matrix(ncol=length(tableCols), nrow=length(codeList)), row.names=codeList)
colnames(dfrnk) <- tableCols
#View(dfrnk) #test

for (code in codeList) {
  
  dfrnk[code,"Avg DE"] <- signif(mean(dfde[,paste(code,"D")]),4)
  dfrnk[code,"Avg Lower"] <- signif(mean(dfde[,paste(code,"DL")]),4)
  dfrnk[code,"Avg Upper"] <- signif(mean(dfde[,paste(code,"DU")]),4)
  
  if (dfrnk[code,"Avg DE"] < (1-0.2)) {
    dfrnk[code,"Classify"] <- "UNDER"
  } else if (dfrnk[code,"Avg DE"] > (1+0.2)) {
    dfrnk[code,"Classify"] <- "OVER"
  } else {
    dfrnk[code,"Classify"] <- "SAME"
  }
  
  if (dfrnk[code,"Avg Upper"] < 1) {
    dfrnk[code,"Test Result"] <- "UNDER"
  } else if (dfrnk[code,"Avg Lower"] > 1) {
    dfrnk[code,"Test Result"] <- "OVER"
  } else {
    dfrnk[code,"Test Result"] <- "---"
  }
  #print(code)
}

#give it the long names so we can read it
row.names(dfrnk) <- getLongNames(codeList)
#sort descending (hence the minus)
#rank the lower bounds of the dynamic elasticities as ascending...
dfrnk <- dfrnk[order(-dfrnk[,"Avg DE"]),]

dfrnk

#write.csv(dfrnk,"Commod_Rnk_Table.csv")

#RANKINGS FOR SUBSET/CATEGORY =========================================================
#and classification of the dynamic elasticities 

codeListNew <- c("start")
for (code in codeList) { 
  if (getCategory(code) == "Ag Raw") codeListNew <- c(codeListNew,code) 
}
codeListNew <- codeListNew[-1]

tableCols <- c("Avg DE","Avg Lower","Avg Upper","Classify","Test Result")
dfrnk <- data.frame(matrix(ncol=length(tableCols), nrow=length(codeListNew)), row.names=codeListNew)
colnames(dfrnk) <- tableCols
#View(dfrnk) #test

for (code in codeListNew) {
  
  dfrnk[code,"Avg DE"] <- signif(mean(dfde[,paste(code,"D")]),4)
  dfrnk[code,"Avg Lower"] <- signif(mean(dfde[,paste(code,"DL")]),4)
  dfrnk[code,"Avg Upper"] <- signif(mean(dfde[,paste(code,"DU")]),4)
  
  if (dfrnk[code,"Avg DE"] < (1-0.2)) {
    dfrnk[code,"Classify"] <- "UNDER"
  } else if (dfrnk[code,"Avg DE"] > (1+0.2)) {
    dfrnk[code,"Classify"] <- "OVER"
  } else {
    dfrnk[code,"Classify"] <- "SAME"
  }
  
  if (dfrnk[code,"Avg Upper"] < 1) {
    dfrnk[code,"Test Result"] <- "UNDER"
  } else if (dfrnk[code,"Avg Lower"] > 1) {
    dfrnk[code,"Test Result"] <- "OVER"
  } else {
    dfrnk[code,"Test Result"] <- "---"
  }
  #print(code)
}

#give it the long names so we can read it
row.names(dfrnk) <- getLongNames(codeListNew)
#sort descending (hence the minus)
#rank the lower bounds of the dynamic elasticities as ascending...
dfrnk <- dfrnk[order(-dfrnk[,"Avg DE"]),]
dfrnk

#write.csv(dfrnk,"Commod_Rnk_Fuels.csv")
#write.csv(dfrnk,"Commod_Rnk_Metals.csv")
#write.csv(dfrnk,"Commod_Rnk_AgRaw.csv")

#IRF PLOTS=============================================================================
#PLOTS FOR: Commod and TWEX Response to Twex (IRFs, 1% change in XCHG Rate)

par(mfrow=c(2,2))
for (code in codeList) {

  #irf.11: twex response to twex
  irf.ti <- dfirf[,paste(code,"Ti")]
  irf.tl <- dfirf[,paste(code,"TL")]
  irf.tu <- dfirf[,paste(code,"TU")]
  
  #irf.21: commod resp to twex
  irf.ci <- dfirf[,paste(code,"Ci")]
  irf.cl <- dfirf[,paste(code,"CL")]
  irf.cu <- dfirf[,paste(code,"CU")]

  #plot(irf.11$irf$TWEX, main=paste("TWEX to TWEX:",comData[2,code]), type="l", ylab="TWEX", xlab="", ylim=c(1.44,3.31))
  plot(irf.ti, main=paste(comData[2,code],"and TWEX Response"), type="l", ylab="Response", xlab="", ylim=c(-5,3.31))
  lines(irf.tl, col="red", lty=2)
  lines(irf.tu, col="red", lty=2)
  text(13,irf.ti[13],labels="TWEX",adj=c(1,1))
  abline(h=0,col="red")

  lines(irf.ci, col="blue")
  lines(irf.cl, col="blue", lty=2)
  lines(irf.cu, col="blue", lty=2)
  text(13,irf.ci[13],labels=comData[2,code],adj=c(1,1),col="blue")

  #if (readline("Press enter to continue, type 'end' to exit loop...") == "end") break
}

#JUST IRFS OF COMMODS, NOT TWEX ========================================================
#just the IRFs of the commodities, not the 

par(mfrow=c(2,2))
for (code in codeList) {
  
  #irf.11: twex response to twex
  irf.ti <- dfirf[,paste(code,"Ti")]
  irf.tl <- dfirf[,paste(code,"TL")]
  irf.tu <- dfirf[,paste(code,"TU")]
  
  #irf.21: commod resp to twex
  irf.ci <- dfirf[,paste(code,"Ci")]
  irf.cl <- dfirf[,paste(code,"CL")]
  irf.cu <- dfirf[,paste(code,"CU")]
  
  #plot(irf.11$irf$TWEX, main=paste("TWEX to TWEX:",comData[2,code]), type="l", ylab="TWEX", xlab="", ylim=c(1.44,3.31))
  # plot(irf.ti, main=paste(comData[2,code],"and TWEX Response"), type="l", ylab="Response", xlab="", ylim=c(-5,3.31))
  # lines(irf.tl, col="red", lty=2)
  # lines(irf.tu, col="red", lty=2)
  # text(13,irf.ti[13],labels="TWEX",adj=c(1,1))
  
  #, ylim=c(-5,3.31)
  plot(irf.ci, col="blue",main=paste(comData[2,code],"Response"), type="l", 
       ylab="Response", xlab="", ylim=c(-5,0.1))
  lines(irf.cl, col="blue", lty=2)
  lines(irf.cu, col="blue", lty=2)
  #text(13,irf.ci[13],labels=comData[2,code],adj=c(1,1),col="blue")
  abline(h=0,col="red")
  
  #if (readline("Press enter to continue, type 'end' to exit loop...") == "end") break
}

#Dynamic Elasticity PLOTS==============================================================
#PLOTS FOR: Dynamic Elasticity

par(mfrow=c(2,2))
for (code in codeList) {

  dynelas <- dfde[,paste(code,"D")]
  DyneElastLower <- dfde[,paste(code,"DL")]
  DyneElastUpper <- dfde[,paste(code,"DU")]
  
  plot(dynelas, ylim=c(-0.43, 1.74), type="l", main=paste("Dynamic Elast:",comData[2,code]))
  lines(DyneElastLower, col="red", lty=2)  #lower 95% bound 
  lines(DyneElastUpper, col="red", lty=2)  #upper 95% bound
  abline(h=1, col="purple") #horizontal at 1.0... designation of over or under reaction
  #abline(h=mean(dynelas), col="green")  #mean of dynamic elasticity
  #abline(h=mean(DyneElastLower), col="green", lty=2) #mean of lower bound
  #abline(h=mean(DyneElastUpper), col="green", lty=2) #mean of upper bound
  
  #if (readline("Press enter to continue, type 'end' to exit loop...") == "end") break
}

count <- 0
for (i in 0:10) {
  count <- count+ ((i/10)*(1/10))
}
count

#end=================================================================