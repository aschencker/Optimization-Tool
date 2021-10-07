library(glmnet)
library(NlcOptim)
library(lpSolve)
library(Rblpapi)
blpConnect()

Cash <- 11233.57
CashMax <- .0030
GoalCash <- .00251
SectorDiffMax <- .001
Sells <- c()
Buys <- c()
Date <- as.Date("2020-10-27")

Portfolio <- getPortfolio("U########-# client","portfolio_mposition",overrides = c(REFERENCE_DATE=gsub("-","",Date)))
NewHoldings <- cbind(rep(0,length(Buys)),Buys)
colnames(NewHoldings) <- colnames(Portfolio)
Portfolio <- rbind(Portfolio,NewHoldings)
Holdings <- Portfolio[,2]
SectorWeights <- bds("MIDL1 Index","INDX_GWEIGHT",overrides = c(END_DATE_OVERRIDE=gsub("-","",Date)))

PullDatesStep1 <- bdh("MDY US EQUITY","TOT_RETURN_INDEX_GROSS_DVDS",Date - 2000,Date)
PullDatesStep2 <- PullDatesStep1[seq(nrow(PullDatesStep1)-1260,nrow(PullDatesStep1),by = 21),]
MDY <- PullDatesStep2[-1,2]/PullDatesStep2[-61,2]-1
RF <- bdh("USGG1M INDEX","PX_LAST",Date - 2000,Date)
dates <- PullDatesStep2[,1]
RF <- RF[RF[,1] %in% dates[-1],2]/1200

PriceList <- bdh(Holdings,"TOT_RETURN_INDEX_GROSS_DVDS",dates[1],dates[61])
PriceMatrix <- matrix(NA,61,length(PriceList))

for(i in 1:length(PriceList)){
  Usable <- PriceList[[i]][which(PriceList[[i]][,"date"] %in% dates),]
  Rows <- which(dates %in% Usable[,"date"] == TRUE)
  PriceMatrix[Rows,i] <- Usable[,2]
}

ReturnMatrix <- PriceMatrix[-1,]/PriceMatrix[-61,]-1
colnames(ReturnMatrix) <- names(PriceList)

Sectors <- bdp(names(PriceList),"GICS_SECTOR_NAME")

HoldingsWithNA <- names(which(apply(ReturnMatrix,2,anyNA)))

if(length(HoldingsWithNA) > 0){
  
  SytheticIF <- function(x){
    if(x == "Consumer Discretionary"){
      return("S4COND US EQUITY")
    }
    if(x == "Consumer Staples"){
      return("S4CONS US EQUITY")
    }
    if(x == "Energy"){
      return("S4ENRS US EQUITY")
    }
    if(x == "Financials"){
      return("S4FINL US EQUITY")
    }
    if(x == "Health Care"){
      return("S4HLTH US EQUITY")
    }
    if(x == "Industrials"){
      return("S4INDU US EQUITY")
    }
    if(x == "Information Technology"){
      return("S4INFT US EQUITY")
    }
    if(x == "Materials"){
      return("S4MATR US EQUITY")
    }
    if(x == "Real Estate"){
      return("S4RLST US EQUITY")
    }
    if(x == "Communication Services"){
      return("S4TELS US EQUITY")
    }
    if(x == "Utilities"){
      return("S4UTIL US EQUITY")
    }
  }
  
  SytheticBench <- rbind(HoldingsWithNA,sapply(Sectors[HoldingsWithNA,1],SytheticIF))
  
  StyleFactors <- c("PVALUEUS INDEX","PVOLAUS INDEX","PLEVERUS INDEX","PDIVYUS INDEX",
                    "PTRADEUS INDEX","PSIZEUS INDEX","PEARNVUS INDEX",
                    "PGRWTHUS INDEX","PPROFTUS INDEX","PMOMENUS INDEX")
  
  FactorPriceList <- bdh(StyleFactors,"PX_LAST",Date - 2000,Date)
  FactorPriceMatrix <- NULL
  for(i in 1:length(FactorPriceList)){
    FactorPriceMatrix <- cbind(FactorPriceMatrix,FactorPriceList[[i]][,"PX_LAST"])
  }
  FactorPriceMatrix <- FactorPriceMatrix[which(FactorPriceList[[1]][,1] %in% dates),]
  FactorReturnMatrix <- FactorPriceMatrix[-1,]/FactorPriceMatrix[-61,] - 1 
  colnames(FactorReturnMatrix) <- names(FactorPriceList)
  
  Benchmarks <- c("S4COND US EQUITY","S4CONS US EQUITY","S4ENRS US EQUITY",
                  "S4HLTH US EQUITY","S4FINL US EQUITY","S4INDU US EQUITY",
                  "S4INFT US EQUITY","S4MATR US EQUITY","S4RLST US EQUITY",
                  "S4TELS US EQUITY","S4UTIL US EQUITY")
  
  BenchmarkPriceList <- bdh(Benchmarks,"TOT_RETURN_INDEX_GROSS_DVDS",Date - 2000,Date)
  BenchPriceMatrix <- NULL
  for(i in 1:length(BenchmarkPriceList)){
    Data <- BenchmarkPriceList[[i]]
    Row <- which(Data[,1] %in% dates)
    BenchPriceMatrix <- cbind(BenchPriceMatrix,Data[Row,"TOT_RETURN_INDEX_GROSS_DVDS"])
  }
  BenchReturnMatrix <- BenchPriceMatrix[-1,]/BenchPriceMatrix[-61,]-1
  colnames(BenchReturnMatrix) <- names(BenchmarkPriceList)
  
  Peerlist <- list()
  for(i in 1:length(HoldingsWithNA)){
    Peerlist[[i]] <- bds(HoldingsWithNA[i],"BLOOMBERG_PEERS")
  }
  
  PeerReturnList <- list()
  for(i in 1:length(Peerlist)){
    Rets <- NULL
    for(j in 1:nrow(Peerlist[[i]])){
      TK <- paste(Peerlist[[i]][j,1]," EQUITY",sep = "")
      Pr <- bdh(TK,"TOT_RETURN_INDEX_GROSS_DVDS",Date - 2000,Date)
      Rows <- which(Pr[,1] %in% dates)
      if(length(Rows) < 61){
        next
      }else{
        Rets <- cbind(Rets,Pr[Rows[-1],2]/Pr[Rows[-61],2]-1)
      }
    }
    PeerReturnList[[i]] <- Rets
  }
  
  Xs <- list()
  for(i in 1:length(PeerReturnList)){
    Xs[[i]] <- cbind(BenchReturnMatrix[,SytheticBench[2,i]],FactorReturnMatrix,PeerReturnList[[i]])
  }
  
  lambda_seq <- seq(.005,.5, by = .0025)
  for(i in 1:length(Xs)){
    NArows <- which(is.na(ReturnMatrix[,HoldingsWithNA[i]]))
    KnowRows <- c(1:60)[-NArows]
    cv_output <- cv.glmnet(Xs[[i]][KnowRows,],ReturnMatrix[KnowRows,HoldingsWithNA[i]],alpha = 1, lambda = lambda_seq)
    best_lambda <- cv_output$lambda.min
    lasso_best <- glmnet(Xs[[i]][KnowRows,],ReturnMatrix[KnowRows,HoldingsWithNA[i]],alpha = 1, lambda = best_lambda)
    pred <- predict(lasso_best, s = best_lambda, newx = Xs[[i]][NArows,])
    ReturnMatrix[NArows,HoldingsWithNA[i]] <- pred
  }
}

WhatSector <- function(x){
  if(x == "Consumer Discretionary"){
    return("S4COND")
  }
  if(x == "Consumer Staples"){
    return("S4CONS")
  }
  if(x == "Energy"){
    return("S4ENRS")
  }
  if(x == "Financials"){
    return("S4FINL")
  }
  if(x == "Health Care"){
    return("S4HLTH")
  }
  if(x == "Industrials"){
    return("S4INDU")
  }
  if(x == "Information Technology"){
    return("S4INFT")
  }
  if(x == "Materials"){
    return("S4MATR")
  }
  if(x == "Real Estate"){
    return("S4RLST")
  }
  if(x == "Communication Services"){
    return("S4TELS")
  }
  if(x == "Utilities"){
    return("S4UTIL")
  }
}

ReturnMatrix <- ReturnMatrix[,!colnames(ReturnMatrix) %in% Sells]

RM <- matrix(NA,nrow(ReturnMatrix),ncol(ReturnMatrix))
for(i in 1:ncol(ReturnMatrix)){
  RM[,i] <- ReturnMatrix[,which(colnames(ReturnMatrix) == sort(colnames(ReturnMatrix))[i])]
}
colnames(RM) <- sort(colnames(ReturnMatrix))
ReturnMatrix <- RM

MKT <- MDY-RF
LMs <- lm((ReturnMatrix-RF)~MKT)
Alphas <- -as.numeric(coef(LMs)[1,])
Omega <- (cov(resid(LMs))*59)

Sectors <- bdp(colnames(ReturnMatrix),"GICS_SECTOR_NAME")
Sectors2 <- sapply(unlist(Sectors),WhatSector)

Cond <- which(Sectors2 %in% "S4COND")
Cons <- which(Sectors2 %in% "S4CONS")
Enrs <- which(Sectors2 %in% "S4ENRS")
Finl <- which(Sectors2 %in% "S4FINL")
Hlth <- which(Sectors2 %in% "S4HLTH")
Indu <- which(Sectors2 %in% "S4INDU")
Inft <- which(Sectors2 %in% "S4INFT")
Matr <- which(Sectors2 %in% "S4MATR")
Rlst <- which(Sectors2 %in% "S4RLST")
Tels <- which(Sectors2 %in% "S4TELS")
Util <- which(Sectors2 %in% "S4UTIL")

BoxFun <- function(){
  BenchWeights <- SectorWeights[,2]/c(length(Cond),length(Cons),length(Enrs),
                                      length(Finl),length(Hlth),length(Indu),
                                      length(Inft),length(Matr),length(Rlst),
                                      length(Tels),length(Util))/100
  
  UB <- matrix(NA,1,ncol(ReturnMatrix))
  colnames(UB) <- Sectors2
  LB <- UB
  
  GoodWeight <- which(BenchWeights < .035 & BenchWeights > .01)
  GoodSectors <- SectorWeights[GoodWeight,1]
  UB[,colnames(UB) %in% GoodSectors] <- .035
  LB[,colnames(LB) %in% GoodSectors] <- .01
  
  BadSectors <- SectorWeights[-GoodWeight,1]
  BadSectorWeight <- BenchWeights[-GoodWeight]
  
  if(length(BadSectorWeight) > 0){
    for(i in 1:length(BadSectorWeight)){
      UB[,colnames(UB) %in% BadSectors[i]] <- BadSectorWeight[i] + .0001
      LB[,colnames(LB) %in% BadSectors[i]] <- BadSectorWeight[i] - .0001
    }
  }
  return(rbind(LB,UB))
}
Box <- BoxFun()

ObjFun <- function(x){
  return((Alphas %*% x)/sqrt(t(x) %*% Omega %*% x / 58))
}

x <- rep(1/ncol(ReturnMatrix),ncol(ReturnMatrix))

ConFun <- function(x){
  f <- NULL
  f <- rbind(f,sum(as.numeric(x)[Cond])-SectorWeights[1,2]/100)
  f <- rbind(f,sum(as.numeric(x)[Cons])-SectorWeights[2,2]/100)
  f <- rbind(f,sum(as.numeric(x)[Enrs])-SectorWeights[3,2]/100)
  f <- rbind(f,sum(as.numeric(x)[Finl])-SectorWeights[4,2]/100)
  f <- rbind(f,sum(as.numeric(x)[Hlth])-SectorWeights[5,2]/100)
  f <- rbind(f,sum(as.numeric(x)[Indu])-SectorWeights[6,2]/100)
  f <- rbind(f,sum(as.numeric(x)[Inft])-SectorWeights[7,2]/100)
  f <- rbind(f,sum(as.numeric(x)[Matr])-SectorWeights[8,2]/100)
  f <- rbind(f,sum(as.numeric(x)[Rlst])-SectorWeights[9,2]/100)
  f <- rbind(f,sum(as.numeric(x)[Tels])-SectorWeights[10,2]/100)
  f <- rbind(f,sum(as.numeric(x)[Util])-SectorWeights[11,2]/100)
  return(list(ceq=f,c=NULL))
}

lb = Box[1,]
ub = Box[2,]

Weights <- solnl(x,ObjFun,ConFun,lb = lb, ub = ub)
Weights <- Weights[["par"]]*(1-GoalCash)
Weights <- as.matrix(Weights)
rownames(Weights) <- colnames(ReturnMatrix)

Portfolio <- cbind(Portfolio,rep(0,nrow(Portfolio)),rep(0,nrow(Portfolio)))

for(i in 1:nrow(Weights)){
  Portfolio[which(rownames(Weights)[i] == Portfolio[,2]),3] <- Weights[i]
}

CurrentPrice <- bdh(as.vector(Portfolio[,2]),"PX_LAST",start.date = Date, end.date = Date)

for(i in 1:length(CurrentPrice)){
  Portfolio[which(names(CurrentPrice)[i] == Portfolio[,2]),4] <- as.numeric(CurrentPrice[[i]])[2]
}

PortfolioValue <- as.numeric(Portfolio[,1]) %*% as.numeric(Portfolio[,4]) + Cash
CurrentWeights <- as.numeric(Portfolio[,1])*as.numeric(Portfolio[,4])/as.numeric(PortfolioValue)
Portfolio <- cbind(Portfolio,CurrentWeights)

Cashrow <- cbind(Cash,"Cash",GoalCash,1,Cash/PortfolioValue)
colnames(Cashrow) <- colnames(Portfolio)
Portfolio <- rbind(Portfolio,Cashrow)

GoalCashAmount <- round(as.numeric(Portfolio[,3])*as.numeric(PortfolioValue),1)
CurrentCashAmount <- round(as.numeric(Portfolio[,5])*as.numeric(PortfolioValue),1)
CashDiff <- GoalCashAmount - CurrentCashAmount
CashPer10 <- c(as.numeric(Portfolio[-nrow(Portfolio),4])*10,1)
CashUpperBound <- GoalCashAmount+CashPer10+min(CashPer10)
CashLowerBound <- GoalCashAmount-CashPer10-min(CashPer10)
CashUpperBound[length(CashUpperBound)] <- as.numeric(PortfolioValue * CashMax)
CashUpperBound[which(Portfolio[,2] %in% Sells)] <- 0
CashLowerBound[which(Portfolio[,2] %in% Sells)] <- 0

Sectors3 <- bdp(Portfolio[-nrow(Portfolio),2],"GICS_SECTOR_NAME")
Sectors3 <- sapply(unlist(Sectors3),WhatSector)

Cond <- which(Sectors3 %in% "S4COND")
Cons <- which(Sectors3 %in% "S4CONS")
Enrs <- which(Sectors3 %in% "S4ENRS")
Finl <- which(Sectors3 %in% "S4FINL")
Hlth <- which(Sectors3 %in% "S4HLTH")
Indu <- which(Sectors3 %in% "S4INDU")
Inft <- which(Sectors3 %in% "S4INFT")
Matr <- which(Sectors3 %in% "S4MATR")
Rlst <- which(Sectors3 %in% "S4RLST")
Tels <- which(Sectors3 %in% "S4TELS")
Util <- which(Sectors3 %in% "S4UTIL")

CondCash <- sum(GoalCashAmount[Cond])
ConsCash <- sum(GoalCashAmount[Cons])
EnrsCash <- sum(GoalCashAmount[Enrs])
FinlCash <- sum(GoalCashAmount[Finl])
HlthCash <- sum(GoalCashAmount[Hlth])
InduCash <- sum(GoalCashAmount[Indu])
InftCash <- sum(GoalCashAmount[Inft])
MatrCash <- sum(GoalCashAmount[Matr])
RlstCash <- sum(GoalCashAmount[Rlst])
TelsCash <- sum(GoalCashAmount[Tels])
UtilCash <- sum(GoalCashAmount[Util])

SectorCash <- c(CondCash,ConsCash,EnrsCash,FinlCash,HlthCash,InduCash,InftCash,MatrCash,RlstCash,TelsCash,UtilCash)
SectorMax <- as.numeric(PortfolioValue) * (SectorWeights[,2]/100*(1-GoalCash)+SectorDiffMax)
SectorMin <- as.numeric(PortfolioValue) * (SectorWeights[,2]/100*(1-GoalCash)-SectorDiffMax)

SectorMinMaxCon <- matrix(0,22,nrow(Portfolio))
SectorMinMaxCon[c(1,12),Cond] <- 1
SectorMinMaxCon[c(2,13),Cons] <- 1
SectorMinMaxCon[c(3,14),Enrs] <- 1
SectorMinMaxCon[c(4,15),Finl] <- 1
SectorMinMaxCon[c(5,16),Hlth] <- 1
SectorMinMaxCon[c(6,17),Indu] <- 1
SectorMinMaxCon[c(7,18),Inft] <- 1
SectorMinMaxCon[c(8,19),Matr] <- 1
SectorMinMaxCon[c(9,20),Rlst] <- 1
SectorMinMaxCon[c(10,21),Tels] <- 1
SectorMinMaxCon[c(11,22),Util] <- 1

SectorBounds <- sweep(SectorMinMaxCon,2,CashPer10,"*")

ConMat <- rbind(diag(nrow(Portfolio))*CashPer10,diag(nrow(Portfolio))*CashPer10,SectorBounds,CashPer10,CashPer10)
ConDir <- c(rep(">=",nrow(Portfolio)),rep("<=",nrow(Portfolio)),rep(">=",11),rep("<=",11),">=","<=")
ConRhs <- c(CashLowerBound,CashUpperBound,SectorMin,SectorMax,round(PortfolioValue,0)-10,round(PortfolioValue,0)+10)

NewShares <- lp(objective.in = rep(1,nrow(Portfolio)),const.mat = ConMat,const.dir = ConDir,const.rhs = ConRhs, all.int = TRUE) 
NewShares <- c(rep(10,nrow(Portfolio)-1),1) * NewShares$solution

Start_Position <- as.numeric(Portfolio[,1])
Security <- as.vector(Portfolio[,2])
Goal_Weight <- as.numeric(Portfolio[,3])
Price <- as.numeric(Portfolio[,4])
Current_Weight <- as.numeric(Portfolio[,5])
End_Weight <- (as.numeric(Price) * NewShares)/sum((as.numeric(Price) * NewShares))
End_Position <- NewShares
Buy_Sell_Shares <- NewShares - Start_Position
Ticker <- c(substr(Security[-length(Security)],1,nchar(Security)-10),"Cash")
Trade_Value <- Buy_Sell_Shares * Price

CondWeight <- sum(End_Weight[Cond])
ConsWeight <- sum(End_Weight[Cons])
EnrsWeight <- sum(End_Weight[Enrs])
FinlWeight <- sum(End_Weight[Finl])
HlthWeight <- sum(End_Weight[Hlth])
InduWeight <- sum(End_Weight[Indu])
InftWeight <- sum(End_Weight[Inft])
MatrWeight <- sum(End_Weight[Matr])
RlstWeight <- sum(End_Weight[Rlst])
TelsWeight <- sum(End_Weight[Tels])
UtilWeight <- sum(End_Weight[Util])

BuySell <- function(x){
  if(x > 0){
    return("BUY")
  }
  if(x < 0){
    return("SELL")
  }
  if(x == 0){
    return("NO TRADE")
  }
}

SectorWeights <- cbind(SectorWeights[,1],as.numeric(SectorWeights[,2]*100))
PortfolioSectorWeights <- rbind(CondWeight,ConsWeight,EnrsWeight,FinlWeight,HlthWeight,InduWeight,InftWeight,MatrWeight,RlstWeight,TelsWeight,UtilWeight)*10000
Diffrence <- PortfolioSectorWeights-as.numeric(SectorWeights[,2])
SectorWeights <- cbind(as.data.frame(SectorWeights),PortfolioSectorWeights,Diffrence)
NAs <- matrix(NA,length(Trade_Value)-11,4)
colnames(SectorWeights) <- c("Sector","Sector Weights","Porftolio Weights","Difference")
colnames(NAs) <- colnames(SectorWeights)
SectorWeights <- rbind(SectorWeights,NAs)
BuyOrSell <- sapply(Buy_Sell_Shares,BuySell)
BuyOrSell[length(BuyOrSell)] <- "NO TRADE"
Sectors <- t(cbind(t(as.vector(Sectors3)),"Cash"))
Trade_File <- cbind(Security,Sectors,Start_Position,Price,Current_Weight,Goal_Weight,End_Weight,Ticker,BuyOrSell,Buy_Sell_Shares,Trade_Value,Security,End_Position,SectorWeights)
Trade_File <- cbind(rbind(Trade_File[order(Trade_File[-nrow(Trade_File),1]),-c(14:17)],Trade_File[nrow(Trade_File),-c(14:17)]),Trade_File[,14:17])
rownames(Trade_File) <- c(1:nrow(Trade_File))

setwd("C:/Users/SIU854220350/Desktop")
write.csv(Trade_File,file = "Trade_File.csv")

