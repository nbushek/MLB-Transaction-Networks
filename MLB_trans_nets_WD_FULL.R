# MLB Collaboration Network

# library imports

library(TDA)
library(igraph)
library(tcltk2)
library(combinat)
library(sets)
library(ggplot2)
library(stats)



# data imports

# Pathname for Nate: 

transactions <- read.csv("~/Desktop/MLB_transactions/tran.csv")

winPct <- read.csv("~/Desktop/MLB_transactions/sportsRefMlbWinTotalByYear.csv")



# data cleaning

transactions <- subset(transactions, transactions$primary.date >= 19201101)
transactions <- subset(transactions, transactions$type == "T " | 
                         transactions$type == "C "| transactions$type == "P ") 

#3/1/23, removed type = P as this had NA to.team
#| transactions$type == "P ")

#The following reduces to only transactions between AL and NL teams in any combination. 

transactions <- subset(transactions, (transactions$to.league == "AL" & transactions$from.league == "AL") |
                         (transactions$to.league == "NL" & transactions$from.league == "NL") |
                         (transactions$to.league == "AL" & transactions$from.league == "NL") |
                         (transactions$to.league == "NL" & transactions$from.league == "AL"))

#The following iterates through the subsetted data and replaces team codes corresponding
#to the same franchise historically with the same constant code that corrresponds to the
#team's modern name.

for (i in 1:nrow(transactions)) {
  
  #To Team
  if (transactions$to.team[i] == "BSN") {
    transactions$to.team[i] <- "ATL"
  }
  if (transactions$to.team[i] == "SLA") {
    transactions$to.team[i] <- "BAL"
  }
  if (transactions$to.team[i] == "PHA") {
    transactions$to.team[i] <- "OAK"
  }
  if (transactions$to.team[i] == "NY1") {
    transactions$to.team[i] <- "SFN"
  }
  if (transactions$to.team[i] == "BRO") {
    transactions$to.team[i] <- "LAN"
  }
  if (transactions$to.team[i] == "WS1") {
    transactions$to.team[i] <- "MIN"
  }
  if (transactions$to.team[i] == "MLN") {
    transactions$to.team[i] <- "ATL"
  }
  if (transactions$to.team[i] == "LAA") {
    transactions$to.team[i] <- "ANA"
  }
  if (transactions$to.team[i] == "KC1") {
    transactions$to.team[i] <- "OAK"
  }
  if (transactions$to.team[i] == "SE1") {
    transactions$to.team[i] <- "MIL"
  }
  if (transactions$to.team[i] == "WS2") {
    transactions$to.team[i] <- "TEX"
  }
  if (transactions$to.team[i] == "CAL") {
    transactions$to.team[i] <- "ANA"
  }
  if (transactions$to.team[i] == "MON") {
    transactions$to.team[i] <- "WAS"
  }
  if (transactions$to.team[i] == "FLO") {
    transactions$to.team[i] <- "MIA"
  }
  
  #From Team
  if (transactions$from.team[i] == "BSN") {
    transactions$from.team[i] <- "ATL"
  }
  if (transactions$from.team[i] == "SLA") {
    transactions$from.team[i] <- "BAL"
  }
  if (transactions$from.team[i] == "PHA") {
    transactions$from.team[i] <- "OAK"
  }
  if (transactions$from.team[i] == "NY1") {
    transactions$from.team[i] <- "SFN"
  }
  if (transactions$from.team[i] == "BRO") {
    transactions$from.team[i] <- "LAN"
  }
  if (transactions$from.team[i] == "WS1") {
    transactions$from.team[i] <- "MIN"
  }
  if (transactions$from.team[i] == "MLN") {
    transactions$from.team[i] <- "ATL"
  }
  if (transactions$from.team[i] == "LAA") {
    transactions$from.team[i] <- "ANA"
  }
  if (transactions$from.team[i] == "KC1") {
    transactions$from.team[i] <- "OAK"
  }
  if (transactions$from.team[i] == "SE1") {
    transactions$from.team[i] <- "MIL"
  }
  if (transactions$from.team[i] == "WS2") {
    transactions$from.team[i] <- "TEX"
  }
  if (transactions$from.team[i] == "CAL") {
    transactions$from.team[i] <- "ANA"
  }
  if (transactions$from.team[i] == "MON") {
    transactions$from.team[i] <- "WAS"
  }
  if (transactions$from.team[i] == "FLO") {
    transactions$from.team[i] <- "MIA"
  }
}

AllTeams <- c(unique(c(as.character(transactions$from.team), as.character(transactions$to.team))))

winPct1 <- winPct[, c(TRUE, TRUE, colnames(winPct[ ,- c(1,2)]) %in% AllTeams)]


#Check

setdiff(AllTeams, colnames(winPct1[ ,- c(1,2)]))

setdiff(colnames(winPct1[ ,- c(1,2)]), AllTeams)

winPct2 <- winPct1[(winPct1$Year >= 1921) & (winPct1$Year <= 2021), ]

winLose0 <- winPct2

for (i in 1:nrow(winPct2)) {
  for (j in 3:ncol(winPct2)) {
    winPct2[i, j] <- winPct2[i, j] / winPct2[i, 2]
    if (is.na(winPct2[i, j])) {
      winLose0[i, j] <- NA
    }
    if (!is.na(winPct2[i, j])){

      #Change threshold to be a 'winner' below.

      if (winPct2[i, j] >= 0.52) {
        winLose0[i, j] <- "W"
      }
      else {
        winLose0[i, j] <- "L"
      }
    }
  }
}

winLose1 <- winLose0[, -2]

WLrow <- nrow(winLose1)*(ncol(winLose1)-1)

WLdf <- data.frame(matrix(0, nrow = WLrow, ncol = 3))

ind <- 1

WLteams <- colnames(winLose1[-1, ])

for (i in 1:nrow(winLose1)){
  for (j in 2:ncol(winLose1)){
    WLdf[ind, 1] <- winLose1[i, 1]
    WLdf[ind, 2] <- as.character(WLteams[j])
    WLdf[ind, 3] <- as.character(winLose1[i, j])

    ind <- ind + 1
  }
}

colnames(WLdf) <- c("Year", "Team", "Label")

WLdf <- WLdf[!is.na(WLdf[,3]), ]
# 
# write.csv(WLdf , file = "~/Desktop/WinnersLoserDF.csv", row.names = FALSE)


#WL <- read.csv(file = "~/Desktop/WinnersLoserDF.csv")




League_by_year <- function(rawT){
  
  
  nTrans <- nrow(rawT)
  
  
  TeamToLeague_Yearly_tmp <- data.frame(matrix(0, nrow =(2*nTrans), ncol = 3))
  names(TeamToLeague_Yearly_tmp) <- c("Year", "Team", "League")
  
  
  
  for (i in 1:nTrans){
    dateYear <- signif(rawT$primary.date[i], digits = 4)/10000
    dateMD <- rawT$primary.date[i] - signif(rawT$primary.date[i], digits = 4)
    
    j <- i +nTrans
    
    if (dateMD > 1031 ){
      TeamToLeague_Yearly_tmp[i, 1] <-  (dateYear + 1)
      TeamToLeague_Yearly_tmp[j, 1] <-  (dateYear + 1)
    }
    else{
      TeamToLeague_Yearly_tmp[i, 1] <-  dateYear
      TeamToLeague_Yearly_tmp[j, 1] <-  dateYear
    }
    
    TeamToLeague_Yearly_tmp[i, 2] <- as.character(rawT$from.team[i])
    TeamToLeague_Yearly_tmp[i, 3] <- as.character(rawT$from.league[i])
    TeamToLeague_Yearly_tmp[j, 2] <- as.character(rawT$to.team[i])
    TeamToLeague_Yearly_tmp[j, 3] <- as.character(rawT$to.league[i])
  }
  
  TeamToLeague_Yearly <- TeamToLeague_Yearly_tmp[!duplicated(TeamToLeague_Yearly_tmp), ]
  
  srtTeamToLeague_Yearly <- TeamToLeague_Yearly[order(TeamToLeague_Yearly$Year,TeamToLeague_Yearly$Team), ]
  
  srtTeamToLeague_Yearly
}




##FUNCTION 1
#Note, starting and ending dates in format YYYYMMDD, and YYYY0000 is allowed
#starting is nonstrict, ending is strict

date_based_Adj <- function(starting, ending){
  
  # Select for timeframe
  
  datedTrans <- subset(transactions, transactions$primary.date >= starting & transactions$primary.date < ending)
  
  # set weights between teams
  
  teamNames <- c(as.character(datedTrans$to.team), as.character(datedTrans$from.team))
  teamNames <- unique(teamNames)
  
  
  ## make matrix with columns of team names
  weights <- matrix(0, ncol = length(teamNames), nrow = length(teamNames))
  colnames(weights) <- teamNames
  rownames(weights) <- teamNames
  
  transIDs <- unique(datedTrans$transaction.ID)
  
  #Loop over each transaction by ID
  for (id in transIDs) {
    
    #select transactions matching given id
    IDTrans0 <- subset(datedTrans, datedTrans$transaction.ID == id)
    
    #Remove any lines with blank player, from team, or to team
    IDTrans1 <- subset(IDTrans0, IDTrans0$player != "") 
    IDTrans2 <- subset(IDTrans1, as.character(IDTrans1$from.team) != "")
    IDTrans <- subset(IDTrans2, as.character(IDTrans2$to.team) != "")
    
    
    #Save remaining team names associated to that transaction. 
    #using as.character to convert from factor form to character vector
    IDteams <- unique(c(as.character(IDTrans$from.team), as.character(IDTrans$to.team)))
    
    #the number of teams involved in the transaction
    n = length(IDteams)
    
    #actually assigns the weight increases.
    #we only bother with one direction in matrix, from and to is irrelevant.
    #symmetrization later will resolve this
    
    #We assume any teams involved in the same transaction actually traded with
    #everyone else in the same transaction, otherwise, it would have been 
    #two transactions.
    
    if (n >1){
      for (j in 2:n){
        for (k in 1:(j-1)){
          
          weights[IDteams[j], IDteams[k]] <- 
            weights[IDteams[j], IDteams[k]] + 1/(n-1)
          
          weights[IDteams[k], IDteams[j]] <- 
            weights[IDteams[k], IDteams[j]] + 1/(n-1)
          
        }
      }
    }
    #Ends the id loop.
  }  

 weights
}


net_Edge_BD <- function(A){
  if (is.matrix(A)) {
    wts0 <- unique(as.vector(A))
    wts <- wts0[!wts0 %in% c(0)]
    wtsOrder <- wts[order(wts, decreasing = TRUE)]
  }
  else {print("Error, net_Edge_BD needs matrix input")}
  wtsOrder
}


#Input is a adjacency matrix A and label data frame B. 
#Label data frame B should have 2 columns: "Team", "Label"

DyadCounts <- function(A,B, InCat){
  nAll <- nrow(A)
  Cat1 <- InCat
  RelB <- B[ (B[ , 1] %in% colnames(A)), ]
  InLeagueTeams <- as.character(RelB[RelB[ ,2] == Cat1, 1])
  OutLeagueTeams <- as.character(RelB[RelB[ ,2] != Cat1, 1])
  
  TeamsWRecord <- c(InLeagueTeams, OutLeagueTeams)

  A <- A[TeamsWRecord, TeamsWRecord]
  
    
  nIn <- length(InLeagueTeams)
  nOut <- length(OutLeagueTeams)
  
  Min <- .5*sum(A[InLeagueTeams, InLeagueTeams])
  Mout <- .5*sum(A[OutLeagueTeams, OutLeagueTeams])
  Mdif <- sum(A[OutLeagueTeams, InLeagueTeams])
  Mtot <- .5*sum(A)
  
  if (Mtot != Min + Mout + Mdif){cat("Error in Dyad Cnts"," : ", InLeagueTeams," : ", OutLeagueTeams," : ", colnames(A))}
  
  ExptdEdgeWt <- Mtot/(nIn + nOut)
  
  
  p <- 2*(Mtot)/(nAll*(nAll -1))
    
  EIn <- nIn*(nIn -1)*p/2
  EOut <-  nOut*(nOut -1)*p/2
  EDif <- nIn*(nAll - nIn)*p

  ETot <- EIn + EOut + EDif
  
  EWt <- Mtot/ETot
  
  
  EWtSumIn <- EWt*nIn*(nIn -1)*p/2
  EWtSumOut <- EWt*nOut*(nOut -1)*p/2
  EWtSumDif <- EWt*nIn*(nAll - nIn)*p
    
  
  DIn <- Min/EWtSumIn
  DOut <- Mout/EWtSumOut
  H <- Mdif/EWtSumDif

  
  InC <- nIn*(nIn -1)/2
  OutC <-  nOut*(nOut -1)/2
  DifC <- nIn*nOut
  
  list("InDyadWts" = Min, "OutDyadWts" = Mout, "NonDyadWts" = Mdif, "InCat" =Cat1,
       "WtDyadIn" =DIn, "WtDyadOut" = DOut, "WtHeteroph" = H, "InC" = InC, "OutC" = OutC, "DifC" = DifC)
}


#the following takes a start point and a B which is a yearly 
#data frame of categoy labels for teams, and the In Category of those labels
#Columns of B should be: Year, Team, Label, in that order

DandH_Mat <- function(start, B, InCat){
  
  numloops <- ceiling(2021- start)
  
  DandH <- data.frame(matrix(-1, nrow = 3*numloops, ncol = 4))
  colnames(DandH) <- c( "Year", "Value", "Type", "InCat")
  
    
  for (i in 1:numloops)  {
    
    startYear <- start + (i-1)
    starting <- startYear*10000 + 1101
    ending <- (start + i)*10000 + 1031
    
    Adj <- date_based_Adj(starting, ending)
    
    #Only orginal entries, we just keep the last occurance of each team. If one team has two labels in the 
    #time span, we stick with the last label.
    
    DictNoRep <- B[B$Year == (startYear + 1),  ]
    
    #Drop the year
    
    CatsDict <- DictNoRep[ , c(2,3)]
    
    Dyads <- DyadCounts(Adj, CatsDict, InCat)
  
    #We add 1 to the start year because we want Year to correspond to the season of play.
    
    DandH[i,"Year"] <- (startYear +1)
    DandH[i,"InCat"] <- InCat
    DandH[i,"Value"] <- Dyads$WtDyadIn
    DandH[i,"Type"] <- "WtDyadIn"
      
    j <- i + numloops

    DandH[j,"Year"] <- (startYear +1)
    DandH[j,"InCat"] <- InCat
    DandH[j,"Value"] <- Dyads$WtDyadOut
    DandH[j,"Type"] <- "WtDyadOut"
    
    k <- i + 2*numloops
    
    DandH[k,"Year"] <- (startYear +1)
    DandH[k,"InCat"] <- InCat
    DandH[k,"Value"] <- Dyads$WtHeteroph
    DandH[k,"Type"] <- "WtHeteroph"
    
    }
  
  DandH  
}



#Options include "AL" or "NL" for marker, and "DyadIn", "DyadOut" and "Heteroph"
#note that "AL" with "DyadOut" should be the same as "NL" with "DyadIn".

Annual_Leagues <- League_by_year(transactions)

fDH <- DandH_Mat(1921, Annual_Leagues, "AL")

fDHWL <- DandH_Mat(1921, WLdf, "W")

seasonLabels <- c(as.character(seq(1922,2012,10)), "2021")

ggplot(fDH, aes(x = Year, y = Value,  color = Type)) + 
  geom_point() + geom_line()+ #   geom_smooth(method = "lm", fill = NA) + 
  scale_colour_discrete(breaks= c("WtDyadIn", "WtDyadOut", "WtHeteroph"), labels=c("AL-WtDyad", "NL-WtDyad", "WtHeteroph"))+
  labs( title = "", x = "Season", y = "Single Season Value", col = "Weighted Dyadicity") + 
  theme(axis.text.x = element_text(angle = 0, size = 10)) +
  scale_x_continuous(breaks = c(seq(1922,2012,10), 2021), labels = seasonLabels)+
  coord_cartesian(ylim = c(0,3))

ggplot(fDHWL, aes(x = Year, y = Value,  color = Type)) + 
  geom_point() + geom_line()+ #   geom_smooth(method = "lm", fill = NA) + 
  scale_colour_discrete(breaks= c("WtDyadIn", "WtDyadOut", "WtHeteroph"), labels=c("W-WtDyad", "L-WtDyad", "WtHeteroph"))+
  labs( title = "", x = "Season", y = "Single Season Value", col = "Weighted Dyadicity") + 
  theme(axis.text.x = element_text(angle = 0, size = 10)) +
  scale_x_continuous(breaks = c(seq(1922,2012,10), 2021), labels = seasonLabels)+
  coord_cartesian(ylim = c(0,2))


#This repeats what DandH_Mat does but for raw trans values in Adj matrices
#the following takes a start point and a B which is a yearly 
#data frame of categoy labels for teams, and the In Category of those labels
#Columns of B should be: Year, Team, Label, in that order

TransCounts <- function(start, B, InCat){
  
  numloops <- ceiling(2021- start)
  
  TrCounts <- data.frame(matrix(-1, nrow = 4*numloops, ncol = 4))
  colnames(TrCounts) <- c( "Year", "Value", "Type", "InCat")
  
  NumTrTeams <- data.frame(matrix(-1, nrow = numloops, ncol = 2))
  names(NumTrTeams) <- c("Year", "NumTrTeams")
  
  for (i in 1:numloops)  {
    
    startYear <- start + (i-1)
    starting <- startYear*10000 + 1101
    ending <- (start + i)*10000 + 1031
    
    Adj <- date_based_Adj(starting, ending)
    
    #Only orginal entries, we just keep the last occurance of each team. If one team has two labels in the 
    #time span, we stick with the last label.
    
    DictNoRep <- B[B$Year == (startYear + 1),  ]
    
    #Drop the year
    
    CatsDict <- DictNoRep[ , c(2,3)]
    
    Dyads <- DyadCounts(Adj, CatsDict, InCat)
    
    #We add 1 to the start year because we want Year to correspond to the season of play.
    
    TrCounts[i,"Year"] <- (startYear +1)
    TrCounts[i,"InCat"] <- InCat
    TrCounts[i,"Value"] <- Dyads$InDyadWts
    TrCounts[i,"Type"] <- "CountIn"
    
    j <- i + numloops
    
    TrCounts[j,"Year"] <- (startYear +1)
    TrCounts[j,"InCat"] <- InCat
    TrCounts[j,"Value"] <- Dyads$OutDyadWts
    TrCounts[j,"Type"] <- "CountOut"
    
    k <- i + 2*numloops
    
    TrCounts[k,"Year"] <- (startYear +1)
    TrCounts[k,"InCat"] <- InCat
    TrCounts[k,"Value"] <- Dyads$NonDyadWts
    TrCounts[k,"Type"] <- "CountBw"
    
    l <- i + 3*numloops
    
    TrCounts[l,"Year"] <- (startYear +1)
    TrCounts[l,"InCat"] <- InCat
    TrCounts[l,"Value"] <-(Dyads$InDyadWts + Dyads$OutDyadWts + Dyads$NonDyadWts)
    TrCounts[l,"Type"] <- "TotalCount"
  }
  
  TrCounts 
}

Annual_Leagues <- League_by_year(transactions)

LeagueCounts <- TransCounts(1921, Annual_Leagues, "AL")

WLCounts <- TransCounts(1921, WLdf, "W")

seasonLabels <- c(as.character(seq(1922,2012,10)), "2021")

ggplot(LeagueCounts, aes(x = Year, y = Value,  color = Type)) + 
  geom_point() + geom_line()+ 
  scale_colour_discrete(breaks= c("CountIn", "CountOut", "CountBw", "TotalCount"),
                        labels=c("Intra-AL Volume", "Intra-NL Volume", "Inter-AL/NL Volume", "Total Volume"))+
  labs( title = "", x = "Season", y = "Volume", col = "League Volume Type") +
  theme(axis.text.x = element_text(angle = 0, size = 10)) +
  scale_x_continuous(breaks = c(seq(1922,2012,10), 2021), labels = seasonLabels)+
  geom_vline(xintercept = 1961, linetype = "dotted") +
  geom_vline(xintercept = 1962, linetype = "dotted") +
  geom_vline(xintercept = 1969, linetype = "dotted") +
  geom_vline(xintercept = 1977, linetype = "dotted") +
  geom_vline(xintercept = 1993, linetype = "dotted") +
  geom_vline(xintercept = 1998, linetype = "dotted") 

ggplot(WLCounts, aes(x = Year, y = Value,  color = Type)) + 
  geom_point() + geom_line()+ 
  scale_colour_discrete(breaks= c("CountIn", "CountOut", "CountBw", "TotalCount"), 
                        labels=c("Intra-W Volume", "Intra-L Volume", "Inter-W/L Volume", "Total Volume"))+
  labs( title = "", x = "Season", y = "Volume", col = "Competitiveness Volume Type") + 
  theme(axis.text.x = element_text(angle = 0, size = 10)) +
  scale_x_continuous(breaks = c(seq(1922,2012,10), 2021), labels = seasonLabels)+
  geom_vline(xintercept = 1961, linetype = "dotted") + 
  geom_vline(xintercept = 1962, linetype = "dotted") + 
  geom_vline(xintercept = 1969, linetype = "dotted") + 
  geom_vline(xintercept = 1977, linetype = "dotted") + 
  geom_vline(xintercept = 1993, linetype = "dotted") + 
  geom_vline(xintercept = 1998, linetype = "dotted") 

TeamIdDict_Pay <- read.csv(file = "~/Desktop/MLB_transactions/MLB_Team_ID_Payroll.csv")

Payroll <- read.csv(file = "~/Desktop/MLB_transactions/MLB_payrolls.csv")


for (i in 1:nrow(Payroll)){
  id <- Payroll$ID[i]
  Team <- TeamIdDict_Pay$Team[which(TeamIdDict_Pay$ID == id)]
  Payroll[i, 5] <- Team
}

TeamPays <- Payroll[Payroll$Year <= 2021, c(1, 5, 4)]
names(TeamPays) <- c("Year", "Team", "Payroll")

PayYears <- 1988:2021

for (x in PayYears){
  #AnnMean <- median(TeamPays[TeamPays$Year == x, "Payroll"])
  AnnMean <- quantile(TeamPays[TeamPays$Year == x, "Payroll"], .75, type = 1)
    TeamPays[TeamPays$Year == x, 4] <- AnnMean
}

names(TeamPays)[4] <- c("AnnualMean")
                     
for (j in 1:nrow(TeamPays)){
  if(TeamPays[j, "Payroll"] > TeamPays[j, "AnnualMean"]){TeamPays[j, 5] <- "A"}
  if(TeamPays[j, "Payroll"] <= TeamPays[j, "AnnualMean"]){TeamPays[j, 5] <- "B"}
}

names(TeamPays)[5] <- c("Label")

PayrollLabels <- TeamPays[ ,c(1, 2, 5)]


fPayroll <- DandH_Mat(1987, PayrollLabels, "A")

seasonLabels <- c(as.character(seq(1922,2012,10)), "2021")

ggplot(fPayroll, aes(x = Year, y = Value,  color = Type)) + 
  geom_point() + geom_line()+ 
  scale_colour_discrete(breaks= c("WtDyadIn", "WtDyadOut", "WtHeteroph"), labels=c("A-WtDyad", "B-WtDyad", "WtHeteroph"))+
  labs( title = "", x = "Season", y = "Single Season Value (A in Upper Quartile)", col = "Weighted Dyadicity (A in Q4)") + 
  theme(axis.text.x = element_text(angle = 0, size = 10)) +
  scale_x_continuous(breaks = c(seq(1922,2012,10), 2021), labels = seasonLabels)+
  coord_cartesian(ylim = c(0,2))


ABCounts <- TransCounts(1987, PayrollLabels, "A")

PaySeasonLabels <- c(as.character(seq(1988,2018,10)), "2021")

ggplot(ABCounts, aes(x = Year, y = Value,  color = Type)) + 
  geom_point() + geom_line()+ 
  scale_colour_discrete(breaks= c("CountIn", "CountOut", "CountBw", "TotalCount"),
                        labels=c("Intra-A Volume", "Intra-B Volume", "Inter-A/B Volume", "Total Volume"))+
  labs( title = "", x = "Season", y = "Volume", col = "Payroll Volume Type") +
  theme(axis.text.x = element_text(angle = 0, size = 10)) +
  scale_x_continuous(breaks = c(seq(1988,2018,10), 2021), labels = PaySeasonLabels)+
  geom_vline(xintercept = 1993, linetype = "dotted") +
  geom_vline(xintercept = 1998, linetype = "dotted") 


### Node Combination Counts

NodeCombs_Mat <- function(start, B, InCat){
  
  numloops <- 2021- start
  
  NodeCombs <- data.frame(matrix(-1, nrow = 3*numloops, ncol = 4))
  colnames(NodeCombs) <- c( "Year", "Value", "Type", "InCat")
  
  
  for (i in 1:numloops)  {
    
    startYear <- start + (i-1)
    starting <- startYear*10000 + 1101
    ending <- (start + i)*10000 + 1031
    
    Adj <- date_based_Adj(starting, ending)
    
    #Only orginal entries, we just keep the last occurance of each team. If one team has two labels in the 
    #time span, we stick with the last label.
    
    DictNoRep <- B[B$Year == (startYear + 1),  ]
    
    #Drop the year
    
    CatsDict <- DictNoRep[ , c(2,3)]
    
    Dyads <- DyadCounts(Adj, CatsDict, InCat)
    
    #We add 1 to the start year because we want Year to correspond to the season of play.
    
    NodeCombs[i,"Year"] <- (startYear +1)
    NodeCombs[i,"InCat"] <- InCat
    NodeCombs[i,"Value"] <- Dyads$InC
    NodeCombs[i,"Type"] <- "InCombs"
    
    j <- i + numloops
    
    NodeCombs[j,"Year"] <- (startYear +1)
    NodeCombs[j,"InCat"] <- InCat
    NodeCombs[j,"Value"] <- Dyads$OutC
    NodeCombs[j,"Type"] <- "OutCombs"
    
    k <- i + 2*numloops
    
    NodeCombs[k,"Year"] <- (startYear +1)
    NodeCombs[k,"InCat"] <- InCat
    NodeCombs[k,"Value"] <- Dyads$DifC
    NodeCombs[k,"Type"] <- "BwCombs"
    
  }
  
  NodeCombs  
}


NodeComLeague <-  NodeCombs_Mat(1921, Annual_Leagues, "AL")

NodeComWL <- NodeCombs_Mat(1921, WLdf, "W")

NodeComPay <- NodeCombs_Mat(1987, PayrollLabels, "A")


seasonLabels <- c(as.character(seq(1922,2012,10)), "2021")

PaySeasonLabels <- c(as.character(seq(1988,2018,10)), "2021")


ggplot(NodeComLeague, aes(x = Year, y = Value,  color = Type)) + 
  geom_point() + #   geom_smooth(method = "lm", fill = NA) + 
  scale_colour_discrete(breaks= c("InCombs", "OutCombs", "BwCombs"), 
                        labels=c("Intra-AL", "Intra-NL", "Inter-AL/NL"))+
  labs( title = "", x = "Season", y = "Possible Link Count", col = "Link Type") + 
  theme(axis.text.x = element_text(angle = 0, size = 10)) +
  scale_x_continuous(breaks = c(seq(1922,2012,10), 2021), labels = seasonLabels)+
  geom_vline(xintercept = 1961, linetype = "dotted") +
  geom_vline(xintercept = 1962, linetype = "dotted") +
  geom_vline(xintercept = 1969, linetype = "dotted") +
  geom_vline(xintercept = 1977, linetype = "dotted") +
  geom_vline(xintercept = 1993, linetype = "dotted") +
  geom_vline(xintercept = 1998, linetype = "dotted") 

ggplot(NodeComWL, aes(x = Year, y = Value,  color = Type)) + 
  geom_point() +  #   geom_smooth(method = "lm", fill = NA) + 
  scale_colour_discrete(breaks= c("InCombs", "OutCombs", "BwCombs"), labels=c("Intra-W", "Intra-L", "Inter-W/L"))+
  labs( title = "", x = "Season", y = "Possible Link Count", col = "Link Type") + 
  theme(axis.text.x = element_text(angle = 0, size = 10)) +
  scale_x_continuous(breaks = c(seq(1922,2012,10), 2021), labels = seasonLabels)+
  geom_vline(xintercept = 1961, linetype = "dotted") +
  geom_vline(xintercept = 1962, linetype = "dotted") +
  geom_vline(xintercept = 1969, linetype = "dotted") +
  geom_vline(xintercept = 1977, linetype = "dotted") +
  geom_vline(xintercept = 1993, linetype = "dotted") +
  geom_vline(xintercept = 1998, linetype = "dotted") 

ggplot(NodeComPay, aes(x = Year, y = Value,  color = Type)) + 
  geom_point() +  
  scale_colour_discrete(breaks= c("InCombs", "OutCombs", "BwCombs"), labels=c("Intra-A", "Intra-B", "Intra-A/B"))+
  labs( title = "", x = "Season", y = "Possible Link Count", col = "Link Type") + 
  theme(axis.text.x = element_text(angle = 0, size = 10)) +
  scale_x_continuous(breaks = c(seq(1988,2018,10), 2021), labels = PaySeasonLabels)+
  geom_vline(xintercept = 1993, linetype = "dotted") +
  geom_vline(xintercept = 1998, linetype = "dotted") 


#Comdf <- NodeComLeague
#Comdf <- NodeComWL
Comdf <- NodeComPay



YearRange0 <- unique(Comdf$Year)
YearRange <- YearRange0[order(YearRange0)]
numYrs <- length(YearRange)

LinkRatios <- data.frame(matrix(0, nrow = numYrs, ncol = 2))
names(LinkRatios) <- c("Year", "Ratio")
for (i in 1:numYrs){
  yr <- YearRange[i]
  InC <- Comdf[Comdf$Year == yr & Comdf$Type == "InCombs", "Value"]
  OutC <- Comdf[Comdf$Year == yr & Comdf$Type == "OutCombs", "Value"]
  BwC <- Comdf[Comdf$Year == yr & Comdf$Type == "BwCombs", "Value"]

  LinkRatios[i, 1] <- yr
  LinkRatios[i, 2] <- BwC/(InC + OutC)
}

#LeagNodeRatios <- LinkRatios
#CompNodeRatios <- LinkRatios
# PayNodeRatios <- LinkRatios
# 
# 
# 
# ggplot(LeagNodeRatios, aes(x = Year, y = Ratio)) + 
#   geom_point() +  #   geom_smooth(method = "lm", fill = NA) + 
#   labs( title = "", x = "Season", y = "League Possible Link Inter/Sum-Intra") + 
#   theme(axis.text.x = element_text(angle = 0, size = 10)) +
#   scale_x_continuous(breaks = c(seq(1922,2012,10), 2021), labels = seasonLabels)+
#   geom_vline(xintercept = 1961, linetype = "dotted") +
#   geom_vline(xintercept = 1962, linetype = "dotted") +
#   geom_vline(xintercept = 1969, linetype = "dotted") +
#   geom_vline(xintercept = 1977, linetype = "dotted") +
#   geom_vline(xintercept = 1993, linetype = "dotted") +
#   geom_vline(xintercept = 1998, linetype = "dotted") 
# 
# 
# ggplot(CompNodeRatios, aes(x = Year, y = Ratio)) + 
#   geom_point() +  #   geom_smooth(method = "lm", fill = NA) + 
#   labs( title = "", x = "Season", y = "Competitiveness Possible Link Inter/Sum-Intra") + 
#   theme(axis.text.x = element_text(angle = 0, size = 10)) +
#   scale_x_continuous(breaks = c(seq(1922,2012,10), 2021), labels = seasonLabels)+
#   geom_vline(xintercept = 1961, linetype = "dotted") +
#   geom_vline(xintercept = 1962, linetype = "dotted") +
#   geom_vline(xintercept = 1969, linetype = "dotted") +
#   geom_vline(xintercept = 1977, linetype = "dotted") +
#   geom_vline(xintercept = 1993, linetype = "dotted") +
#   geom_vline(xintercept = 1998, linetype = "dotted") 
# 
# 
# ggplot(PayNodeRatios, aes(x = Year, y = Ratio)) + 
#   geom_point() +  #   geom_smooth(method = "lm", fill = NA) + 
#   labs( title = "", x = "Season", y = "Payroll Possible Link Inter/Sum-Intra") + 
#   theme(axis.text.x = element_text(angle = 0, size = 10)) +
#   scale_x_continuous(breaks = c(seq(1988,2018,10), 2021), labels = PaySeasonLabels)+
#   geom_vline(xintercept = 1993, linetype = "dotted") +
#   geom_vline(xintercept = 1998, linetype = "dotted") 
# 
#   