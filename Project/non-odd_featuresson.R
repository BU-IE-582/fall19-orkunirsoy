

#Function intents to extract related features of a specific team using its last n match and stat data
extract_last_ngames <- function(matches,N=10){
  
  
  #Create dataframe to store extracted features
  L_n_matches<-cbind(matches,L_n_Homegoals= as.numeric(NA),L_n_Awaygoals=as.numeric(NA),L_n_Homeconceded= as.numeric(NA),
                     L_n_Awayconceded=as.numeric(NA),L_n_Homewins= as.numeric(NA),L_n_Awaywins=as.numeric(NA),
                     L_n_HomeELOchange= as.numeric(NA),L_n_AwayELOchange=as.numeric(NA),L_n_HomeGoalAttempts= as.numeric(NA),
                     L_n_AwayGoalAttempts= as.numeric(NA),L_n_HomeGoalkeeperSaves= as.numeric(NA), 
                     L_n_AwayGoalkeepersaves= as.numeric(NA),L_n_HomeBallPossession= as.numeric(NA),
                     L_n_AwayBallPossession= as.numeric(NA))
  
  # Home Team
  for (i in 1:length(L_n_matches$matchId)) {
      
      # Get home teams name and corresponding date
      h<- L_n_matches$Home[i]
      matchdate<- L_n_matches$Match_Date[i]	
      
      # Create last n games data
      x<-matches[(Home==h|Away==h)& Match_Date<matchdate ]
      x<-x[!is.na(Match_Result)]
      x<-x[order(Match_Date)]  
      
      # If team doesn't have last n data for the corresponding date skip the procedure (leave NA)
      if (length(x$matchId) < N) {next}
      
      # Obtain last n game wins,goals scored and conceded
      y<-tail(x,N)
      L_n_matches[i,]$L_n_Homegoals<-sum(as.numeric(y$Home==h)*y$Home_Score + as.numeric(y$Away==h)*y$Away_Score)
      L_n_matches[i,]$L_n_Homeconceded<-sum(as.numeric(y$Home==h)*y$Away_Score + as.numeric(y$Away==h)*y$Home_Score)
      L_n_matches[i,]$L_n_Homewins<-sum(as.numeric(y$Home==h)*y$Result_Home + as.numeric(y$Away==h)*y$Result_Away)
      
      # Obtain ELO difference between the first and nth game
      ELO1<-as.numeric(y[1,]$Home==h)*y[1,]$Home_Elo + as.numeric(y[1,]$Away==h)*y[1,]$Away_Elo
      ELON<-as.numeric(y[N,]$Home==h)*y[N,]$Home_Elo + as.numeric(y[N,]$Away==h)*y[N,]$Away_Elo
      L_n_matches[i,]$L_n_HomeELOchange<-ELON-ELO1
      
      # For stat data merge dataframes to obtain last n games' statitstics
      z<-merge(x,statdata[,list(matchId,home_GoalAttempts,away_GoalAttempts,home_GoalkeeperSaves,away_GoalkeeperSaves,home_BallPossession,away_BallPossession)], by = "matchId")
      z=z[complete.cases(z)==TRUE]
      
      # If team doesn't have last n data for the corresponding date skip the procedure (leave NA)
      if (length(z$matchId) < N) {next}
      z<-tail(z,N)
      
      # Obtain last n game goal attempts, goalkeeper saves and ball possession
      L_n_matches[i,]$L_n_HomeGoalAttempts<-sum(as.numeric(z$Home==h)*z$home_GoalAttempts+as.numeric(z$Away==h)*z$away_GoalAttempts)
      L_n_matches[i,]$L_n_HomeGoalkeeperSaves<-sum(as.numeric(z$Home==h)*z$home_GoalkeeperSaves+as.numeric(z$Away==h)*z$away_GoalkeeperSaves)
      L_n_matches[i,]$L_n_HomeBallPossession<-sum(as.numeric(z$Home==h)*z$home_BallPossession+as.numeric(z$Away==h)*z$away_BallPossession)
      
  }
  
  # Away Team
  for (i in 1:length(L_n_matches$matchId)) {
    
    # Get away teams name and corresponding date
    a<- L_n_matches$Away[i]
    matchdate<- L_n_matches$Match_Date[i]
    
    # Create last n games data
    x<-matches[(Home==a|Away==a)& Match_Date<=matchdate ]
      x<-x[!is.na(Match_Result)]
      x<-x[order(Match_Date)]  
      
      # If team doesn't have last n data for the corresponding date skip the procedure (leave NA)
      if (length(x$matchId) < N){next}
      
      # Obtain last n game wins,goals scored and conceded
      y<-tail(x,N)
      L_n_matches[i,]$L_n_Awaygoals<-sum(as.numeric(y$Home==a)*y$Home_Score + as.numeric(y$Away==a)*y$Away_Score)
      L_n_matches[i,]$L_n_Awayconceded<-sum(as.numeric(y$Home==a)*y$Away_Score + as.numeric(y$Away==a)*y$Home_Score)
      L_n_matches[i,]$L_n_Awaywins<-sum(as.numeric(y$Home==a)*y$Result_Home + as.numeric(y$Away==a)*y$Result_Away)
      
      # Obtain ELO difference between the first and nth game
      ELO1<-as.numeric(y[1,]$Home==a)*y[1,]$Home_Elo + as.numeric(y[1,]$Away==a)*y[1,]$Away_Elo
      ELON<-as.numeric(y[N,]$Home==a)*y[N,]$Home_Elo + as.numeric(y[N,]$Away==a)*y[N,]$Away_Elo
      L_n_matches[i,]$L_n_AwayELOchange<-ELON-ELO1
      
      
      # For stat data merge dataframes to obtain last n games' statitstics
      z<-merge(x,statdata[,list(matchId,home_GoalAttempts,away_GoalAttempts,home_GoalkeeperSaves,away_GoalkeeperSaves,home_BallPossession,away_BallPossession)], by = "matchId")
      z=z[complete.cases(z)==TRUE]
      
      # If team doesn't have last n data for the corresponding date skip the procedure (leave NA)
      if (length(z$matchId) < N) {next}
      z<-tail(z,N)
      
      # Obtain last n game goal attempts, goalkeeper saves and ball possession
      L_n_matches[i,]$L_n_AwayGoalAttempts<-sum(as.numeric(z$Home==a)*z$home_GoalAttempts+as.numeric(z$Away==a)*z$away_GoalAttempts)
      L_n_matches[i,]$L_n_AwayGoalkeepersaves<-sum(as.numeric(z$Home==a)*z$home_GoalkeeperSaves+as.numeric(z$Away==a)*z$away_GoalkeeperSaves)
      L_n_matches[i,]$L_n_AwayBallPossession<-sum(as.numeric(z$Home==a)*z$home_BallPossession+as.numeric(z$Away==a)*z$away_BallPossession)
      
  }
  return(L_n_matches)
}