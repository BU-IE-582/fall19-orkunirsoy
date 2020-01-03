
#' Performance Metric: Ranked Probability Score
#'
#' @param probs vector of 3, predicted probabilities
#' @param outcomes vector of 3, binary outcomes, should be provided with the same order as probs
#' @export
#' @examples
RPS_single<- function(probs,outcomes){
  probs = cumsum(probs)
  outcomes = cumsum(outcomes)
  RPS = sum((probs-outcomes )^2) / (length(probs)-1)
  return(RPS)
}

RPS_matrix<- function(probs,outcomes){
  probs=as.matrix(probs)
  outcomes=as.matrix(outcomes)
  probs=t(apply(t(probs), 2, cumsum))
  outcomes=t(apply(t(outcomes), 2, cumsum))
  RPS = apply((probs-outcomes)^2,1,sum) / (ncol(probs)-1)
  return(RPS)
}

res_matrix=function(data,colbegin)
{
  outcomes=matrix(0,nrow(data),3)
  for(i in 1:nrow(data))
  {
    if(data$Match_Result[i]=='Home')
    {
      outcomes[i,1]=1
    }
    if(data$Match_Result[i]=='Tie')
    {
      outcomes[i,2]=1
    }
    if(data$Match_Result[i]=='Away')
    {
      outcomes[i,3]=1
    }
  }
  return(outcomes)
}
rps_calc=function(data,colbegin)
{
  outcomes=matrix(0,nrow(data),3)
  for(i in 1:nrow(data))
  {
    if(data$Match_Result[i]=='Home')
    {
      outcomes[i,1]=1
    }
    if(data$Match_Result[i]=='Tie')
    {
      outcomes[i,2]=1
    }
    if(data$Match_Result[i]=='Away')
    {
      outcomes[i,3]=1
    }
  }
  RPS_TEST=RPS_matrix(data[,colbegin:(colbegin+2)],outcomes)
  return(RPS_TEST)
}
