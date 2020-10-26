#' vote information of parliament members
#' @name vott
#'
#' @param name1 A char.name of the politician
#' @param startY a integer. start year
#' @param endY a integer. end year
#'
#' @description get the data from parliament API and return the attendance rate, attitude and participation of each member.
#' @importFrom  jsonlite fromJSON
#' @importFrom  httr GET content
#' @return l. vote information of certain member
#' @usage vott (name1,startY,endY)
#' @examples
#' l1 <- vott("C",2015,2020)
#' @export vott


vott <- function(name1="C",startY=2016,endY=2020){

   # library(httr)
   # library(jsonlite)

  start_la <- startY%%2000
  end_la <- endY%%2000

  chara <- NULL
  for(i in startY:endY){
    i_la <- i%%2000
    chara <- paste(i,"%2F",i_la+1,"&rm=",chara,sep = "",collapse = "")
  }
  #get the URL
  vot_url <- paste("http://data.riksdagen.se/voteringlista/?rm=",chara,"&bet=&punkt=&parti=",name1,'&valkrets=&rost=&iid=&sz=500&utformat=json&gruppering=namn',sep = "",collapse = "")
  vot <- GET(vot_url)
  #convert the format from json to R
  vot_data <- fromJSON(content(vot,"text"))
  # vot_data <- fromJSON(rawToChar(vot$content))

  #get the data
  nam = approve = nay = absent = abstain<- NULL
  nam <- vot_data$voteringlista$votering$namn
  approve <- as.numeric(vot_data$voteringlista$votering$Ja)
  # nay <- as.numeric(vot_data$voteringlista$votering$Nej)
  # absent <- as.numeric(vot_data$voteringlista$votering[,4])
  # abstain <- as.numeric(vot_data$voteringlista$votering[,5])
 
  # make it into a dataframe
  l <- data.frame(cbind(name=nam,approve=approve))
  return(l)
}


