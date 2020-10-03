#' vote information of parliament members
#' @name vott vote informaton
#'
#' @description get the data from parliament API and return the attendance rate, attitude and participation of each member.
#'
#' @return vote information of certain member
#'
#' @examples
#' x <- vott()
#' cal_info(x,A Eriksson, Erik  (C))
#' @export
#'

#unfinished. need some input
vott <- function(){

  #load package httr and jsonlite
  library(httr)
  library(jsonlite)

  #get the URL
  vot_url <- "http://data.riksdagen.se/voteringlista/?bet=&punkt=&valkrets=&rost=&iid=&sz=500&utformat=json&gruppering=namn"
  # vot_url2 <- "http://data.riksdagen.se/voteringlista/?bet=&punkt=&valkrets=&rost=&iid=&sz=1000&utformat=json&gruppering=namn"

  vot <- GET(vot_url)

  #convert the format from json to R
  vot_data <- fromJSON(content(vot,"text"))

  #get the data
  nam = approve = nay = absent = abstain<- NULL
  nam <- vot_data$voteringlista$votering$namn
  approve <- as.numeric(vot_data$voteringlista$votering$Ja)
  nay <- as.numeric(vot_data$voteringlista$votering$Nej)
  absent <- as.numeric(vot_data$voteringlista$votering[["Frånvarande"]])
  abstain <- as.numeric(vot_data$voteringlista$votering[["Avstår"]])

  #calculate the attendance rate, attitude and participation
  pon = pap = attend <- NULL
  for(i in 1:length(vot_data$voteringlista$votering$namn)){

      # the attitde is positive or negtive
      pon[i] <- approve[i]/(approve[i]+nay[i])

      # participation of the member
      pap[i] <- abstain[i]/(approve[i]+nay[i])

      # attendence rate of the member
      attend[i] <- 1-absent[i]/(approve[i]+nay[i]+abstain[i])
    }

  # make it into a dataframe
  df <- data.frame(cbind(nam,approve,nay,absent,abstain,pon,pap,attend))
  attr(df,"class") <- "vott"
  return(df)
}

#define S3method
cal_info <- function(x){
  UseMethod('cal_info')
}

#call the information of certain members (unfinished)
cal_info.vott <- function(x,name1){
  result <- NULL
  for(i in 1:length(vot_data$voteringlista$votering$namn)){
    if(nam[i] == name1){
      name_mem <- nam[i]
      result <- as.vector(x[i,])
      cat(result)
      break
    }
  }
}


# personaldata <- "http://data.riksdagen.se/personlista/?iid=&fnamn=&enamn=&f_ar=&kn=&parti=&valkrets=&rdlstatus=&org=&utformat=json&sort=sorteringsnamn&sortorder=asc&termlista="

# voting:"http://data.riksdagen.se/voteringlista/?bet=&punkt=&valkrets=&rost=&iid=&sz=500&utformat=json&gruppering="

# voting_name:"http://data.riksdagen.se/voteringlista/?bet=&punkt=&valkrets=&rost=&iid=&sz=500&utformat=json&gruppering=namn"





