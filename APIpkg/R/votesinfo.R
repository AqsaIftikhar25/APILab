#' vote information of parliament members
#' @name vott vote informaton
#' @param name1 A char.name of the politician
#' @param startY a integer. start year
#' @param endY a integer. end year
#' @description get the data from parliament API and return the attendance rate, attitude and participation of each member.
#'
#' @return l. vote information of certain member
#' @usage vott (name1,startY,endY)
#' @importFrom  jsonlite fromJSON
#' @importFrom  httr GET content
#' @examples
#' l1 <- vott("C",2015,2020)
#' @export
#'

#unfinished. need some input
vott <- function(name1,startY,endY){

  library(httr)
  library(jsonlite)
  # name1 <- "C"
  # startY <- 2015
  # endY <- 2020

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
  nay <- as.numeric(vot_data$voteringlista$votering$Nej)
  absent <- as.numeric(vot_data$voteringlista$votering[,4])
  abstain <- as.numeric(vot_data$voteringlista$votering[,5])

  # #calculate the attendance rate, attitude and participation
  # pon = pap = attend <- NULL
  # for(i in 1:length(vot_data$voteringlista$votering$namn)){
  #     # the attitde is positive or negtive
  #     pon[i] <- approve[i]/(approve[i]+nay[i])
  #
  #     # participation of the member
  #     pap[i] <- abstain[i]/(approve[i]+nay[i])
  #
  #     # attendence rate of the member
  #     attend[i] <- 1-absent[i]/(approve[i]+nay[i]+abstain[i]+absent[i])
  #   }
  #
  # for(i in 1:length(vot_data$voteringlista$votering$namn)){
  #   if(nam[i]==name1){
  #     name_mem <- nam[i]
  #     approve_mem <- approve[i]
  #     nay_mem <- nay[i]
  #     absent_mem <- absent[i]
  #     abstain_mem <- abstain[i]
  #     pon_mem <- pon[i]
  #     pap_mem <- pap[i]
  #     attend_mem <- attend[i]
  #     break
  #   }
  # }
  #class(nam)
  # make it into a dataframe
  l <- data.frame(cbind(name=nam,approve=approve))
  # attr(l,"class") <- "vott"
  return(l)
}

#'
#define S3method
# calinfo <- function(x){
#   UseMethod('calinfo')
# }

#call the information of certain members (unfinished)
# calinfo.vott <- function(x){
#   # result <- NULL
#   # name_tem <- x$name
#   # app_tem <- x$approve
#    # result <- as.data.frame(cbind(name_tem,app_tem))
#    return(x$name,x$approve)
# }

# usethis::use_testthat()



