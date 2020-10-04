#' vote information of parliament members
#' @name vott vote informaton
#'
#' @description get the data from parliament API and return the attendance rate, attitude and participation of each member.
#'
#' @return vote information of certain member
#'
#' @import methods
#' @importFrom  jsonlite fromJSON
#' @importFrom  httr GET content
#' @examples
#' x <- vott()
#' cal_info(x,A Eriksson, Erik  (C))
#' @export
#'

#unfinished. need some input
vott <- function(name1){

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
  absent <- as.numeric(vot_data$voteringlista$votering[,4])
  abstain <- as.numeric(vot_data$voteringlista$votering[,5])

  #calculate the attendance rate, attitude and participation
  pon = pap = attend <- NULL
  for(i in 1:length(vot_data$voteringlista$votering$namn)){
      # the attitde is positive or negtive
      pon[i] <- approve[i]/(approve[i]+nay[i])

      # participation of the member
      pap[i] <- abstain[i]/(approve[i]+nay[i])

      # attendence rate of the member
      attend[i] <- 1-absent[i]/(approve[i]+nay[i]+abstain[i]+absent[i])
    }

  for(i in 1:length(vot_data$voteringlista$votering$namn)){
    if(nam[i]==name1){
      name_mem <- nam[i]
      approve_mem <- approve[i]
      nay_mem <- nay[i]
      absent_mem <- absent[i]
      abstain_mem <- abstain[i]
      pon_mem <- pon[i]
      pap_mem <- pap[i]
      attend_mem <- attend[i]
      break
    }
  }

  # make it into a dataframe
  l <- list(name=name_mem,approve=approve_mem,nay=nay_mem,absent=absent_mem,abstain=abstain_mem,pon=pon_mem,pap=pap_mem,attend=attend_mem)
  attr(l,"class") <- "vott"
  return(l)
}

#' @rdname calinfo
#' @method calinfo vott
#' @export
#'
#define S3method
calinfo <- function(x){
  UseMethod('calinfo')
}

#call the information of certain members (unfinished)
calinfo.vott <- function(x){
  # result <- NULL
  # name_tem <- x$name
  # app_tem <- x$approve
  # result <- as.data.frame(cbind(name_tem,app_tem))
   return(x$name,x$approve)
}


#cannot get the package function??

# personaldata <- "http://data.riksdagen.se/personlista/?iid=&fnamn=&enamn=&f_ar=&kn=&parti=&valkrets=&rdlstatus=&org=&utformat=json&sort=sorteringsnamn&sortorder=asc&termlista="

# voting:"http://data.riksdagen.se/voteringlista/?bet=&punkt=&valkrets=&rost=&iid=&sz=500&utformat=json&gruppering="

# voting_name:"http://data.riksdagen.se/voteringlista/?bet=&punkt=&valkrets=&rost=&iid=&sz=500&utformat=json&gruppering=namn"





