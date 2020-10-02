# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# hello <- function() {
#   print("Hello, world!")
# }

library(httr)
library(jsonlite)
vot_url <- "http://data.riksdagen.se/voteringlista/?bet=&punkt=&valkrets=&rost=&iid=&sz=500&utformat=json&gruppering=namn"

vot <- GET(vot_url)

vot_data <- fromJSON(content(vot,"text"))

nam = agree = disagree = absent = abstain<- NULL

for(i in 1:length(vot_data$voteringlista$votering$namn)){

   nam[i] <- vot_data$voteringlista$votering$namn[[i]]

   approve[i] <- vot_data$voteringlista$votering$Ja[[i]]

   nay[i] <- vot_data$voteringlista$votering$Nej[[i]]

   absent[i] <- vot_data$voteringlista$votering$Franvarande[[i]]

   abstain[i] <- vot_data$voteringlista$votering$Avstår[[i]]
}
absent

vott <- function(){



}

# personaldata <- "http://data.riksdagen.se/personlista/?iid=&fnamn=&enamn=&f_ar=&kn=&parti=&valkrets=&rdlstatus=&org=&utformat=json&sort=sorteringsnamn&sortorder=asc&termlista="

# voting:"http://data.riksdagen.se/voteringlista/?bet=&punkt=&valkrets=&rost=&iid=&sz=500&utformat=json&gruppering="

# voting_name:"http://data.riksdagen.se/voteringlista/?bet=&punkt=&valkrets=&rost=&iid=&sz=500&utformat=json&gruppering=namn"





