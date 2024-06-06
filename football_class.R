library(jsonlite)
library(httr)
library(rjson)
library(data.table)
  
  url <- paste("http://api.football-data.org/v4/matches/?date=2024-06-14")
  res <- GET(url,  add_headers(`X-Auth-Token` = "82651a7780a24613af6601960b77fcfd"))
  str_res_json <- rawToChar(res$content)
  parsed_data <- fromJSON(str_res_json)
  parsed_data
  
  
  url <- paste("http://api.football-data.org/v4/competitions/")
  res <- GET(url,  add_headers(`X-Auth-Token` = "82651a7780a24613af6601960b77fcfd"))
  str_res_json <- rawToChar(res$content)
  parsed_data <- fromJSON(str_res_json)
  parsed_data
  
  url <- paste("http://api.football-data.org/v4/competitions/EC/teams")
  res <- GET(url,  add_headers(`X-Auth-Token` = "82651a7780a24613af6601960b77fcfd"))
  str_res_json <- rawToChar(res$content)
  parsed_data <- fromJSON(str_res_json)
  parsed_data
  
  
  teams_repo = new("Teams_Repository")
  teams_repo = updateTeamsFromAPI(teams_repo, competition = "EC")
  
  
  
# S4 class containing list of slots.
setClass("Teams_Repository", slots=list(competition = "EC"))  
  
  getLatestTeamsFromAPI <- function(competition = "EC") {
    url <- paste("http://api.football-data.org/v4/competitions/",competition,"/teams", sep = "")
    res <- GET(url,  add_headers(`X-Auth-Token` = "82651a7780a24613af6601960b77fcfd"))
    return(res)
  }
  
  # Define a method to add data to the repository
  setGeneric(name = "updateTeamsFromAPI",
             def = function(object, competition = "EC") {
               standardGeneric("updateTeamsFromAPI")
             })
  
  setMethod(f = "updateTeamsFromAPI",
            signature = c("Teams_Repository"),
            definition = function(object, competition) {
              newData <- getLatestTeamsFromAPI(competition)
              str_res_json <- rawToChar(newData$content)
              parsed_events <- fromJSON(str_res_json)
              data_table <- rbindlist(lapply(parsed_data$teams[[1]]$squad, as.data.table))
              #object$data <- data_table
              #object$time <- Sys.time()
              #object@all <- all
              return(data_table)
            })