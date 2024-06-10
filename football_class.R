library(jsonlite)
library(httr)
library(rjson)
library(data.table)
  
  url <- paste("http://api.football-data.org/v4/matches/?dateFrom=2023-06-14&dateTo=2024-06-14")
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
  
  formatTeamTable <- function(parsed_data) {
    total_table <- data.table()
    for (i in 1:parsed_data$count){
      coach_table <- as.data.table(parsed_data$teams[[i]]$coach)
      coach_table[["contract_start"]] <- coach_table[["contract"]][[1]]
      coach_table[["contract_end"]] <- coach_table[["contract"]][[2]]
      coach_table <- coach_table[-2]
      coach_table[["position"]] <- "coach"
      coach_table[, c("firstName", "lastName", "contract") := NULL]
      squad_table <- rbindlist(lapply(parsed_data$teams[[i]]$squad, as.data.table))
      team_table <- rbind(squad_table, coach_table, fill = TRUE)
      team_table[["team"]] <- parsed_data$teams[[i]]$area$name
      team_table[["team_id"]] <- parsed_data$teams[[i]]$area$id
      total_table <- rbind(team_table, total_table, fill = TRUE)
    }
    return(total_table)
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
              parsed_data <- fromJSON(str_res_json)
              data_table <- formatTeamTable(parsed_data)
              return(data_table)
            })