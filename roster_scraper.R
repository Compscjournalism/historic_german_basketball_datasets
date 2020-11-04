library(XML)
library(RSelenium)
library(RCurl)





###part to fine-tune datasets
season_00_01_boxscores$X1 <- NULL
season_00_01_boxscores <- dplyr::filter(season_00_01_boxscores, !grepl(x = V1, "[AEIOUaeiou]{1}"))
colnames(season_00_01_boxscores) <- c("jersey_number", "name", "games", "games_started",
                                      "minutes", "points", "fgm_2", "fga_2", "p2_perc",
                                      "fgm_3", "fga_3", "p3_perc",
                                      "fgm", "fga", "fg_perc",
                                      "ftm", "fta", "ft_perc",
                                      "dr", "or", "tr", "as", "st", "to",
                                      "bs", "pf", "ef", "plus_minus", "dbl_dbl",
                                      "season", "team")











team_ids <<- c("421-gie", "415-bon","428-mbc","554-hh",
               "422-bra","425-bay","426-fra","486-fcb",
               "433-lud","446-cra","430-old",
               "541-vec","420-bam","540-wur","477-got", #for wue some are also wur
               "418-ulm","439-bre","483-jen","517-erf", # continue at goettingen
               "432-tub","473-hag","435-art","414-tri",
               "413-ber", "427-ham", "431-koel", "416-lev",
               "445-sch", "434-kar", "475-pad", "447-nuer",
               "484-noer", "536-dues", "540-wue",
               "439-bhv", "432-tueb",
               "477-goet", "423-wuer",
               "417-hag", "432-lud")
               


seasons <<- c("2018-2019","2017-2018","2016-2017","2015-2016",
             "2014-2015","2013-2014","2012-2013","2011-2012",
             "2010-2011","2009-2010","2008-2009","2007-2008",
             "2006-2007","2005-2006","2004-2005","2003-2004",
             "2002-2003","2001-2002","2000-2001")
team_aggregate_data <<- data.frame()
##crawl here

#collect data for one team

remDr <- rsDriver(verbose = T,
                  remoteServerAddr = "localhost",
                  port = 4444L,
                  browser=c("firefox"))
rm <- remDr$client
rm$getStatus()




season <- 5
season_x_boxscore <- data.frame()
for(team in seq(1, length(team_ids))){

try(season_x_boxscore <- dplyr::bind_rows(season_x_boxscore,
                                         crawl_team_boxscore_data(season, team, rm)))

  
}





rm$close()
rm(remDr)
rm(rm)
gc()






team_data_boxscores_filtered <- dplyr::filter(team_data_boxscores_filtered, !grepl(x = V3, "[AEIOUaeiou]{1}"))
team_data_boxscores_filtered <- dplyr::filter(team_data_boxscores_filtered, !grepl(x = V1, "[AEIOUaeiou]{1}"))




test <- crawl_team_data(7,1)
##





test <- team_data_boxscores %>% 
  distinct(season, team)
  






crawl_team_boxscore_data <- function(index_season, index_team, rm){
  
  url <- paste0("https://www.easycredit-bbl.de/de/easycredit-bbl/historie/teams/t/",
                seasons[index_season],
                "/",
                team_ids[index_team],
                "/")
  rm$navigate(url) 
  Sys.sleep(3)
  
  
  rm$findElement(using = "xpath", 
                 '/html/body/div[3]/section[1]/div/div/ul/li[2]/a')$clickElement()
  Sys.sleep(3)
  
  rm$findElement(using = "xpath", 
                 '/html/body/div[3]/section[1]/div/div/ul/li[2]/a')$clickElement()
  Sys.sleep(3)
  
  rm$findElement(using = "xpath", 
                 '/html/body/div[3]/section[1]/div/div/ul/li[2]/a')$clickElement()
Sys.sleep(10)
  
  page <- unlist(rm$getPageSource()) 
  tpage <- htmlParse(page)
  
  
  
  node <- xpathSApply(tpage,"//tr[@style='display: table-row;']")
  boxscore_data <- c()
  for(index in seq(1, length(node))){
    
    player <- as.vector(xpathSApply(node[[index]], ".//td", xmlValue))
    if(length(player == 29)){
      boxscore_data <- rbind(boxscore_data, player)
    }
  }
  boxscore_data <- as.data.frame(boxscore_data)
  boxscore_data$season <- seasons[index_season]
  boxscore_data$team <-  team_ids[index_team]
  return(boxscore_data)
  
}




crawl_team_roster_data <- function(index_season, index_team, rm){
  
#remDr <- rsDriver(verbose = T,
#                    remoteServerAddr = "localhost",
#                    port = 4443L,
#                    browser=c("firefox"))
#rm <- remDr$client
#rm$getStatus()
  url <- paste0("https://www.easycredit-bbl.de/de/easycredit-bbl/historie/teams/t/",
                seasons[index_season],
                "/",
                team_ids[index_team],
                "/")
  rm$navigate(url) 
  Sys.sleep(5)


page <- unlist(rm$getPageSource()) 
tpage <- htmlParse(page)


roster_data <- c()
node <- xpathSApply(tpage, "//tr[@style='display: table-row;']")
print(length(node))

for(index in seq(1, length(node))){
  
  
  final_standing_table <- xpathSApply(tpage, "/html/body/div[3]/section[1]/div/div/div[2]/dl[1]/dd[1]", xmlValue)
  print(final_standing_table)  
  
player <- as.vector(xpathSApply(node[[index]], ".//td", xmlValue))
if(length(player == 10)){
  roster_data <- rbind(roster_data, player)
  }
}
roster_data <- as.data.frame(roster_data)
roster_data$season <- seasons[index_season]
roster_data$team <-  team_ids[index_team]
roster_data$team_standing <- final_standing_table
colnames(roster_data) <- c("jersey_number", "first_name", "last_name", "dob", "age", "height", "weight", "position", "nationality", "last_team",
                           "season", "team", "standing")
team_aggregate_data <<- rbind(team_aggregate_data, 
                              append(as.vector(xpathSApply(tpage, "//div[@class='boxcol5']//span", xmlValue)), c(team_ids[index_team], seasons[index_season])))
colnames(team_aggregate_data) <- c("avg_age", "avg_height", "avg_weight", "national_player", "internation_player")
index <- 0
#rm$close()
#rm(remDr)
#rm(rm)
#gc()
return(roster_data)
}
