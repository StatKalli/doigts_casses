library("magrittr")
library("rvest")
library("dplyr")

# function
get_into_list <- function(l, element){
  as.vector(do.call("rbind", lapply(l, "[[", element)))
}

datetime <- Sys.time()

# PLAYERS ----

db_checkpoint <- readRDS("data/clan_players.rds")

url <- c('https://royaleapi.com/clan/P8JYU22U',
         'https://royaleapi.com/clan/YP0QL8QY',
         'https://royaleapi.com/clan/YP0VLRU2')

data_players <- function(u, datetime){
  raw_page <- xml2::read_html(u, encoding = "UTF8")
  clan_name <- gsub("[\n]", "", raw_page %>%
                      rvest::html_nodes("h1") %>%
                      rvest::html_text())
  table <- raw_page %>% rvest::html_nodes("table") %>%
    rvest::html_table() %>%
    as.data.frame()

  players <- data.frame(date = datetime,
                        clan = clan_name,
                        player = rep(NA, dim(table)[1]),
                        last_co = NA,
                        role = NA,
                        tag = NA,
                        trophies = NA,
                        level = NA,
                        donated = NA,
                        received =NA)

  # Cleaning table
  players[["player"]] <- strsplit(table[,2], "[\n]") %>%
    get_into_list(1)
  players[["player"]] <- gsub('\\[*email.protected*\\]', "M@", players[["player"]])

  players[["last_co"]] <- strsplit(table[,2], "[\n]") %>%
    get_into_list(3)

  players[["role"]] <- table[["Role"]]
  players[["tag"]] <- table[["Tag"]]
  players[["trophies"]] <- as.numeric(gsub(",", "", table[["Trophies"]]))
  players[["level"]] <- table[["Level"]]
  players[["donated"]] <- as.numeric(gsub(",", "", table[["Donated"]]))
  players[["received"]] <- as.numeric(gsub(",", "", table[["Received"]]))

  players
}

clan3e <- data_players(url[1], datetime = datetime)
clan2e <- data_players(url[2], datetime = datetime)
clan1e <- data_players(url[3], datetime = datetime)

db_clan <- unique(rbind(db_checkpoint, clan3e, clan2e, clan1e))
db_clan <- dplyr::arrange(db_clan, datetime, desc(clan))

saveRDS(db_clan, "data/clan_players.rds")

# WAR CLAN ----

db_checkpoint <- readRDS("data/clan_wars.rds")

url_war <- paste0(url, '/war/analytics')

data_warclan <- function(u, datetime){
  raw_page <- xml2::read_html(u, encoding = "UTF8")
  clan_name <- gsub("[\n]", "", raw_page %>% html_nodes("h1") %>% html_text())

  table <- raw_page %>% rvest::html_nodes("table") %>%
    rvest::html_table()
  table <- as.data.frame(table[[1L]])

  warclan <- data.frame(date = rep(datetime, dim(table)[1L]),
                        clan = clan_name)

  warclan <- cbind(warclan, table)

  # Cleaning table
  warclan[["Player"]] <- gsub('\\[*email.protected*\\]', "M@", warclan[["Player"]])

  names(warclan) <- c("date", "clan", "player", "member", "participation", "contribution", names(warclan)[7:dim(warclan)[2]])
  names(warclan) <- gsub("\\s\\-\\\n", "\\_", names(warclan))

  warclan
}

warclan3e <- data_warclan(url_war[1], datetime = datetime)
warclan2e <- data_warclan(url_war[2], datetime = datetime)
warclan1e <- data_warclan(url_war[3], datetime = datetime)

db_warclan <- dplyr::bind_rows(db_checkpoint, warclan3e, warclan2e, warclan1e) %>% unique()
db_warclan <- dplyr::arrange(db_warclan, datetime, desc(clan))

# check member
maxdate <- max(db_clan$date)
db_warclan[db_warclan$player %in% unique(db_clan[db_clan$date == maxdate, "player"]), "member"] <- TRUE
db_warclan[is.na(db_warclan$member), "member"] <- FALSE

saveRDS(db_warclan, "data/clan_wars.rds")

# CURRENT WAR CLAN ----

db_checkpoint <- readRDS("data/clan_warcurrent.rds")

url_war <- paste0(url, '/war/race')

data_warcurrent <- function(u, datetime){
  raw_page <- read_html(u, encoding = "UTF8")
  clan_name <- gsub("[\n]", "", raw_page %>% html_nodes("h1") %>% html_text())

  table <- raw_page %>% rvest::html_nodes("table") %>%
    rvest::html_table()

  if(length(table) > 0){
    table <- as.data.frame(table[[2L]])

    warclan <- data.frame(date = datetime,
                          clan = clan_name,
                          player = rep(NA, dim(table)[1]),
                          role = NA,
                          boat_attack = NA,
                          repair = NA,
                          fame = NA,
                          contribution = NA)

    # Cleaning table
    warclan[["player"]] <- strsplit(table[,2], "[\n]") %>%
      get_into_list(1)
    warclan[["player"]] <- gsub('\\[*email.protected*\\]', "M@", warclan[["player"]])

    warclan[["role"]] <- strsplit(table[,2], "[\n]") %>%
      get_into_list(4)

    warclan[["boat_attack"]] <- strsplit(table[,2], "[\n]") %>%
      get_into_list(8)

    warclan[["repair"]] <- strsplit(table[,2], "[\n]") %>%
      get_into_list(9)

    warclan[["fame"]] <- strsplit(table[,2], "[\n]") %>%
      get_into_list(10)

    warclan[["contribution"]] <- strsplit(table[,2], "[\n]") %>%
      get_into_list(11)

    warclan[["boat_attack"]] <- as.numeric(gsub(",", "", warclan[["boat_attack"]]))
    warclan[["repair"]] <- as.numeric(gsub(",", "", warclan[["repair"]]))
    warclan[["fame"]] <- as.numeric(gsub(",", "", warclan[["fame"]]))
    warclan[["contribution"]] <- as.numeric(gsub(",", "", warclan[["contribution"]]))
  } else {
    warclan <- data.frame(NULL)
  }

  warclan
}

warclan3e <- data_warcurrent(url_war[1], datetime = datetime)
warclan2e <- data_warcurrent(url_war[2], datetime = datetime)
warclan1e <- data_warcurrent(url_war[3], datetime = datetime)

db_warclan <- dplyr::bind_rows(db_checkpoint, warclan3e, warclan2e, warclan1e) %>% unique()
db_warclan <- dplyr::arrange(db_warclan, datetime, desc(clan))

saveRDS(db_warclan, "data/clan_warcurrent.rds")
