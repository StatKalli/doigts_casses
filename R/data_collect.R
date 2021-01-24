
# function
get_into_list <- function(l, element){
  as.vector(do.call("rbind", lapply(l, "[[", element)))
}


#' @import rvest
#' @importFrom magrittr `%>%`
#' @export
data_collect <- function(){
  datetime <- Sys.time()

  # PLAYERS ----

  db_checkpoint <- readRDS("data/clan_players.rds")

  url <- c('https://royaleapi.com/clan/P8JYU22U',
           'https://royaleapi.com/clan/YP0QL8QY',
           'https://royaleapi.com/clan/YP0VLRU2')

  data_players <- function(u, datetime){
    raw_page <- read_html(u, encoding = "UTF8")
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
    players[["donated"]] <- table[["Donated"]]
    players[["received"]] <- table[["Received"]]

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

  url <- paste0(url, '/war/analytics')

  data_warclan <- function(u, datetime){
    raw_page <- read_html(u, encoding = "UTF8")
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

  warclan3e <- data_warclan(url[1], datetime = datetime)
  warclan2e <- data_warclan(url[2], datetime = datetime)
  warclan1e <- data_warclan(url[3], datetime = datetime)

  db_warclan <- dplyr::bind_rows(db_checkpoint, warclan3e, warclan2e, warclan1e) %>% unique()
  db_warclan <- dplyr::arrange(db_warclan, datetime, desc(clan))

  # check member
  maxdate <- max(db_clan$date)
  db_warclan[db_warclan$player %in% unique(db_clan[db_clan$date == maxdate, "player"]), "member"] <- TRUE
  db_warclan[is.na(db_warclan$member), "member"] <- FALSE

  saveRDS(db_warclan, "data/clan_wars.rds")
}

