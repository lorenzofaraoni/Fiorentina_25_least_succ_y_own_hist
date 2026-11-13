library(httr2)
library(rvest)
library(dplyr)

scarica_tabella_tm <- function(url) {
  
  answer <- request(url) |>
    req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/128.0.0.0 Safari/537.36") |>
    req_throttle(rate = 10 / 60) |> 
    req_perform()
  
  tab <- answer |>
    resp_body_html() |>
    html_element("table.items") |>
    html_table()
  
  return(tab)
}

url_standings <- "https://www.transfermarkt.it/ac-florenz/platzierungen/verein/430"
url_market     <- "https://www.transfermarkt.it/ac-florenz/transferbilanz/verein/430"
url_cups       <- "https://www.transfermarkt.it/ac-florenz/pokalhistorie/verein/430"

raw_standings <- download_tab_tm(url_league)
raw_market     <- download_tab_tm(url_market)
raw_cups       <- download_tab_tm(url_cups)

saveRDS(raw_standings, "data/raw_standings.rds")
saveRDS(raw_market, "data/raw_market.rds")
saveRDS(raw_cups, "data/raw_cups.rds")

glimpse(raw_standings)
glimpse(raw_market)
glimpse(raw_cups)