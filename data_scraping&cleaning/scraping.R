library(httr2)
library(rvest)
library(dplyr)

get_tm_html <- function(url) {
  request(url) |>
    req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/128.0.0.0 Safari/537.36") |>
    req_throttle(rate = 10 / 60) |>
    req_perform() |>
    resp_body_html()
}

clean_raw_table <- function(df) {
  names(df) <- make.names(names(df), unique = TRUE)
  df |> mutate(across(everything(), as.character))
}

#League standings
url_standings <- "https://www.transfermarkt.com/ac-florenz/platzierungen/verein/430"
raw_standings <- get_tm_html(url_standings) |>
  html_element("table.items") |>
  html_table() |>
  clean_raw_table()

saveRDS(raw_standings, "data/raw_standings.rds")

#Cup placements
url_cups <- "https://www.transfermarkt.com/ac-florenz/pokalhistorie/verein/430"
raw_cups <- get_tm_html(url_cups) |>
  html_elements("div.responsive-table table") |>
  html_table() |>
  lapply(clean_raw_table) |>
  bind_rows()

saveRDS(raw_cups, "data/raw_cups.rds")

#Transfer history
url_market <- "https://www.transfermarkt.com/ac-florenz/alletransfers/verein/430"
boxes <- get_tm_html(url_market) |> html_elements("div.box")

raw_market <- lapply(boxes, function(b) {
  titolo  <- b |> html_element("h2.content-box-headline") |> html_text2()
  tabella <- b |> html_element("table")
  
  if (!is.na(titolo) && !inherits(tabella, "xml_missing")) {
    df <- tabella |> html_table() |> clean_raw_table()
    df$Season_Box <- titolo
    return(df)
  }
  return(NULL)
}) |> bind_rows()

saveRDS(raw_market, "data/raw_market.rds")

glimpse(raw_standings)
glimpse(raw_cups)
glimpse(raw_market)