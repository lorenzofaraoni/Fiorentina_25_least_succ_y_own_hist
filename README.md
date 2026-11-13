# ACF Fiorentina: 25 Years Without a Trophy (2001–2026)

An end-to-end R data pipeline and interactive _Shiny_ dashboard investigating the longest trophy drought in ACF Fiorentina's history (following the 2000/01 Coppa Italia).

## Live Dashboard
* **Interactive App:** **[Fiorentina25ywoat App](https://lorenzofaraoni-fiorentina25ywoat.share.connect.posit.cloud)**

## Project Structure
* `data_scraping&cleaning/`: Web scraping (`httr2`, `rvest`) and data wrangling (`dplyr`, `stringr`) scripts pulling historical standings, cup runs, and transfer market records from *[Transfermarkt](https://www.transfermarkt.com)*.
* `data/`: Raw and processed `.rds` datasets covering 25 complete seasons (`2001/02` – `2025/26`).
* `app/`: Interactive Shiny web application (`bslib`, `plotly`, `reactable`) with `manifest.json` for reproducible cloud deployment.

## Methodological Choices
* **38-Match Points Normalization:** Pre-2004/05 18-team Serie A seasons (34 matches) are rescaled to a 38-game equivalent.
* **On-Pitch Performance Adjustment:** Restores the 2006/07 Calciopoli -15 point deduction to measure true sporting competitiveness.
* **Weighted Cup Deep-Run Score:** Assigns round points from the Round of 16 onward (excluding preliminary/early rounds) weighted by tournament tier (`UCL 2.0x`, `UEL/UEFA Cup 1.5x`, `UECL 1.2x`, `Coppa Italia 1.0x`).
* **Lag Effects (`t -> t+1`):** Evaluates how major summer sales and net transfer balances affect performance in the subsequent season.
