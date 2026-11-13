library(shiny)
library(bslib)
library(plotly)
library(reactable)
library(dplyr)

data_path <- if (file.exists("data/fiorentina_25y.rds")) {
  "data/fiorentina_25y.rds"
} else {
  "../data/fiorentina_25y.rds"
}
df <- readRDS(data_path) |>
  mutate(combined_score = round(points_norm_38 + cup_score, 2))

df_sa <- df |> filter(league == "Serie A")
r_tenure_cup <- round(cor(df_sa$manager_tenure, df_sa$cup_score, use = "complete.obs"), 2)
r_tenure_pts <- round(cor(df_sa$manager_tenure, df_sa$points_norm_38, use = "complete.obs"), 2)
r_sale_cup   <- round(cor(df_sa$prev_top_sale_fee_m, df_sa$cup_score, use = "complete.obs"), 2)
r_sale_pts   <- round(cor(df_sa$prev_top_sale_fee_m, df_sa$points_norm_38, use = "complete.obs"), 2)

x_choices <- c(
  "Managerial Continuity (Consecutive Seasons)" = "manager_tenure",
  "Total Squad Turnover (Arrivals + Departures)" = "total_turnover",
  "Incoming Loans Share (%)" = "loan_arrival_pct",
  "Net Transfer Balance (M €, + = Net Seller)" = "transfer_balance_m",
  "Transfer Expenditure (M €)" = "expenditure_m",
  "Transfer Income (M €)" = "income_m",
  "Previous Season Top Sale (M €, Lag t-1)" = "prev_top_sale_fee_m",
  "Previous Season Net Balance (M €, Lag t-1)" = "prev_transfer_balance_m"
)

y_choices <- c(
  "Serie A Normalized Points (38-Game avg)" = "points_norm_38",
  "Weighted Cup Deep-Run Score" = "cup_score",
  "Final League Rank (Lower = Better)" = "rank",
  "Goal Difference" = "goal_diff"
)

#UI
ui <- page_navbar(
  title = "ACF Fiorentina (2001–2026)",
  fillable = FALSE,
  theme = bs_theme(
    version = 5,
    bootswatch = "zephyr",
    primary = "#482E92",
    secondary = "#8B6914"
  ),
  
  #TAB1
  nav_panel(
    "Overview & Timeline",
    p(
      class = "text-muted mb-2 fw-semibold",
      "The longest unsuccessful run in the club's history: 25 years without a trophy (2001/02 – 2025/26)"
    ),
    layout_column_wrap(
      width = 1/4,
      fill = FALSE,
      class = "mb-3",
      value_box(
        title = "Trophy Drought",
        value = "25 Seasons",
        p("Last title: 2000/01 Coppa Italia", class = "mb-0 small"),
        theme = "primary",
        max_height = "110px"
      ),
      value_box(
        title = "Peak Serie A pts",
        value = paste0(max(df$points_norm_38[df$league == "Serie A"], na.rm = TRUE), " pts"),
        p("2006/07 (73 on-pitch pts, Prandelli)", class = "mb-0 small"),
        max_height = "110px"
      ),
      value_box(
        title = "Cup Finals Lost",
        value = "4 Finals",
        p("Coppa Italia ('14, '23) & UECL ('23, '24)", class = "mb-0 small"),
        max_height = "110px"
      ),
      value_box(
        title = "Cumulative Net Balance",
        value = paste0("€", round(sum(df$transfer_balance_m, na.rm = TRUE), 1), "M"),
        p("Nominal EUR (2001/02 – 2025/26)", class = "mb-0 small"),
        max_height = "110px"
      )
    ),
    card(
      class = "mb-3",
      card_header("25-Year Trajectory: Normalized League Points, Weighted Cups Score & Combined Index"),
      plotlyOutput("timeline_plot", height = "460px")
    ),
    card(
      card_header("Key Analytical Takeaways & Methodological Notes"),
      layout_column_wrap(
        width = 1/3,
        div(
          h6(strong("1. Coaching Continuity Matters")),
          p(paste0("Managerial tenure shows the strongest positive correlation with both weighted cup runs (r = ", r_tenure_cup, ") and Serie A points (r = ", r_tenure_pts, "). Multi-year cycles (Prandelli, Montella, Italiano) consistently outperform short-term resets."
          ))
        ),
        div(
          h6(strong("2. Correlation does not mean Causation")),
          p(paste0("Selling a star player in season t shows near-zero league impact in t+1 (r = ", r_sale_pts, ") and a positive link with cup score (r = ", r_sale_cup, ") driven by post-2017 market inflation coinciding with the introduction of the UEFA Conference League, shortly thereafter."
          ))
        ),
        div(
          h6(strong("3. Data Normalization Standards")),
          p("Pre-2004/05 18-team seasons are scaled to a 38-match calendar. The 2006/07 Calciopoli -15 penalty is restored to measure true on-pitch performance, and cup rounds are weighted by tournament tier.")
        )
      )
    )
  ),
  
  #TAB2
  nav_panel(
    "Correlation Explorer",
    layout_sidebar(
      sidebar = sidebar(
        width = 310,
        selectInput("x_var", "Select Driver (X-Axis):", choices = x_choices, selected = "manager_tenure"),
        selectInput("y_var", "Select Outcome (Y-Axis):", choices = y_choices, selected = "cup_score"),
        hr(),
        checkboxInput("only_serie_a", "Serie A seasons only (exclude Serie B/C2)", value = TRUE),
        checkboxInput("show_trend", "Show linear regression line", value = TRUE),
        hr(),
        uiOutput("stat_summary_box")
      ),
      card(
        card_header(textOutput("scatter_title")),
        plotlyOutput("cor_scatter", height = "520px")
      )
    )
  ),
  
  #TAB3
  nav_panel(
    "Season Archive",
    card(
      card_header("Complete Dataset (2001/02 – 2025/26) — Search & Sort"),
      reactableOutput("archive_table")
    )
  )
)

#SERVER
server <- function(input, output, session) {
  
  #TAB1
  output$timeline_plot <- renderPlotly({
    plot_ly(df, x = ~season) |>
      add_bars(
        y = ~points_norm_38,
        name = "Normalized League Points",
        legendrank = 1,
        marker = list(color = "rgba(72, 46, 146, 0.35)"),
        hovertemplate = "<b>%{x}</b> (%{customdata})<br>Norm. Points: %{y}<extra></extra>",
        customdata = ~paste(league, "-", manager)
      ) |>
      add_trace(
        y = ~cup_score,
        name = "Weighted Cups Score",
        legendrank = 2,
        type = "scatter",
        mode = "lines+markers",
        yaxis = "y2",
        line = list(color = "#C5A059", width = 3),
        marker = list(color = "#8B6914", size = 8),
        hovertemplate = "<b>%{x}</b><br>Cup Score: %{y}<br>Path: %{customdata}<extra></extra>",
        customdata = ~best_cup_run
      ) |>
      add_trace(
        y = ~combined_score,
        name = "Combined Index",
        legendrank = 3,
        type = "scatter",
        mode = "lines+markers",
        line = list(color = "#482E92", width = 2.5, dash = "dash"),
        marker = list(color = "#482E92", size = 6, symbol = "diamond"),
        hovertemplate = "<b>%{x}</b><br>Combined Index: %{y}<extra></extra>"
      ) |>
      layout(
        xaxis = list(title = "Season", tickangle = -45),
        yaxis = list(
          title = list(text = "Normalized Points / Combined Index", standoff = 15),
          automargin = TRUE,
          rangemode = "tozero"
        ),
        yaxis2 = list(
          title = list(text = "Weighted Cups Score", standoff = 15),
          automargin = TRUE,
          overlaying = "y",
          side = "right",
          rangemode = "tozero",
          showgrid = FALSE
        ),
        legend = list(orientation = "h", x = 0.05, y = 1.12),
        margin = list(l = 80, r = 75, t = 45, b = 65)
      )
  })
  
  #TAB2
  filtered_df <- reactive({
    d <- df
    if (input$only_serie_a) {
      d <- d |> filter(league == "Serie A")
    }
    d |>
      mutate(
        x_val = .data[[input$x_var]],
        y_val = .data[[input$y_var]]
      ) |>
      filter(!is.na(x_val), !is.na(y_val))
  })
  
  output$scatter_title <- renderText({
    x_lab <- names(x_choices)[x_choices == input$x_var]
    y_lab <- names(y_choices)[y_choices == input$y_var]
    paste(y_lab, "vs.", x_lab)
  })
  
  output$stat_summary_box <- renderUI({
    d <- filtered_df()
    req(nrow(d) >= 3)
    test <- cor.test(d$x_val, d$y_val)
    r_val <- round(test$estimate, 2)
    p_val <- round(test$p.value, 3)
    rho_val <- suppressWarnings(round(cor(d$x_val, d$y_val, method = "spearman"), 2))
    sig_label <- if (p_val < 0.05) "Statistically significant (p < 0.05)" else "Not statistically significant (p >= 0.05)"
    
    div(
      class = "p-3 bg-light rounded border",
      h6(strong("Live Correlation Stats")),
      p(class = "mb-1", paste0("Sample size: N = ", nrow(d), " seasons")),
      p(class = "mb-1", strong(paste0("Pearson r = ", r_val))),
      p(class = "mb-1", paste0("p-value = ", p_val)),
      p(class = "mb-1", paste0("Spearman ρ = ", rho_val)),
      tags$small(class = "text-muted", sig_label)
    )
  })
  
  output$cor_scatter <- renderPlotly({
    d <- filtered_df()
    x_lab <- names(x_choices)[x_choices == input$x_var]
    y_lab <- names(y_choices)[y_choices == input$y_var]
    
    p <- plot_ly(
      data = d,
      x = ~x_val,
      y = ~y_val,
      type = "scatter",
      mode = "markers+text",
      text = ~season,
      textposition = "top center",
      textfont = list(size = 10, color = "#8B6914"),
      marker = list(color = "#482E92", size = 11, opacity = 0.85),
      hovertemplate = paste0(
        "<b>Season %{text}</b><br>",
        "Manager: %{customdata}<br>",
        x_lab, ": %{x}<br>",
        y_lab, ": %{y}<extra></extra>"
      ),
      customdata = ~manager
    )
    
    if (input$show_trend && nrow(d) >= 3) {
      d_sorted <- d |> arrange(x_val)
      fit <- lm(y_val ~ x_val, data = d_sorted)
      p <- p |>
        add_lines(
          data = d_sorted,
          x = ~x_val,
          y = fitted(fit),
          name = "Regression Trendline",
          line = list(color = "#8B6914", width = 2, dash = "dash"),
          inherit = FALSE
        )
    }
    
    p |>
      layout(
        xaxis = list(title = x_lab),
        yaxis = list(title = y_lab),
        showlegend = FALSE
      )
  })
  
  #TAB3
  output$archive_table <- renderReactable({
    table_df <- df |>
      select(
        Season = season,
        League = league,
        Rank = rank,
        `Norm. Pts` = points_norm_38,
        `Cup Score` = cup_score,
        Manager = manager,
        `Tenure (Yrs)` = manager_tenure,
        Turnover = total_turnover,
        `Loans (%)` = loan_arrival_pct,
        `Net Balance (€M)` = transfer_balance_m,
        `Top Sale` = top_sale_fee_m,
        top_sale_name = top_sale_player,
        `Cup Path` = best_cup_run
      )
    
    reactable(
      table_df,
      searchable = TRUE,
      striped = TRUE,
      highlight = TRUE,
      defaultPageSize = 15,
      columns = list(
        Season = colDef(minWidth = 80, style = list(fontWeight = "bold")),
        League = colDef(minWidth = 90),
        Rank = colDef(minWidth = 65, align = "center"),
        `Norm. Pts` = colDef(minWidth = 95, align = "right"),
        `Cup Score` = colDef(minWidth = 95, align = "right"),
        Manager = colDef(minWidth = 150),
        `Tenure (Yrs)` = colDef(minWidth = 95, align = "center"),
        Turnover = colDef(minWidth = 85, align = "center"),
        `Loans (%)` = colDef(minWidth = 90, align = "right"),
        `Net Balance (€M)` = colDef(
          minWidth = 125,
          align = "right",
          style = function(value) {
            if (is.na(value)) return(list(color = "#6c757d"))
            color <- if (value > 0) "#198754" else if (value < 0) "#dc3545" else "#6c757d"
            list(color = color, fontWeight = "bold")
          }
        ),
        `Top Sale` = colDef(
          minWidth = 190,
          align = "right",
          cell = function(value, index) {
            player <- table_df$top_sale_name[index]
            if (is.na(value) || value == 0) {
              return("No relevant sales")
            }
            paste0(player, " (€", format(value, nsmall = 1), "M)")
          }
        ),
        top_sale_name = colDef(show = FALSE),
        `Cup Path` = colDef(minWidth = 220)
      )
    )
  })
}

shinyApp(ui, server)