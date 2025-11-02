library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(tidyr)

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "cosmo"),
  titlePanel("Australian Bushfire: Temperature & FWI Data Explorer"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput(
        "dataset", "Choose dataset:",
        choices = c("Temperature anomaly" = "climate",
                    "Fire Weather Index (FWI)" = "fwi"),
        selected = "climate"
      ),
      selectInput(
        "time_period", "Time aggregation:",
        choices = c("Yearly" = "yearly", "Monthly" = "monthly", "Quarterly" = "quarterly"),
        selected = "yearly"
      ),
      sliderInput("year_range", "Year range:", min = 1900, max = 2025,
                  value = c(1950, 2020), sep = ""),
      conditionalPanel(
        condition = "input.time_period == 'monthly'",
        selectInput("month", "Month:", choices = month.abb, selected = "Jan")
      ),
      selectInput(
        "plot_type", "Plot type:",
        choices = c("Line" = "line", "Bar" = "bar", "Boxplot (by quarter)" = "boxplot"),
        selected = "line"
      ),
      hr(),
      h4("Data description"),
      uiOutput("data_description")
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel(
          "Trends",
          plotlyOutput("data_plot", height = "500px"),
          h4("How to interpret"),
          uiOutput("interpretation_guide")
        ),
        tabPanel(
          "Statistics",
          verbatimTextOutput("statistical_summary"),
          h4("Notes on statistics"),
          uiOutput("statistical_notes")
        ),
        tabPanel(
          "Comparison",
          plotlyOutput("comparison_plot", height = "500px"),
          h4("What this shows"),
          p("This chart compares temperature anomalies and the Fire Weather Index (FWI) over time to help relate climate conditions to fire weather risk.")
        ),
        tabPanel(
          "About",
          includeMarkdown(system.file("shiny", "about.md", package = "ausbushfire"))
        )
      )
    )
  )
)

server <- function(input, output, session) {

  # helper for validating columns
  .require_cols <- function(df, cols, name) {
    missing <- setdiff(cols, names(df))
    if (length(missing) > 0) {
      stop(sprintf("Dataset '%s' is missing columns: %s",
                   name, paste(missing, collapse = ", ")), call. = FALSE)
    }
  }

  # main reactive: always available (no bindEvent)
  filtered_data <- reactive({
    if (input$dataset == "climate") {
      data <- ausbushfire::climate_data
      value_col   <- "Temperature_Anomaly"
      value_label <- "Temperature anomaly (°C)"
      .require_cols(data, c("Year","Date","Month","Quarter","Temperature_Anomaly"), "climate_data")
    } else {
      data <- ausbushfire::fwi_data
      value_col   <- "FWI"
      value_label <- "Fire Weather Index (FWI)"
      .require_cols(data, c("Year","Date","Month","Quarter","FWI"), "fwi_data")
    }

    data <- dplyr::filter(data, .data$Year >= input$year_range[1],
                          .data$Year <= input$year_range[2])

    if (input$time_period == "monthly") {
      data <- dplyr::filter(data, .data$Month == input$month)
    }

    data[[value_col]] <- suppressWarnings(as.numeric(data[[value_col]]))

    validate(
      need(nrow(data) > 0, "No rows after filtering — adjust the year range or month."),
      need(!all(is.na(data[[value_col]])), paste0("Column '", value_col,
                                                  "' is all NA after coercion — check your data types."))
    )

    list(data = data, value_col = value_col, value_label = value_label)
  })

  # Trends plot
  output$data_plot <- renderPlotly({
    result <- filtered_data()
    data <- result$data
    value_col <- result$value_col
    value_label <- result$value_label

    validate(
      need(nrow(data) > 0, "No rows after filtering."),
      need(value_col %in% names(data), paste0("Column '", value_col, "' not found."))
    )

    data <- dplyr::filter(data, !is.na(.data[[value_col]]))

    if (input$time_period == "yearly") {
      df <- data |>
        dplyr::group_by(Year) |>
        dplyr::summarise(Value = mean(.data[[value_col]], na.rm = TRUE), .groups = "drop")

      validate(need(nrow(df) > 0, "No annual data to plot."))

      if (input$plot_type == "bar") {
        plotly::plot_ly(df, x = ~Year, y = ~Value, type = "bar") |>
          plotly::layout(yaxis = list(title = value_label),
                         xaxis = list(title = "Year"),
                         title = paste(value_label, "— annual averages"))
      } else {
        plotly::plot_ly(df, x = ~Year, y = ~Value, type = "scatter", mode = "lines+markers") |>
          plotly::layout(yaxis = list(title = value_label),
                         xaxis = list(title = "Year"),
                         title = paste(value_label, "— annual averages"))
      }

    } else if (input$time_period == "monthly") {
      validate(need(nrow(data) > 0, "No monthly data to plot."))

      if (input$plot_type == "bar") {
        plotly::plot_ly(data, x = ~Year, y = data[[value_col]], type = "bar") |>
          plotly::layout(yaxis = list(title = value_label),
                         xaxis = list(title = "Year"),
                         title = paste(value_label, "— selected month:", input$month))
      } else if (input$plot_type == "boxplot") {
        plotly::plot_ly(data, x = ~Quarter, y = data[[value_col]], type = "box", color = ~Quarter) |>
          plotly::layout(yaxis = list(title = value_label),
                         xaxis = list(title = "Quarter"),
                         title = paste(value_label, "— quarterly distribution (", input$month, ")"))
      } else {
        plotly::plot_ly(data, x = ~Date, y = data[[value_col]],
                        type = "scatter", mode = "lines+markers") |>
          plotly::layout(yaxis = list(title = value_label),
                         xaxis = list(title = "Date"),
                         title = paste(value_label, "— time series (", input$month, ")"))
      }

    } else { # quarterly
      df <- data |>
        dplyr::group_by(Year, Quarter) |>
        dplyr::summarise(Value = mean(.data[[value_col]], na.rm = TRUE), .groups = "drop")

      validate(need(nrow(df) > 0, "No quarterly data to plot."))

      if (input$plot_type == "boxplot") {
        plotly::plot_ly(df, x = ~Quarter, y = ~Value, type = "box", color = ~Quarter) |>
          plotly::layout(yaxis = list(title = value_label),
                         xaxis = list(title = "Quarter"),
                         title = paste(value_label, "— quarterly distribution"))
      } else if (input$plot_type == "bar") {
        df$YQ <- paste0(df$Year, "-", df$Quarter)
        plotly::plot_ly(df, x = ~YQ, y = ~Value, type = "bar") |>
          plotly::layout(yaxis = list(title = value_label),
                         xaxis = list(title = "Year-Quarter", tickangle = -45),
                         title = paste(value_label, "— quarterly means"))
      } else {
        plotly::plot_ly(type = "scatter", mode = "lines+markers") |>
          plotly::add_trace(data = dplyr::filter(df, Quarter == "Q1"),
                            x = ~Year, y = ~Value, name = "Q1") |>
          plotly::add_trace(data = dplyr::filter(df, Quarter == "Q2"),
                            x = ~Year, y = ~Value, name = "Q2") |>
          plotly::add_trace(data = dplyr::filter(df, Quarter == "Q3"),
                            x = ~Year, y = ~Value, name = "Q3") |>
          plotly::add_trace(data = dplyr::filter(df, Quarter == "Q4"),
                            x = ~Year, y = ~Value, name = "Q4") |>
          plotly::layout(yaxis = list(title = value_label),
                         xaxis = list(title = "Year"),
                         title = paste(value_label, "— quarterly means"))
      }
    }
  })

  # Comparison plot
  output$comparison_plot <- renderPlotly({
    climate <- ausbushfire::climate_data |>
      dplyr::filter(Year >= input$year_range[1], Year <= input$year_range[2]) |>
      dplyr::transmute(Date, Series = "Temperature anomaly",
                       Value = as.numeric(Temperature_Anomaly)) |>
      dplyr::filter(!is.na(Value))

    fwi <- ausbushfire::fwi_data |>
      dplyr::filter(Year >= input$year_range[1], Year <= input$year_range[2]) |>
      dplyr::transmute(Date, Series = "FWI",
                       Value = as.numeric(FWI)) |>
      dplyr::filter(!is.na(Value))

    validate(need(nrow(climate) > 0 || nrow(fwi) > 0, "No data in selected period."))

    cols_2 <- c("Temperature anomaly" = "#E74C3C",
                "FWI"                 = "#3498DB")

    plotly::plot_ly(type = "scatter", mode = "lines") %>%
      plotly::add_trace(data = climate, x = ~Date, y = ~Value,
                        name = "Temperature anomaly",
                        line = list(width = 2, color = cols_2[["Temperature anomaly"]])) %>%
      plotly::add_trace(data = fwi,     x = ~Date, y = ~Value,
                        name = "FWI",
                        line = list(width = 2, color = cols_2[["FWI"]])) %>%
      plotly::layout(title = "Temperature anomaly vs FWI over time",
                     xaxis = list(title = "Date"),
                     yaxis = list(title = "Value"),
                     legend = list(orientation = "h"))
  })

  # Statistical summary
  output$statistical_summary <- renderPrint({
    result <- filtered_data()
    data <- result$data
    value_col <- result$value_col

    cat("Dataset: ", input$dataset, "\n")
    cat("Year range: ", input$year_range[1], "-", input$year_range[2], "\n")
    cat("Rows: ", nrow(data), "\n\n")

    vals <- data[[value_col]]
    cat("Descriptive statistics:\n")
    cat("Min: ", min(vals, na.rm = TRUE), "\n")
    cat("Max: ", max(vals, na.rm = TRUE), "\n")
    cat("Mean: ", round(mean(vals, na.rm = TRUE), 3), "\n")
    cat("Median: ", median(vals, na.rm = TRUE), "\n")
    cat("SD: ", round(sd(vals, na.rm = TRUE), 3), "\n")
    cat("IQR: ", IQR(vals, na.rm = TRUE), "\n\n")

    annual <- data |>
      group_by(Year) |>
      summarise(
        Mean = round(mean(.data[[value_col]], na.rm = TRUE), 3),
        Max  = round(max(.data[[value_col]], na.rm = TRUE), 3),
        Min  = round(min(.data[[value_col]], na.rm = TRUE), 3),
        .groups = "drop"
      )

    cat("Annual summary:\n")
    print(annual)
  })

  # Dynamic data description
  output$data_description <- renderUI({
    if (input$dataset == "climate") {
      tagList(
        p("Temperature anomaly: deviation from a long-term climatological mean."),
        p("• Positive = warmer than baseline"),
        p("• Negative = cooler than baseline"),
        p("• Example source: NASA GISTEMP (processed for this package)")
      )
    } else {
      tagList(
        p("Fire Weather Index (FWI): a composite index indicating fire weather conditions."),
        p("• 0–3: Low"),
        p("• 3–7: Moderate"),
        p("• 7–10: High"),
        p("• 10+: Extreme"),
        p("• Here: synthetic/demo FWI for teaching/illustration")
      )
    }
  })

  # Interpretation guide
  output$interpretation_guide <- renderUI({
    if (input$dataset == "climate") {
      tagList(
        p("Interpreting temperature anomalies:"),
        p("• Persistent upward trends suggest warming"),
        p("• Seasonal patterns reflect natural climate cycles"),
        p("• Large positive spikes can align with heatwaves/drought conditions")
      )
    } else {
      tagList(
        p("Interpreting FWI:"),
        p("• Higher values indicate more favorable fire weather"),
        p("• Sustained high values may imply extended fire seasons"),
        p("• Peaks often coincide with extreme fire weather events")
      )
    }
  })

  # Notes on statistics
  output$statistical_notes <- renderUI({
    tagList(
      p("• Mean: central tendency"),
      p("• SD: dispersion around the mean"),
      p("• IQR: middle 50% spread"),
      p("• Extremes: helps spot outlier years or events")
    )
  })
}
# Run the application
shinyApp(ui, server)
