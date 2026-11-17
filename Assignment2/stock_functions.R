# stock_functions.R
# Assignment 2 - Technical Analysis using R (Preliminary Stage)
# Functions to load stock data and compute basic statistics.
#
# Usage: source("stock_functions.R")   (working directory = Assignment2 folder)

library(quantmod)   # getSymbols() for downloading stock data
library(TTR)        # SMA() for the moving average

# ---------------------------------------------------------------------------
# read_portfolio(): read stock symbols from a text file, one symbol per line
# ---------------------------------------------------------------------------
read_portfolio <- function(path = "portfolio.txt") {
  if (!file.exists(path)) stop("Portfolio file not found: ", path)
  symbols <- trimws(readLines(path, warn = FALSE))
  symbols <- toupper(symbols[symbols != ""])
  if (length(symbols) == 0) stop("No stock symbols found in ", path)
  unique(symbols)
}

# ---------------------------------------------------------------------------
# load_stock_data(): import data for every symbol in portfolio.txt using
# quantmod. Returns a named list with ONE DATA FRAME PER SYMBOL.
# ---------------------------------------------------------------------------
load_stock_data <- function(portfolio_file = "portfolio.txt",
                            from = Sys.Date() - 365,
                            to = Sys.Date()) {
  symbols <- read_portfolio(portfolio_file)
  stocks <- list()

  for (sym in symbols) {
    raw <- tryCatch(
      getSymbols(sym, src = "yahoo", from = from, to = to, auto.assign = FALSE),
      error = function(e) {
        warning("Could not download ", sym, ": ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(raw)) next

    df <- data.frame(Date = index(raw), coredata(raw))
    names(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
    stocks[[sym]] <- df
  }

  if (length(stocks) == 0) stop("No stock data could be loaded.")
  stocks
}

# ---------------------------------------------------------------------------
# get_mode(): base R has no statistical mode function, so we write one.
# Prices are continuous, so values are rounded before finding the most
# frequent one. If several values tie, the first (lowest) is returned.
# ---------------------------------------------------------------------------
get_mode <- function(x, digits = 0) {
  x <- round(x[!is.na(x)], digits)
  counts <- table(x)
  as.numeric(names(counts)[which.max(counts)])
}

# ---------------------------------------------------------------------------
# calculate_statistics(): statistics for ONE stock's data frame.
# Returns a list with:
#   summary        - one-row data frame (mean, median, mode, sd, latest MA)
#   moving_average - the full moving average series (same length as the data)
# ---------------------------------------------------------------------------
calculate_statistics <- function(df, symbol = "", ma_window = 20) {
  close <- df$Close
  ma <- SMA(close, n = ma_window)

  summary_df <- data.frame(
    Symbol       = symbol,
    Observations = length(close),
    Mean         = round(mean(close, na.rm = TRUE), 2),
    Median       = round(median(close, na.rm = TRUE), 2),
    Mode         = get_mode(close),
    StdDev       = round(sd(close, na.rm = TRUE), 2),
    Latest_MA    = round(tail(ma[!is.na(ma)], 1), 2)
  )
  names(summary_df)[names(summary_df) == "Latest_MA"] <-
    paste0("MA", ma_window, "_Latest")

  list(summary = summary_df, moving_average = ma)
}

# ---------------------------------------------------------------------------
# calculate_all_statistics(): run calculate_statistics() for every stock.
# Returns list(table = combined data frame, moving_averages = named list)
# ---------------------------------------------------------------------------
calculate_all_statistics <- function(stocks, ma_window = 20) {
  results <- lapply(names(stocks), function(sym) {
    calculate_statistics(stocks[[sym]], symbol = sym, ma_window = ma_window)
  })
  names(results) <- names(stocks)

  list(
    table = do.call(rbind, lapply(results, function(r) r$summary)),
    moving_averages = lapply(results, function(r) r$moving_average)
  )
}
