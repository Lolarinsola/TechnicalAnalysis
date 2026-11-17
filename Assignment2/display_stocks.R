# display_stocks.R
# Assignment 2 - Technical Analysis using R (Preliminary Stage)
# Displays the imported stock data, the calculated statistics and charts.
#
# Run with the working directory set to the Assignment2 folder:
#   source("display_stocks.R")

source("stock_functions.R")

# ---- Display utilities ------------------------------------------------------

# Show the first and last rows of one stock's data frame
display_stock_data <- function(df, symbol, n = 5) {
  cat("\n==========", symbol, "==========\n")
  cat("Rows:", nrow(df), " | From", format(min(df$Date)),
      "to", format(max(df$Date)), "\n\n")
  cat("First", n, "rows:\n")
  print(head(df, n), row.names = FALSE)
  cat("\nLast", n, "rows:\n")
  print(tail(df, n), row.names = FALSE)
}

# Show the latest quote in a Yahoo Finance style summary line
display_latest_quote <- function(df, symbol) {
  last <- tail(df, 1)
  prev <- tail(df, 2)[1, ]
  change <- last$Close - prev$Close
  pct <- 100 * change / prev$Close
  cat(sprintf("%-6s Close: %8.2f   Change: %+7.2f (%+.2f%%)   Volume: %s\n",
              symbol, last$Close, change, pct,
              format(last$Volume, big.mark = ",")))
}

# Line chart: closing price, moving average and mean
plot_stock_statistics <- function(df, symbol, ma, ma_window = 20) {
  plot(df$Date, df$Close, type = "l", col = "steelblue", lwd = 1.5,
       xlab = "Date", ylab = "Close price (USD)",
       main = paste(symbol, "- Close price with", ma_window, "day moving average"))
  lines(df$Date, ma, col = "red", lwd = 2)
  abline(h = mean(df$Close, na.rm = TRUE), col = "darkgreen", lty = 2)
  legend("topleft", bty = "n",
         legend = c("Close", paste0(ma_window, "-day MA"), "Mean"),
         col = c("steelblue", "red", "darkgreen"),
         lty = c(1, 1, 2), lwd = c(1.5, 2, 1))
}

# Candlestick chart with moving average (quantmod)
plot_candlestick <- function(df, symbol, ma_window = 20) {
  x <- xts::xts(df[, c("Open", "High", "Low", "Close", "Volume")],
                order.by = df$Date)
  chartSeries(x, name = symbol, theme = chartTheme("white"),
              TA = paste0("addSMA(n=", ma_window, ")"))
}

# ---- Run the analysis -------------------------------------------------------

ma_window <- 20
stocks <- load_stock_data("portfolio.txt")

cat("Loaded", length(stocks), "stocks:", paste(names(stocks), collapse = ", "), "\n")

# 1. Data frames
for (sym in names(stocks)) display_stock_data(stocks[[sym]], sym)

# 2. Latest quotes
cat("\n---------- Latest quotes ----------\n")
for (sym in names(stocks)) display_latest_quote(stocks[[sym]], sym)

# 3. Statistics table
stats <- calculate_all_statistics(stocks, ma_window = ma_window)
cat("\n---------- Summary statistics (Close price) ----------\n")
print(stats$table, row.names = FALSE)

# 4. Visualisations
op <- par(mfrow = c(2, 3))
for (sym in names(stocks)) {
  plot_stock_statistics(stocks[[sym]], sym, stats$moving_averages[[sym]], ma_window)
}
par(op)

for (sym in names(stocks)) plot_candlestick(stocks[[sym]], sym, ma_window)
