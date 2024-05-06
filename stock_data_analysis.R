# Load necessary libraries
library(quantmod)
library(data.table)
library(ggplot2)

# Specify the stocks and the date range
stocks <- c("AAPL", "GOOGL")
start_date <- as.Date("2023-01-01")
end_date <- as.Date("2023-01-31")

# Function to fetch stock data
get_stock_data <- function(stock, start, end) {
  getSymbols(stock, src = "yahoo", from = start, to = end, auto.assign = FALSE)[, .(Date = index(.), Close = Cl(.))]
}

# Fetch data for all specified stocks
stock_data <- rbindlist(lapply(stocks, get_stock_data, start = start_date, end = end_date), idcol = "Stock")
setDT(stock_data)  # Ensure it's a data.table

# Convert 'Date' to Date type if not already
stock_data[, Date := as.Date(Date)]

# Calculate moving average of the closing price for each stock
stock_data[, `:=`(Moving_Avg = frollmean(Close, n = 3, align = "right")), by = .(Stock)]

# Plotting the data with ggplot2
ggplot(stock_data, aes(x = Date, y = Close, color = Stock)) +
  geom_line() +
  geom_point() +
  geom_line(aes(y = Moving_Avg), linetype = "dashed") +
  labs(title = "Stock Closing Prices and Moving Average",
       x = "Date",
       y = "Closing Price") +
  theme_minimal()
