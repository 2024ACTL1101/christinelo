
## Algorithmic Trading Strategy

## Introduction

In this assignment, you will develop an algorithmic trading strategy by incorporating financial metrics to evaluate its profitability. This exercise simulates a real-world scenario where you, as part of a financial technology team, need to present an improved version of a trading algorithm that not only executes trades but also calculates and reports on the financial performance of those trades.

## Background

Following a successful presentation to the Board of Directors, you have been tasked by the Trading Strategies Team to modify your trading algorithm. This modification should include tracking the costs and proceeds of trades to facilitate a deeper evaluation of the algorithm’s profitability, including calculating the Return on Investment (ROI).

After meeting with the Trading Strategies Team, you were asked to include costs, proceeds, and return on investments metrics to assess the profitability of your trading algorithm.

## Objectives

1. **Load and Prepare Data:** Open and run the starter code to create a DataFrame with stock closing data.

2. **Implement Trading Algorithm:** Create a simple trading algorithm based on daily price changes.

3. **Customize Trading Period:** Choose your entry and exit dates.

4. **Report Financial Performance:** Analyze and report the total profit or loss (P/L) and the ROI of the trading strategy.

5. **Implement a Trading Strategy:** Implement a trading strategy and analyze the total updated P/L and ROI. 

6. **Discussion:** Summarise your finding.


## Instructions

### Step 1: Data Loading

Start by running the provided code cells in the "Data Loading" section to generate a DataFrame containing AMD stock closing data. This will serve as the basis for your trading decisions. First, create a data frame named `amd_df` with the given closing prices and corresponding dates. 

```r
# Load data from CSV file
amd_df <- read.csv("AMD.csv")
# Convert the date column to Date type and Adjusted Close as numeric
amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)
amd_df <- amd_df[, c("date", "close")]
```

#### Plotting the Data
Plot the closing prices over time to visualize the price movement.
```r
plot(amd_df$date, amd_df$close,'l')
```

### Step 2: Trading Algorithm
Implement the trading algorithm as per the instructions. You should initialize necessary variables, and loop through the dataframe to execute trades based on the set conditions.

- Initialize Columns: Start by ensuring dataframe has columns 'trade_type', 'costs_proceeds' and 'accumulated_shares'.
- Change the algorithm by modifying the loop to include the cost and proceeds metrics for buys of 100 shares. Make sure that the algorithm checks the following conditions and executes the strategy for each one:
  - If the previous price = 0, set 'trade_type' to 'buy', and set the 'costs_proceeds' column to the current share price multiplied by a `share_size` value of 100. Make sure to take the negative value of the expression so that the cost reflects money leaving an account. Finally, make sure to add the bought shares to an `accumulated_shares` variable.
  - Otherwise, if the price of the current day is less than that of the previous day, set the 'trade_type' to 'buy'. Set the 'costs_proceeds' to the current share price multiplied by a `share_size` value of 100.
  - You will not modify the algorithm for instances where the current day’s price is greater than the previous day’s price or when it is equal to the previous day’s price.
  - If this is the last day of trading, set the 'trade_type' to 'sell'. In this case, also set the 'costs_proceeds' column to the total number in the `accumulated_shares` variable multiplied by the price of the last day.



```r
# Initialize columns for trade type, costs proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

for (i in 1:nrow(amd_df)) {
# Fill your code here
# Set current_price as the closing price of the current day
  current_price <- amd_df$close[i]
# If previous_price is 0, buy shares and calculate the cost_proceeds and accumulated_shares
if(previous_price == 0) {
  amd_df$trade_type[i] <- "buy"
  amd_df$costs_proceeds <- -(share_size*current_price)
  accumulated_shares <- share_size
} else { 
# Set previous_day_price as the price before the current day
  previous_day_price <- amd_df$close[i-1]
# Buy shares if current_price is less than previous_day_price
  if(current_price < previous_day_price ) {
  amd_df$trade_type[i] <-"buy"
  amd_df$costs_proceeds[i] <- -(current_price*share_size)
  accumulated_shares <- accumulated_shares+share_size
} else {
# Hold shares if current_price is higher than previous_day_price
  amd_df$trade_type[i] <- NA
  amd_df$costs_proceeds[i] <- 0
}
}
# Sell shares on the last day
  if (i == nrow(amd_df)) {
    amd_df$trade_type[i] <- "sell"
    amd_df$costs_proceeds[i] <- accumulated_shares * current_price
    accumulated_shares <- 0
  }
  amd_df$accumulated_shares[i] <- accumulated_shares
  previous_price <- current_price
}

```


### Step 3: Customize Trading Period
- Define a trading period you wanted in the past five years 
```r
# Fill your code here
# Trading period
start_date <- as.Date("2023-01-01")
end_date <- as.Date("2023-12-31")

# Add a subset to the trading period
amd_df <- subset(amd_df, date >= start_date & date <= end_date)

# Initialize columns for trade type, costs proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

for (i in 1:nrow(amd_df)) {
# Set current_price as the closing price of the current day
  current_price <- amd_df$close[i]
# If previous_price is 0, buy shares and calculate the costs proceeds and accumulated shares
if(previous_price == 0) {
  amd_df$trade_type[i] <- "buy"
  amd_df$costs_proceeds <- -(share_size*current_price)
  accumulated_shares <- share_size
} else { 
# Set previous_day_price as the price before the current day
  previous_day_price <- amd_df$close[i-1]
# Buy shares if current_price is less than previous_day_price
  if(current_price < previous_day_price ) {
  amd_df$trade_type[i] <-"buy"
  amd_df$costs_proceeds[i] <- -(current_price*share_size)
  accumulated_shares <- accumulated_shares+share_size
} else {
# Hold the shares if current_price is higher than previous_day_price
  amd_df$trade_type[i] <- NA
  amd_df$costs_proceeds[i] <- 0
}
}
# Sell shares on the last day
  if (i == nrow(amd_df)) {
    amd_df$trade_type[i] <- "sell"
    amd_df$costs_proceeds[i] <- accumulated_shares * current_price
    accumulated_shares <- 0
  }
  amd_df$accumulated_shares[i] <- accumulated_shares
  previous_price <- current_price
}
```


### Step 4: Run Your Algorithm and Analyze Results
After running your algorithm, check if the trades were executed as expected. Calculate the total profit or loss and ROI from the trades.

- Total Profit/Loss Calculation: Calculate the total profit or loss from your trades. This should be the sum of all entries in the 'costs_proceeds' column of your dataframe. This column records the financial impact of each trade, reflecting money spent on buys as negative values and money gained from sells as positive values.
- Invested Capital: Calculate the total capital invested. This is equal to the sum of the 'costs_proceeds' values for all 'buy' transactions. Since these entries are negative (representing money spent), you should take the negative sum of these values to reflect the total amount invested.
- ROI Formula: $$\text{ROI} = \left( \frac{\text{Total Profit or Loss}}{\text{Total Capital Invested}} \right) \times 100$$

```r
# Fill your code here
# Trading period
start_date <- as.Date("2023-01-01")
end_date <- as.Date("2023-12-31")

# Add a subset to the dataframe such that only the trading period is shown
amd_df <- subset(amd_df, date >= start_date & date <= end_date)

# Initialize columns for trade type, costs proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

# Loop codes for all rows
for (i in 1:nrow(amd_df)) {
# Define variables
  current_price <- amd_df$close[i]
# If previous_price is 0, buy shares and calculate the cost_proceeds and accumulated_shares
  if(previous_price == 0) {
    amd_df$trade_type[i] <- "buy"
    amd_df$costs_proceeds[i] <- -(share_size * current_price)
    accumulated_shares <- share_size
  } else { 
# Set previous_day_price as the price on the day before
    previous_day_price <- amd_df$close[i-1]
# Buy shares if the price on the current day is less than the price on the day before
    if(current_price < previous_day_price) {
      amd_df$trade_type[i] <- "buy"
      amd_df$costs_proceeds[i] <- -(current_price * share_size)
      accumulated_shares <- accumulated_shares + share_size
    } else {
# Hold shares if the price on the current day is higher than the price on the previous day
      amd_df$trade_type[i] <- NA
      amd_df$costs_proceeds[i] <- 0
    }
  }
# Sell shares on the last day
  if (i == nrow(amd_df)) {
    amd_df$trade_type[i] <- "sell"
    amd_df$costs_proceeds[i] <- accumulated_shares * current_price
    accumulated_shares <- 0
  }
  amd_df$accumulated_shares[i] <- accumulated_shares
  previous_price <- current_price
}

# Find the Total Profit/Loss by summing all of the costs proceeds
Profit_and_Loss <- sum(amd_df$costs_proceeds, na.rm=TRUE)

# Find the Total Capital Invested by summing the costs proceeds if the trade type is buy
Total_Capital_Invested <- -sum(amd_df$costs_proceeds[amd_df$trade_type == "buy"], na.rm=TRUE)

# Find the ROI using the formula Profit/Loss divided by Total Capital Invested
ROI <- ((Profit_and_Loss)/(Total_Capital_Invested))*100

# Display the results
print(paste("Total Profit/Loss: ", Profit_and_Loss))
print(paste("Total Capital Invested: ", Total_Capital_Invested))
print(paste("ROI: ", ROI, "%"))
```

### Step 5: Profit-Taking Strategy or Stop-Loss Mechanisum (Choose 1)
- Option 1: Implement a profit-taking strategy that you sell half of your holdings if the price has increased by a certain percentage (e.g., 20%) from the average purchase price.
- Option 2: Implement a stop-loss mechanism in the trading strategy that you sell half of your holdings if the stock falls by a certain percentage (e.g., 20%) from the average purchase price. You don't need to buy 100 stocks on the days that the stop-loss mechanism is triggered.


```r
# Fill your code here
# Trading period
start_date <- as.Date("2023-01-01")
end_date <- as.Date("2023-12-31")
# Add a subset to the dataframe such that only the trading period is shown
amd_df <- subset(amd_df, date >= start_date & date <= end_date)

# Initialize columns for trade type, costs proceeds, accumulated shares, accumulated purchasing price, and average purchasing price in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- 0  
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking
amd_df$accumulated_purchasing_price <- 0
amd_df$average_purchasing_price <- 0

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

# Loop codes for all rows
for (i in 1:nrow(amd_df)) {
# Define variables
  current_price <- amd_df$close[i]
# Buy shares on the first day, calculate the accumulated purchasing price and average purchasing price
  if (i==1) { 
    amd_df$trade_type[i] <- "buy"
    amd_df$costs_proceeds[i] <- -amd_df$close[1]* share_size  
    accumulated_shares <- share_size
    amd_df$accumulated_shares[i] <- accumulated_shares
# Accumulated purchasing price can be calculated using the formula: close price*accumulated shares
    amd_df$accumulated_purchasing_price[i] <- -amd_df$costs_proceeds[i]
# Average purchasing price can be calculated using the formula: accumulated purchasing price/accumulated shares
    amd_df$average_purchasing_price[i] <- amd_df$accumulated_purchasing_price[i] / accumulated_shares
  } else {
# From the second day onward  
# Define the average purchasing price that is 20% higher than the average purchasing price on the previous day
    h_average_purchasing_price <- amd_df$average_purchasing_price[i-1] * 1.2
# Sell half of the shares if the current closing price is higher or equal to 20% of the average purchasing price on the previous day, then find the accumulated purchasing price and average purchasing price   
    if (!is.na(h_average_purchasing_price) && current_price >= h_average_purchasing_price) {
      amd_df$trade_type[i] <- "sell"
      amd_df$costs_proceeds[i] <- 0.5 * accumulated_shares * current_price  
# Accumulated purchasing price on the current day is the accumulated purchasing price on the previous day - half of the accumulated shares* average purchasing price on the previous day
      amd_df$accumulated_purchasing_price[i] <- amd_df$accumulated_purchasing_price[i-1] - (0.5*accumulated_shares * amd_df$average_purchasing_price[i-1] )
      accumulated_shares <- accumulated_shares * 0.5
      amd_df$accumulated_shares[i] <- accumulated_shares
# If the accumulated shares is 0, the average purchasing price is 0; otherwise, the average purchasing price is the accumulated purchasing price on the current day/ accumulated shares
      amd_df$average_purchasing_price[i] <- ifelse(accumulated_shares == 0, 0, amd_df$accumulated_purchasing_price[i] / accumulated_shares)  
    } else 
# Buy the shares if the current price is lower than the price on the day before
      if (current_price < amd_df$close[i-1]) {
      amd_df$trade_type[i] <- "buy"
      amd_df$costs_proceeds[i] <- -(share_size * current_price)
      accumulated_shares <- accumulated_shares + share_size
      amd_df$accumulated_shares[i] <- accumulated_shares 
      amd_df$accumulated_purchasing_price[i] <- amd_df$accumulated_purchasing_price[i-1] + (share_size * current_price)
      amd_df$average_purchasing_price[i] <- amd_df$accumulated_purchasing_price[i] / accumulated_shares
    } else {
# Hold the shares if the current price is higher than the previous price but is less than 20% higher than the average purchasing price on the previous day
      amd_df$trade_type[i] <- NA  
      amd_df$costs_proceeds[i] <- 0
      amd_df$accumulated_shares[i] <- accumulated_shares 
      amd_df$accumulated_purchasing_price[i] <- amd_df$accumulated_purchasing_price[i-1]
      amd_df$average_purchasing_price[i] <- amd_df$average_purchasing_price[i-1]
    }
  }
}

# Sell the shares on the last day
 if (i==nrow(amd_df)) {
   amd_df$trade_type[i] <- "sell"
   amd_df$costs_proceeds[i] <- amd_df$accumulated_shares[i-1]*amd_df$close[i]
# The accumulated shares is 0 on the last day since all of the shares are sold
   amd_df$accumulated_shares[i] <- 0
   amd_df$accumulated_purchasing_price[i] <- 0
   amd_df$average_purchasing_price[i] <- 0
 }

# Find the Total Profit/Loss 
Profit_and_Loss_pts <- sum(amd_df$costs_proceeds, na.rm=TRUE)

# Find the Total Capital Invested
Total_Capital_Invested_pts <- -sum(amd_df$costs_proceeds[amd_df$trade_type == "buy"], na.rm=TRUE)

# Find the ROI  
ROI_pts <- (Profit_and_Loss_pts / Total_Capital_Invested_pts) * 100

# Display the results
print(paste("Total Profit/Loss_pts: ", Profit_and_Loss_pts))
print(paste("Total Capital Invested_pts: ", Total_Capital_Invested_pts))
print(paste("ROI_pts: ", ROI_pts, "%"))

```


### Step 6: Summarize Your Findings
- Did your P/L and ROI improve over your chosen period?
- Relate your results to a relevant market event and explain why these outcomes may have occurred.


```r
# Fill your code here and Discuss
# P/L and ROI derived in Step 4
paste("Step 4")
print(paste("Total Profit/Loss: ", Profit_and_Loss))
print(paste("ROI: ", ROI, "%"))
# P/L and ROI derived in Step 5
paste("Step 5")
print(paste("Total Profit/Loss_pts: ", Profit_and_Loss_pts))
print(paste("ROI_pts: ", ROI_pts, "%"))

paste("Discussion")
paste("As calculated in steps 4 and 5, the P/L and ROI derived are 56938.05 and 46.44 % respectively before using any strategy whereas the P/L and ROI are 256402.09 and 26.70 % respectively after implementing the Profit-Taking strategy between 2023-01-01 and 2023-12-31, indicating that both the P/L and ROI worsened after undertaking the stratgey.")
paste ("This scenario can be applied to the oil, gas and energy indsutry. The decline in profits and capital invested stems by the Israel-Hamas war, which started in October 2023. The Israel-Hamas war caused disruptions in energy infrastructure in the Middle East which heavily impacted the supply of gas. As the Middle East is one of the primary oil and gas producers, this posed a threat on the global supply of oil, gas and energy. The effect is evident in December 2023 when there is a general trend of increasing closing prices. Considering how it takes two to three months for investors to react and it is winter in December for the countries in the northern hemisphere, the instability in the oil, gas, and energy industry resulted in a loss of confidence of investors, causing the sales of shares in the majority of December. This means that despite undertaking the strategy, the uncertainty of investors dominated its effectiveness of the strategy, thus the deteriorating ROI. Moreover, as the fall in profits is more significant than the decrease in total capital invested, this suggests that the overall ROI is lowered after implementing the strategy.")  
```

Sample Discussion: On Wednesday, December 6, 2023, AMD CEO Lisa Su discussed a new graphics processor designed for AI servers, with Microsoft and Meta as committed users. The rise in AMD shares on the following Thursday suggests that investors believe in the chipmaker's upward potential and market expectations; My first strategy earned X dollars more than second strategy on this day, therefore providing a better ROI.




