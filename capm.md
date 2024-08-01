
# CAPM Analysis

## Introduction

In this assignment, you will explore the foundational concepts of the Capital Asset Pricing Model (CAPM) using historical data for AMD and the S&P 500 index. This exercise is designed to provide a hands-on approach to understanding how these models are used in financial analysis to assess investment risks and returns.

## Background

The CAPM provides a framework to understand the relationship between systematic risk and expected return, especially for stocks. This model is critical for determining the theoretically appropriate required rate of return of an asset, assisting in decisions about adding assets to a diversified portfolio.

## Objectives

1. **Load and Prepare Data:** Import and prepare historical price data for AMD and the S&P 500 to ensure it is ready for detailed analysis.
2. **CAPM Implementation:** Focus will be placed on applying the CAPM to examine the relationship between AMD's stock performance and the overall market as represented by the S&P 500.
3. **Beta Estimation and Analysis:** Calculate the beta of AMD, which measures its volatility relative to the market, providing insights into its systematic risk.
4. **Results Interpretation:** Analyze the outcomes of the CAPM application, discussing the implications of AMD's beta in terms of investment risk and potential returns.

## Instructions

### Step 1: Data Loading

- We are using the `quantmod` package to directly load financial data from Yahoo Finance without the need to manually download and read from a CSV file.
- `quantmod` stands for "Quantitative Financial Modelling Framework". It was developed to aid the quantitative trader in the development, testing, and deployment of statistically based trading models.
- Make sure to install the `quantmod` package by running `install.packages("quantmod")` in the R console before proceeding.

```r
# Set start and end dates
start_date <- as.Date("2019-05-20")
end_date <- as.Date("2024-05-20")

# Load data for AMD, S&P 500, and the 1-month T-Bill (DTB4WK)
amd_data <- getSymbols("AMD", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
gspc_data <- getSymbols("^GSPC", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
rf_data <- getSymbols("DTB4WK", src = "FRED", from = start_date, to = end_date, auto.assign = FALSE)

# Convert Adjusted Closing Prices and DTB4WK to data frames
amd_df <- data.frame(Date = index(amd_data), AMD = as.numeric(Cl(amd_data)))
gspc_df <- data.frame(Date = index(gspc_data), GSPC = as.numeric(Cl(gspc_data)))
rf_df <- data.frame(Date = index(rf_data), RF = as.numeric(rf_data[,1]))  # Accessing the first column of rf_data

# Merge the AMD, GSPC, and RF data frames on the Date column
df <- merge(amd_df, gspc_df, by = "Date")
df <- merge(df, rf_df, by = "Date")
```

#### Data Processing 
```r
colSums(is.na(df))
# Fill N/A RF data
df <- df %>%
  fill(RF, .direction = "down") 
```

### Step 2: CAPM Analysis

The Capital Asset Pricing Model (CAPM) is a financial model that describes the relationship between systematic risk and expected return for assets, particularly stocks. It is widely used to determine a theoretically appropriate required rate of return of an asset, to make decisions about adding assets to a well-diversified portfolio.

#### The CAPM Formula
The formula for CAPM is given by:

$$
E(R_i) = R_f + \beta_i (E(R_m) - R_f)
$$

Where:

- $E(R_i)$ is the expected return on the capital asset,
- $R_f$ is the risk-free rate,
- $\beta_i$ is the beta of the security, which represents the systematic risk of the security,
- $E(R_m)$ is the expected return of the market.



#### CAPM Model Daily Estimation

- **Calculate Returns**: First, we calculate the daily returns for AMD and the S&P 500 from their adjusted closing prices. This should be done by dividing the difference in prices between two consecutive days by the price at the beginning of the period.
  
$$
\text{Daily Return} = \frac{\text{Today's Price} - \text{Previous Trading Day's Price}}{\text{Previous Trading Day's Price}}
$$

```r
# Calculate daily returns for AMD and S&P 500
df <- df %>%
  mutate(AMD_Daily_Return = (AMD - lag(AMD)) / lag(AMD),
         GSPC_Daily_Return = (GSPC - lag(GSPC)) / lag(GSPC))
```

- **Calculate Risk-Free Rate**: Calculate the daily risk-free rate by conversion of annual risk-free Rate. This conversion accounts for the compounding effect over the days of the year and is calculated using the formula:
  
$$
\text{Daily Risk-Free Rate} = \left(1 + \frac{\text{Annual Rate}}{100}\right)^{\frac{1}{360}} - 1
$$

```r
#Add a new column daily_risk-free rate
#Calculate the daily risk-free rate 
#Annual rate can be derived from the RF in df
df <- df%>%
  mutate(daily_risk_free_rate = (1+RF/100)^(1/360)-1)
```


- **Calculate Excess Returns**: Compute the excess returns for AMD and the S&P 500 by subtracting the daily risk-free rate from their respective returns.

```r
#Add two new columns AMD_excess_returns and GSPC_excess_returns
#Calculate the excess rate by finding the difference between daily return and daily risk-free rate
df <- df %>%
  mutate(AMD_excess_returns = (AMD_Daily_Return - daily_risk_free_rate),
         GSPC_excess_returns= (GSPC_Daily_Return - daily_risk_free_rate))
```


- **Perform Regression Analysis**: Using linear regression, we estimate the beta (\(\beta\)) of AMD relative to the S&P 500. Here, the dependent variable is the excess return of AMD, and the independent variable is the excess return of the S&P 500. Beta measures the sensitivity of the stock's returns to fluctuations in the market.

```r
#Let lrModel be the linear regression model
lrModel <- lm(AMD_excess_returns~GSPC_excess_returns,data=df)
summary(lrModel)
```


#### Interpretation

What is your \(\beta\)? Is AMD more volatile or less volatile than the market?

**Answer:**
beta <-summary(lrModel)$coefficients[2,1]
beta
#The value of beta is 1.5699987. The volatility of AMD can be determined by beta since beta represents the systematic risk of the security, meaning it measures the sensitivity to the stock's returns to flunctuations in the market. AMD is more volatile than the market if beta is greater than 1 as it suggests that the stock's returns change more in response to market movements. Therefore, AMD is more volatile than the market.

#### Plotting the CAPM Line
Plot the scatter plot of AMD vs. S&P 500 excess returns and add the CAPM regression line.

```r
plot<-ggplot(df, aes(x=GSPC_excess_returns, y=AMD_excess_returns))+
  geom_point()+ #Adds the scatterplot
  geom_smooth(method="lm", col="red", )+ #Adds regression line
  labs(title="AMD vs S&P excess returns", x="S&P", y="AMD")

plot
```

### Step 3: Predictions Interval
Suppose the current risk-free rate is 5.0%, and the annual expected return for the S&P 500 is 13.3%. Determine a 90% prediction interval for AMD's annual expected return.



**Answer:**

```r
#Calculate the annual expected return for AMD
#Define risk free rate
Rf <- 0.05
#Define the annual expected return for S&P 500
#Define beta
beta <-summary(lrModel)$coefficients[2,1]
ERm <- 0.133
#Find the annual expected return for AMD
ERi <- Rf+beta*(ERm-Rf)
#Number of observations
n<-length(df$GSPC_excess_returns)
#Convert current risk free rate to daily risk free rate
drfr = (1+Rf)^(1/360)-1
#Mean of daily return for S&P 500
avg_gspc_return <- mean(df$GSPC_excess_returns, na.rm = T)
#Standard error of the estimate
se<-sqrt(sum(residuals(lrModel)^2)/(n-1-1))
#Sum of squares of the daily return of S&P 500
SSX<-sum((df$GSPC_excess_returns-avg_gspc_return)^2, na.rm = T)
#Calculate the standard error of the forecast
sf<- se*sqrt(1+1/n+(((ERm/252)-drfr)-avg_gspc_return)^2/SSX)
#Calculate the annual standard error for prediction
ansf<- sqrt(252)*sf
#90% prediction interval
alpha<- 0.10
#tvalue for 90% confidence interval level with n-1-1 degrees of freedom
tvalue<-qt(1-alpha/2, df=n-2)
#Prediction interval
lowerbound <-ERi-tvalue*ansf
upperbound <- ERi+tvalue*ansf
#Print the results
cat("The 90% prediction interval is[", round(lowerbound, 2), ",", round(upperbound, 2),"]. The wide interval further reiterates the high volatility of AMD's annual expected return.\n")          
```
