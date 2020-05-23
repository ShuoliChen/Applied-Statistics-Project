# Applied-Statistics-Project
## GARCH model
Generalized AutoRegressive Conditional Heteroskedasticity (GARCH) is a statistical model used in analyzing time-series data where the variance error is believed to be serially autocorrelated. GARCH models assume that the variance of the error term follows an autoregressive moving average process. It is a statistical modeling technique used to help predict the volatility of returns on financial assets.
## Data
The data is the daily closing price of the New York stock exchange(NYSE). It includes the data of NYSE from February 7, 1984 to July 16, 2012. Closing price is the final price at which it trades during regular market hours on any given day. It is considered the most accurate valuation of a stock or other security until trading resumes on the next trading day.
## Code
NYSE_Data_process.R: data preprocessing and ploting

NYSE_Linear_model.R: a suitable ARMA model built for NYSE

NYSE_GARCH.r: garch model built for ARMA's heteroscedastic residue
## other Information
R version 3.6.0 (2019-04-26)

TSA, version:	1.2

forecast, version: 8.12

tseries, version: 1.3.21

zoo, version: 1.8-7

FinTS, version:	0.3-3

ggplot2, version:	3.2.1

timeDate, version:	3043.102

timeSeries, version: 3062.100

fGarch，version: 3042.83.2

parallel，version: 33.6.0

rugarch，version: 1.4-2
