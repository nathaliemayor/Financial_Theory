# Introduction 

This repository contains R code for a Financial Theory (M.A.) course at UNISG.

# Specifications on Files

## [A1_ptf_choice_mvf.R](https://github.com/nathaliemayor/Financial_Theory/blob/main/A1_PTF_choice_MVF.R) - *Return Calculations, Portfolio Choice and Mean-Variance Frontier, Correlation Analysis*

1. Calculation of returns, volatility and Sharpe ratio of single stocks as well as portfolios. Creation of (i) an equally weighted portfolio, (ii) a low volatility portfolio and (iii) a high momentum portfolio and analysis of their relative performance. 
2. Computation of the mean-variance efficient frontier and tangency portfolio given an investment universe. Given an investor's expected utility (preferences), computation of the optimal investor portfolio.
3. Computation of monthly correlation matrices. Computation and visualization of the time-series of mean, max and minimum correlation. Compute and visualization of the monthly time path of an equally weighted portfolio over the same observation horizon.

## [A2_risk_measures.R](https://github.com/nathaliemayor/Financial_Theory/blob/main/A2_risk_measures.R) - *VaR, Target Semivariance, Maximum Drawdown and Tracking error*

1. Computation of empirical and theoretical VaRs of stocks and portfolios.
2. Computation of the semivariance, semi-standard deviation, Sharpe ratio, Sortino ratio. Computation of the max drawdown after each new ATH.
3. Computation of the index tracking error between our own synthetic portfolio and the index and minimization of the tracking error by rebalancing the portfolio. Computation of empirical standard deviation, mean absolute deviation, MaxDrawdown, 95% and 99% 1-month VaR and Expected Shortfall.

## [A3_testing_ap_measures.R](https://github.com/nathaliemayor/Financial_Theory/blob/main/A3_testing_ap_measures.R) - *CAPM, Fama-French model, HML<sub>devil</sub>*

1. Estimation of CAPM equations on the monthly excess return. Performing simple CAPM tests. Computation of the SML and CML for the US market.
2. Performing time-series regressions of various portfolios on the three Fama-French factors: market excess return, SMB (returns of small firms minus big firms) and HML (returns of high book-to-market or value stocks minus returns of low book-to-market or growth stocks).
3. Comparing the Fama-French model with the HML and HML<sub>devil</sub> [[1]](#1) (B/P ratio updates with every incremental price data point,
and not just on the dates when the book value updates) approaches. 

## [A4_interest_rate_models.R](https://github.com/nathaliemayor/Financial_Theory/blob/main/A4_interest_rate_models.R) - *Vasicek model (1977)*[[2]](#2)

we apply Vasicek model (1977) to forecast future short rates. The Vasicek model is one of the earliest no–arbitrage interest rate models, which is based on the idea of mean reverting interest rates. We assume there is no risk premium. 

1. Assuming that the short interest rate follows an AR(1) process, we estimate and plot the expected future short rate based on 30–year horizon and initial short rates of 0.5%, 2%, and 5% (μ = 2%). We consider the two cases of mean reversion: ρ= 0.5 and ρ = 0.95.
2. We estimate and plot separately the intercept *a(m)* and the slope *b(m)* of the long interest rates for the both values of *ρ*.

## [A5_financial_options.R](https://github.com/nathaliemayor/Financial_Theory/blob/main/A5_financial_options.R) - *Option Strategy*

We use option derivatives to construct a portfolio and compare it to some simple buy&hold equity strategy.

1. Based on the expected volatility estimate, we use the option contracts to create a specific payoff structure. We use the Black-Scholes option pricing formula to calculate the price of a call and a put. 
2. Going long with the call and short with the put, we calculate the value of the position and draw the respective payoff diagram. We compute the expected return and expected volatility of the option strategy. 
3. We compare the option portfolio with a simple buy&hold strategy where we directly invest into the risk free asset (lend or borrow without restrictions) and an ETF perfectly tracking the S&P500. 
4. We measure the probability for (i) the option strategy and (ii) for the buy&hold strategy to be profitable at maturity date.



# References

<a id="1">[1]</a> 
 Cliff Asness, Andrea Frazzini (2013). The Devil in HML's Detail. The Journal of Portfolio Management. Volume 9. Number 4.
 
 <a id="2">[2]</a> 
 Vasicek, O., 1977. An equilibrium characterization of the term structure. Journal of financial economics, 5(2), pp.177-188.
