# Introduction 

This repository contains R code for a Financial Theory (M.A.) course at UNISG.

# Specfications on Files

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

# References


<a id="1">[1]</a> 
 Cliff Asness, Andrea Frazzini (2013). The Devil in HML's Detail. The Journal of Portfolio Management. Volume 9. Number 4.
