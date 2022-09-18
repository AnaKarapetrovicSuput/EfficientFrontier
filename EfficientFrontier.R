library(tidyquant) # To download the data
library(plotly) # To create interactive charts
library(timetk) # To manipulate the data series
library(tidyr)
library(dplyr)

#Choose stocks
tick = c("GIS", "SPGI", "WMT", "GE", "PFE")

#Get quantitative data in tibble format
efficient_frontier <- function(tick){
  obj <- NULL
  price_data <- tq_get(tick,
                       from = '2015-01-01',
                       to = '2021-12-31',
                       get = 'stock.prices') #Get stock price from yahoo finance
  
  #New variable "year" in price_data
  price_data_year <-price_data %>%
    mutate(year = substring(date, 1,4))
  
  #Calculate mean price pear year
  means_year <- price_data_year %>%
    group_by(symbol,year) %>%
    summarise_at(vars(adjusted), list(~mean(.)))#, na.rm=TRUE)))
  #Remove stocks with missing data
  stocks_na <-means_year%>%
    group_by(symbol)%>%
    summarise_at(vars(adjusted), list(~any(is.na(.))))
  means_year = means_year[means_year[["symbol"]] %in% stocks_na[["symbol"]][!stocks_na[["adjusted"]]],]
  
  ##Dismiss stocks stocks with greater value in the first year than in the last 
  means_year_min <- means_year[means_year[["year"]]==min(means_year[["year"]]),]
  means_year_max <- means_year[means_year[["year"]]==max(means_year[["year"]]),]
  selected_vars <- means_year_max[["symbol"]][means_year_max[["adjusted"]] > means_year_min[["adjusted"]]]
  
  if(length(selected_vars)<=1){
    obj = (selected_vars)
  }else{
    
    #Select stocks with greater value in the last than in the first year
    price_data_selected = price_data[price_data[["symbol"]] %in% selected_vars,]
    #Group price_data by symbol (=tick)
    log_ret_tidy <- price_data_selected %>%
      group_by(symbol) %>%
      #Calculates daily logarithmic periodic return
      tq_transmute(select = adjusted,
                   mutate_fun = periodReturn,
                   period = 'daily',
                   col_rename = 'ret',
                   type = 'log')
    #head(log_ret_tidy)
    
    #Stocks as colnames and date as rownames
    log_ret_xts <- log_ret_tidy %>%
      spread(symbol, value = ret) %>%
      tk_xts(date_var = date)
    #head(log_ret_xts)
    
    #Calculate daily mean
    mean_ret <- colMeans(log_ret_xts)
    #print(round(mean_ret, 5))
    
    #Calculate covariance matrix and anualize it
    cov_mat <- cov(log_ret_xts) * 252
    print(round(cov_mat,4))
    
    #Calculate weights
    n = ncol(cov_mat)
    wts = matrix(NA, ncol = n, nrow = 11^n)
    for (i in 1:n){
      wts[,i] <- rep(seq(0,1, 0.1), each = 11^(n-i), times = 11^(i-1))
    }
    wts <- wts[rowSums(wts)==1,]
    wts
    
    port_returns <- vector(length = dim(wts)[1])
    port_risk <- vector(length = dim(wts)[1])
    sharpe_ratio <- vector(length = dim(wts)[1])
    for (i in 1:dim(wts)[1]) {
      
      # Portfolio returns
      
      port_ret <- sum(wts[i,] * mean_ret)
      port_ret <- ((port_ret + 1)^252) - 1
      # Storing Portfolio Returns values
      port_returns[i] <- port_ret
      
      
      # Creating and storing portfolio risk
      port_sd <- sqrt(t(wts[i,]) %*% (cov_mat  %*% (wts[i,])))
      port_risk[i] <- port_sd
      
      # Creating and storing Portfolio Sharpe Ratios
      # Assuming 0% Risk free rate
      
      sr <- port_ret/port_sd
      sharpe_ratio[i] <- sr
      
    }
    
    # Storing the values in the table
    portfolio_values <- tibble(Return = port_returns,
                               Risk = port_risk,
                               SharpeRatio = sharpe_ratio)
    
    #Converting matrix to tibble
    all_wts <- tk_tbl(wts, preserve_index = FALSE)
    colnames(all_wts) <- colnames(log_ret_xts)
    
    # Combing all the values together
    portfolio_values <- tk_tbl(cbind(all_wts, portfolio_values), preserve_index = FALSE)
    
    #Minimum variance portfolio
    min_var <- portfolio_values[which.min(portfolio_values$Risk),]
    obj$min_var = min_var
    
    #Plot weights
    vars <- names(min_var)[names(min_var) %in% c("Return", "Risk", "SharpeRatio") == FALSE]
    obj$vars = vars
    p <- min_var %>%
      gather(vars, key = Asset, ##sollte hier alles nehmen, aber es nimmt nur GIS und PFE
             value = Weights) %>%
      mutate(Asset = as.factor(Asset)) %>%
      ggplot(aes(x = Asset, y = Weights, fill = Asset)) +
      geom_bar(stat = 'identity') +
      theme_minimal() +
      labs(x = 'Assets', y = 'Weights', title = "Minimum Variance Portfolio Weights") +
      scale_y_continuous(labels = scales::percent) 
    
    #ggplotly(p)
    obj$plot_minvar = p
    
    #tangency portfolio (the portfolio with highest sharpe ratio)
    max_sr <- portfolio_values[which.max(portfolio_values$SharpeRatio),]
    obj$max_sr = max_sr
    
    p <- max_sr %>%
      gather(vars, key = Asset,
             value = Weights) %>%
      mutate(Asset = as.factor(Asset)) %>%
      ggplot(aes(x = Asset, y = Weights, fill = Asset)) +
      geom_bar(stat = 'identity') +
      theme_minimal() +
      labs(x = 'Assets', y = 'Weights', title = "Tangency Portfolio Weights") +
      scale_y_continuous(labels = scales::percent) 
    
    #ggplotly(p)
    obj$plot_maxsr = p
    
    #Efficient frontier
    p <- portfolio_values %>%
      ggplot(aes(x = Risk, y = Return, color = SharpeRatio)) +
      geom_point() +
      theme_classic() +
      scale_y_continuous(labels = scales::percent) +
      scale_x_continuous(labels = scales::percent) +
      labs(x = 'Annualized Risk',
           y = 'Annualized Returns',
           title = "Portfolio Optimization and Efficient Frontier") +
      geom_point(aes(x = Risk, 
                     y = Return), data = min_var, color = 'orange') +
      geom_point(aes(x = Risk,
                     y = Return), data = max_sr, color = 'red') +
      annotate('text', x = max_sr$Risk*1.1, y = max_sr$Return*1.1, label = "Tangency Portfolio", color = "red") +
      annotate('text', x = min_var$Risk*1.1, y = min_var$Return*1.5, label = "Minimum Variance Portfolio", color = "orange")
      #
    
    #ggplotly(p)
    obj$plot_efficient_frontier = p
  }
  return(obj)
  
}

res = efficient_frontier(tick = tick)
