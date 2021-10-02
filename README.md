# stock_visualization_app

This app allows a user to perform various analysis of stocks, similar to google/yahoo finance.

It was created with Rshiny using the tidyquant package.

The data for each stock is sourced from [Yahoo Finance](https://finance.yahoo.com/), and is obtained through the tidyquant tq_get() function.

The Application is divided into two sections: single stock analysis and multiple stock analysis.  Single stock analysis is good for examining a single stock's performance or daily trading history, while Multiple Stock analysis is beneficial for comparing returns vs other companies or any market index.
