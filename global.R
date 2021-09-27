#load in packages
library(shiny)
library(tidyverse)
library(plotly)
#library(ggpmisc)
library(coefplot)
library(broom)
library(corrplot)
library(ggpubr)

#load in tidyquant for this analysis
library(tidyquant)


if(file.info("stocks_us.csv")$mtime>Sys.Date()){
  stocks_us <- readr::read_csv("stocks_us.csv")
} else {
  NYSE <- tq_exchange("NYSE")
  NASDAQ <- tq_exchange("NASDAQ")
  stocks_us <- dplyr::union(NYSE, NASDAQ)
  stocks_us %>% readr::write_csv("stocks_us.csv", col_names=TRUE)
}

# NYSE <- tq_exchange("NYSE")
# NASDAQ <- tq_exchange("NASDAQ")
# stocks_us <- dplyr::union(NYSE, NASDAQ)
# stocks_us <- readr::read_csv("stocks_us.csv")

cols<- c("Symbol","Date", "Open", "High", "Low", "Close", "Volume", "Adj_Close")
sp <- set_names(tq_get("SPY", "stock.prices"), cols)