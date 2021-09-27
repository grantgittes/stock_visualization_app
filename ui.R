



shinyUI(navbarPage(
  "Grant's Stock Application",
  tabPanel("Welcome",
           includeMarkdown("stock_welcome.md")),
  tabPanel(
    "Single Stock Analysis",
    sidebarLayout(
      sidebarPanel(
        # textInput("ticker", "Enter Ticker and Click Below!", "MSFT"),
        textInput("ticker", "Enter Ticker and Click Below!"),
        actionButton("run_s",
                     "Plot Company"),
        radioButtons(
          "date_opt",
          "Date Range:",
          choices = c(
            "1 Month" = "1m",
            "3 Months" = "3m",
            "1 Year" = "1y",
            "3 Year" = "3y",
            "Custom (use slider)" = "cust"
          ),
          selected = "1y",
          inline = TRUE
        ),
        
        sliderInput(
          "date_c",
          "Custom Date Range:",
          min = as.Date("2011/01/03"),
          max = Sys.Date(),
          value = c(as.Date("2011/01/03"), Sys.Date())
        ),
        checkboxInput("sma1",
                      "Add a simple moving average?",
                      value = FALSE),
        sliderInput(
          "sma1_n",
          "Number of Days:",
          min = 5,
          max = 180,
          step = 5,
          value = 30
        ),
        checkboxInput("sma2",
                      "Add another simple moving average?",
                      value = FALSE),
        sliderInput(
          "sma2_n",
          "Number of Days:",
          min = 5,
          max = 180,
          step = 5,
          value = 90
        ),
        radioButtons(
          "per_opt",
          "Periodicity (Beta purposes only):",
          choices = c(
            "Daily" = "daily",
            "Weekly" = "weekly",
            "Monthly" = "monthly"
          ),
          selected = "weekly",
          inline = TRUE
        )
        
      ),
      mainPanel(tabsetPanel(
        type = 'tabs',
        tabPanel("Company Overview",
                 tableOutput("info")),
        tabPanel(
          "Line Chart",
          htmlOutput("period_info"),
          plotlyOutput("line_plot_s"),
          includeMarkdown("single_stock.md")
        ),
        tabPanel(
          "Candlestick Chart",
          htmlOutput("period_info2"),
          plotOutput("candle_plot_s"),
          includeMarkdown("candle.md")
        ),
        tabPanel(
          "Beta Analysis",
          h4("Company Returns vs. S&P500"),
          plotOutput("beta_plot"),
          h4("Beta Range"),
          plotOutput("coef_single"),
          includeMarkdown("beta.md")
        )
        
      ))
    )
    
  ),
  tabPanel(
    "Multiple Stock Analysis",
    sidebarLayout(
      sidebarPanel(
        # textInput("ticker_m", "Primary Ticker", "MSFT"),
        # textInput("peer1", "Peers to Compare", "AAPL"),
        # textInput("peer2", NULL, "FB"),
        # textInput("peer3", NULL, "NFLX"),
        # textInput("peer4", NULL, "GOOGL"),
        # textInput("peer5", NULL, "AMZN"),
        textInput("ticker_m", "Primary Ticker"),
        textInput("peer1", "Peers to Compare"),
        textInput("peer2", NULL),
        textInput("peer3", NULL),
        textInput("peer4", NULL),
        textInput("peer5", NULL),
        actionButton("run_m",
                     "Use tickers"),
        radioButtons(
          "date_opt_m",
          "Date Range:",
          choices = c(
            "1 Month" = "1m",
            "3 Months" = "3m",
            "1 Year" = "1y",
            "3 Year" = "3y",
            "Custom (use slider)" = "cust"
          ),
          selected = "1y",
          inline = TRUE
        ),
        
        sliderInput(
          "date_c_m",
          "Custom Date Range:",
          min = as.Date("2011/01/03"),
          max = Sys.Date(),
          value = c(as.Date("2011/01/03"), Sys.Date())
        ),
        radioButtons(
          "per_opt_m",
          "Periodicity (all charts):",
          choices = c(
            "Daily" = "daily",
            "Weekly" = "weekly",
            "Monthly" = "monthly"
          ),
          selected = "weekly",
          inline = TRUE
        )
      ),
      mainPanel(tabsetPanel(
        type = 'tabs',
        tabPanel(
          "Select Peers",
          h4("Selected Company"),
          tableOutput("company_table"),
          h4("Potential peers (Top 20 by market cap in industry)"),
          tableOutput("peer_table"),
        ),
        tabPanel("Returns Comparison",
                 plotlyOutput("line_rel"),
                 includeMarkdown("multi_stock.md")),
        tabPanel("Period Returns",
                 plotOutput("return_violin"),
                 includeMarkdown("violin.md")),
        tabPanel("Beta Analysis",
                 h4("Company Returns vs. S&P500"),
                 plotlyOutput("beta_plot_m"),
                 h4("Estimated Beta via Regression"),
                 plotlyOutput("beta_plot_m_n"),
                 includeMarkdown("beta.md")
                 ),
        tabPanel("Company Comparison",
                 h3("Company comparison scatter plot"),
                 selectInput("s_xa",
                             "Select input 1:",
                             choices = "SP500_Returns"),
                 selectInput("s_xb",
                             "Select input 2:",
                             choices = "SP500_Returns"),
                 plotOutput("companies_scatter"),
                 includeMarkdown("scatter.md")
                 ),
        tabPanel("Correlation Plot",
                 h3("Correlation Plot Between Companies' Returns"),
                 plotOutput("corrplot"),
                 includeMarkdown("corrplot.md"))
        
      ))
    )
  )
  
))
