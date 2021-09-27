
shinyServer(function(input, output, session) {
    df_s <- eventReactive(input$run_s, {
        set_names(tq_get(input$ticker, get = "stock.prices"),
                  cols)
    })
    company_current <- reactive({
        stocks_us %>%
            mutate(market_cap_B = market.cap / 10 ^ 9) %>%
            filter(symbol == input$ticker) %>%
            select(symbol,
                   company,
                   market_cap_B,
                   country,
                   ipo.year,
                   industry)
    })
    
    output$info <-
        renderTable({
            company_current()
        }, striped = TRUE, bordered = TRUE, hover = TRUE)
    
    observe({
        if (!is.null(df_s())) {
            updateSliderInput(
                session,
                "date_c",
                min = min(df_s()$Date),
                max = max(df_s()$Date)
            )
        }
    })
    date_min <- reactive({
        if (input$date_opt == "1m") {
            dmin <- max(df_s()$Date) %m+% months(-1)
        }
        else if (input$date_opt == "3m") {
            dmin <- max(df_s()$Date) %m+% months(-3)
        }
        else if (input$date_opt == "1y") {
            dmin <- max(df_s()$Date) %m+% months(-12)
        }
        else if (input$date_opt == "3y") {
            dmin <- max(df_s()$Date) %m+% months(-36)
        }
        else{
            dmin <- input$date_c[1]
        }
    })
    date_max <- reactive({
        if (input$date_opt != "cust") {
            dmax <- max(df_s()$Date)
        }
        else{
            dmax <- input$date_c[2]
        }
    })
    per_start_price <- reactive({
        if (is.null(df_s())) {
            1
        }
        else{
            as.numeric(
                df_s() %>%
                    filter(Date >= date_min() &
                               Date <= date_max()) %>%
                    head(1) %>%
                    select(Adj_Close)
            )
        }
    })
    per_end_price <- reactive({
        if (is.null(df_s())) {
            1
        }
        else{
            as.numeric(
                df_s() %>%
                    filter(Date >= date_min() &
                               Date <= date_max()) %>%
                    tail(1) %>%
                    select(Adj_Close)
            )
        }
    })
    
    text_out <- reactive({
        paste(
            "<b>",
            company_current()$company[1],
            "</b>",
            "<br>",
            "<em>Period dollar change: ",
            per_end_price() - per_start_price(),
            "<br>",
            "Period percent change: ",
            (per_end_price() - per_start_price()) /
                per_start_price(),
            "</em>"
        )
    })
    
    output$period_info <- renderText({
        text_out()
    })
    output$period_info2 <- renderText({
        text_out()
    })
    
    
    output$line_plot_s <- renderPlotly({
        if (is.null(df_s())) {
            return()
        }
        g <- df_s() %>%
            tq_mutate(
                select = Adj_Close,
                mutate_fun = SMA,
                n = input$sma1_n,
                col_rename = "SMA1"
            ) %>%
            tq_mutate(
                select = Adj_Close,
                mutate_fun = SMA,
                n = input$sma2_n,
                col_rename = "SMA2"
            ) %>%
            filter(Date >= date_min() &  Date <= date_max()) %>%
            ggplot(mapping = aes(x = Date, y = Adj_Close)) +
            geom_line(color = 'steelblue') +
            theme_tq()
        if (input$sma1) {
            g <- g + geom_line(
                mapping = aes(x = Date, y = SMA1),
                linetype = 5,
                size = .5,
                color = 'blue'
            )
        }
        if (input$sma2) {
            g <- g + geom_line(
                mapping = aes(x = Date, y = SMA2),
                linetype = 5,
                size = .5,
                color = 'grey'
            )
        }
        ggplotly(g)
    })
    
    output$candle_plot_s <- renderPlot({
        if (is.null(df_s())) {
            return()
        }
        g <- df_s() %>%
            tq_mutate(
                select = Adj_Close,
                mutate_fun = SMA,
                n = input$sma1_n,
                col_rename = "SMA1"
            ) %>%
            tq_mutate(
                select = Adj_Close,
                mutate_fun = SMA,
                n = input$sma2_n,
                col_rename = "SMA2"
            ) %>%
            filter(Date >= date_min() &  Date <= date_max()) %>%
            ggplot(mapping = aes(x = Date, y = Close)) +
            geom_candlestick(
                mapping = aes(
                    open = Open,
                    high = High,
                    low = Low,
                    close = Close
                ),
                colour_up = "grey",
                colour_down = "grey",
                fill_up  = "darkgreen",
                fill_down  = "darkred",
                
                size = 1
            ) +
            theme_bw()
        if (input$sma1) {
            g <- g + geom_line(
                mapping = aes(x = Date, y = SMA1),
                linetype = 5,
                size = .5,
                color = 'blue'
            )
        }
        if (input$sma2) {
            g <- g + geom_line(
                mapping = aes(x = Date, y = SMA2),
                linetype = 5,
                size = .5,
                color = 'grey'
            )
        }
        g
    })
    
    merged <- reactive({
        if (is.null(df_s())) {
            return()
        }
        c_r <- df_s() %>%
            filter(Date >= date_min() &  Date <= date_max()) %>%
            tq_transmute(select = Adj_Close, mutate_fun = periodReturn, period = input$per_opt, col_rename = "Company_Returns")
        sp_r <- sp %>%
            filter(Date >= date_min() &  Date <= date_max()) %>%
            tq_transmute(select = Adj_Close, mutate_fun = periodReturn, period = input$per_opt, col_rename = "SP500_Returns")
        merge(c_r, sp_r)
    })
    
    
    output$beta_plot <- renderPlot({
        if (is.null(df_s())) {
            return()
        }
        formula <- as.formula("y~x")
        g <- merged() %>%
            ggplot(mapping = aes(x = SP500_Returns, y = Company_Returns))+
            geom_point()+
            # stat_poly_line(formula =formula)+
            # stat_poly_eq(aes(label =  paste(stat(eq.label), stat(adj.rr.label),
            #                                 sep = "*\", \"*")),
            #              formula = formula, size = rel(6))+
            # theme_tq()
            stat_smooth(method = "lm", formula = formula) +
            stat_regline_equation(
                aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
                formula = formula, size =6) +
            theme_tq()
        g
    })
    
    output$coef_single<- renderPlot({
        if (is.null(df_s())) {
            return()
        }
        lm(Company_Returns~SP500_Returns, data = merged()) %>% 
            coefplot()+
            theme_tq() +
            guides(color = 'none', linetype = 'none', shape='none')
    })
    
    
    company_current_2 <-
        reactive({
            stocks_us %>% filter(symbol == input$ticker_m)
        })
    
    
    output$company_table <- renderTable({
        stocks_us %>%
            mutate(market_cap_B = market.cap / 10 ^ 9) %>%
            filter(symbol == input$ticker_m) %>%
            select(symbol,
                   company,
                   market_cap_B,
                   country,
                   ipo.year,
                   industry)
    },
    striped = TRUE,
    bordered = TRUE,
    hover = TRUE)
    
    output$peer_table <- renderTable({
        if(input$ticker_m==""){
            return()
        }
        stocks_us %>%
            mutate(market_cap_B = market.cap / 10 ^ 9) %>%
            filter(industry == company_current_2()$industry) %>%
            arrange(desc(market.cap)) %>%
            select(symbol,
                   company,
                   market_cap_B,
                   country,
                   ipo.year,
                   industry) %>%
            head(20)
    }, striped = TRUE, bordered = TRUE, hover = TRUE)
    
    df_m <- eventReactive(input$run_m, {
        ticks <- c(input$ticker_m)
        if (!is.na(input$peer1)) {
            ticks <- append(ticks, input$peer1)
        }
        if (!is.na(input$peer2)) {
            ticks <- append(ticks, input$peer2)
        }
        if (!is.na(input$peer3)) {
            ticks <- append(ticks, input$peer3)
        }
        if (!is.na(input$peer4)) {
            ticks <- append(ticks, input$peer4)
        }
        if (!is.na(input$peer5)) {
            ticks <- append(ticks, input$peer5)
        }
        set_names(tq_get(ticks, get = "stock.prices"), cols)
    })
    
    observe({
        if (!is.null(df_m())) {
            updateSliderInput(
                session,
                "date_c_m",
                min = min(df_m()$Date),
                max = max(df_m()$Date)
            )
        }
    })
    date_min_m <- reactive({
        if (input$date_opt_m == "1m") {
            dmin <- max(df_m()$Date) %m+% months(-1)
        }
        else if (input$date_opt_m == "3m") {
            dmin <- max(df_m()$Date) %m+% months(-3)
        }
        else if (input$date_opt_m == "1y") {
            dmin <- max(df_m()$Date) %m+% months(-12)
        }
        else if (input$date_opt_m == "3y") {
            dmin <- max(df_m()$Date) %m+% months(-36)
        }
        else{
            dmin <- input$date_c_m[1]
        }
    })
    date_max_m <- reactive({
        if (input$date_opt_m != "cust") {
            dmax <- max(df_m()$Date)
        }
        else{
            dmax <- input$date_c_m[2]
        }
    })
    
    
    
    output$line_rel <- renderPlotly({
        if (is.null(df_m())) {
            return()
        }
        g <- df_m() %>%
            filter(Date >= date_min_m() &
                       Date <= date_max_m()) %>%
            group_by(Symbol) %>% 
            tq_transmute(select = Adj_Close, mutate_fun = periodReturn, period = input$per_opt_m, col_rename = "Company_Returns") %>%
            mutate(cumulative_return = 100 * cumprod(1 + Company_Returns)) %>%
            ggplot(mapping = aes(
                x = Date,
                y = cumulative_return,
                color = Symbol
            )) +
            geom_line(size = 1) +
            theme_tq() +
            scale_color_tq()
        ggplotly(g)
    })
    
    output$return_violin <- renderPlot({
        g <- df_m() %>%
            filter(Date >= date_min_m() &
                       Date <= date_max_m()) %>%
            group_by(Symbol) %>%
            tq_transmute(select = Adj_Close, mutate_fun = periodReturn, period = input$per_opt_m, col_rename = "Company_Returns") %>%
            ggplot(mapping = aes(x = Symbol, y = Company_Returns)) +
            geom_violin(mapping = aes(fill = Symbol), alpha = .4) +
            scale_fill_tq()+
            theme_tq()
        g
    })
    
    merged_m <- reactive({
        if (is.null(df_m())) {
            return()
        }
        c_r <- df_m() %>%
            filter(Date >= date_min_m() & Date <= date_max_m()) %>%
            group_by(Symbol) %>%
            tq_transmute(select = Adj_Close, mutate_fun = periodReturn, period = input$per_opt_m, col_rename = "Company_Returns")
        sp_r <- sp %>%
            filter(Date >= date_min_m() & Date <= date_max_m()) %>%
            tq_transmute(select = Adj_Close, mutate_fun = periodReturn, period = input$per_opt_m, col_rename = "SP500_Returns")
        merge(c_r, sp_r)
    })
    
    output$beta_plot_m <- renderPlotly({
        if (is.null(df_m())) {
            return()
        }
        g <- merged_m() %>%
            ggplot(mapping = aes(x = SP500_Returns, y = Company_Returns))+
            geom_point(mapping = aes(color = Symbol),
                       alpha =.5)+
            geom_smooth(method = lm,
                        mapping = aes(color = Symbol), se = FALSE)+
            
            theme_tq() +
            scale_color_tq()
        ggplotly(g)
            
    })
    
    output$beta_plot_m_n <- renderPlotly({
        if (is.null(df_m())) {
            return()
        }
        models <- merged_m() %>% 
            select(Symbol, Company_Returns, SP500_Returns) %>% 
            group_by(Symbol) %>% 
            do(tidy(lm(Company_Returns~SP500_Returns, data = .)))
        
        g <- models %>% 
            filter(term=="SP500_Returns") %>% 
            mutate(se_low = estimate - 2*std.error) %>% 
            mutate(se_high = estimate + 2*std.error) %>% 
            ggplot(mapping = aes(x = Symbol, ymin = se_low, ymax = se_high,
                                 color = Symbol))+
            geom_linerange()+
            geom_point(mapping = aes(y = estimate))+
            geom_hline(yintercept=1, linetype="dashed", color = "grey")+
            geom_hline(yintercept=0, linetype="dashed", color = "grey")+
            theme_tq() +
            scale_color_tq()
        
        ggplotly(g)
    })
    
    observe({
        if (is.null(df_m())) {
            return()
        }
        symbols <- unique(df_m()$Symbol)
        symbols <- c("SP500_Returns", symbols)
        updateSelectInput(session,
                          "s_xa",
                          choices = symbols,
                          selected = symbols[1])
        
        updateSelectInput(session,
                          "s_xb",
                          choices = symbols,
                          selected = symbols[2])
    })
    
    output$companies_scatter <- renderPlot({
        formula <- as.formula("y~x")
        
        g <- merged_m() %>% 
            spread(Symbol, Company_Returns) %>% 
            ggplot(mapping = aes_string(x = input$s_xa, 
                                        y = input$s_xb))+
            geom_point()+
            # stat_poly_line(formula =formula)+
            # stat_poly_eq(aes(label =  paste(stat(eq.label), stat(adj.rr.label), 
            #                             sep = "*\", \"*")),
            #          formula = formula, size = rel(6))+
            stat_smooth(method = "lm", formula = formula) +
            stat_regline_equation(
                aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
                formula = formula, size =6) +
            theme_tq()
        
        
        g
    })
    
    output$corrplot <-renderPlot({
        g <- merged_m() %>% 
            spread(Symbol, Company_Returns) %>% 
            rename(SP500 = SP500_Returns) %>% 
            select(-Date) %>% 
            cor() %>% 
            corrplot.mixed()
    })
    
})
