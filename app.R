library(tidyverse)
library(readxl)
library(lubridate)
library(quantmod)
library(TTR)
library(plotly)

sp <- NULL

df0 <- read_excel("Events.xlsx")
names(df0) <- c("ticker", "drug", "disease", "stage", "event_date", "event_desc")

min_date <- as.Date("2017-01-01")
max_date <- as.Date("2022-01-31")

df <- df0 %>%
  mutate(event_date0 = as.numeric(event_date),
         event_date = as.Date(ifelse(grepl("\\/", event_date),
                                     as.Date(event_date, format = "%m/%d/%Y"),
                                     as.Date(as.character(as.Date(event_date0, origin = "1899-12-30")), 
                                             format = "%Y-%d-%m")),
                              origin = "1970-01-01"),
         stage_cor = case_when(stage == "Approved" & event_desc == "CRL announced November 17, 2020" ~ "CRL",
                               grepl("PDUFA", stage) & grepl("Approved", event_desc) ~ "Approved",
                               grepl("PDUFA", stage) ~ "PDUFA",
                               grepl("Filing", stage) ~ "BLA/ NDA/ sNDA Filing",
                               stage %in% c("Phase 2a", "Phase 2b") ~ "Phase 2",
                               stage %in% c("Phase 1a", "Phase 1b") ~ "Phase 1",
                               TRUE ~ stage)) %>%
  select(-event_date0) %>%
  filter(event_date >= min_date & event_date <= max_date)

tickers <- unique(df0$ticker)

evts <- c("Approved", "CRL", "BLA/ NDA/ sNDA Filing", "PDUFA", "Phase 1", 
          "Phase 1/2", "Phase 2", "Phase 2/3", "Phase 3")
evts_list <- setNames(as.list(evts), evts)
evts_col <- setNames(as.list(c("#316395", "#E65100", "#BAB0AC", "#9C755F",
                               "#DADAEB", "#BCBDDC", "#9E9AC8", "#807DBA", "#2171B5",
                               "#EDC948")), c(evts, "Several events"))
evts_shape <- setNames(as.list(c("cross-dot", "x-dot", "triangle-left-dot", "triangle-right-dot", 
                                 rep("circle-dot", 5), "diamond-dot")), c(evts, "Several events"))

hline <- function(y = 0, color = "blue", dash = "solid", width = 2) {
  list(
    type = "line", 
    x0 = 0, 
    x1 = 1, 
    xref = "paper",
    y0 = y, 
    y1 = y, 
    line = list(color = color, dash = dash, width = width)
  )
}

# UI

ui <- fluidPage(
  br(),
  sidebarLayout(fluid = FALSE, 
                sidebarPanel(
                  width = 2,
                  selectInput("ticker", label = "Ticker",
                              choices = c("", tickers[order(tickers)]), selected = ""),
                  checkboxGroupInput("events", "Events to show", choices = evts_list,
                                     selected = c("Approved", "CRL")),
                  selectInput("graph_type", label = "Graph type for prices",
                              choices = c("Close only", "Candles", "OHLC")),
                  dateRangeInput("period", "Time span for plot",
                                 start = min_date, end = max_date, 
                                 format = "dd.mm.yyyy", weekstart = 1),
                  dateInput("buydate", "Buying date", 
                            min = min_date, max = max_date, value = min_date,
                            format = "dd.mm.yyyy", weekstart = 1),
                  selectInput("returns", label = "Returns to plot",
                              choices = c("% (ROI)", "USD")),
                  checkboxGroupInput("inds", "Indicators to plot", 
                                     choices = c("SMA", "EMA", "RSI", "MACD"),
                                     inline = TRUE),
                  sliderInput("sma_days", label = "SMA: days for short and long MA",
                              min = 20, max = 200, step = 10,
                              value = c(20, 50)),
                  sliderInput("ema_days", label = "EMA: days for short and long MA",
                              min = 10, max = 50, step = 1,
                              value = c(12, 26)),
                  sliderInput("rsi_days", label = "RSI: days",
                              min = 10, max = 20, step = 1,
                              value = 14)
                ),
                mainPanel(
                  plotlyOutput("plot")
                )
  )
)

# Server

server <- function(input, output) {
  
  ticker <- reactive({input$ticker})
  events <- reactive({input$events})
  graph_type <- reactive({input$graph_type})
  inds <- reactive({input$inds})
  sma_days <- reactive({input$sma_days})
  ema_days <- reactive({input$ema_days})
  rsi_days <- reactive({input$rsi_days})
  buydate <- reactive({input$buydate})
  returns <- reactive({input$returns})
  period <- reactive({input$period})
  
  output$plot <- renderPlotly({
    
    req(input$ticker != "")
    
    dfe <- getSymbols(input$ticker, from = min_date, to = max_date + 29, auto.assign = F)
    dfe <- tibble(Date = index(dfe), as_tibble(dfe))
    names(dfe) <- gsub(paste0(ticker(), "."), "", names(dfe))
    dfe <- dfe %>%
      select(Date:Volume) %>%
      arrange(Date) %>%
      mutate(sma_min = SMA(Close, sma_days()[1]),
             sma_max = SMA(Close, sma_days()[2]),
             ema_min = EMA(Close, ema_days()[1]),
             ema_max = EMA(Close, ema_days()[2]),
             rsi = RSI(Close, rsi_days(), "EMA"))
    
    macd <- MACD(dfe$Close, 12, 26, 9, "EMA")
    dfe$macd <- macd[,1]
    dfe$signal <- macd[,2]
    dfe$macd_diff <- dfe$macd - dfe$signal
    
    buydatex <- ifelse(buydate() < min(dfe$Date), min(dfe$Date), buydate())
    buyprice <- dfe$Open[dfe$Date == buydatex]
    
    dfe <- dfe %>%
      mutate(ret = ifelse(Date < buydatex, NA, Close - buyprice),
             roi = ifelse(Date < buydatex, NA, ret/buyprice*100),
             loss = factor(ret < 0, c(FALSE, TRUE), c("Gain", "Loss")))
    
    dfe <- dfe %>%
      full_join(df %>% 
                  dplyr::filter(ticker %in% ticker() & 
                                  event_date >= period()[1] & event_date <= period()[2] &
                                  stage_cor %in% events()) %>%
                  transmute(lbl = sprintf("%s\nEVENT: %s\nDRUG: %s\nDISEASE: %s\n", 
                                          as.character(format(event_date, "%b %d, %Y")), stage_cor, drug, disease),
                            stage_cor, Date = event_date) %>%
                  group_by(Date) %>%
                  summarise(lbl = paste(lbl, collapse = "\n"),
                            stage_cor = ifelse(n() == 1, stage_cor, "Several events")),
                by = "Date") %>%
      mutate(ylbl = 1.05*High,
             stage_cor = factor(stage_cor, c(evts, "Several events"))) %>%
      arrange(Date)
    
    
    if (sum (!is.na(dfe$lbl)) > 0) {
      fig2 <- dfe %>%
        plot_ly(x = ~Date, y = "Events", text = ~lbl, hoverinfo = "text",
                type = 'scatter', mode = "markers", 
                symbol = ~stage_cor, color = ~stage_cor,
                symbols = unlist(evts_shape),
                colors = unlist(evts_col),
                marker = list(size = 10), alpha = 0.5,
                legendgroup = "Events") %>%
        layout(xaxis = list(title = ""),
               yaxis = list(showgrid = FALSE, zeroline = FALSE),
               hoverlabel = list(align = "left"))
    } else {
      fig2 <- plot_ly(x = min(dfe$Date), type = 'scatter',
                      mode = 'text', text = "No events", hoverinfo = "skip",
                      textposition = "middle right", showlegend = FALSE) %>%
        layout(xaxis = list(title = "", range = list(min(dfe$Date), max(dfe$Date))),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    }
    
    fig0 <- dfe %>%
      plot_ly(x = ~Date,
              width = 1200, height = ifelse("RSI" %in% inds(),
                                            ifelse("MACD" %in% inds(), 1000, 800),
                                            ifelse("MACD" %in% inds(), 800, 600)))
    
    if (graph_type() == "Candles") {
      
      fig1 <- fig0 %>%
        add_trace(type = "candlestick",
                  open = ~Open, close = ~Close,
                  high = ~High, low = ~Low,
                  increasing = list(line = list(color = "#BDBDBD", width = 1)),
                  decreasing = list(line = list(color = "#1A1A1A", width = 1)),
                  alpha = 0.5, name = "Candles",
                  legendgroup = "Prices") %>%
        layout(title = list(text = paste0("<b>", ticker(), "</b>")),
               xaxis = list(rangeslider = list(visible = F),
                            title = ""),
               yaxis = list(title = "USD"),
               hovermode = "x unified")
      
    } else if (graph_type() == "OHLC") {
      
      fig1 <- fig0 %>%
        add_trace(type = "ohlc",
                  open = ~Open, close = ~Close,
                  high = ~High, low = ~Low,
                  increasing = list(line = list(color = "#BDBDBD", width = 2)),
                  decreasing = list(line = list(color = "#1A1A1A", width = 2)),
                  alpha = 0.5, name = "OHLC",
                  legendgroup = "Prices") %>%
        layout(title = list(text = paste0("<b>", ticker(), "</b>")),
               xaxis = list(rangeslider = list(visible = F),
                            title = ""),
               yaxis = list(title = "USD"),
               hovermode = "x unified")
      
    } else {
      
      fig1 <- fig0 %>%
        add_trace(y = ~Close, type = "scatter", mode = "lines",
                  line = list(color = "#635761", dash = "dot", width = 1),
                  name = "Close",
                  legendgroup = "Prices") %>%
        layout(title = list(text = paste0("<b>", ticker(), "</b>")),
               xaxis = list(rangeslider = list(visible = F),
                            title = ""),
               yaxis = list(title = "USD"),
               hovermode = "x unified")
      
    }
    
    if ("SMA" %in% inds()) {
      
      fig1 <- fig1 %>%
        add_trace(y = ~sma_min, type = "scatter", mode = "lines",
                  line = list(color = "#9FA8DA", dash = "solid", width = 2),
                  name = sprintf("SMA (%d)", sma_days()[1]),
                  legendgroup = "Prices") %>%
        add_trace(y = ~sma_max, type = "scatter", mode = "lines",
                  line = list(color = "#303F9F", dash = "solid", width = 2),
                  name = sprintf("SMA (%d)", sma_days()[2]),
                  legendgroup = "Prices")
    }
    
    if ("EMA" %in% inds()) {
      
      fig1 <- fig1 %>%
        add_trace(y = ~ema_min, type = "scatter", mode = "lines",
                  line = list(color = "#FFAB91", dash = "solid", width = 2),
                  name = sprintf("EMA (%d)", ema_days()[1]),
                  legendgroup = "Prices") %>%
        add_trace(y = ~ema_max, type = "scatter", mode = "lines",
                  line = list(color = "#D84315", dash = "solid", width = 2),
                  name = sprintf("EMA (%d)", ema_days()[2]),
                  legendgroup = "Prices")
    }
    
    if (returns() == "USD") {
      fig5 <- dfe %>%
        plot_ly(x = ~Date, y = ~ret, type = "bar", 
                color = ~loss, colors = c("#86BCB6", "#FDDDA0"),
                legendgroup = "Returns") %>%
        layout(xaxis = list(title = ""),
               yaxis = list(title = "USD"),
               hovermode = "x unified",
               shapes = list(hline(0, "#333333", "solid", 1)))
      
    } else {
      
      fig5 <- dfe %>%
        plot_ly(x = ~Date, y = ~roi, type = "bar", 
                color = ~loss, colors = c("#86BCB6", "#FDDDA0"),
                legendgroup = "Returns") %>%
        layout(xaxis = list(title = ""),
               yaxis = list(title = "%"),
               hovermode = "x unified",
               shapes = list(hline(0, "#333333", "solid", 1)))
      
      
    }
    
    fig1 <- subplot(fig1, fig5, nrows = 2, shareX = TRUE, heights = c(0.8, 0.2), titleY = TRUE)
    
    if ("RSI" %in% inds()) {
      
      fig3 <- dfe %>%
        plot_ly(x = ~Date, y = ~rsi, type = "scatter", mode = "lines",
                line = list(color = "#B07AA1", dash = "solid", width = 2),
                name = sprintf("RSI (%d)", rsi_days()),
                legendgroup = "RSI") %>%
        layout(yaxis = list(title = "RSI"),
               shapes = list(hline(30, "#333333", "dot", 1), hline(70, "#333333", "dot", 1)))
      
      fig1 <- subplot(fig1, fig3, nrows = 2, shareX = TRUE, heights = c(1/1.2, 1-1/1.2), titleY = TRUE)
      
    }
    
    if ("MACD" %in% inds()) {
      
      fig4 <- dfe %>%
        plot_ly(x = ~Date, y = ~macd_diff, type = "bar", 
                marker = list(color = "#BAB0AC"), alpha = 0.5,
                name = "MACD histogram",
                legendgroup = "MACD") %>%
        add_trace(data = dfe, x = ~Date, y = ~signal, type = "scatter", mode = "lines",
                  line = list(color = "#F28E2B", dash = "solid", width = 2),
                  name = "MACD signal", inherit = FALSE,
                  legendgroup = "MACD") %>%
        add_trace(data = dfe, x = ~Date, y = ~macd, type = "scatter", mode = "lines",
                  line = list(color = "#4E79A7", dash = "solid", width = 2),
                  name = "MACD", inherit = FALSE,
                  legendgroup = "MACD") %>%
        layout(yaxis = list(title = "MACD"))
      
      if ("RSI" %in% inds()) {
        fig1 <- subplot(fig1, fig4, nrows = 2, shareX = TRUE, heights = c(1/(2-1/1.2), 1-1/(2-1/1.2)), titleY = TRUE)
      } else {
        fig1 <- subplot(fig1, fig4, nrows = 2, shareX = TRUE, heights = c(1/1.2, 1-1/1.2), titleY = TRUE)
      }
    }
    
    subplot(fig2, fig1, nrows = 2, shareX = TRUE, heights = c(0.05, 0.95), titleY = TRUE) %>%
      layout(xaxis = list(title = ""),
             hovermode = "x unified",
             plot_bgcolor = "#F5F5F5",
             paper_bgcolor = "#F5F5F5",
             margin = list(t = 50))
  })
}

# Run the application 
shinyApp(ui = ui, server = server, options = list(height = 1200, width = 1400))
