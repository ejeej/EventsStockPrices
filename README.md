# Events vs. Stock Prices in Biotech (2017-2022)

The repository contains necessary data and code for the Shiny web application, which allows you to obtain the plot for the dynamics of the stock prices for the chosen company ticket (security symbol), indicators for all events happend with this company during the chosen period and several financial indicators:

- Stock returns after the chosen buying date,
- SMA,
- EMA,
- RSI,
- MACD.

Using [BioPharmCatalyst](https://www.biopharmcatalyst.com) events in Biotech companies were parsed for the period from 2017-01-01 to 2022-01-31 and placed into Events.xlsx file. All events were classified into several groups: 

- Approved - a drug was approved by the FDA, 
- CRL - Complete Response Letter, rejection of approval by the FDA, 
- BLA/ NDA/ sNDA Filing - Biologic License Application/ New Drug Application/ supplemental NDA,
- PDUFA - Prescription Drug User Fee Act date, the date assigned by the FDA to issue an approval decision or CRL for a drug application,
- Phase 1 - Phase 3 - events concerning the results of the corresponding phases of the clinical trials held by the company.

All stock prices are loaded for the chosen security code from [Yahoo Finance](https://finance.yahoo.com/) using function `getSymbols` from R `quantmod` package.

Plots are rendered using R `plotly` package, so they are interactive and have tooltips for data points.

The full list of the required R packages:
- shiny,
- tidyverse
- readxl,
- lubridate,
- quantmod,
- TTR,
- plotly

You can run the Shiny app by running the code in the RunApp.R file (it contains code for installing necessary packages if they are missing on your site) or by running the code in the app.R file inside the EventsStockPricesApp folder.

![image](https://user-images.githubusercontent.com/9775181/193405081-a872439d-061f-4adc-b5e9-0d3f464ea6aa.png)
