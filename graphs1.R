data("economics")
library(lubridate)


data <- economics
data$rate <- (data$unemploy/data$pop)*100
c1 <- ggplot(data,aes(x = date,y = rate)) + geom_line()
c2 <- ggplot(data,aes(x = date,y = rate)) + geom_line(aes(size = rate))
library(lubridate)
brks <- data$date[seq(1,length(data$date),12)]
lbls <- year(brks)

c1 + scale_x_date(labels = lbls,breaks = brks) + theme(axis.text.x = element_text(angle = 90))

#========= quantmod
library(quantmod)
getSymbols("TCS.NS",src ='yahoo')
df = data.frame(Date = index(TCS.NS),coredata(TCS.NS))
tcs <- df
df <- tail(df,30)

c1 <- ggplot(df,aes(x = Date)) + geom_line(aes(y = TCS.NS.Open),size = 1.5,col = "red")
c2 <- c1 + geom_line(aes(y = TCS.NS.High),size = 1.5,col = "green")
c3 <- c2 + geom_line(aes(y = TCS.NS.Low),size = 1.5,col = "blue")
c4 <- c3 + geom_line(aes(y = TCS.NS.Close),size = 1.5,col = "magenta")

brks <- df$Date[seq(1,length(df$Date),365.25)]
lbls <- year(brks)
c4 + scale_x_date(labels = lbls,breaks = brks)  + theme(axis.text.x = element_text(angle = 90))


# ================= OHLC Graphs
p = plot_ly(data = df,x = ~Date,type = "candlestick",
            open = ~TCS.NS.Open,close = ~TCS.NS.Close,
            high = ~TCS.NS.High,low = ~TCS.NS.Low) %>% layout(title= "Basic Candle Stick Chart")
p

# =================== Olympics
library(data.table)
cost <- fread("E:/Data Visualization/Data Set/Data Set/Cost per event and cost per athlete in the Olympics .csv")
View(cost)


ggplot(cost,aes(x = factor(Year), y = `Cost per event, mio. USD`)) + 
  geom_bar(aes(fill = Type),stat = "identity",position = "dodge")

ggplot(cost,aes(x = Type, y = `Cost per event, mio. USD`)) + geom_bar(stat = "identity")

plotly.data <- cost %>% group_by(Type) %>% summarise(Total.Cost = sum(`Cost per event, mio. USD`))

plot_ly(data = plotly.data,type = "pie",values = ~Total.Cost,labels = ~Type,
        textposition = "inside",textinfo = "label+percent",showlegend = FALSE,
        hoverinfo = "text",text = ~paste("$",Total.Cost,'million') 
        )

