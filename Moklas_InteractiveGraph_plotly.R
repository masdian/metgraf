install.packages("plotly")
install.packages("gapminder",dependencies = T)
library(gapminder)
library(plotly)

p<-plot_ly(midwest,x=~percollege, color=~state, type = "box")
p

d<-diamonds[sample(nrow(diamonds),1000),]
p<-plot_ly(d, x = ~carat, y = ~price, color = ~carat, size = ~carat)
p

p<-plot_ly(d, x = ~carat, y = ~price, color = ~carat, size = ~carat,
           text = ~paste("Price: ", price, "$<br>Cut:", cut))
p


p <- plot_ly(pop, x = gdp$X2000, y = life$X2000, color = ~Region,
             size = pop$X2000)
p %>% layout(xaxis = list(range = c(100, 150000)))

p <- plot_ly(x = gdp$X2015,
             y = life$X2015,
             size = pop$X2015,
             color = pop$Region,
             text = pop$negara,
             hoverinfo = "text",
             type = 'scatter',
             mode = 'markers')
p %>% layout(x.axis = list(type = "log"))

p<-gapminder %>%
  plot_ly(
    x = ~gdpPercap,
    y = ~lifeExp,
    size = ~pop,
    color = ~continent,
    frame = ~year,
    text = ~country,
    hoverinfo = "text",
    type = 'scatter',
    mode = 'markers'
  ) %>%
  layout(
    xaxis = list(
      type = "log"
    )
  )
p


gdp<-read.csv("gdp.csv", header = T)
dim(gdp)
names(gdp)
gdp<-gdp[,c(3,220,4:219)]

life<-read.csv("life.csv", header = T)
names(life)
dim(life)
life<-life[,c(3,221,4:220)]

pop<-read.csv("pop.csv", header = T)
names(pop)
pop<-pop[,c(3,85,4:84)]

library(tidyr)
gdp1<-gather(gdp, year, gdp, X1800:X2015, factor_key = T)
gdp1$year<-as.numeric(substr(gdp1$year,2,5))
gdp1<-gdp1[complete.cases(gdp1),]
str(gdp1)

life1<-gather(life, year, lifeex, X1800:X2016, factor_key = T)
life1$year <- as.numeric(substr(life1$year,2,5))
life1<-life1[complete.cases(life1),]
str(life1)

pop1 <- gather(pop, year, pop, X1800:X2015, factor_key = T)
pop1$year <- as.numeric(substr(pop1$year, 2, 5))
pop1<-pop1[complete.cases(pop1),]
str(pop1)

gap<-merge(life1, gdp1, by.x = c(1,3), by.y = c(1,3), all = F)
head(gap)
dim(gap)
dim(life1)
dim(gdp1)
gap<-gap[,c(1,3,2,4,6)]
names(gap)
gap1<-merge(gap, pop1, by.x = c(1,3), by.y = c(1,3), all = F)
dim(gap1)
dim(pop1)
names(gap1)
gap1<-gap1[,c(1,3,2,4,5,7)]
names(gap1)
gap1$pop1<-sqrt(gap1$pop/100000)
summary(gap1$pop1)

p<-gap1 %>%
  plot_ly(
    x = ~gdp,
    y = ~lifeex,
    size = ~pop1,
    color = ~Region.x,
    frame = ~year,
    text = ~negara,
    hoverinfo = "text",
    type = 'scatter',
    mode = 'markers',
    colors = c("red","blue","black","purple"),
    sizes = c(10,max(gap1$pop1,na.rm = T)),
    marker = list(
      symbol = "circle",
      sizemode = "diameter",
      line = list(
        width = 2,
        color = "#FFFFFF"
      )
    )
  ) %>%
  layout(
    title = "Pendapatan per Kapita vs Angka Harapan Hidup",
    xaxis = list(
      title = "Pendapatan per Kapita",
      type = "log",
      showgrid = F),
    yaxis = list(
      title = "Angka Harapan Hidup (tahun)",
      showgrid = F)
  )

p


Sys.setenv("plotly_username"="masdian")
Sys.setenv("plotly_api_key"="ajn7dqfPKcWyE8dP1Xfd")

chart_link<-api_create(p, filename = "bubble-euy")
chart_link

p<-plot_ly(midwest,x=~percollege, color=~state, type = "box")
p

p<-gdp1 %>%
  plot_ly(
    x = ~gdp,
    color = ~Region,
    frame = ~year,
    text = ~Region,
    hoverinfo = "text",
    type = "box",
    colors = c("red","blue","black","purple")
  ) %>%
  layout(
    title = "Pendapatan per Kapita di 4 Benua",
    xaxis = list(
      title = "Pendapatan per Kapita",
      type = "log",
      showgrid = F)
  )

p


