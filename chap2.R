rm(list=ls())
library(fpp3)
Sys.setlocale(locale = "English") 

# creating an annual time series in a tsibble format

y <- tsibble(
  Year = 2015:2019,
  Observation = c(123, 39, 78, 52, 110),
  index = Year
)

# creating a tibble

z <- tibble('Month' = c('2019 Jan','2019 Feb','2019 Mar','2019 Apr','2019 May'),
            'Observation' = c(50,23,34,30,25))

# converting to a tsibble

z1 <- mutate(z, Month = yearmonth(Month)) 
z1ts <- as_tsibble(z1, index = Month)

# alternative syntax

z1b <- z %>%
  mutate(Month = yearmonth(Month)) %>%
  as_tsibble(index = Month)

# plotting

autoplot(z1ts,.vars=Observation) + labs(x = 'Month') # ok
autoplot(z) # error

# multiple time series

distinct(olympic_running, Sex)
women100 <- filter(olympic_running, Sex == 'women', Length == '100')
missObs <- filter(women100,is.na(women100$Time)) # find missing values...
women100short <- filter_index(women100,. ~ '1936', '1948' ~ .) # ... remove them ...
autoplot(women100,.vars = Time)
autoplot(women100short,.vars = Time) # ...and interpolate them in the plot

# Replace NA by linear interpolation

# w100exp = fill_gaps(women100short)
# IntM = model(w100exp, naive =
#       ARIMA(Time ~ -1 + pdq(0,1,0) + PDQ(0,0,0)))
# wgInt = interpolate(IntM,w100exp)

# PBS

PBS
filter(PBS, ATC2 == "A10")
A10all <- filter(PBS, ATC2 == "A10") # filter() selects rows
A10 <- select(A10all, Month, Concession, Type, Cost) # select() selects columns
TotalC <- summarise(A10, TotalC=sum(Cost))
a10 <- mutate(TotalC,Cost=TotalC/1e6) # add a column with a new variable

# reading data from a csv file

library(readr)
rawdata <- read_csv("https://OTexts.com/fpp3/extrafiles/prison_population.csv")
select(rawdata,Date) # see dates
prisonData <- mutate(rawdata, Quarter = yearquarter(Date)) 
temp <- select(prisonData,-Date)
prison <- as_tsibble(temp,key = c(State, Gender, Legal, Indigenous), index = Quarter)

# subsetting

pr_short <- filter_index(prison, "2015 Q1", "2016 Q2") # 2015 Q1 and 2016 Q2 only
prQ1 <- filter(prison,quarter(Quarter) == 1) # observations from Q1 only

# Fig. 2.1

melsyd_economy0 <- filter(ansett, Airports == "MEL-SYD",
                  Class == "Economy") 
melsyd_economy <- mutate(melsyd_economy0,Passengers = Passengers/1000) # replaces the Passengers values
autoplot(melsyd_economy, Passengers) +
  labs(title = "Ansett airlines economy class",
  subtitle = "Melbourne-Sydney", y = "Passengers ('000)")

# Fig. 2.2

autoplot(a10, Cost) +
  labs(y = "$ (millions)", title = "Australian antidiabetic drug sales")

# Fig. 2.3
# Get Google prices

library(tidyquant)
GOOGdata <- tq_get("GOOG",get="stock.prices",from="2017-01-01",to="2023-08-31")
temp <- select(GOOGdata,"date","close")
GOOGtsib <- as_tsibble(temp, index = date)
GOOGtsib = mutate(GOOGtsib,close,diff=close-lag(close))

library(gridExtra)
p231 <- autoplot(as_tsibble(fma::hsales, index = date)) + # sales of new one-family houses, USA
  ggtitle("Sales of new one-family houses, USA") +
  ylab("millions") + xlab("Year")
p232 <- autoplot(as_tsibble(fma::ustreas)) +  # US treasury bill contracts
  ggtitle("US treasury bill contracts") +
  ylab("Number") + xlab("Day")
p233 <- autoplot(select(aus_production, Electricity)) + # the "Details" section in the help reports a wrong frequencey            # Australian quarterly electricity production
  ggtitle("Australian quarterly electricity production") +
  ylab("billion kWh") + xlab("Quarter")
p234 <- autoplot(GOOGtsib,diff) +    # Daily closing stock prices of Google Inc
  ggtitle("Daily differences of Google closing prices") +
  ylab("Change in prices") + xlab("Day")
grid.arrange(p231,p232,p233,p234,nrow=2)

# Figure 2.4

gg_season(a10, Cost, labels = "both") + labs(y = "$ (millions)", 
        title = "Seasonal plot: Antidiabetic drug sales") 

# Figure 2.5

gg_season(vic_elec, Demand, period = "day") + 
  theme(legend.position = "none") +
  labs(y="MW", title="Electricity demand: Victoria")

# Figure 2.6

gg_season(vic_elec, Demand, period = "week") +
  theme(legend.position = "none") + 
  labs(y="MW", title="Electricity demand: Victoria")

# Figure 2.7

gg_season(vic_elec, Demand, period = "year") +
  labs(y="MW", title="Electricity demand: Victoria")

# Figure 2.8

gg_subseries(a10, Cost) +
  labs(y = "$ (millions)", title = "Australian antidiabetic drug sales")

# Australian holiday

holitour <- filter(tourism, Purpose == "Holiday")
holigr <- group_by(holitour, State)
holidays <- summarise(holigr,Trips = sum(Trips))

# Figure 2.9

autoplot(holidays, Trips) +
labs(y = "Overnight trips ('000)", title = "Australian domestic holidays")

# Figure 2.10

gg_season(holidays, Trips) +
labs(y = "Overnight trips ('000)", title = "Australian domestic holidays")

# Figure 2.11

gg_subseries(holidays, Trips) +
labs(y = "Overnight trips ('000)", title = "Australian domestic holidays")

# Figure 2.12

autoplot(filter(vic_elec,year(Time) == 2014),Demand) +
  labs(y = "GW", title = "Half-hourly electricity demand: Victoria")

# Figure 2.13

autoplot(filter(vic_elec, year(Time) == 2014),Temperature) +
  labs(y = "Degrees Celsius",
       title = "Half-hourly temperatures: Melbourne, Australia")

# Figure 2.14

ggplot(filter(vic_elec, year(Time) == 2014),
aes(x = Temperature, y = Demand)) +
geom_point() +
labs(x = "Temperature (degrees Celsius)",y = "Electricity demand (GW)")

# Figure 2.15

library(mvtnorm)
rho = 0.9
temp <- rmvnorm(1000,c(0,0),cbind(c(1,rho),c(rho,1)))
Y <- tibble(x = temp[,1],y = temp[,2])
sp <- ggplot(Y, aes(x=x, y=y)) + geom_point()
print(sp)

# Figure 2.17

visitors <- summarise(group_by(tourism, State), Trips = sum(Trips))
ggplot(visitors, aes(x = Quarter, y = Trips)) +
  geom_line() +
  facet_grid(vars(State), scales = "free_y") +
  labs(title = "Australian domestic tourism", y= "Overnight trips ('000)")

# Figure 2.18

library(GGally)
ggpairs(pivot_wider(visitors, values_from=Trips, names_from=State),
columns = 2:9)

# Figure 2.19

recent_production <- filter(aus_production, year(Quarter) >= 2000)
gg_lag(recent_production, Beer, geom = "point") +
  labs(x = "lag(Beer, k)")

# correlogram

ACF(recent_production, Beer, lag_max = 9) # better avoid using lag_max

# Figure 2.20

autoplot(ACF(recent_production, Beer)) +
  labs(title="Australian beer production")

# Figure 2.21

autoplot(ACF(a10, Cost, lag_max = 48)) +
  labs(title="Australian antidiabetic drug sales")

# Figure 2.22

set.seed(30)
y <- tsibble(sample = 1:50, wn = rnorm(50), index = sample)
autoplot(y,wn) + labs(title = "White noise", y = "")

autoplot(ACF(y,wn)) + labs(title = "White noise")
