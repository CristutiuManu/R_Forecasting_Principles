rm(list=ls())
library(fpp3)

algeria_economy <- filter(global_economy, Country == "Algeria")
autoplot(algeria_economy, Exports) +
  labs(y = "% of GDP", title = "Exports: Algeria")

# Estimate parameters

fit <- model(algeria_economy, ETS(Exports ~ error("A") + trend("N") +
                season("N")))
report(fit)
fc <- forecast(fit, h = 5)

# Figure 8.2

autoplot(fc, algeria_economy) + geom_line(aes(y = .fitted), col="#D55E00",
            data = augment(fit)) +
  labs(y="% of GDP", title="Exports: Algeria") + guides(colour = "none")

# Figure 8.3

global_aus <- filter(global_economy, Code == "AUS")
aus_economy <- mutate(global_aus, Pop = Population / 1e6)
autoplot(aus_economy, Pop) +
  labs(y = "Millions", title = "Australian population")

fit <- model(aus_economy, AAN = ETS(Pop ~ error("A") + trend("A") +
                      season("N")))
fc <- forecast(fit, h = 10)

# Figure 8.4

HoltM <- model(aus_economy, "Holt's method" = ETS(Pop ~ error("A") +
                            trend("A") + season("N")),
    "Damped Holt's method" = ETS(Pop ~ error("A") + # note that phi is SET to 0.9
                                   trend("Ad",phi=0.9) + season("N")))
fore <- forecast(HoltM, h = 15)
report(select(HoltM,"Holt's method"))
report(select(HoltM,"Damped Holt's method"))
autoplot(fore, aus_economy, level = NULL) +
  labs(title = "Australian population", y = "Millions") +
  guides(colour = guide_legend(title = "Forecast"))

# Figure 8.5

www_usage <- as_tsibble(WWWusage)
autoplot(www_usage, value) +
  labs(x="Minute", y="Number of users",  title = "Internet usage per minute")

model3 <- model(stretch_tsibble(www_usage, .init = 10),
    SES = ETS(value ~ error("A") + trend("N") + season("N")),
    Holt = ETS(value ~ error("A") + trend("A") + season("N")),
    Damped = ETS(value ~ error("A") + trend("Ad") + season("N")))
fore <- forecast(model3, h = 1)
select(accuracy(fore, www_usage),c(.model,RMSE,MAE,MAPE))

fit <- model(www_usage, Damped = ETS(value ~ error("A") + trend("Ad") +
                            season("N")))

# Figure 8.6

autoplot(forecast(fit, h = 10), www_usage) +
  labs(x="Minute", y="Number of users",
       title = "Internet usage per minute")

# Figure 8.7

aus_holidays <- summarise(filter(tourism, Purpose == "Holiday"),
          Trips = sum(Trips)/1e3)
fit <- model(aus_holidays, additive = ETS(Trips ~ error("A") + trend("A")
                                    + season("A")),
    multiplicative = ETS(Trips ~ error("M") + trend("A") + season("M")))
fc <- forecast(fit, h = "3 years")
autoplot(fc, aus_holidays, level = NULL) +
labs(title="Australian domestic tourism", y="Overnight trips (millions)") +
  guides(colour = guide_legend(title = "Forecast"))
fitA <- model(aus_holidays, additive = ETS(Trips ~ error("A") + trend("A")
                              + season("A")))
autoplot(components(fitA))  # Figure 8.8 (a)
gg_tsresiduals(fitA)
fitM <- model(aus_holidays, multiplicative = ETS(Trips ~ error("M") +
                      trend("A") + season("M")))
autoplot(components(fitM))  # Figure 8.8 (b)
gg_tsresiduals(fitM)

# Figure 8.9

ped_data <- filter(pedestrian, Date >= "2016-07-01",
         Sensor == "Southern Cross Station")
sth_cross_ped <- summarise(index_by(ped_data, Date),
            Count = sum(Count)/1000)
mod_ped <- model(filter(sth_cross_ped, Date <= "2016-07-31"),
    hw = ETS(Count ~ error("M") + trend("A") + season("M")))
fore <- forecast(mod_ped, h = "2 weeks")
autoplot(fore, filter(sth_cross_ped, Date <= "2016-08-14"),level=NULL) +
  labs(title = "Daily traffic: Southern Cross",
       y="Pedestrians ('000)")
