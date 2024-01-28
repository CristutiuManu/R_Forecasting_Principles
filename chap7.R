rm(list=ls())
library(fpp3)
library(gridExtra)

# Figure 7.2

autoplot(pivot_longer(us_change, c(Consumption, Income),
      names_to="Series"), value) + labs(y = "% change")

# Alternative solution:

ggplot(us_change, aes(x = Quarter)) +
  geom_line(aes(y = Consumption, colour = "Consumption")) +
  geom_line(aes(y = Income, colour = "Income")) +
  labs(x = "Quarter", y = "% change") +
  scale_colour_manual(values=c(Consumption="black",Income="red")) +
  guides(colour = guide_legend(title = NULL))

# Figure 7.3

ggplot(us_change, aes(x = Income, y = Consumption)) +
  labs(y = "Consumption (quarterly % change)",
       x = "Income (quarterly % change)") + geom_point() +
  geom_smooth(method = "lm", se = FALSE)

report(model(us_change, TSLM(Consumption ~ Income)))

# Figure 7.4

autoplot(us_change,vars(Production,Savings,Unemployment)) + 
  ylab("% change") + xlab("Year")

# Figure 7.5

GGally::ggpairs(us_change, columns = 2:6)

fit_consMR <- model(us_change,
tslm = TSLM(Consumption ~ Income + Production + Unemployment + Savings))
report(fit_consMR)

# Figure 7.6

ggplot(augment(fit_consMR), aes(x = Quarter)) +
  geom_line(aes(y = Consumption, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(y = NULL, title = "Percent change in US consumption expenditure") +
  scale_colour_manual(values=c(Data="black",Fitted="#D55E00")) +
  guides(colour = guide_legend(title = NULL))

# Figure 7.7

ggplot(augment(fit_consMR), aes(x = Consumption, y = .fitted)) +
  geom_point() + labs(y = "Fitted values",
    x = "Data (actual values)",
    title = "Percent change in US consumption expenditure") +
  geom_abline(intercept = 0, slope = 1)
  
# Figure 7.8
  
gg_tsresiduals(fit_consMR)  

features(augment(fit_consMR),.innov, ljung_box, lag = 10, dof = 5)

# Figure 7.9

data <- pivot_longer(left_join(us_change, residuals(fit_consMR),
by = "Quarter"), Income:Unemployment, names_to = "regressor",
values_to = "x")
ggplot(data, aes(x = x, y = .resid)) + geom_point() +
  facet_wrap(. ~ regressor, scales = "free_x") +
  labs(y = "Residuals", x = "")

# Figure 7.9 again (alternative solution)

dataRes <- left_join(us_change, residuals(fit_consMR))
p1 <- ggplot(dataRes,aes(x=Income,y=.resid)) + geom_point() +
  ylab('Residuals') + ggtitle('Income')
p2 <- ggplot(dataRes,aes(x=Production,y=.resid)) + geom_point() +
  ylab('Residuals') + ggtitle('Production')
p3 <- ggplot(dataRes,aes(x=Savings,y=.resid)) + geom_point() +
  ylab('Residuals') + ggtitle('Savings')
p4 <- ggplot(dataRes,aes(x=Unemployment,y=.resid)) + geom_point() +
  ylab('Residuals') + ggtitle('Unemployment')
grid.arrange(p1,p2,p3,p4,nrow=2)

# Figure 7.10

ggplot(augment(fit_consMR), aes(x = .fitted, y = .resid)) +
  geom_point() + labs(x = "Fitted", y = "Residuals")

# Spurious regression

data <- left_join(filter(aus_airpassengers, Year <= 2011),
            guinea_rice, by = "Year")
fit <- model(data, TSLM(Passengers ~ Production))
report(fit)

# Figure 7.13 

gg_tsresiduals(fit)

# Figure 7.14

recent_production <- filter(aus_production, year(Quarter) >= 1992)
autoplot(recent_production, Beer) +
labs(y = "Megalitres", title = "Australian quarterly beer production")

fit_beer <- model(recent_production, TSLM(Beer ~ trend() + season()))
report(fit_beer)

# Figure 7.15

ggplot(augment(fit_beer), aes(x = Quarter)) +
geom_line(aes(y = Beer, colour = "Data")) +
geom_line(aes(y = .fitted, colour = "Fitted")) +
scale_colour_manual(values = c(Data = "black", Fitted = "#D55E00")) +
labs(y = "Megalitres", title = "Australian quarterly beer production") +
guides(colour = guide_legend(title = "Series"))

# Figure 7.16

ggplot(augment(fit_beer), aes(x = Beer, y = .fitted,
             colour = factor(quarter(Quarter)))) + geom_point() +
  labs(y = "Fitted", x = "Actual values",
  title = "Australian quarterly beer production") +
  geom_abline(intercept = 0, slope = 1) +
  guides(colour = guide_legend(title = "Quarter"))

# Selecting predictors

select(glance(fit_consMR), adj_r_squared, CV, AIC, AICc, BIC)

#!!!!!!!!!!
#Here for chapter 7.5 & 7.6
#!!!!!!!!!!

# Figure 7.17

recent_production <- filter(aus_production, year(Quarter) >= 1992)
fit_beer <- model(recent_production, TSLM(Beer ~ trend() + season()))
fc_beer <- forecast(fit_beer)
autoplot(fc_beer, recent_production) + 
  labs(title = "Forecasts of beer production using regression",
    y = "megalitres")

# Figure 7.18

fit_consBest <- model(us_change,
    lm = TSLM(Consumption ~ Income + Savings + Unemployment))
NewData <- new_data(us_change, 4)
future_scenarios <- scenarios(Increase = mutate(NewData,
  Income=1, Savings=0.5, Unemployment=0),
  Decrease = mutate(NewData, Income=-1, Savings=-0.5,
  Unemployment=0),
  names_to = "Scenario")

fc <- forecast(fit_consBest, new_data = future_scenarios)

autoplot(us_change, Consumption) + autolayer(fc) +
  labs(title = "US consumption", y = "% change")

# Figure 7.19

fit_cons <- model(us_change, TSLM(Consumption ~ Income))
new_cons <- scenarios("Average increase" = mutate(NewData,
                      Income = mean(us_change$Income)),
"Extreme increase" = mutate(NewData, Income = 12), names_to = "Scenario")
fcast <- forecast(fit_cons, new_cons)

autoplot(us_change, Consumption) + autolayer(fcast) +
  labs(title = "US consumption", y = "% change")

# Figure 7.20

BMafter1924 <- filter(boston_marathon, Year >= 1924)
boston_men0 <- filter(BMafter1924, Event == "Men's open division")
boston_men <- mutate(boston_men0, Minutes = as.numeric(Time)/60)

# Figure 7.20

fit_boston <- model(boston_men, TSLM(Minutes ~ trend()))
ggplot(augment(fit_boston), aes(x = Year)) +
  geom_line(aes(y = Minutes, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(values = c(Data = "black", Fitted = "#D55E00")) +
  labs(y = "Megalitres",
  title = "Australian quarterly beer production") +
  guides(colour = guide_legend(title = "Series"))
gg_tsresiduals(fit_boston)

fit_trends <- model(boston_men, linear = TSLM(Minutes ~ trend()),
    exponential = TSLM(log(Minutes) ~ trend()),
    piecewise = TSLM(Minutes ~ trend(knots = c(1950, 1980))))
fc_trends <- forecast(fit_trends, h = 10)

# Figure 7.21

autoplot(boston_men, Minutes) +
  geom_line(data = augment(fit_trends), aes(y = .fitted,
  colour = .model)) +
  autolayer(fc_trends, alpha = 0.5, level = 95) +
  labs(y = "Minutes", title = "Boston marathon winning times")
