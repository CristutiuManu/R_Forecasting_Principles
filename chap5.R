rm(list=ls())
library(fpp3)
Sys.setlocale(locale = "English") 

gdppc <- mutate(global_economy, "GDP_per_capita" = GDP / Population)

# plot

autoplot(filter(gdppc, Country == "Sweden"), GDP_per_capita) +
  labs(y = "$US", title = "GDP per capita for Sweden")

# define the model

TSLM(GDP_per_capita ~ trend())

# estimate

fit <- model(gdppc, trend_model = TSLM(GDP_per_capita ~ trend()))
filter(gdppc,is.na(GDP_per_capita))
report(filter(fit,Country == 'Sweden')) # see output and evaluate

# forecast

forecast(fit, h = "3 years")

# plot everything for some country

fore <- filter(forecast(fit, h = "3 years"), Country == "Sweden") 
autoplot(fore, gdppc) + labs(y = "$US",
  title = "GDP per capita for Sweden") # color='black'

# Figure 5.3 (mean)

recent_prod <- filter_index(aus_production, "1970 Q1" ~ "2004 Q4")
bricks <- select(recent_prod,Bricks)
mean_fit <- model(bricks, MEAN(Bricks))

tidy(mean_fit)  # extract output (1)
results_list <- mean_fit$'MEAN(Bricks)'[[1]] # extract output (2)
mean_results <- results_list$fit

mean_fc <- forecast(mean_fit, h = 12)
bricks_mean = mutate(bricks,hline=mean_fc$.mean[1]) # add a dashed line
autoplot(mean_fc, bricks, level = NULL) +
  autolayer(bricks_mean,hline,linetype='dashed',color='blue')

# Figure 5.4 (naive)

naive_fit <- model(bricks,NAIVE(Bricks))
naive_fc <- forecast(naive_fit, h = 12)
autoplot(naive_fc, bricks, level = NULL)

# Figure 5.5 (seasonal naive)

snaive_fit <- model(bricks,SNAIVE(Bricks ~ lag("year")))
snaive_fc <- forecast(snaive_fit, h = 12)
autoplot(snaive_fc, bricks, level = NULL)

# Figure 5.6 (drift)

drift_fit <- model(bricks,RW(Bricks ~ drift()))
drift_fc <- forecast(drift_fit, h = 12)
autoplot(drift_fc, bricks, level = NULL)

# adding fitted values to the plot

T <- length(bricks$Bricks)
b <- (bricks$Bricks[1] - bricks$Bricks[T])/(1 - T)
a <- 397 - b*140
y <- a + b * seq(1,140,by=1)
DashDR <- tibble(y,Date=bricks$Quarter)
DashDRts <- as_tsibble(DashDR,index=Date)
autoplot(drift_fc, bricks, level = NULL) +
  autolayer(DashDRts,y,color='blue',linetype='dashed')

# Figure 5.7

train <- filter_index(aus_production, "1992 Q1" ~ "2006 Q4") # Set training data from 1992 to 2006
beer_fit <- model(train, Mean = MEAN(Beer), Naive = NAIVE(Beer),
'Seasonal naive' = SNAIVE(Beer))# Fit the models (quotation marks needed for names with blanks)
beer_fc <- forecast(beer_fit, h = 14) # Generate forecasts for 14 quarters
autoplot(beer_fc, train, level = NULL) + # Plot forecasts against actual values
autolayer(filter_index(aus_production, "2007 Q1" ~ .), colour = "black") +
labs(y = "Megalitres", title = "Forecasts for quarterly beer production") +
guides(colour = guide_legend(title = "Forecast"))

# Figure 5.8

recent_GOOG <- filter(gafa_stock, Symbol == "GOOG", year(Date) >= 2015)
goog <- mutate(recent_GOOG, day = row_number())
google_stock <- update_tsibble(goog, index = day, regular = TRUE)
google_2015 <- filter(google_stock, year(Date) == 2015) # Filter the year of interest
google_fit <- model(google_2015, # Fit the models
    Mean = MEAN(Close), Naive = NAIVE(Close), Drift = NAIVE(Close ~ drift()))
google_jan_2016 <- filter(google_stock, yearmonth(Date) == yearmonth("2016 Jan")) # Produce forecasts for the trading days in January 2016
google_fc <- forecast(google_fit, new_data = google_jan_2016)
# Plot the forecasts
autoplot(google_fc, google_2015, level = NULL) +
  autolayer(google_jan_2016, Close, colour = "black") +
  labs(y = "$US", title = "Google daily closing stock prices",
       subtitle = "(Jan 2015 - Jan 2016)") +
  guides(colour = guide_legend(title = "Forecast"))

# Sect. 5.3

augment(beer_fit)
beer_fit1 <- model(train, SNAIVE(Beer))
mean_fitted <- augment(beer_fit1) # fitted values for a single method
ggplot(mean_fitted, aes(x = Quarter)) +
  geom_line(aes(y = Beer),color='black') +
  geom_line(aes(y = .fitted),color='red') 
gg_tsresiduals(beer_fit1)

# Figure 5.9

autoplot(google_2015, Close) +
  labs(y = "$US", title = "Google daily closing stock prices in 2015")

# Figure 5.10

aug <- augment(model(google_2015, NAIVE(Close)))
autoplot(aug, .innov) +
  labs(y = "$US", title = "Residuals from the Naive method")

# Figure 5.11

ggplot(aug, aes(x = .innov)) + geom_histogram() +
  labs(title = "Histogram of residuals")

p0 <- ggplot(aug, aes(x = .innov)) +
  geom_histogram(aes(y=..density..), bins = 20,
  color="black", fill="white")
p0 + stat_function(fun = dnorm, colour = "red",
  args = list(mean = mean(aug$.innov,na.rm=TRUE),
  sd = sd(aug$.innov,na.rm=TRUE)))

# Figure 5.12

autoplot(ACF(aug, .innov)) +
  labs(title = "Residuals from the Naive method")

# Figure 5.13

gg_tsresiduals(model(google_2015, NAIVE(Close)))
features(aug, .innov, list(avg = ~ mean(.,na.rm=TRUE)))

# Portmanteau tests

features(aug, .innov, box_pierce, lag = 10, dof = 0) # Box-Pierce

features(aug, .innov, ljung_box, lag = 10, dof = 0) # Ljung-Box

fit <- model(google_2015, RW(Close ~ drift()))
tidy(fit) # estimate is (y_T-y_1)/(T-1)= (759-522)/251 = 0.944
features(augment(fit), .innov, ljung_box, lag=10, dof=1)

hilo(forecast(model(google_2015, NAIVE(Close)), h = 10))

# Figure 5.14

fore <- forecast(model(google_2015, NAIVE(Close)), h = 10)
autoplot(fore, google_2015) +
  labs(title="Google daily closing stock price", y="$US" )

# Figure 5.15

fit <- model(google_2015, NAIVE(Close))
sim <- generate(fit, h = 30, times = 5, bootstrap = TRUE)
sim

ggplot(google_2015, aes(x = day)) +
  geom_line(aes(y = Close)) +
  geom_line(aes(y = .sim, colour = as.factor(.rep)), data = sim) +
  labs(title="Google daily closing stock price", y="$US" ) +
  guides(colour = "none")

fc <- forecast(fit, h = 30, bootstrap = TRUE) # , times=100
fc

# Figure 5.16

autoplot(fc, google_2015) +
  labs(title="Google daily closing stock price", y="$US" )

fore <- forecast(model(google_2015, NAIVE(Close)), h = 10,
        bootstrap = TRUE, times = 1000)
hilo(fore)

# Figure 5.18 (forecast the seasonally adjusted time series)

us_retail_employment <- filter(us_employment, year(Month) >= 1990,
                        Title == "Retail Trade")
US_model_0 <- model(us_retail_employment,
              STL(Employed ~ trend(window = 7), robust = TRUE))
US_model_1 <- select(components(US_model_0), -.model)
US_fore <- forecast(model(US_model_1, NAIVE(season_adjust)))
autoplot(US_fore, US_model_1) +
  labs(y = "Number of people", title = "US retail employment")

# Figure 5.19 (forecast the time series)

fit_dcmp <- model(us_retail_employment,
    stlf = decomposition_model(STL(Employed ~ trend(window = 7),
          robust = TRUE), NAIVE(season_adjust)))
autoplot(forecast(fit_dcmp), us_retail_employment) +
  labs(y = "Number of people", title = "Monthly US retail employment")

# Figure 5.20

gg_tsresiduals(fit_dcmp)
features(augment(fit_dcmp), .innov, list(avg = ~ mean(.,na.rm=TRUE)))

# subsetting

filter(aus_production, year(Quarter) >= 1995)

slice(aus_production, n()-19:0) # last 20 observations

slice(group_by(aus_retail, State, Industry), 1:12) # working with groups

# Forecast errors

recent_production <- filter(aus_production, year(Quarter) >= 1992)
beer_train <- filter(recent_production, year(Quarter) <= 2007)
beer_fit <- 
  model(beer_train, Mean = MEAN(Beer),
    Naive = NAIVE(Beer),
    'Seasonal naive' = SNAIVE(Beer),
    Drift = RW(Beer ~ drift()))
beer_fc <- forecast(beer_fit, h = 10)

# Figure 5.21

autoplot(beer_fc, recent_production, level = NULL) +
  labs(y = "Megalitres", title = "Forecasts for quarterly beer production") +
  guides(colour = guide_legend(title = "Forecast"))

accTable <- accuracy(beer_fc, recent_production)
select(accTable,.model,RMSE,MAE,MAPE)

# Figure 5.22

google_fit <- model(google_2015, Mean = MEAN(Close),
    Naive = NAIVE(Close),
    Drift = RW(Close ~ drift()))

google_fc <- forecast(google_fit, google_jan_2016)
autoplot(google_fc, bind_rows(google_2015, google_jan_2016),
         level = NULL) +
  labs(y = "$US", title = "Google closing stock prices from Jan 2015") +
  guides(colour = guide_legend(title = "Forecast"))

accTable <- accuracy(google_fc, google_stock)
select(accTable,.model,RMSE,MAE,MAPE)

# Figure 5.23

autoplot(filter(google_fc, .model == "Naive"),
         bind_rows(google_2015, google_jan_2016), level=80) +
  labs(y = "$US",title = "Google closing stock prices")

# Quantile score

accuracy(filter(google_fc, .model == "Naive", Date == "2016-01-04"), google_stock, list(qs=quantile_score), probs=0.10)

# Winkler score

accuracy(filter(google_fc, .model == "Naive", Date == "2016-01-04"), google_stock,
           list(winkler = winkler_score), level = 80)

# Continuous Ranked Probability Score

accuracy(google_fc, google_stock, list(crps = CRPS))

# Cross-validation

google_2015_tr <- relocate(stretch_tsibble(google_2015, .init = 3,
                  .step = 1), Date, Symbol, .id)

# TSCV accuracy

fore <- forecast(model(google_2015_tr, RW(Close ~ drift())), h = 1)
accuracy(fore, google_2015)

# Training set accuracy

accuracy(model(google_2015, RW(Close ~ drift())))

# Figure 5.24

google_2015_tr <- stretch_tsibble(google_2015, .init = 3, .step = 1)
fore <- forecast(model(google_2015_tr, RW(Close ~ drift())), h = 8)
# forecasts for h=1,...,8
test_group <- mutate(group_by(fore, .id), h = row_number())
# for each group (i.e., each training set), set h equal to the row number of the group
tgf = as_fable(test_group,response = "Close", distribution = Close)
accMes = accuracy(tgf, google_2015, by = "h")
ggplot(accMes, aes(x = h, y = RMSE)) +
  geom_point() # display RMSE for each training set and each h
fc <- ungroup(tgf)
ggplot(accuracy(fc, google_2015, by = c("h", ".model")),
       aes(x = h, y = RMSE)) + geom_point() # display average RMSE over all training sets

