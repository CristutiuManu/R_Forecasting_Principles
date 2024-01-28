rm(list=ls())
library(fpp3)
library(latex2exp)
library(slider)
library(gridExtra)
Sys.setlocale(locale = "English") 

# Figure 3.4

guer <- features(aus_production, Gas, features = guerrero)
lambda <- guer$lambda_guerrero
autoplot(aus_production, box_cox(Gas, lambda)) +
  labs(y = "", title = TeX(paste0("Transformed gas production with $\\lambda$ = ",
         round(lambda,2))))

# Figure 3.5

us_ret <- filter(us_employment, year(Month) >= 1990, Title == "Retail Trade")
us_retail_employment <- select(us_ret, -Series_ID)
autoplot(us_retail_employment, Employed) +
  labs(y = "Persons (thousands)", title = "Total employment in US retail")

dcmp <- model(us_retail_employment, stl = STL(Employed))
components(dcmp)

# Figure 3.6

autoplot(as_tsibble(components(dcmp)), Employed, colour="gray") +
  geom_line(aes(y=trend), colour = "#D55E00") +
  labs(y = "Persons (thousands)", title = "Total employment in US retail")

# Figure 3.7

autoplot(components(dcmp))

# Figure 3.8

autoplot(as_tsibble(components(dcmp)), Employed, colour = "gray") +
geom_line(aes(y=season_adjust), colour = "#0072B2") +
  labs(y = "Persons (thousands)", title = "Total employment in US retail")

# Figure 3.9

autoplot(filter(global_economy, Country == "Australia"), Exports) +
  labs(y = "% of GDP", title = "Total Australian exports")

# Figure 3.10

aus_exports <- mutate(filter(global_economy, Country == "Australia"),
    `5-MA` = slide_dbl(Exports, mean, .before = 2, .after = 2, .complete = TRUE))
autoplot(aus_exports, Exports) +
  geom_line(aes(y = `5-MA`), colour = "#D55E00") +
  labs(y = "% of GDP", title = "Total Australian exports") +
  guides(colour = guide_legend(title = "series"))

# Figure 3.11

aus_exports <- mutate(filter(global_economy, Country == "Australia"),
                      `3-MA` = slide_dbl(Exports, mean, .before = 1, .after = 1, .complete = TRUE),
                      `5-MA` = slide_dbl(Exports, mean, .before = 2, .after = 2, .complete = TRUE),
                      `7-MA` = slide_dbl(Exports, mean, .before = 3, .after = 3, .complete = TRUE),
                      `9-MA` = slide_dbl(Exports, mean, .before = 4, .after = 4, .complete = TRUE))
p1 <- autoplot(aus_exports, Exports) + # MA3
  geom_line(aes(y = `3-MA`), colour = "#D55E00") +
  labs(y = "% of GDP", title = "MA3") +
  guides(colour = guide_legend(title = "series"))
p2 <- autoplot(aus_exports, Exports) + # MA5
  geom_line(aes(y = `5-MA`), colour = "#D55E00") +
  labs(y = "% of GDP", title = "MA5") +
  guides(colour = guide_legend(title = "series"))
p3 <- autoplot(aus_exports, Exports) + # MA7
  geom_line(aes(y = `7-MA`), colour = "#D55E00") +
  labs(y = "% of GDP", title = "MA7") +
  guides(colour = guide_legend(title = "series"))
p4 <- autoplot(aus_exports, Exports) + # MA9
  geom_line(aes(y = `9-MA`), colour = "#D55E00") +
  labs(y = "% of GDP", title = "MA9") +
  guides(colour = guide_legend(title = "series"))
grid.arrange(p1,p2,p3,p4,nrow=2)

# Table 3.2

beer <- select(filter(aus_production, year(Quarter) >= 1992), Quarter, Beer)
beer_ma <- mutate(beer, `4-MA` = slide_dbl(Beer, mean,
          .before = 1, .after = 2, .complete = TRUE),
        `2x4-MA` = slide_dbl(`4-MA`, mean, .before = 1, .after = 0, .complete = TRUE))
autoplot(beer_ma, Beer) + 
  geom_line(aes(y = `2x4-MA`), colour = "red")

# Figure 3.12

us_retail_employment_ma <- mutate(us_retail_employment, `12-MA` = slide_dbl(Employed, mean,
                                .before = 5, .after = 6, .complete = TRUE),
    `2x12-MA` = slider::slide_dbl(`12-MA`, mean,
                                  .before = 1, .after = 0, .complete = TRUE)
  )
autoplot(us_retail_employment_ma, Employed, colour = "gray") +
  geom_line(aes(y = `2x12-MA`), colour = "#D55E00") +
  labs(y = "Persons (thousands)", title = "Total employment in US retail")

# Figure 3.13

dec <- model(us_retail_employment, classical_decomposition(Employed, type = "additive"))
dec_comp <- components(dec)
autoplot(dec_comp) +
  labs(title = "Classical additive decomposition of total US retail employment")

# Figure 3.18

stl_dec <- model(us_retail_employment, STL(Employed ~ trend(window = 7) +
          season(window = "periodic"), robust = TRUE))
autoplot(components(stl_dec)) # see also lines 23-30 above

