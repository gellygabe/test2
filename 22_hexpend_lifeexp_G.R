################################################################################
# HEALTH EXPENDITURES AND LIFE EXPECTANCY
################################################################################

#CLEAR MEMORY
rm(list=ls())


library(readr)
library(dplyr)
library(stargazer)
library(ggplot2)
library(gridExtra)  # to combine graphs
library(plm)  # for panels


# CHECK WORKING DIRECTORY
setwd("C:\\Users\\Géhl Gábor\\Documents\\Text\\G\\Study\\CEU Study\\2016-2017\\Data_Analysis_3\\Seminars\\Seminar_3\\10_LifeExpectancy")
getwd()


getwd()
setwd("c:/Munka/Google Drive/CEU_PHD/Winter 2017/TA/DA3/data_lab/10_LifeExpectancy/")
# Program applies additional functions from here
source("C:\\Users\\Géhl Gábor\\Documents\\Text\\G\\Study\\CEU Study\\2016-2017\\Data_Analysis_3\\Seminars\\Seminar_1\\da_helper_functions.R")
options(digits = 4)

# Load data --------------------------------------------------------------------

# Explore possible errors in the database
health_orig <- read_csv('22_WB_data_on_health.csv')
prob_save <- problems(health_orig)  # population is expected to be integer but is numeric sometimes

# Change the number format from integer to double
health <- read_csv(
    '22_WB_data_on_health.csv', 
    col_types = cols(`Population, total` = col_double())
)

# Take the original names
original_names <- names(health)
# Add new names to the columns
names(health) <- c(
    'year', 'year_code', 'country_name', 'country_code', 
    'hexppc', 'gdppc', 'gdp_growth', 
    'lifeexp', 'infmortality', 'u5mortality', 'pop'
)

# Drops observation before 2011, and countries without data
health <- health %>%
    filter(
        year < 2011,
        !is.na(hexppc), !is.na(lifeexp), !is.na(pop), !is.na(gdppc) 
    ) %>% 
    mutate(pop = pop/10^6) # Count the population in million

# Some descriptives about the dataset --------------------------------------------------------------

# nonmissing years for each country
health %>%
    count(country_name) %>%
    arrange(n) %>%
    print(n = 25)

# missing countries in 2010
health %>%
    group_by(country_name) %>%
    mutate(n = n()) %>%
    filter(year == 2010, n < 16) %>%
    select(hexppc, lifeexp, gdppc, pop) %>%
    summarise_each(funs(mean))

# statistics from 2010
health %>% 
    filter(year == 2010) %>%
    select(hexppc, lifeexp, gdppc, pop) %>%
    as.data.frame() %>%
    stargazer(
        type = 'text', flip = TRUE, digits = 1,
        summary.stat = c('mean', 'min', 'p25', 'median', 'p75', 'max', 'n')
    )

# gdp vs health expenditure (log per capita)
health %>%
    filter(year == 2010) %>%
    ggplot(aes(log(gdppc), log(hexppc))) + 
    geom_point(size = 3, color = 'darkred') +
    xlab('ln GDP per capita (PPP constant 2005$') +
    ylab('ln health expenditure per capita (PPP constant 2005$)')


# world trends: motivation charts
# plotting average life expectancy, log of health expenditure and GDP

world_trend <- health %>%
    group_by(year) %>%
    summarise(
        hexppc = weighted.mean(hexppc, w = pop),
        lifeexp = weighted.mean(lifeexp, w = pop),
        gdppc = weighted.mean(gdppc, w = pop))

world_trend$rellnhexppc = log(world_trend$hexppc) - first(log(world_trend$hexppc))
world_trend$rellngdp = log(world_trend$gdppc) - first(log(world_trend$gdppc)) 

# subplot (1)
p1 <- world_trend %>%
    ggplot(aes(year, lifeexp)) + 
    geom_line(size = 1, color = 'darkgreen') + 
    ylab('average life expectancy') # y-axis label

# subplot (2)
p2 <- world_trend %>%
    ggplot(aes(x = year)) +
    geom_line(aes(y = rellnhexppc), size = 1, linetype = 'dotted', color = 'blue') +
    geom_line(aes(y = rellngdp), size = 1, linetype = 'dashed', color = 'firebrick') +
    geom_text(x = 2009, y = 0.7, label = 'health expenditure', color = 'blue') +
    geom_text(x = 2009, y = 0.25, label = 'GDP', color = 'firebrick') +
    ylab('log change from 1995')

# arrange these two subplots
grid.arrange(p1, p2)


# country-specific trends: selected countries, difference btw 2010 and 1995 data
interesting_countries <- c(
    "Austria", "Bangladesh", "China", "Hungary", 
    "Kenya", "Mexico", "Sweden", "Chad", "United States"
)

health %>%
    filter(country_name %in% interesting_countries) %>%
    group_by(country_name) %>%
    ggplot(aes(year, lifeexp)) + 
        geom_line(aes(color = country_name, linetype = country_name), size = 1) +
        ylab('Life expectancy')


health$rellnhexppc = log(health$hexppc) - first(log(health$hexppc))
health$rellngdp = log(health$gdppc) - first(log(health$gdppc)) 

health %>%
    filter(country_name %in% interesting_countries) %>%
    arrange(year) %>%
    group_by(country_name) %>%
    ggplot(aes(year, rellnhexppc)) + 
        geom_line(aes(color = country_name, linetype = country_name), size = 1) +
        ylab('Health expenditure (log change from 1995)')

# Analysis ---------------------------------------------------------------------

# Panel unit root tests: first we construct the panel database without missing data

countries_without_missing <- health %>%
    count(country_name) %>%
    filter(n == max(n)) %>%
    select(country_name)

panel_health <- health %>%
    right_join(countries_without_missing) %>%  # exclude other countries
    filter(country_name != 'Liberia') %>%  # health expenditure 0 for 3 years
    pdata.frame(index = c('country_name', 'year'))
head(panel_health)
# pdata ....generate the panel dataset???????




purtest(panel_health$lifeexp, exo = 'trend', pmax = 0, test = 'ips')
purtest(log(panel_health$hexppc), exo = 'trend', pmax = 0, test = 'ips')
purtest(log(panel_health$pop), exo = 'trend', pmax = 0, test = 'ips')
#this helps test wether we have serial corelations or don't have. The result says (in leifexp) that there is no serial corelation. We don't need to adjust.
# if there is serial corr., solution can be that we can use diff...??? regression.(talán FE v FD???)

# xcountry OLS: pooled OLS regardless country difference

ols1 <- lm(lifeexp ~ log(hexppc), filter(health, year == 2003))
ols2 <- lm(lifeexp ~ log(hexppc) + log(gdppc) + log(pop), filter(health, year == 2003))

stargazer_r(list(ols1, ols2), digits = 2)


# FE effect = 'twoways'
# Effect: individual, time fixed effect
# This gives the R2 only for coefficients
#plm - panel data p)m

fe1 <- plm( 
    lifeexp ~ log(hexppc), data = panel_health, 
    model = 'within', effect = 'twoways')

fe2 <- plm( 
    lifeexp ~ log(hexppc) + log(gdppc) + log(pop), data = panel_health, 
    model = 'within', effect = 'twoways'
)

fe3 <- plm( 
    lifeexp ~ log(hexppc) + log(pop), data = panel_health, 
    model = 'within', effect = 'twoways'
)

fe_models <- list(fe1, fe2, fe3)
# same as: fe_models <- lapply(ls(pattern = '^fe'), get)
stargazer_r(fe_models, digits = 2)


# FE with explicit time dummies

fe1b <- plm( 
    lifeexp ~ log(hexppc) + year, data = panel_health, 
    model = 'within'
)

fe2b <- plm( 
    lifeexp ~ log(hexppc) + log(gdppc) + log(pop) + year, data = panel_health, 
    model = 'within'
)

fe3b <- plm( 
    lifeexp ~ log(hexppc) + log(pop) + year, data = panel_health, 
    model = 'within'
)
stargazer_r(list(fe1b, fe2b, fe3b), digits = 2)

# this gives the overall R2

# if interested in FE estimates, you may use this:  fixef.plm()

# DETOUR: FE and OLS with demeaning, intuitively leads to the same results

ls_all <- lm(lifeexp ~ log(hexppc) + country_code + year, panel_health)
fe_all <- plm(lifeexp ~ log(hexppc) + year, data = panel_health, model = 'within')
fe_tw <- plm(lifeexp ~ log(hexppc), data = panel_health, model = 'within', effect = 'twoways')

panel_health <- panel_health %>%
    group_by(country_code) %>%
    mutate(lifeexp_dm = lifeexp - mean(lifeexp), lhexppc_dm = log(hexppc) - mean(log(hexppc))) %>%
    group_by(year) %>%
    mutate(lifeexp_ddm = lifeexp_dm - mean(lifeexp_dm), lhexppc_ddm = lhexppc_dm - mean(lhexppc_dm)) %>%
    pdata.frame(index = c('country_name', 'year'))

ls_demean <- lm(lifeexp_dm ~ lhexppc_dm + year, panel_health)
ls_ddmean <- lm(lifeexp_ddm ~ lhexppc_ddm + year, panel_health)

stargazer_r(
    list(ls_all, fe_all, ls_demean, fe_tw, ls_ddmean), 
    digits = 2, omit = 'country_code'
)

# RE: running the same above with random effects

re1 <- plm( 
    lifeexp ~ log(hexppc) + year, data = panel_health, 
    model = 'random'
)

re2 <- plm( 
    lifeexp ~ log(hexppc) + log(gdppc) + log(pop) + year, data = panel_health, 
    model = 'random'
)

re3 <- plm( 
    lifeexp ~ log(hexppc) + log(pop) + year, data = panel_health, 
    model = 'random'
)

re_models <- lapply(ls(pattern = '^re'), get)

stargazer_r(re_models, digits = 2)

# First differences with lags

# dplyr overwrites the lag function of stats, but we want to use the latter
# as it can generate multiple lags at once --> stats::lag() does this
# (previously, we used the lags() from da_helper_functions with as.formula())

diff1 <- plm(
    diff(lifeexp) ~ diff(log(hexppc)) + year, 
    data = panel_health, model = 'pooling'
)

# Adding lags
diff2 <- plm(
    diff(lifeexp) ~ diff(log(hexppc)) + stats::lag(diff(log(hexppc)), 1:2) + year,
    data = panel_health, model = 'pooling'
)
diff3 <- plm(
    diff(lifeexp) ~ diff(log(hexppc)) + stats::lag(diff(log(hexppc)), 1:4) + year, 
    data = panel_health, model = 'pooling'
)
diff4 <- plm(
    diff(lifeexp) ~ diff(log(hexppc)) + stats::lag(diff(log(hexppc)), 1:6) + year,
    data = panel_health, model = 'pooling'
)
diff5 <- plm(
    diff(lifeexp) ~ diff(log(hexppc)) + stats::lag(diff(log(hexppc)), 1:8) + year, 
    data = panel_health, model = 'pooling'
)

stargazer_r(
    list(diff1, diff2, diff3, diff4, diff5), type = 'text',
    omit = 'year', digits = 2
)

# for an unfortunate technical reason the following model cannot be summarized 
# -- although it can be estimated, and gives the same coefficients
# similar to this error: http://stats.stackexchange.com/questions/24131/using-r-and-plm-to-estimate-fixed-effects-models-that-include-interactions-with

# in a loop
diff_models1 <- lapply(
    seq(0, 8, 2), 
    function(max_lag) {
        plm(
            diff(lifeexp) ~ stats::lag(diff(log(hexppc)), 0:max_lag) + year, 
            data = panel_health, 
            model = 'pooling'
        )
    }
)

stargazer_r(diff_models1, type = 'text', omit = 'year', digits = 2)


# Focus on cumulative associations
plm(diff(lifeexp) ~ stats::lag(diff(log(hexppc)), 2) + stats::lag(diff(diff(log(hexppc))), 0:1) + year, data = panel_health, model = 'pooling')
plm(diff(lifeexp) ~ stats::lag(diff(log(hexppc)), 4) + stats::lag(diff(diff(log(hexppc))), 0:3) + year, data = panel_health, model = 'pooling')
plm(diff(lifeexp) ~ stats::lag(diff(log(hexppc)), 6) + stats::lag(diff(diff(log(hexppc))), 0:5) + year, data = panel_health, model = 'pooling')
plm(diff(lifeexp) ~ stats::lag(diff(log(hexppc)), 8) + stats::lag(diff(diff(log(hexppc))), 0:7) + year, data = panel_health, model = 'pooling')

diff_models2 <- lapply(
    seq(2, 8, 2), 
    function(max_lag) {
        plm(
            diff(lifeexp) ~ stats::lag(diff(log(hexppc)), max_lag) + 
                stats::lag(diff(diff(log(hexppc))), 0:(max_lag-1)) + year, 
            data = panel_health, 
            model = 'pooling'
        )
    }
)

stargazer_r(
    c(list(diff1), diff_models2), type = 'text', digits = 2,
    omit = 'year', column.labels = c('base', paste('max lag:', seq(2, 8, 2)))
)


# Cumulative associations with controls
diff3_1 <- plm(
    diff(lifeexp) ~ stats::lag(diff(log(hexppc)), 4) + 
        stats::lag(diff(log(gdppc)), 4) + 
        stats::lag(diff(log(pop)), 4) +
        stats::lag(diff(diff(log(hexppc))), 0:3) + 
        stats::lag(diff(diff(log(gdppc))), 0:3) + 
        stats::lag(diff(diff(log(pop))), 0:3) + year, 
    data = panel_health, model = 'pooling'
)

diff3_2 <- plm(
    diff(lifeexp) ~ stats::lag(diff(log(hexppc)), 4) + 
        stats::lag(diff(log(pop)), 4) +
        stats::lag(diff(diff(log(hexppc))), 0:3) + 
        stats::lag(diff(diff(log(pop))), 0:3) + year, 
    data = panel_health, model = 'pooling'
)

diff3_3 <- plm(
    diff(lifeexp) ~ stats::lag(diff(log(hexppc)), 6) + 
        stats::lag(diff(log(gdppc)), 6) + 
        stats::lag(diff(log(pop)), 6) +
        stats::lag(diff(diff(log(hexppc))), 0:5) + 
        stats::lag(diff(diff(log(gdppc))), 0:5) + 
        stats::lag(diff(diff(log(pop))), 0:5) + year, 
    data = panel_health, model = 'pooling'
)

diff3_4 <- plm(
    diff(lifeexp) ~ stats::lag(diff(log(hexppc)), 6) + 
        stats::lag(diff(log(pop)), 6) +
        stats::lag(diff(diff(log(hexppc))), 0:5) + 
        stats::lag(diff(diff(log(pop))), 0:5) + year, 
    data = panel_health, model = 'pooling'
)


stargazer_r(list(diff3_1, diff3_2), omit = 'year', digits = 2)

stargazer_r(list(diff3_3, diff3_4), omit = 'year', digits = 2)

# Robustness checks

outliers <- c('HTI') # Exclude Haiti (earthquake in the past)
panel_health_no_outlier <- health %>%
    right_join(countries_without_missing) %>%  # exclude other countries
    filter(country_name != 'Liberia') %>%  # health expenditure 0 for 3 years
    filter(!country_code %in% outliers) %>%    
    pdata.frame(index = c('country_name', 'year'))

robust1 <- diff3_4

robust2 <- plm(
    diff(lifeexp) ~ stats::lag(diff(log(hexppc)), 6) + 
        stats::lag(diff(log(pop)), 6) +
        stats::lag(diff(diff(log(hexppc))), 0:5) + 
        stats::lag(diff(diff(log(pop))), 0:5)+year, 
    data = panel_health_no_outlier, model = 'pooling'
)

stargazer_r(
    list(robust1, robust2), digits = 2, omit = 'year',
    column.labels = c('base', 'no outlier')
)

panel_health <- panel_health %>%
    group_by(country_name) %>%
    mutate(mean_pop = mean(pop), mean_gdp = mean(gdppc)) %>% 
    group_by() %>%
    mutate(
        pop_cat = ifelse(mean_pop < median(mean_pop), 'small', 'large'),
        gdp_cat = ifelse(mean_gdp < median(mean_gdp), 'poor', 'rich')
    ) %>%
    pdata.frame(index = c('country_name', 'year'))

robust3 <- plm(
    diff(lifeexp) ~ stats::lag(diff(log(hexppc)), 6) + 
        stats::lag(diff(log(pop)), 6) +
        stats::lag(diff(diff(log(hexppc))), 0:5) + 
        stats::lag(diff(diff(log(pop))), 0:5) + year, 
    data = panel_health, subset = (pop_cat == 'small'), model = 'pooling'
)

robust4 <- plm(
    diff(lifeexp) ~ stats::lag(diff(log(hexppc)), 6) + 
        stats::lag(diff(log(pop)), 6) +
        stats::lag(diff(diff(log(hexppc))), 0:5) + 
        stats::lag(diff(diff(log(pop))), 0:5) + year, 
    data = panel_health, subset = (pop_cat == 'large'), model = 'pooling'
)

stargazer_r(
    list(robust3, robust4), digits = 2, omit = 'year',
    column.labels = c('small', 'large')
)

robust5 <- plm(
    diff(lifeexp) ~ stats::lag(diff(log(hexppc)), 6) + 
        stats::lag(diff(log(pop)), 6) +
        stats::lag(diff(diff(log(hexppc))), 0:5) + 
        stats::lag(diff(diff(log(pop))), 0:5) + year, 
    data = panel_health, subset = (gdp_cat == 'poor'), model = 'pooling'
)

robust6 <- plm(
    diff(lifeexp) ~ stats::lag(diff(log(hexppc)), 6) + 
        stats::lag(diff(log(pop)), 6) +
        stats::lag(diff(diff(log(hexppc))), 0:5) + 
        stats::lag(diff(diff(log(pop))), 0:5) + year, 
    data = panel_health, subset = (gdp_cat == 'rich'), model = 'pooling'
)

stargazer_r(
    list(robust5, robust6), digits = 2, omit = 'year',
    column.labels = c('poor', 'rich')
)

