---
title: "Proj_try"
output: 
  html_document:
    keep_md: TRUE
    toc: TRUE
    toc_float: TRUE
---



## Import


```r
data <- read.csv("suicide.csv")
```




```r
glimpse(data)
```

```
## Observations: 27,820
## Variables: 12
## $ X...country        <fct> Albania, Albania, Albania, Albania, Albania...
## $ year               <int> 1987, 1987, 1987, 1987, 1987, 1987, 1987, 1...
## $ sex                <fct> male, male, female, male, male, female, fem...
## $ age                <fct> 15-24 years, 35-54 years, 15-24 years, 75+ ...
## $ suicides_no        <int> 21, 16, 14, 1, 9, 1, 6, 4, 1, 0, 0, 0, 2, 1...
## $ population         <int> 312900, 308000, 289700, 21800, 274300, 3560...
## $ suicides.100k.pop  <dbl> 6.71, 5.19, 4.83, 4.59, 3.28, 2.81, 2.15, 1...
## $ country.year       <fct> Albania1987, Albania1987, Albania1987, Alba...
## $ HDI.for.year       <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ gdp_for_year....   <fct> "2,156,624,900", "2,156,624,900", "2,156,62...
## $ gdp_per_capita.... <int> 796, 796, 796, 796, 796, 796, 796, 796, 796...
## $ generation         <fct> Generation X, Silent, Generation X, G.I. Ge...
```

## Overview

###Total

Number of suicides per 100,000 people all over the world from 1985 to 2016.


```r
total <- data %>%
  select("X...country":"suicides.100k.pop", "HDI.for.year":"generation") %>%
  group_by(year) %>%
  summarize(suicides_per_100k = (sum(suicides_no)/sum(population)) * 100000)
mean <- round(mean(total$suicides_per_100k), 2)
mean
```

```
## [1] 13.08
```


```r
ggplot(total) +
  geom_point(aes(year, suicides_per_100k)) +
  geom_smooth(aes(year, suicides_per_100k), se = T, span = .3)+
  ylim(5, 17) +
  geom_hline(yintercept = 13.08)
```

![](proj_try_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

###Sex

Number of suicides per 100k for males and females.


```r
sex <- data %>%
  select("year":"suicides.100k.pop", "HDI.for.year":"generation") %>%
  group_by(year, sex) %>%
  summarize(suicides_per_100k = (sum(suicides_no)/sum(population)) * 100000)
sex
```

```
## # A tibble: 64 x 3
## # Groups:   year [32]
##     year sex    suicides_per_100k
##    <int> <fct>              <dbl>
##  1  1985 female              6.33
##  2  1985 male               16.9 
##  3  1986 female              6.45
##  4  1986 male               17.2 
##  5  1987 female              6.26
##  6  1987 male               17.1 
##  7  1988 female              6.13
##  8  1988 male               17.1 
##  9  1989 female              6.57
## 10  1989 male               20.0 
## # ... with 54 more rows
```


```r
ggplot() +
  geom_point(data = sex, aes(year, suicides_per_100k, color = sex)) +
  geom_smooth(data = sex, aes(year, suicides_per_100k, color = sex), span = 0.3)+
  geom_smooth(data = total, aes(year, suicides_per_100k), span = 0.2)
```

![](proj_try_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


```r
sex_country <- data %>%
  select("X...country":"suicides.100k.pop", "HDI.for.year":"generation") %>%
  group_by(X...country, sex) %>%
  summarize(suicides_per_100k = (sum(suicides_no)/sum(population)) * 100000)
sex_bar <- ggplot(sex_country) +
  geom_col(aes(reorder(X...country, suicides_per_100k), suicides_per_100k, fill = sex), identity = "dodge") +
  coord_flip()
```

###Age

Number of suicides per 100k for each age group.


```r
age <- data %>%
  select("year":"suicides.100k.pop", "HDI.for.year":"generation") %>%
  group_by(year, age) %>%
  summarize(suicides_per_100k = (sum(suicides_no)/sum(population))*100000)
age
```

```
## # A tibble: 191 x 3
## # Groups:   year [32]
##     year age         suicides_per_100k
##    <int> <fct>                   <dbl>
##  1  1985 5-14 years              0.494
##  2  1985 15-24 years             9.07 
##  3  1985 25-34 years            12.0  
##  4  1985 35-54 years            14.5  
##  5  1985 55-74 years            18.8  
##  6  1985 75+ years              29.8  
##  7  1986 5-14 years              0.465
##  8  1986 15-24 years             9.07 
##  9  1986 25-34 years            12.3  
## 10  1986 35-54 years            14.8  
## # ... with 181 more rows
```


```r
ggplot() +
   geom_point(data = age, aes(year, suicides_per_100k, color = age)) +
  geom_smooth(data = age, aes(year, suicides_per_100k, span = .2, color = age)) +
  geom_smooth(data = total, aes(year, suicides_per_100k), span = .2, color = "black")
```

![](proj_try_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

###Generation

Number of suicides per 100k for each generation.


```r
gen <- data %>%
  select("year":"suicides.100k.pop", "HDI.for.year":"generation") %>%
  group_by(year, generation) %>%
  summarize(suicides_per_100k = (sum(suicides_no)/sum(population))*100000)
gen
```

```
## # A tibble: 146 x 3
## # Groups:   year [32]
##     year generation      suicides_per_100k
##    <int> <fct>                       <dbl>
##  1  1985 G.I. Generation             21.1 
##  2  1985 Silent                      14.5 
##  3  1985 Boomers                     12.0 
##  4  1985 Generation X                 4.76
##  5  1986 G.I. Generation             21.4 
##  6  1986 Silent                      14.8 
##  7  1986 Boomers                     12.3 
##  8  1986 Generation X                 4.73
##  9  1987 G.I. Generation             21.6 
## 10  1987 Silent                      14.9 
## # ... with 136 more rows
```


```r
ggplot() +
  geom_smooth(data = gen, aes(year, suicides_per_100k, color = generation), span =.5) +
  geom_smooth(data = total, aes(year, suicides_per_100k), span = .3, color = "black")
```

![](proj_try_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

###Women more than men?


```r
country_sex <- data %>%
  group_by(X...country, year, sex) %>%
  summarize(suicides_per_100k = (sum(suicides_no)/sum(population))*100000) %>%
  ungroup() %>%
  group_by(X...country, sex) %>%
  summarize(suicides_per_100k = sum(suicides_per_100k)/n()) %>%
  rename(country = X...country)
country_sex <- spread(country_sex, sex, suicides_per_100k)
filter(country_sex, male > female)
```

```
## # A tibble: 99 x 3
## # Groups:   country [101]
##    country              female  male
##    <fct>                 <dbl> <dbl>
##  1 Albania              2.19    4.10
##  2 Antigua and Barbuda  0.0772  1.06
##  3 Argentina            3.50   12.5 
##  4 Armenia              1.22    3.86
##  5 Aruba                2.53   14.2 
##  6 Australia            5.61   20.5 
##  7 Austria             10.5    31.7 
##  8 Azerbaijan           0.719   2.29
##  9 Bahamas              0.386   2.48
## 10 Bahrain              1.22    4.01
## # ... with 89 more rows
```

```r
filter(country_sex, male <= female)
```

```
## # A tibble: 2 x 3
## # Groups:   country [101]
##   country               female  male
##   <fct>                  <dbl> <dbl>
## 1 Dominica                   0     0
## 2 Saint Kitts and Nevis      0     0
```

##Country most


```r
country_total_pop <- data %>%
  group_by(X...country, year) %>%
  summarize(population = sum(population)) %>%
  ungroup() %>%
  group_by(X...country) %>%
  summarise(average_population_1m = (sum(population)/n())/1e6) %>%
  rename(country = X...country)

country <- data %>%
  group_by(X...country, year) %>%
  summarize(suicides_per_100k = (sum(suicides_no)/sum(population))*100000) %>%
  ungroup() %>%
  group_by(X...country) %>%
  summarize(average_suicides_per_100k = sum(suicides_per_100k)/n()) %>%
  rename(country = X...country)

Country <- left_join(country, country_total_pop, by = "country")
```


```r
Country <- arrange(Country, desc(average_suicides_per_100k))
Country
```

```
## # A tibble: 101 x 3
##    country            average_suicides_per_100k average_population_1m
##    <fct>                                  <dbl>                 <dbl>
##  1 Lithuania                               40.7                  3.09
##  2 Russian Federation                      32.7                137.  
##  3 Sri Lanka                               30.8                 16.6 
##  4 Belarus                                 30.2                  9.40
##  5 Hungary                                 29.6                  9.56
##  6 Latvia                                  28.0                  2.14
##  7 Kazakhstan                              27.0                 14.5 
##  8 Slovenia                                26.4                  1.92
##  9 Estonia                                 25.7                  1.29
## 10 Ukraine                                 24.7                 45.9 
## # ... with 91 more rows
```


```r
ggplot(Country) +
  geom_col(aes(reorder(country, average_suicides_per_100k), average_suicides_per_100k, color = average_suicides_per_100k)) +
  scale_fill_viridis_c()
```

![](proj_try_files/figure-html/unnamed-chunk-16-1.png)<!-- -->


```r
map_world <- map_data("world")
country_map <- Country 
country_map$country <- recode(country_map$country, 'United States' = 'USA','United Kingdom' = 'UK', "Russian Federation" = "Russia", "Republic of Korea" = "South Korea", "United Arab Emirates"= "UAE")
world_map <- left_join(map_world, country_map, by = c('region' = 'country'))
```

```r
ggplot(world_map) +
  geom_polygon(aes(long, lat, group = group, fill = average_suicides_per_100k)) +
  labs(fill = "s") +
  scale_fill_viridis_c() +
  coord_quickmap()
```

![](proj_try_files/figure-html/unnamed-chunk-18-1.png)<!-- -->


##GDP vs. suicides


```r
country_year <- data %>%
  group_by(country.year) %>%
  summarize(suicides_per_100k = (sum(suicides_no)/sum(population))*100000) %>%
  ungroup()
eco <- data %>%
  select(X...country, year, country.year:gdp_per_capita....) %>%
  unique() %>%
  rename(country = X...country, gdp_per_capita = gdp_per_capita....)
```


```r
country_eco <- inner_join(country_year, eco)
country_eco$continent <- countrycode(country_eco$country, "country.name", "continent")
```


```r
country_pop <- select(data, country.year, population) %>%
  group_by(country.year) %>%
  summarize(population = sum(population))
country_eco <- left_join(country_eco, country_pop)
```

###line, by continent, GDP vs. suicides


```r
continent_eco_gdp <- country_eco %>%
  group_by(year, country) %>%
  na.omit(gdp_per_capita) %>%
  summarise(pop = mean(population), suicides_per_100k = mean(suicides_per_100k), gdp_per_capita = mean(gdp_per_capita)) %>%
  ungroup() 
continent_eco_gdp$continent <- countrycode(continent_eco_gdp$country, "country.name", "continent")
country_eco_gdp <- continent_eco_gdp %>%
  group_by(country) %>%
  summarise(suicides_per_100k = mean(suicides_per_100k), gdp_per_capita = mean(gdp_per_capita))
country_eco_gdp$continent <- countrycode(country_eco_gdp$country, "country.name", "continent")
country_eco_gdp
```

```
## # A tibble: 90 x 4
##    country             suicides_per_100k gdp_per_capita continent
##    <fct>                           <dbl>          <dbl> <chr>    
##  1 Albania                         2.12           2356  Europe   
##  2 Antigua and Barbuda             0             13679  Americas 
##  3 Argentina                       7.84           9744. Americas 
##  4 Armenia                         2.64           2605. Asia     
##  5 Australia                      12.6           49847. Oceania  
##  6 Austria                        19.7           39290. Europe   
##  7 Azerbaijan                      0.789           586. Asia     
##  8 Bahamas                         2.08          30700. Americas 
##  9 Bahrain                         2.38          21668. Asia     
## 10 Barbados                        2.48          13497. Americas 
## # ... with 80 more rows
```


```r
country_gdp <- select(country_eco_gdp, gdp_per_capita, suicides_per_100k)

ggplot(country_eco_gdp, aes(gdp_per_capita, suicides_per_100k)) +
  geom_point(aes(color = continent), show.legend = T) +
  geom_smooth(method = "lm", formula = y~x)
```

![](proj_try_files/figure-html/unnamed-chunk-23-1.png)<!-- -->

```r
summary(lm(country_eco_gdp$gdp_per_capita~country_eco_gdp$suicides_per_100k))
```

```
## 
## Call:
## lm(formula = country_eco_gdp$gdp_per_capita ~ country_eco_gdp$suicides_per_100k)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -23178 -13740  -9081  12383  64090 
## 
## Coefficients:
##                                   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                        17353.5     3343.9   5.190 1.34e-06 ***
## country_eco_gdp$suicides_per_100k    182.2      241.7   0.754    0.453    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 19500 on 88 degrees of freedom
## Multiple R-squared:  0.006415,	Adjusted R-squared:  -0.004876 
## F-statistic: 0.5682 on 1 and 88 DF,  p-value: 0.453
```


##HDI


```r
continent_eco_hdi <- country_eco %>%
  group_by(year, country) %>%
  na.omit(HDI.for.year) %>%
  summarise(pop = mean(population), suicides_per_100k = mean(suicides_per_100k), HDI = mean(HDI.for.year)) %>%
  ungroup() 
country_eco_hdi <- continent_eco_hdi %>%
  group_by(country) %>%
  summarise(suicides_per_100k = mean(suicides_per_100k), HDI= mean(HDI))
country_eco_hdi$continent <- countrycode(country_eco_hdi$country, "country.name", "continent")
country_eco_hdi
```

```
## # A tibble: 90 x 4
##    country             suicides_per_100k   HDI continent
##    <fct>                           <dbl> <dbl> <chr>    
##  1 Albania                         2.12  0.673 Europe   
##  2 Antigua and Barbuda             0     0.782 Americas 
##  3 Argentina                       7.84  0.780 Americas 
##  4 Armenia                         2.64  0.690 Asia     
##  5 Australia                      12.6   0.913 Oceania  
##  6 Austria                        19.7   0.848 Europe   
##  7 Azerbaijan                      0.789 0.624 Asia     
##  8 Bahamas                         2.08  0.780 Americas 
##  9 Bahrain                         2.38  0.805 Asia     
## 10 Barbados                        2.48  0.757 Americas 
## # ... with 80 more rows
```


```r
ggplot(country_eco_hdi) +
  geom_point(aes(HDI, suicides_per_100k, color = continent)) 
```

![](proj_try_files/figure-html/unnamed-chunk-25-1.png)<!-- -->

```r
summary(lm(country_eco_hdi$HDI~country_eco_hdi$suicides_per_100k))
```

```
## 
## Call:
## lm(formula = country_eco_hdi$HDI ~ country_eco_hdi$suicides_per_100k)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.194460 -0.057336 -0.001286  0.068159  0.148827 
## 
## Coefficients:
##                                   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                       0.740259   0.014059  52.655   <2e-16 ***
## country_eco_hdi$suicides_per_100k 0.002516   0.001016   2.476   0.0152 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.08197 on 88 degrees of freedom
## Multiple R-squared:  0.06512,	Adjusted R-squared:  0.0545 
## F-statistic:  6.13 on 1 and 88 DF,  p-value: 0.0152
```
