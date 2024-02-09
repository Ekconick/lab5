Lab 05 - Data Wrangling
================

# Learning goals

- Use the `merge()` function to join two datasets.
- Deal with missings and impute data.
- Identify relevant observations using `quantile()`.
- Practice your GitHub skills.

# Lab description

For this lab we will be dealing with the meteorological dataset `met`.
In this case, we will use `data.table` to answer some questions
regarding the `met` dataset, while at the same time practice your
Git+GitHub skills for this project.

This markdown document should be rendered using `github_document`
document.

# Part 1: Setup a Git project and the GitHub repository

1.  Go to wherever you are planning to store the data on your computer,
    and create a folder for this project

2.  In that folder, save [this
    template](https://github.com/JSC370/JSC370-2024/blob/main/labs/lab05/lab05-wrangling-gam.Rmd)
    as “README.Rmd”. This will be the markdown file where all the magic
    will happen.

3.  Go to your GitHub account and create a new repository of the same
    name that your local folder has, e.g., “JSC370-labs”.

4.  Initialize the Git project, add the “README.Rmd” file, and make your
    first commit.

5.  Add the repo you just created on GitHub.com to the list of remotes,
    and push your commit to origin while setting the upstream.

Most of the steps can be done using command line:

``` sh
# Step 1
cd ~/Documents
mkdir JSC370-labs
cd JSC370-labs

# Step 2
wget https://raw.githubusercontent.com/JSC370/JSC370-2024/main/labs/lab05/lab05-wrangling-gam.Rmd
mv lab05-wrangling-gam.Rmd README.Rmd
# if wget is not available,
curl https://raw.githubusercontent.com/JSC370/JSC370-2024/main/labs/lab05/lab05-wrangling-gam.Rmd --output README.Rmd

# Step 3
# Happens on github

# Step 4
git init
git add README.Rmd
git commit -m "First commit"

# Step 5
git remote add origin git@github.com:[username]/JSC370-labs
git push -u origin master
```

You can also complete the steps in R (replace with your paths/username
when needed)

``` r
# Step 1
setwd("~/Documents")
dir.create("JSC370-labs")
setwd("JSC370-labs")

# Step 2
download.file(
  "https://raw.githubusercontent.com/JSC370/JSC370-2024/main/labs/lab05/lab05-wrangling-gam.Rmd",
  destfile = "README.Rmd"
  )

# Step 3: Happens on Github

# Step 4
system("git init && git add README.Rmd")
system('git commit -m "First commit"')

# Step 5
system("git remote add origin git@github.com:[username]/JSC370-labs")
system("git push -u origin master")
```

Once you are done setting up the project, you can now start working with
the MET data.

## Setup in R

1.  Load the `data.table` (and the `dtplyr` and `dplyr` packages),
    `mgcv`, `ggplot2`, `leaflet`, `kableExtra`.

``` r
library(data.table)
library(dtplyr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     between, first, last

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(mgcv)
```

    ## Loading required package: nlme

    ## 
    ## Attaching package: 'nlme'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     collapse

    ## This is mgcv 1.8-42. For overview type 'help("mgcv-package")'.

``` r
library(ggplot2)
library(leaflet)
library(kableExtra)
```

    ## 
    ## Attaching package: 'kableExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     group_rows

``` r
fn <- "https://raw.githubusercontent.com/JSC370/JSC370-2024/main/data/met_all_2023.gz"
if (!file.exists("met_all_2023.gz"))
  download.file(fn, destfile = "met_all_2023.gz")
met <- data.table::fread("met_all_2023.gz")

# Some EDA codes from last week
met$lat <- met$lat/1000
met$lon <- met$lon/1000
met$wind.sp <- met$wind.sp/10
met$temp <- met$temp/10
met$dew.point <- met$dew.point/10
met$atm.press <- met$atm.press/10

# Relative Humidity
met$rh <- 100*((112-0.1*met$temp+met$dew.point)/(112+0.9*met$temp))^8
```

2.  Load the met data from
    <https://github.com/JSC370/JSC370-2024/main/data/met_all_2023.gz> or
    (Use
    <https://raw.githubusercontent.com/JSC370/JSC370-2024/main/data/met_all_2023.gz>
    to download programmatically), and also the station data. For the
    latter, you can use the code we used during lecture to pre-process
    the stations data:

``` r
# Download the data
stations <- fread("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv")
stations[, USAF := as.integer(USAF)]
```

    ## Warning in eval(jsub, SDenv, parent.frame()): NAs introduced by coercion

``` r
# Dealing with NAs and 999999
stations[, USAF   := fifelse(USAF == 999999, NA_integer_, USAF)]
stations[, CTRY   := fifelse(CTRY == "", NA_character_, CTRY)]
stations[, STATE  := fifelse(STATE == "", NA_character_, STATE)]

# Selecting the three relevant columns, and keeping unique records
stations <- unique(stations[, list(USAF, CTRY, STATE, LAT, LON)])

# Dropping NAs
stations <- stations[!is.na(USAF)]

# Removing duplicates
stations[, n := 1:.N, by = .(USAF)]
stations <- stations[n == 1,][, n := NULL]

# Read in the met data and fix lat, lon, temp
```

3.  Merge the data as we did during the lecture. Use the `merge()` code
    and you can also try the tidy way with `left_join()`

``` r
names(stations)[names(stations) == "USAF"] <- "USAFID"
merged_data <- merge(stations, met, by = "USAFID")
```

## Question 1: Identifying Representative Stations

Across all weather stations, which stations have the median values of
temperature, wind speed, and atmospheric pressure? Using the
`quantile()` function, identify these three stations. Do they coincide?

``` r
median_temp <- median(merged_data$temp, na.rm = TRUE)
median_wind_sp <- median(merged_data$wind.sp, na.rm = TRUE)
median_press <- median(merged_data$atm.press, na.rm = TRUE)
```

``` r
library(dplyr)

station_medians <- merged_data %>%
  group_by(USAFID) %>%
  summarise(
    median_temperature = median(temp, na.rm = TRUE),
    median_wind_speed = median(wind.sp, na.rm = TRUE),
    median_pressure = median(atm.press, na.rm = TRUE)
  )

matching_stations <- station_medians %>%
  filter(
    median_temperature == median_temp,
    median_wind_speed == median_wind_sp,
    median_pressure == median_press
  )
```

Yes, there is coincidence.

Next identify the stations have these median values.

``` r
print(matching_stations$USAFID)
```

    ## [1] 723119

Knit the document, commit your changes, and save it on GitHub. Don’t
forget to add `README.md` to the tree, the first time you render it.

## Question 2: Identifying Representative Stations per State

Now let’s find the weather stations by state with closest temperature
and wind speed based on the euclidean distance from these medians.

``` r
library(dplyr)

state_medians <- merged_data %>%
  group_by(STATE) %>%
  summarise(
    median_temperature = median(temp, na.rm = TRUE),
    median_wind_speed = median(wind.sp, na.rm = TRUE)
  )

weather_data_with_medians <- merged_data %>%
  left_join(state_medians, by = "STATE")

weather_data_with_medians <- weather_data_with_medians %>%
  mutate(
    distance = sqrt((temp - median_temperature)^2 + (wind.sp - median_wind_speed)^2)
  )
```

``` r
q2a <- weather_data_with_medians %>%
  group_by(STATE)  %>% 
  slice_min(order_by = distance, n = 1) %>%
  select(STATE, USAFID, distance) %>%
   ungroup()

q2b <- weather_data_with_medians %>%
  group_by(STATE)  %>% 
  slice_min(order_by = distance, n = 1) %>%
  arrange(USAFID, distance) %>%
  distinct(USAFID, .keep_all = TRUE)

unique_stations <- q2a %>%
  arrange(USAFID, distance) %>%
  distinct(USAFID, .keep_all = TRUE)

print(unique_stations)
```

    ## # A tibble: 1,109 × 3
    ##    STATE USAFID distance
    ##    <chr>  <int>    <dbl>
    ##  1 SC    720120        0
    ##  2 IL    720137        0
    ##  3 IL    720170        0
    ##  4 MI    720198        0
    ##  5 OR    720202        0
    ##  6 WA    720254        0
    ##  7 GA    720257        0
    ##  8 MN    720258        0
    ##  9 TX    720261        0
    ## 10 GA    720263        0
    ## # ℹ 1,099 more rows

Knit the doc and save it on GitHub.

## Question 3: In the Geographic Center?

For each state, identify which station is closest to the geographic
mid-point (median) of the state. Combining these with the stations you
identified in the previous question, use `leaflet()` to visualize all
~100 points in the same figure, applying different colors for the
geographic median and the temperature and wind speed median.

``` r
library(dplyr)

geographic_midpoints <- weather_data_with_medians %>%
  group_by(STATE) %>%
  summarise(
    median_lat = median(LAT, na.rm = TRUE),
    median_lon = median(LON, na.rm = TRUE)
  )

closest_geo_stations_list <- list()

for (state in unique(geographic_midpoints$STATE)) {
  state_data <- subset(weather_data_with_medians, STATE == state)
  state_midpoint <- subset(geographic_midpoints, STATE == state)
  
  distances <- sqrt((state_data$LAT - state_midpoint$median_lat)^2 + (state_data$LON - state_midpoint$median_lon)^2)
  
  min_distance_index <- which.min(distances)
  
  closest_geo_stations_list[[state]] <- state_data[min_distance_index, ]
}

closest_geo_stations <- do.call(rbind, closest_geo_stations_list)
```

``` r
combined_stations <- rbind(
  closest_geo_stations %>% mutate(Source = "Geographic Median"),
  q2b %>% mutate(Source = "Temp & Wind Median")
)
```

``` r
# library(leaflet)

# leaflet(combined_stations) %>%
#   addTiles() %>%
#   addCircleMarkers(
#     ~LON, ~LAT, color = ~ifelse(Source == "Geographic Median", "blue", "red"),
#     popup = ~paste(STATE, USAFID, Source),
#     radius = 6
#   )
```

It shows that the output of the leaflet function cannot be knit as md
file. So I comment the chunk.

Knit the doc and save it on GitHub.

## Question 4: Summary Table with `kableExtra`

Generate a summary table using `kable` where the rows are each state and
the columns represent average temperature broken down by low, median,
and high elevation stations.

Use the following breakdown for elevation:

- Low: elev \< 93
- Mid: elev \>= 93 and elev \< 401
- High: elev \>= 401

``` r
library(tidyr)
merged_data <- merged_data %>%
  mutate(
    elevation_category = case_when(
      elev < 93 ~ "Low",
      elev >= 93 & elev < 401 ~ "Mid",
      elev >= 401 ~ "High",
      TRUE ~ NA_character_
    )
  )

avg_temp_by_state_elevation <- merged_data %>%
  group_by(STATE, elevation_category) %>%
  summarise(
    average_temperature = mean(temp, na.rm = TRUE),
    .groups = 'drop'
  )

avg_temp_table <- avg_temp_by_state_elevation %>%
  pivot_wider(
    names_from = elevation_category,
    values_from = average_temperature,
  )
library(knitr)

kable(avg_temp_table, caption = "Average Temperature by State and Elevation Category")
```

| STATE |      Low |      Mid |      High |
|:------|---------:|---------:|----------:|
| AL    | 25.07106 | 23.79775 |        NA |
| AR    | 25.58698 | 24.40578 | 23.723926 |
| AZ    | 29.28585 | 30.38057 | 23.892609 |
| CA    | 18.25508 | 18.77071 | 18.148808 |
| CO    |       NA |       NA | 15.184075 |
| CT    | 19.37249 | 18.78433 |        NA |
| DE    | 21.40611 |       NA |        NA |
| FL    | 26.61484 |       NA |        NA |
| GA    | 24.80529 | 23.23841 |        NA |
| IA    |       NA | 22.26228 | 21.992787 |
| ID    |       NA |       NA | 16.415667 |
| IL    |       NA | 22.11707 | 20.843173 |
| IN    |       NA | 20.12731 |        NA |
| KS    |       NA | 24.16196 | 22.098776 |
| KY    |       NA | 21.36103 | 20.178196 |
| LA    | 27.61819 | 26.09414 |        NA |
| MA    | 17.44477 | 17.59058 |        NA |
| MD    | 21.25462 | 20.62255 | 20.648332 |
| ME    | 15.23159 | 15.43930 | 15.329681 |
| MI    |       NA | 18.54432 | 17.977982 |
| MN    | 22.66275 | 21.15523 | 19.931963 |
| MO    | 25.79654 | 23.77652 | 23.300286 |
| MS    | 26.34285 | 24.66682 |        NA |
| MT    |       NA |       NA | 16.293015 |
| NC    | 22.82945 | 21.21073 | 18.046833 |
| ND    |       NA | 21.79236 | 20.415848 |
| NE    |       NA | 23.48598 | 21.048920 |
| NH    | 17.78844 | 16.77731 |  7.243417 |
| NJ    | 19.96563 | 19.31963 |        NA |
| NM    |       NA |       NA | 22.448418 |
| NV    |       NA |       NA | 20.849170 |
| NY    | 18.75621 | 18.31489 | 15.887585 |
| OH    |       NA | 19.43774 |        NA |
| OK    |       NA | 25.07676 | 24.000040 |
| OR    | 15.20318 | 16.39100 | 16.711553 |
| PA    | 20.34185 | 19.40527 | 17.286934 |
| RI    | 17.88116 | 17.46589 |        NA |
| SC    | 23.68407 | 22.38995 |        NA |
| SD    |       NA | 22.79495 | 20.639922 |
| TN    | 25.81362 | 22.89642 | 19.457179 |
| TX    | 28.74462 | 28.08021 | 26.500393 |
| UT    |       NA |       NA | 19.754720 |
| VA    | 21.34826 | 20.49998 | 17.954522 |
| VT    |      NaN | 16.89971 |        NA |
| WA    | 15.25193 | 17.80542 | 16.810354 |
| WI    |       NA | 19.56563 | 17.994615 |
| WV    |       NA | 19.31079 | 17.492150 |
| WY    |       NA |       NA | 13.748173 |

Average Temperature by State and Elevation Category

Knit the document, commit your changes, and push them to GitHub.

## Question 5: Advanced Regression

Let’s practice running regression models with smooth functions on X. We
need the `mgcv` package and `gam()` function to do this.

- using your data with the median values per station, first create a
  lazy table. Filter out values of atmospheric pressure outside of the
  range 1000 to 1020. Examine the association between temperature (y)
  and atmospheric pressure (x). Create a scatterplot of the two
  variables using ggplot2. Add both a linear regression line and a
  smooth line.

- fit both a linear model and a spline model (use `gam()` with a cubic
  regression spline on wind speed). Summarize and plot the results from
  the models and interpret which model is the best fit and why.

``` r
library(mgcv)
library(ggplot2)
library(dplyr)
library(data.table)

setDT(merged_data)

weather_data_filtered <- merged_data[elev >= 1000 & elev <= 1020, ]

ggplot(weather_data_filtered, aes(x = atm.press, y = temp)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +  # Linear regression line
  geom_smooth(method = "gam", formula = y ~ s(x), color = "blue") +  # GAM smooth line
  labs(title = "Temperature vs. Atmospheric Pressure",
       x = "Atmospheric Pressure", y = "Temperature")
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 5554 rows containing non-finite values (`stat_smooth()`).
    ## Removed 5554 rows containing non-finite values (`stat_smooth()`).

    ## Warning: Removed 5554 rows containing missing values (`geom_point()`).

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
# Linear model
linear_model <- lm(temp ~ wind.sp, data = weather_data_filtered)
summary(linear_model)
```

    ## 
    ## Call:
    ## lm(formula = temp ~ wind.sp, data = weather_data_filtered)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -19.6484  -4.8247  -0.3919   4.2223  20.9546 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 18.18784    0.17626  103.19   <2e-16 ***
    ## wind.sp      1.17172    0.03937   29.77   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6.8 on 6292 degrees of freedom
    ##   (1527 observations deleted due to missingness)
    ## Multiple R-squared:  0.1234, Adjusted R-squared:  0.1233 
    ## F-statistic:   886 on 1 and 6292 DF,  p-value: < 2.2e-16

``` r
# Spline model using GAM
spline_model <- gam(temp ~ s(wind.sp, bs = "cr"), data = weather_data_filtered)
summary(spline_model)
```

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## temp ~ s(wind.sp, bs = "cr")
    ## 
    ## Parametric coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 22.77218    0.08463   269.1   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##              edf Ref.df   F p-value    
    ## s(wind.sp) 8.618  8.956 120  <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.145   Deviance explained = 14.6%
    ## GCV =  45.15  Scale est. = 45.081    n = 6294

``` r
plot(spline_model)
```

![](README_files/figure-gfm/unnamed-chunk-13-2.png)<!-- --> The result
of GAM are better, GAM can reveal complex relationships not apparent in
linear models

## Deliverables

- .Rmd file (this file)

- link to the .md file (with all outputs) in your GitHub repository
