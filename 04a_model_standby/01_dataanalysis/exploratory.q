---
title: "Exploratory Analysis: StandyBy Data"
author: "Georg Grunsky"
format: html
editor: visual
---

## Reading the Data

Two files provided: sickness_table.csv and sickness_table.xlsx Files are apparently the same. Needs to be checked:

Reading the csv-file

```{r}
#| echo: false
#| message: false
#| warning: false
require(readr)
sickness_csv <- read_csv("C:/Users/ChSnsMgt/iCloudDrive/DLMDWME01/01_data/01_lake/01_use_case_2/sickness_table.csv", 
    col_types = cols(date = col_datetime(format = "%Y-%m-%d")))
head(sickness_csv)
```

Reading the excel-file

```{r}
#| echo: false
#| message: false
#| warning: false
require(readxl)
sickness_xls <- read_excel("C:/Users/ChSnsMgt/iCloudDrive/DLMDWME01/01_data/01_lake/01_use_case_2/sickness_table.xlsx", 
    col_types = c("numeric", "date", "numeric", 
        "numeric", "numeric", "numeric", "numeric", 
        "numeric"))
head(sickness_xls)
```

Identical check

```{r}
identical(sickness_csv, sickness_xls)
```

Quick identical check says false. Datastructure and Datasummary will be run on both datasets for detailed comparison.

## Datastructure

Print out data structure

### sickness_csv

```{r}
#| echo: false
#| message: false
#| warning: false

str(sickness_csv)

```

### sickness_xls

```{r}
#| echo: false
#| message: false
#| warning: false

str(sickness_xls)
```

## Data Summary

Run base package summary on both data sets

### sickness_csv

```{r}
#| echo: false
#| message: false
#| warning: false

print("### sickness_csv:")

summary(sickness_csv)

```

### sickness_xls

```{r}
#| echo: false
#| message: false
#| warning: false


summary(sickness_xls)
```

Conclusion:

1.  Obviously the data sets are identical. In further exploration only the csv will be used.
2.  From the summary one can read that constant 90 n_sby is to much in most cases compared with sby_need but there are occasions where more than that is needed.
3.  The timerange of the data goes from 2016-04-01 to 2019-05-27. Thats a period of three years which started before the "covid era" and ended just after the beginning of the latter. Covid certainly had an ongoing effect on sicknesses and emergency calls in the scenario. So we know, the data is outdated, but we still we need to use it for the prediction and hope to convince the directors board with our model and get the possibility to acquire more actual data in the future.
4.  The number of calls and the number of sick personel seem to have a similar proportion, only by the factor of 100. A thing to keep in mind.

## Look for NAs and missing dates

We also need to check whether there are any NAs or missing dates in the data set. It didn't look like it though.

```{r}

bp <- sickness_csv

# sickness_csv[30,7] <- NA
# sickness_csv[100,2] <- NA

alldays <- seq(from = as.Date("2016-04-01"),
               to = as.Date("2019-05-27"),
               by = 1)
if (identical(alldays, as.Date(sickness_csv$date))) print("no missing dates") else print("you have to deal w/ missing dates")

if (!anyNA.data.frame(sickness_csv)) print("no missing values") else print("you have to deal w/ missing values")

bp -> sickness_csv
```

Luckily we don't have to deal with either missing dates or values.

## Data Quality

asdfasdfaf

## Overview variables

```{r}
#| message: false
#| warning: false
plot(sickness_csv)
```

Conclusion:

sby_need (the predicted variable) is apparently mostly depended on the number of calls, (which makes sense) and on dafted personnel (which will be ignored because that is to be avoided). A hint of dependency is also recognizable between the variables sby_need, calls and n_sick and the variable date, which could acknowledge the alleged seasonality of the data. It is advisable to shift the focus to these features.

Logically there should be a dependency between the number of sick personnel and the n_duty feature, which can't be seen in this plots. In the next section, we will add another plot with an ordered nb_sick column in comparison to n_duty and the number of emergency calls.

## Focus on specific variables

```{r}
#| message: false
#| warning: false
plot(sickness_csv[order(sickness_csv$n_sick),c(3:5)])
plot(sickness_csv[,c(2:4,7)])
```

The first plot shows, that n_duty has only three different values which are not affected by n_sick. We won't have influence on n_duty with our prediction model in the first run, so we can ignore that variable. It also shows that there seems to be no dependency between n_sick and calls. Still there are two ideas

1.  We know from a previous analysis that n_sick and calls possibly have a similar value distribution (only by a factor of 100) but are not related to each other. Still it will be difficult to get these two variables on the same plot. Let's try to add a feature with divided calls by 100 to get them on the same scale.
2.  It sounds logical, that the more sick personel the more reduced the ability to respond to emergency calls (even that n_duty was not related to n_sick). Probably it might be interesting to add another feature that shows calls per sick personnel to describe the mentioned ability to respond to emergencies. If there was a relation between those to, the average result would be around 100 (the factor), but fortunately there wasn't.

In the second figure, it seems even more clearly that the date has an influence on the other three variables. Let's therefore generate some more features out of the date variable to see if it is possible to be more specific about this fact. The shown figures analyse the three variables in comparison to the new features.

```{r}
#| message: false
#| warning: false
require(lubridate, quietly = T)
require(dbplyr, quietly = T)
sick <- sickness_csv[,c(2:4,7)]
sick <- sick %>% dplyr::mutate(year = lubridate::year(date),
                        month = lubridate::month(date),
                        week = lubridate::week(date),
                        wday = lubridate::wday(date),
                        calls100 = calls/100,
                        call_per_sick = calls/n_sick, 
                        day = lubridate::day(date))
plot(sick[,c(4:8)])
plot(sick[,c(2,5:8)])
plot(sick[,c(3,5:8)])
plot(sick[,c(10,4:8)])
plot(sick[,c(2,7,9)])
```

Regarding the figures all three variables in question are mostly affected by the month and the week features. Plus, there seems to be a slight dependency of the target variable sby_need to the new calls_per_sick feature (the value range is similar, so we'll get them on one plot). Let's get into more detail on this.

```{r}
#| message: false
#| warning: false
require(ggplot2, quietly = T)
require(plotly, quietly = T)
require(data.table, quietly = T)
require(reshape2, quietly = T)

# reshape tables
week <- reshape2::melt(sick[,c(2, 4,7,9,10)], id.vars = "week") %>% 
  dplyr::group_by(week, variable) %>%
  dplyr::summarise(n = mean(value))

month <- reshape2::melt(sick[,c(2,4,6,9,10)], id.vars = "month") %>% 
  dplyr::group_by(month, variable) %>%
  dplyr::summarise(n = mean(value))

weekday <- reshape2::melt(sick[,c(2,4,8,10)], id.vars = "wday") %>% 
  dplyr::group_by(wday, variable) %>%
  dplyr::summarise(n = mean(value))

p_weekday <- ggplot(data = weekday) +
  geom_line(aes(x = wday, y = n, colour = variable)) +
  geom_smooth(aes(x = wday, y = n, colour = variable))

p_week <- ggplot(data = week) +
  geom_line(aes(x = week, y = n, colour = variable)) +
  geom_smooth(aes(x = week, y = n, colour = variable))

p_month <- ggplot(data = month) +
  geom_line(aes(x = month, y = n, colour = variable)) +
  geom_smooth(aes(x = month, y = n, colour = variable))


ggplotly(p_week)
ggplotly(p_month)

```

It looks like, with call_per_sick, we found a promising predictor of the target variable, with almost parallel smooth curves. For the seasonality the week scale seems fine, the monthly analyse was rather a little to general. Interestingly in the week scale, one can see periodically reoccurring peaks every 3-5 weeks between week 5 and 44. For call_per_sick the peaks are not as clearly visible as for sby_need and do sometimes have an offset to the latter. On the monthly scale, those peaks disappear completely, but we do see some higher values especially in the months may and august.

A 3-5 weeks period looks like the day of month could be of relevance. The next plot compares therefore the day of month to the possible predictors.

```{r}
#| message: false
#| warning: false
day <- reshape2::melt(sick[,c(2,4,9,10,11)], id.vars = "day") %>% 
  dplyr::group_by(day, variable) %>%
  dplyr::summarise(n = mean(value))

p_day <- ggplot(data = day) +
  geom_line(aes(x = day, y = n, colour = variable)) +
  geom_smooth(aes(x = day, y = n, colour = variable))

ggplotly(p_day)
```

The smooth curve shows an obvious downward trend at the beginning of each month. Smaller peaks occur approximately every three days. Regarding a seasonality pattern, the day of month is surly to be kept in mind.

Let's also check the weekday scale, just to be complete.

```{r}
#| message: false
#| warning: false
ggplotly(p_weekday)


```

According to the smooth curve there seems to be a strong variance in the weekday pattern. Appearently, the lines contain no additional information.

Lastly, see what it looks like if we plot the continuous weekly scale over the whole period. Possibly, there is a larger trend as well.

```{r}
#| message: false
#| warning: false
weeks_per_year <- reshape2::melt(sick[,c(4,5,7,10)],
                                 id.vars = c("year","week")) %>%
  dplyr::group_by(year, week, variable) %>%
  dplyr::summarise(n = mean(value)) %>%
  dplyr::mutate(contweek = year + week/52)
  
  

p_wpy <- ggplot(data = weeks_per_year) +
  geom_line(aes(x = contweek, 
                # x = week,
                y = n,
                colour = variable)) +
                # colour = as.factor(paste0(variable,year)), 
                # group = year)) +
  geom_smooth(aes(x = contweek, 
                  # x = week,
                  y = n, 
                  colour = variable))
                  # colour = as.factor(paste0(variable,year)), 
                  # group = year))

ggplotly(p_wpy)

```

The variable call_per_sick slightly decreases in the long trend but still seems to be a good predictor until start of 2019. By 2019 call_per_sick loses its predictive ability due to the rapid increase of sby_need. There are two ideas to possibly face that.

1.  Add call100 to this figure to see if this feature could help
2.  Try logarithmic scale, if we could flatten the sby_need trend, adding 1 to where sby_need == 0 for displaying logarithmic values.

```{r}
#| message: false
#| warning: false
weeks_per_year2 <- reshape2::melt(sick[,c(4,5,7,9,10)],
                                 id.vars = c("year","week")) %>%
  dplyr::group_by(year, week, variable) %>%
  dplyr::summarise(n = mean(value)) %>%
  dplyr::mutate(contweek = year + week/52)
  
# add 10 to sby_need == 0 to avoid log(0)
weeks_per_year2[(weeks_per_year2$variable == "sby_need" & 
                 weeks_per_year2$n == 0), "n"] <- 1

p_wpy2 <- ggplot(data = weeks_per_year2) +
  geom_line(aes(x = contweek, 
                # x = week,
                # y = n,
                y = log(n),
                colour = variable)) +
                # colour = as.factor(paste0(variable,year)), 
                # group = year)) +
  geom_smooth(aes(x = contweek, 
                  # x = week,
                  # y = n, 
                  y = log(n), 
                  colour = variable))
                  # colour = as.factor(paste0(variable,year)), 
                  # group = year))

ggplotly(p_wpy2)
```

Conclusion:

1.  calls100 doesn't help.
2.  We do have a positive trend over the years with a sharp increase in the need of standby personnel since start of 2019
3.  The logarithmic scale seems to have a positive effect on the ability for call_per_sick to work as a predictor regarding the smooth curve. We could try to acknowledge that if we divide log(call_per_sick) by log(sby_need). The more flatten the line, the more dependency we have (see figure below).
4.  A linear regression based on smooth curves will probably not work out well. We still need to consider a strong weekly seasonality and a minor monthly seasonality.

```{r}
#| message: false
#| warning: false
cps <- weeks_per_year2[weeks_per_year2$variable == "call_per_sick",]$n
need <- weeks_per_year2[weeks_per_year2$variable == "sby_need",]$n

p <- ggplot(data.frame(date = weeks_per_year2$contweek,
                       dep_check = log(cps)/log(need)),
            aes(x = date,
                y = dep_check)) +
  geom_line() + 
  geom_smooth()

ggplotly(p)
```

That doesn't look too bad. We do have reoccurring patterns and we have a small, almost linear trend on the smooth curve. Let's work with that too.

## Over-all Conclusion:

1.  We do have a positive trend over the years with a sharp increase in the need of standby personnel since start of 2019
2.  We possibly can use the call_per_sick feature as predictor regarding the logarithmic smooth curve on a weekly scale. Still, in this case the calls and the number of sick drivers needs to be predicted first, because they will not be known in advance. According to the day of month-plot, that might a doable task.
3.  A linear regression based on smooth curves alone will probably not work out well. We still need to consider a strong weekly seasonality, a dependence on the day of month and a minor monthly seasonality. To properly forceast sby_need values, a decomposition of the time-series data could be advisable.
