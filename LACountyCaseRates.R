#library(plyr)  ## replaced revalue with dplyr::recode
library(dplyr)
library(lubridate) # for working with dates
library(ggplot2)  # for creating graphs
library(scales)   # to access breaks/formatting functions
library(gridExtra) # for arranging plots
#https://www.neonscience.org/resources/learning-hub/tutorials/dc-time-series-plot-ggplot-r


cases <- read.csv("LACountyNewDailyCases.csv")
# Cleaning the bad input data! Why did he do that?
#cases$mnum <- plyr::revalue(cases$Month, c("Jan"="01", "jan"="01", "Dec"=12, "dec"=12, "Feb"="02","Mar"="03","Apr"="04","may"="05", "May"="05"))

cases$mnum <- dplyr::recode(cases$Month, "Jan"="01", "jan"="01", "Dec"="12", "dec"="12", "Feb"="02","Mar"="03","Apr"="04","may"="05", "May"="05","Jun"='06',                        "Jul"="07")

# makin a uniform date from "why did he do that?" ymd columns
cases$dstring <- paste0(cases$Year,cases$mnum,cases$Day)
cases$date <- ymd(cases$dstring)

LACountyPop = 10007445
Divisor100K <- LACountyPop / 100000
cases$per100K <- cases$Cases / Divisor100K

cases$per100K

head(cases)
# plot cases these past few days
qplot(x=date, y=per100K,
      data=cases, na.rm=TRUE,
      main="New Dec 2021/May2022 COVID19 cases in LA County",
      xlab="Date", ylab="cases per 100K")


qplot(x=date, y=Cases,
      data=cases, na.rm=TRUE,
      main="New daily COVID19 cases in LA County Dec18-Jun",
      xlab="Date", ylab="new cases")

cases |> ggplot(aes(x=date,y=Cases)) + geom_point()

### SET UP Captions
captionSource='data source: daily reports at http://publichealth.lacounty.gov'
titlestring <- paste('LA County new Covid19 cases thru ',today()-1)


#### Black lines showing density
cases |> ggplot(aes(x=date,y=Cases)) + geom_col() +
  labs(title=titlestring,
       subtitle = "lines show Christmas, New Years, MLK Day, superbowl/valentines\nhorizontal is 1500",
       x="Date",
       y= "cases",
       caption=captionSource) +
  geom_vline(xintercept = as.numeric(as.Date("2021-12-25")), linetype=4, color='red')  +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), linetype=4, color='green') +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-17")), linetype=4, color='yellow') +
  geom_vline(xintercept = as.numeric(as.Date("2022-02-13")), linetype=9, color='red') +    geom_hline(yintercept = 1500, color='red')


  
cases |> ggplot(aes(x=date,y=Cases)) + geom_line() +
  labs(title=titlestring,
       subtitle = "vertical lines show Christmas, New Years, MLK Day, SuperBowl\nhorizontal is 1500",
       x="Date",
       y= "cases",
       caption=captionSource) +
  geom_vline(xintercept = as.numeric(as.Date("2021-12-25")), linetype=4, color='red')  +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), linetype=4, color='green') +
    geom_vline(xintercept = as.numeric(as.Date("2022-01-17")), linetype=10, color='red') +
  geom_vline(xintercept = as.numeric(as.Date("2022-02-13")), linetype=9, color='red') +    geom_hline(yintercept = 1500, color='red')


str(cases)    

checkpoint <- date('2022-04-23')
aftercheck <- interval(checkpoint,today())

cases$date %within% aftercheck

quit('ask')

## What if the rate is this many times higher than reported?
mulfactor <- 6

## Wrong!
# ifelse(cases$date %within% aftercheck,
#   cases$proj <- mulfactor * cases$Cases,
#   cases$proj <- cases$Cases
# )

## Right!
cases$proj <- ifelse(cases$date %within% aftercheck,
       mulfactor * cases$Cases,
       cases$Cases
)


cases$proj == cases$Cases

## OK

## Now, plotting 'proj'

cases |> ggplot(aes(x=date,y=proj)) + geom_line() +
  labs(title=titlestring,
       subtitle = "vertical lines show Christmas, New Years, MLK Day, SuperBowl\nhorizontal is 1500",
       x="Date",
       y= "cases",
       caption=captionSource) +
  geom_vline(xintercept = as.numeric(as.Date("2021-12-25")), linetype=4, color='red')  +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), linetype=4, color='green') +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-17")), linetype=10, color='red') +
  geom_vline(xintercept = as.numeric(as.Date("2022-02-13")), linetype=9, color='red') +    geom_hline(yintercept = 1500, color='red')


  