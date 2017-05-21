#practise project - electoral commission data for political donations
library(tidyverse)
library(lubridate)

donate <- read_csv("results.csv") %>%
    # Change error in date and then coerce character date variables to datetime
    mutate( AcceptedDate = ifelse(AcceptedDate == "17/06/2106", "17/06/2016", AcceptedDate),
            ReceivedDate = ifelse(ReceivedDate == "17/06/2106", "17/06/2016", ReceivedDate),
            AcceptedDate = dmy(AcceptedDate),
            ReceivedDate = dmy(ReceivedDate),
            ReportedDate = dmy(ReportedDate),
            #remove £ sign from donation value and make numeric
            Value = parse_number(Value)
           )

glimpse(donate)
View(head(donate,100))


#-------------------------------------------------------------------------------

#data organisation
# Time series: AcceptedDate, ReceivedDate, ReportedDate,
#              ReportingPeriodName = time category
# Important Metrics: Value, Counts of donations
# Important Categories/Dimensions:
#   For parties: RegulatedEntityName, RegulatedEntityType,
#   For donors: DonorName, DonorStatus, Post Code, DonationType
# Keys: ECRef, RegulatedEntityID, DonorID



# Questions and Hypotheses
# Biggest value donations go to Conservatives
# Labour have largest volume of small donations
# More donations for referendum than 2015 ge election?
# Does this data set hold predictors for value and timings of future donations?
# If we follow the money we can get insight into motivations of political parties
# Do any donors seem to be connected to new or controversial policies? Could we
# infer undue influence?
# what other data could we join to this that would help with insight or prediction?


#EDA ---------------------------------------------------------------------------
# explore date ranges

summary(donate$AcceptedDate)
#Min.      1st Qu.       Median         Mean      3rd Qu.         Max.
#"2001-01-01" "2006-01-06" "2009-09-17" "2009-08-24" "2013-11-05" "2017-03-27"
#NA's
#"617"

summary(donate$ReceivedDate)
#        Min.      1st Qu.       Median         Mean      3rd Qu.         Max.
#"2001-01-01" "2005-12-09" "2009-08-28" "2009-08-11" "2013-12-02" "2017-03-25"
#NA's
#"2244"

summary(donate$ReportedDate)
#        Min.      1st Qu.       Median         Mean      3rd Qu.         Max.
#"2001-02-01" "2006-04-26" "2009-10-30" "2009-11-13" "2014-01-30" "2017-12-23"
#NA's
#"21"

# fewest missing values for ReportedDate

#variables to change from character to date
#AcceptedDate,
#ReceivedDate,
#ReportedDate

#ReportingPeriodName could be split into month and year variables
#OR keep if meaningful and do the same with one of the other date variables

#explore time line
ggplot(donate, aes(AcceptedDate, Value)) +
    geom_line()
#Warning message:
#Removed 617 rows containing missing values (geom_path).

#want to group dates in order to better see trend
hist(donate$AcceptedDate, breaks = 20)

#be good to compare three dates against each other - so freqpoly I think best
?geom_freqpoly

Accepted_timeline <- donate %>%
    ggplot() +
        geom_freqpoly(aes(AcceptedDate, colour = "AcceptedDate"), binwidth = 90) +
        geom_freqpoly(aes(ReceivedDate, colour = "ReceivedDate"), binwidth = 90) +
        geom_freqpoly(aes(ReportedDate, colour = "ReportedDate"), binwidth = 90)

# count of donations accepted peak for general elections, with highest for ge2015


#donate %>%
#    mutate(months = month(ReportedDate, label = TRUE))
#    ggplot(aes(ReportedDate, Value)) +
#        geom_line()

donate %>%
    mutate(month_reported = month(ReportedDate, label = TRUE)) %>%
    ggplot() +
        geom_point(aes(month_reported, Value))


#ggplot(donate, aes(AcceptedDate)) +
#    geom_histogram() +
#    coord_cartesian(ylim = c(0, 10))
#focusing the y axis reveals just the one outlier, which from the data looks to
#be a type 2106 == 2016
# add a change to


# Top entities ------------------------------------------------------------

#need to filter EntityName to just the top (parties)
top50_entities <- donate %>%
    count(RegulatedEntityName, sort = TRUE) %>%
    top_n(50, n)

View(top50_entities)
names(donate)

#top_entity_stats <- donate %>%
#    filter(RegulatedEntityName == top50_entities$RegulatedEntityName) %>%
#    group_by(RegulatedEntityName) %>%
#    summarise(
#        donated = sum(Value)
#    ) %>%
#    arrange(desc(donated))

#View(top_entity_stats)

#top50_entities <- top50_entities %>%
#    left_join(top_entity_stats, by = "RegulatedEntityName")


# summary stats by party
party_summary <- donate %>%
    group_by(RegulatedEntityName) %>%
    summarise(
        no.donations = n(),
        avg.donation = mean(Value),
        median.donation = median(Value),
        min.donation = min(Value),
        max.donation = max(Value),
        total.donation = sum(Value)
    ) %>%
    top_n(50, no.donations) %>%
    arrange(desc(total.donation))

Party_name <- party_summary$RegulatedEntityName

library(forcats)
party_summary <- party_summary %>%
    mutate(Party = factor(RegulatedEntityName, levels = Party_name))

View(party_summary)
#issues with avg I think (unless such a high value due to a few large outliers skewing distribution)

# explore distribution of donations
hist(donate$Value, breaks = 50)

#add party factor to donate dataset

party_list <- donate %>%
                count(RegulatedEntityName, n(), sort = TRUE)

head(party_list)

donate <- donate %>%
    mutate(Party = factor(RegulatedEntityName, levels = party_list))

View(head(donate))

donate %>%
    group_by(Party) %>%
    ggplot(aes(Party, Value)) +
        geom_col()

donate %>%
    count(DonorName, sort = TRUE)

donors <- donate %>%
    count(Party) %>%
    top_n(50, n)


#create new data frame just with data would want to model, forecast or analyse quantitatively
donate_mod <- donate %>%
    select()


pairs(donate)

glimpse(donate)

ggplot() +
        geom_bar(aes(Party))



ggplot(donate) +
    geom_bar(aes(Party, Value))



head(donate)

