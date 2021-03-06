## Author:  An Accounting and Data Science Nerd's Corner
## Source: https://www.r-bloggers.com/tidying-the-john-hopkins-covid-19-data/
##
rm(list=ls())
suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(gghighlight)
  library(ggrepel)
})

dta <- read_csv(
  "https://joachim-gassen.github.io/data/jh_covid19_data_2020-03-23.csv",
  col_types = cols()
) %>%
  mutate(date = ymd(date))

wb_cs <- read_csv(
  "https://joachim-gassen.github.io/data/jh_add_wbank_data.csv", 
  col_types = cols()
)

# I define event time zero where, for a given country, the confirmed
# cases match or exceed the Chinese case number at the beginning of the
# data so that all countries can be compared across event time.
# Also a require each country to have at least 7 days post event day 0

dta %>% 
  group_by(country) %>%
  filter(confirmed >= min(dta$confirmed[dta$country == "China"])) %>%
  summarise(edate_confirmed = min(date)) -> edates_confirmed

dta %>% 
  left_join(edates_confirmed, by = "country") %>%
  mutate(
    edate_confirmed = as.numeric(date - edate_confirmed)
  ) %>%
  filter(edate_confirmed >= 0) %>%
  group_by(country) %>%
  filter (n() >= 7) %>% 
  ungroup() %>%
  left_join(wb_cs, by = "iso3c") %>% 
  mutate(
    confirmed_1e5pop = 1e5*confirmed/population
  ) -> df

lab_notes <- paste0(
  "Data as provided by Johns Hopkins University Center for Systems Science ", 
  "and Engineering (JHU CSSE)\nand obtained on March 23, 2020. ",
  "The sample is limited to countries with at least seven days of positive\n", 
  "event days data. Code and walk-through: https://joachim-gassen.github.io."
)

lab_x_axis_confirmed <- sprintf(paste(
  "Days since confirmed cases matched or exceeded\n", 
  "initial value reported for China (%d cases)\n"
), min(dta$confirmed[dta$country == "China"]))

gg_my_blob <- list(
  scale_y_continuous(trans='log10', labels = scales::comma),  
  theme_minimal(), 
  theme(
    plot.title.position = "plot", 
    plot.caption.position =  "plot",
    plot.caption = element_text(hjust = 0),
    axis.title.x = element_text(hjust = 1),
    axis.title.y = element_text(hjust = 1),
  ),
  labs(caption = lab_notes,
       x = lab_x_axis_confirmed,
       y = "Confirmed cases (logarithmic scale)"),
  gghighlight(TRUE,  label_key = country, use_direct_label = TRUE,
              label_params = list(segment.color = NA, nudge_x = 1))
)

ggplot(df %>% filter (edate_confirmed <= 90), 
       aes(x = edate_confirmed, color = country, y = confirmed)) +
  geom_line() +
  labs(
    title = "Focus on the first month: Confirmed Cases\n"
  ) +
  gg_my_blob

##
# library(RSQLite)
# db <- dbConnect(SQLite(), dbname="../COVIDDB//COVID.sqlite3")
# dta$date <- as.character(dta$date)
# dbWriteTable(db, "JHUDATA",dta ,overwrite=TRUE)
##
### Begin my stuff David Jackson

  