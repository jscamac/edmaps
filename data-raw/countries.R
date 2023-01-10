# List of country codes recognised by GBIF API.
library(jsonlite)
library(dplyr)

countries <- jsonlite::fromJSON(
  'https://raw.githubusercontent.com/gbif/portal16/master/locales/source/en-DK/enums/country.json'
) %>% with(country) %>%
  stack() %>%
  select(gbif2c=ind, name=values) %>%
  mutate(gbif2c=as.character(gbif2c)) %>%
  arrange(gbif2c)

# documentation
cat(
  c(
    "#' Country codes recognised by the GBIF API",
    "#'",
    "#' A data.frame comprising country codes recognised by the GBIF API, and their",
    "#' associated country names.",
    "#'",
    "#' @format ## `countries`",
    sprintf("#' A data frame with %s rows and %s columns:", nrow(countries), ncol(countries)),
    "#' \\describe{",
    "#'   \\item{gbif2c}{GBIF country code}",
    "#'   \\item{name}{Country name}",
    "#' }",
    "#' @details Occurrence data held by GBIF have associated metadata that includes",
    "#'   the name of the country within which the occurrence was observed. Queries",
    "#'   to the GBIF API can be filtered to return occurrences from specific",
    "#'   countries, by passing country codes to the query. Country codes recognised",
    "#'   by GBIF are roughly equal to those included in ISO-3166-1, yet there are",
    "#'   some differences. This dataset includes the full set of accepted country",
    "#'   codes, as at the time of package compilation.",
    "#' @source <https://raw.githubusercontent.com/gbif/portal16/master/locales/source/en-DK/enums/country.json>",
    "'countries'"
  ),
  sep='\n',
  file='R/countries.R'
)

usethis::use_data(countries, overwrite = TRUE)
