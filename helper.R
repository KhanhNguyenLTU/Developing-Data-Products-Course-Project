aggregate_by_state <- function(dt, year_min, year_max, evtypes) {
  replace_na <- function(x) ifelse(is.na(x), 0, x)
  round_2 <- function(x) round(x, 2)
  
  states <- data.table(STATE=sort(unique(dt$STATE)))
  
  aggregated <- dt %>% filter(YEAR >= year_min, YEAR <= year_max, EVTYPE %in% evtypes) %>%
    group_by(STATE) %>%
    summarise_each(funs(sum), COUNT:CROPDMG)
  
  
  left_join(states,  aggregated, by = "STATE") %>%
    mutate_each(funs(replace_na), FATALITIES:CROPDMG) %>%
    mutate_each(funs(round_2), PROPDMG, CROPDMG)    
}

aggregate_by_year <- function(dt, year_min, year_max, evtypes) {
  round_2 <- function(x) round(x, 2)
  
  # Filter
  dt %>% filter(YEAR >= year_min, YEAR <= year_max, EVTYPE %in% evtypes) %>%
    # Group and aggregate
    group_by(YEAR) %>% summarise_each(funs(sum), COUNT:CROPDMG) %>%
    # Round
    mutate_each(funs(round_2), PROPDMG, CROPDMG) %>%
    rename(
      Year = YEAR, Count = COUNT,
      Fatalities = FATALITIES, Injuries = INJURIES,
      Property = PROPDMG, Crops = CROPDMG
    )
}

compute_affected <- function(dt, category) {
  dt %>% mutate(Affected = {
    if(category == 'both') {
      INJURIES + FATALITIES
    } else if(category == 'fatalities') {
      FATALITIES
    } else {
      INJURIES
    }
  })
}

compute_damages <- function(dt, category) {
  dt %>% mutate(Damages = {
    if(category == 'both') {
      PROPDMG + CROPDMG
    } else if(category == 'crops') {
      CROPDMG
    } else {
      PROPDMG
    }
  })
}


#Prepare plots of impact by year

plot_impact_by_year <- function(dt, dom, yAxisLabel, desc = FALSE) {
  impactPlot <- nPlot(
    value ~ Year, group = "variable",
    data = melt(dt, id="Year") %>% arrange(Year, if (desc) { desc(variable) } else { variable }),
    type = "stackedAreaChart", dom = dom, width = 650
  )
  impactPlot$chart(margin = list(left = 100))
  impactPlot$yAxis(axisLabel = yAxisLabel, width = 80)
  impactPlot$xAxis(axisLabel = "Year", width = 70)
  
  impactPlot
}

# Prepare plot of number of events by year

plot_events_by_year <- function(dt, dom = "eventsByYear", yAxisLabel = "Count") {
  eventsByYear <- nPlot(
    Count ~ Year,
    data = dt,
    type = "lineChart", dom = dom, width = 650
  )
  
  eventsByYear$chart(margin = list(left = 100))
  eventsByYear$yAxis( axisLabel = yAxisLabel, width = 80)
  eventsByYear$xAxis( axisLabel = "Year", width = 70)
  eventsByYear
}

#Prepare dataset for downloads

prepare_downolads <- function(dt) {
  dt %>% rename(
    State = STATE, Count = COUNT,
    Injuries = INJURIES, Fatalities = FATALITIES,
    Property.damage = PROPDMG, Crops.damage = CROPDMG
  ) %>% mutate(State=state.abb[match(State, tolower(state.name))])
}