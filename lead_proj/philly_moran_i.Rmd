---
title: "Lead Risk philadelphia"
author: "Simon Winter"
date: "5/2/2022"
output: html_document
---

# determine housing lead risk in philadelphia 
##using methodology described in Jacobs et al 2002, doi: 10.1289/ehp.021100599


##setup

```{r setup, warning=FALSE, results='hide'}
library(tidycensus)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(sf)
library(tigris)
library(sf)
library(spdep)
library(tmap)
options(tigris_use_cache = TRUE)
```

# Determine lead poisoning risk from housing

## use get_acs to pull up raw housing age data

```{r full_data, warning=FALSE, results='hide'}
full_data_philly <- get_acs(
  geography = "tract",
  state = "PA",
  county = "Philadelphia",
  year = 2019,
  output = "wide",
  variables = c(built_39   = "B25034_011E",   #housing units built before 1939
                built40_49 = "B25034_010E",   #housing units built between 1940 and 1949
                built50_59 = "B25034_009E",   #housing units built between 1950 and 1959
                built60_69 = "B25034_008E",   #housing units built between 1960 and 1969
                built70_79 = "B25034_007E",   #housing units built between 1970 and 1979
                built80_89 = "B25034_006E",   #housing units built between 1980 and 1989
                built90_99 = "B25034_005E",   #housing units built between 1990 and 1999
                built00_09 = "B25034_004E",   #housing units built between 2000 and 2009
                built10_13 = "B25034_003E",   #housing units built between 2010 and 2014
                totalhousing = "B25034_001",  #total housing units
                
                ratio_0_5 = "B17026_002",     #poverty ratio >0.5 (by household)
                ratio0_5_0_99 = "B17026_003", #poverty ratio between 0.5-0.99 (by household)
                ratio01_0_1_24 = "B17026_004",#poverty ratio between 1-1.24 (by household)
                totalhousehold = "B17026_001" #total number of households
                ),
geometry = TRUE
)
```
## organize into 20-year increments instead of by decade built

```{r condense_df}
full_data_philly <- mutate(
    full_data_philly, built40_59 = built40_49 + built50_59,
    built60_79 = built60_69 + built70_79,
    built80_ = built80_89 + built90_99 + built00_09 + built10_13
  )
```

## set up lead risk table by housing age, according to housing age categories that correspond to lead risk in Jacobs et al 2002:

~0% homes built after 1980 have a lead risk
~8% homes built built 1960-1979 have a lead risk
~43% homes built built 1940-1959 have a lead risk
~68% homes built built before 1940 have a lead risk

```{r housing_risk}
full_data_philly <- mutate(
  full_data_philly, houses_at_risk = (0.08 * built60_79) + (0.43 * built40_59) + (0.68 * built_39)
)

```

## determine percentage of houses with lead risk in each census block

```{r at_risk_percent}
full_data_philly <- mutate(
  full_data_philly, at_risk_percent_house = houses_at_risk / totalhousingE
)

```

# Determine lead poisoning risk from poverty
## use get_acs to pull up raw poverty ratio data

## organize into groups: "poverty ratio below 1.24" and "poverty ratio above 1.24"

```{r condense_pov}
full_data_philly <- mutate(
    full_data_philly, ratio_below1_24 = ratio_0_5E + ratio0_5_0_99E + ratio01_0_1_24E,
    ratio_above1_24 = totalhouseholdE - ratio_below1_24
  )
```

## set up lead risk table by poverty ratio, as close as possible to the income categories that correspond to lead risk in Jacobs et al 2002:

~0.5% of households with poverty ratio equal to or greater than 1.3 have a lead risk
~6% of households with poverty ratio lower than 1.3 have a lead risk

### I was not able to retrieve census data of poverty ratio with an increment that has a cutoff at 1.3. I was forced to use a cutoff at 1.24 instead. 

We would expect slightly less than 0.5% of households with poverty ratio equal to or greater than 1.24 to have a lead risk, and more than 6% of households with poverty ratio lower than 1.3 to have a lead risk.However, I did not adjust the weightings to reflect this in order to avoid pulling numbers out of my ass. Unfortunately this is a source of error that I cannot eliminate within the scope of this project.


```{r poverty_risk}
full_data_philly <- mutate(
  full_data_philly, houses_at_risk = (0.005 * ratio_above1_24) + (0.06 * ratio_below1_24)
)
```

## determine percentage of households with lead risk in each census block

```{r at_risk_percent_pov}
full_data_philly <- mutate(
  full_data_philly, at_risk_percent_pov = houses_at_risk / totalhouseholdE
)
```

#determine combined risk from housing and poverty

## combine risk from poverty and risk from housing into a single figure that shows both, using weightings as laid out by Jacobs et al 2002:

housing risk is multiplied by a factor of 0.58
poverty risk is multiplied by a factor of 0.42

both figures are then added together

```{r combined_risk}
combined_risk_philly <- full_data_philly %>%
  select(GEOID, NAME, geometry, at_risk_percent_house, at_risk_percent_pov)

combined_risk_philly <- mutate(
  combined_risk_philly, combined_risk = (0.58 * at_risk_percent_house) + (0.42 * at_risk_percent_house))
```

### quick map to ensure that the data looks correct. 

```{r map3}
plot(combined_risk_philly["combined_risk"])
```



# map of lead risk in Philadelphia

```{r tmap}
data(combined_risk_philly)
tmap_mode("view")
colorRamp <- colorRampPalette(c('#4527a0', '#0080ff', '#daf0ff', '#ffffa7', '#ff7a00', '#ff1500', '#7c0d0e')) #define color ramp

tm_shape(combined_risk_philly) +
  tm_polygons("combined_risk", title = "percent households with lead risk", style = "cont", palette = colorRamp(1000), border.alpha = 0) +
  tm_legend(main.title = "Philadelphia lead risk") +
  tm_layout("Philadelphia lead risk")
```

# Moran's I test
## map geographical relationships between tracts

```{r neighborhood_relationships}
combined_risk_philly[is.na(combined_risk_philly)] <- 0 #need to eliminate na values

neighbors <- poly2nb(combined_risk_philly, queen = TRUE)

philly_coords <- combined_risk_philly %>%
  st_centroid() %>%
  st_coordinates()

plot(combined_risk_philly$geometry)
plot(neighbors, 
     coords = philly_coords, 
     add = TRUE, 
     col = "blue", 
     points = FALSE)
```

## add weights to connection: counties with centroids closer to one another are considered closer to one another

```{r weights}
weights <- nb2listw(neighbors, style = "W")

combined_risk_philly$lag_estimate <- lag.listw(weights, combined_risk_philly$combined_risk)
```

## plots the relationship between percent foreign-born residents with spatial lag

```{r plot_spatial_lag}
ggplot(combined_risk_philly, aes(x = combined_risk, y = lag_estimate)) + 
  geom_point(alpha = 0.3) + 
  geom_abline(color = "red") + 
  theme_minimal() + 
  labs(title = "Percent households at lead risk according to model",
       x = "Percent households at risk",
       y = "Spatial lag, percent households at risk", 
       caption = "Lead hazard prediction model as described in Jacobs et al 2002, doi: 10.1289/ehp.021100599.")
```


## moran test and map spatial correlation using LocalG

```{r moran_test}
moran.test(combined_risk_philly$combined_risk, weights)

# For Gi*, re-compute the weights with `include.self()`
localg_weights <- nb2listw(include.self(neighbors))

combined_risk_philly$localG <- localG(combined_risk_philly$combined_risk, localg_weights)

ggplot(combined_risk_philly) + 
  geom_sf(aes(fill = localG), color = NA) + 
  scale_fill_distiller(palette = "RdYlBu") + 
  theme_void() + 
  labs(fill = "Local Gi* statistic")
```

## highlight hotspots in above map

```{r highlight_hotspots}
combined_risk_philly <- combined_risk_philly %>%
  mutate(hotspot = case_when(
    localG >= 2.50 ~ "High cluster",
    localG <= -2.50 ~ "Low cluster",
    TRUE ~ "Not significant"
  ))

ggplot(combined_risk_philly) + 
  geom_sf(aes(fill = hotspot), color = "grey90", size = 0.1) + 
  scale_fill_manual(values = c("red", "blue", "grey")) + 
  theme_void()
```

