## Foreign-Born Baltimorean clusters project

**Project description:** For this project, I analyzed the relationship between geography and being foreign-born in the Baltimore area. I calculated the Gi statistics to determine spatial relationships between census blocks, and used that to create a map of hotspots and "cold-spots" with high or low populations of baltimoreans who were born outside of the United States.


---
title: "Walker_part8"
author: "Simon Winter"
date: "3/2/2022"
output: html_document
---

# Walker book exercises: Chapter 8

```{r load_packages, warning=FALSE, message=FALSE}
library(tidycensus)
library(tidyverse)
library(segregation)
library(tigris)
library(sf)
```

# Question 1: 
## Compute dissimilarity index of a chosen region and compare segregation in that region to in California.

I will be comparing segregation between major urban areas in Florida. This piece of code is just to pull up racial demographic data by census tract in Texas. I then eliminate data that isn't from a census tract within a major urban area.

```{r get Florida tracts, warning=FALSE, message=FALSE, results="hide"}
# Get Florida tract data by race/ethnicity
fl_acs_data <- get_acs(
  geography = "tract",
  variables = c(
    white = "B03002_003",
    black = "B03002_004",
    asian = "B03002_006",
    hispanic = "B03002_012"
  ), 
  state = "FL",
  geometry = TRUE,
  year = 2019
) 

# Use tidycensus to get urbanized areas by population with geometry, 
# then filter for those that have populations of 750,000 or more
us_urban_areas <- get_acs(
  geography = "urban area",
  variables = "B01001_001",
  geometry = TRUE,
  year = 2019,
  survey = "acs1"
) %>%
  filter(estimate >= 750000) %>%
  transmute(urban_name = str_remove(NAME, 
                                    fixed(", FL Urbanized Area (2010)")))

# Compute an inner spatial join between the Texas tracts and the 
# urbanized areas, returning tracts in the largest Texas urban 
# areas with the urban_name column appended
fl_urban_data <- fl_acs_data %>%
  st_join(us_urban_areas, left = FALSE) %>%
  select(-NAME) %>%
  st_drop_geometry()
```

Use the 'Segregation' package to calculate dissimilarity index for the Miami urban area (only measuring segregation between White and Latino populations)

```{r calculate_dissimilarity}
fl_urban_data %>%
  filter(variable %in% c("white", "hispanic"),
         urban_name == "Miami") %>%
  dissimilarity(
    group = "variable",
    unit = "GEOID",
    weight = "estimate"
  )
```
Arrange segregation index values by urban area so that they can be compared to one another. We can clearly see that of the major urban areas in FLorida, white and Latino people are most segregated in Miami, with the Tampa/St. Petersburg area coming in second.

```{r arrange_by_group}
fl_urban_data %>%
  filter(variable %in% c("white", "hispanic")) %>%
  group_by(urban_name) %>%
  group_modify(~
    dissimilarity(.x,
      group = "variable",
      unit = "GEOID",
      weight = "estimate"
    )
  ) %>% 
  arrange(desc(est))
```

In order to compare segregation between all racial groups, not just White and Latino populations, we can use the Mutual Information Index (M) and the Theil’s Entropy Index (T). Like in the previous analysis, this indicates that Miami is the most racially segregated urban area in Florida. However, However, the Tampa/St. Petersburg area and Jacksonville are very close for second. When accounting for all racial backgrounds, Jacksonville is actually slightly more segregated than Tampa. This data is not at all unlike the California segregation index data. Miami is more racially segregated than Los Angeles, the most segregated city in California. 

```{r M_T_indices}
mutual_within(
  data = fl_urban_data,
  group = "variable",
  unit = "GEOID",
  weight = "estimate",
  within = "urban_name",
  wide = TRUE
)
```

local segregation index to determine segregation by census block (in Miami area only)

```{r mutual_local}
fl_local_seg <- fl_urban_data %>%
  filter(urban_name == "Miami") %>%
  mutual_local(
    group = "variable",
    unit = "GEOID",
    weight = "estimate", 
    wide = TRUE
  )

```

Visualize local segregation in Miami

```{r viz Miami segregation}
fl_tracts_seg <- tracts("FL", cb = TRUE) %>%
  inner_join(fl_local_seg, by = "GEOID") 

fl_tracts_seg %>%
  ggplot(aes(fill = ls)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 3857) + 
  scale_fill_viridis_c(option = "inferno") + 
  theme_void() + 
  labs(fill = "Local\nsegregation index")
```

# Question 2:
## Create a regression model for the chosen region, and compare residual spatial autocorrelation in that region to Texas.


I will continue to use Florida as my chosen region.

Here I am simply loading a variety of census variables in each of the counties in Miami-Dade County. Miami-Dade County is very large, and actually significantly larger than the Miami urban area, since it includes rural land to the West and South of Miami.

```{r load_vars}
miami_counties <- c("Miami-Dade")

variables_to_get <- c(
  median_value = "B25077_001",
  median_rooms = "B25018_001",
  median_income = "DP03_0062",
  total_population = "B01003_001",
  median_age = "B01002_001",
  pct_college = "DP02_0068P",
  pct_foreign_born = "DP02_0094P",
  pct_white = "DP05_0077P",
  median_year_built = "B25037_001",
  percent_ooh = "DP04_0046P"
)

miami_data <- get_acs(
  geography = "tract",
  variables = variables_to_get,
  state = "FL",
  county = miami_counties,
  geometry = TRUE,
  output = "wide",
  year = 2019
) %>%
  select(-NAME) %>%
  st_transform(3857) # Mercator / WGS 84
```

Load 'patchwork' library for following visualizations

```{r patchwork_library, warning=FALSE, message=FALSE}
library(patchwork)
```

Creates a map of median home values in Miami-Dade County and a histogram of home values by census tract
(variables beginning with 'MHV' refer to median home value)

```{r med_home_val}
mhv_map <- ggplot(miami_data, aes(fill = median_valueE)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(labels = scales::dollar) + 
  theme_void() + 
  labs(fill = "Median home value ")

mhv_histogram <- ggplot(miami_data, aes(x = median_valueE)) + 
  geom_histogram(alpha = 0.5, fill = "maroon", color = "red",
                 bins = 100) + 
  theme_minimal() + 
  scale_x_continuous(labels = scales::label_number_si(accuracy = NULL)) + 
  labs(x = "Median home value")

mhv_map + mhv_histogram
```

Same map and histogram, but on a logarithmic scale

```{r log_mhv}
mhv_map_log <- ggplot(miami_data, aes(fill = log(median_valueE))) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c() + 
  theme_void() + 
  labs(fill = "Median home\nvalue (log)")

mhv_histogram_log <- ggplot(miami_data, aes(x = log(median_valueE))) + 
  geom_histogram(alpha = 0.5, fill = "navy", color = "navy",
                 bins = 100) + 
  theme_minimal() + 
  scale_x_continuous() + 
  labs(x = "Median home value (log)")

mhv_map_log + mhv_histogram_log
```

Load 'units', 'corrr', and 'car' libraries for following visualizations

```{r libraries, warning=FALSE, message=FALSE}
library(units)
library(corrr)
library(car)
```

Create variables to track population density and average building age in Miami-Dade County.

```{r pop_dens_home_age}
miami_data_for_model <- miami_data %>%
  mutate(pop_density = as.numeric(set_units(total_populationE / st_area(.), "1/km2")),
         median_structure_age = 2017 - median_year_builtE) %>%
  select(!ends_with("M")) %>% 
  rename_with(.fn = ~str_remove(.x, "E$")) %>%
  na.omit()
```

create a linear regression model to determine what predictors are most important in home value in Miami-Dade County.

```{r reg_model}
formula <- "log(median_value) ~ median_rooms + median_income + pct_college + pct_foreign_born + pct_white + median_age + median_structure_age + percent_ooh + pop_density + total_population"

model1 <- lm(formula = formula, data = miami_data_for_model)

summary(model1)
```

create correlation matrix to see which predictors are most closely linked with home value and with one another

```{r corr_matrix}
miami_estimates <- miami_data_for_model %>%
  select(-GEOID, -median_value, -median_year_built) %>%
  st_drop_geometry()

correlations <- correlate(miami_estimates, method = "pearson")

network_plot(correlations)
```

calculate variance inflation factor for a new model that excludes median_income, which is an outlier and not as useful.

```{r model2}
formula2 <- "log(median_value) ~ median_rooms + pct_college + pct_foreign_born + pct_white + median_age + median_structure_age + percent_ooh + pop_density + total_population"

model2 <- lm(formula = formula2, data = miami_data_for_model)

summary(model2)

vif(model2)
```

### These VIF values are substantially different than the DFW examples from the book. The link between total population of a census tract and home value is much higher in Miami than in Dallas. The same is true of the link between the percent foreign-born of a census tract and home values.



run a princeipal components analysis to reduce the higher-dimensional dataset into a lower-dimensional representation based linear combinations of the variables used.

```{r pca}
pca <- prcomp(
  formula = ~., 
  data = miami_estimates, 
  scale. = TRUE, 
  center = TRUE
)

pca_tibble <- pca$rotation %>%
  as_tibble(rownames = "predictor")

summary(pca)
```

create visualization of following table. 

```{r viz_pca}
pca_tibble %>%
  select(predictor:PC5) %>%
  pivot_longer(PC1:PC5, names_to = "component", values_to = "value") %>%
  ggplot(aes(x = value, y = predictor)) + 
  geom_col(fill = "darkgreen", color = "darkgreen", alpha = 0.5) + 
  facet_wrap(~component, nrow = 1) + 
  labs(y = NULL, x = "Value") + 
  theme_minimal()
```


We can see that variables principal component 1 are very effective for predicting home values in Miami. We can then map them to see where these variables are most effective for predicting home value.

```{r map_PC1}
components <- predict(pca, miami_estimates)

miami_pca <- miami_data_for_model %>%
  select(GEOID, median_value) %>%
  cbind(components) 

ggplot(miami_pca, aes(fill = PC1)) +
  geom_sf(color = NA) +
  theme_void() +
  scale_fill_viridis_c()
```


create principal components regression that uses derived components from the PCA as predictors.

```{r pca_model}
pca_formula <- paste0("log(median_value) ~ ", 
                      paste0('PC', 1:6, collapse = ' + '))

pca_model <- lm(formula = pca_formula, data = miami_pca)

summary(pca_model)
```

### (End of section 8.2, beginning of 8.3)

draw histogram of the residuals of model 2 in order to see if they are normally-distributed.

Like the example in the book, there is possibly a very light skew to the right.

```{r check_resid_dist}
miami_data_for_model$residuals <- residuals(model2)

ggplot(miami_data_for_model, aes(x = residuals)) + 
  geom_histogram(bins = 100, alpha = 0.5, color = "navy",
                 fill = "maroon") + 
  theme_minimal()
```

load spdep library for next code chunk

```{r spdep_lib, warning=FALSE, message=FALSE}
library(spdep)
```


conduct an I test of the residual data in order to determine if it is normally distributed

```{r i_test}
wts <- miami_data_for_model %>%
  poly2nb() %>%
  nb2listw()

moran.test(miami_data_for_model$residuals, wts)
```

The I statistic is 0.16, which is smaller than the book's example of 0.21, but still statistically significant, (just barely).

Here is a visualization of the I statistic:

```{r viz_i_test}
miami_data_for_model$lagged_residuals <- lag.listw(wts, miami_data_for_model$residuals)

i_test <- ggplot(miami_data_for_model, aes(x = residuals, y = lagged_residuals)) + 
  theme_minimal() + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", color = "blue")
```

load spatialreg library for next code chunk

```{r spatialreg_library, warning=FALSE, message=FALSE}
library(spatialreg)
```

Use the spatial lag model in order to estimate the relationship between median house value and its predictors.

```{r spatial_lag}
lag_model <- lagsarlm(
  formula = formula2, 
  data = miami_data_for_model, 
  listw = wts
)

summary(lag_model, Nagelkerke = TRUE)
```

Like in the book's example, the rho parameter is positive and statistically significant (but smaller than the example). In addition, according to this model, the median number of rooms is substantially more important to house value in Miami than it is in Dallas: the effect size in Miami is approximately 0.19, but it's .006 in Dallas-Fort Worth.

The spatial error model tells us the approximate error coefficients from the previous model:

```{r error_model}
error_model <- errorsarlm(
  formula = formula2, 
  data = miami_data_for_model, 
  listw = wts
)

summary(error_model, Nagelkerke = TRUE)
```

### The lambda value of the spatial error model in Miami is 0.421. This is a high value, and shows that spatial autocorrelation is a substantial issue of this method of determining predictors of home value. However, it isn't as high as the lambda value of the same spatial error model applied to Dallas-Fort Worth, (0.473) meaning that spatial autocorrelation is less of an issue in Miami than in Dallas-FOrt WOrth.

# Question 3:
## Create a geodemographic classification for your region using the sample code in this chapter. Does the typology you’ve generated resemble that of Dallas-Fort Worth, or does it differ?

### This first part about GWR modelling isn't actually relevant to this question but I did it and I can't bear to erase it all.

This part requires the GWmodel package.

```{r gwmodel_lib, warning=FALSE, message=FALSE}
library(GWmodel)
```

Determine adaptive kernel bandwidth to run the geographically weighted regression model.

```{r det_kernel}
miami_data_sp <- miami_data_for_model %>%
  as_Spatial()

bw <- bw.gwr(
  formula = formula2, 
  data = miami_data_sp, 
  kernel = "bisquare",
  adaptive = TRUE
)
```

The adaptive bandwidth chosen is 202.

Using the adaptive kernel, evaluate the fit of the GW model. This uses the same formula as earlier in the chapter, except without median house income.

```{r gwr_fit}
formula2 <- "log(median_value) ~ median_rooms + pct_college + pct_foreign_born + pct_white + median_age + median_structure_age + percent_ooh + pop_density + total_population"

gw_model <- gwr.basic(
  formula = formula2, 
  data = miami_data_sp, 
  bw = bw,
  kernel = "bisquare",
  adaptive = TRUE
)

gw_model_results <- gw_model$SDF %>%
  st_as_sf() 

names(gw_model_results)
```

We can now map the spatial polygons data frame (seen in table above) to visualize how accurate the model is predicted to be in each census block in Miami.

```{r map_sdf}
ggplot(gw_model_results, aes(fill = Local_R2)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c() + 
  theme_minimal()
```

This shows that the geographically weighted regression model is less accurate at predicting trends in Miami than it is in the book's example of the DFW area. In Miami, it is much more accurate in downtown areass, and much less effective in the outer suburbs.

We can also visualize how effective the model is for predicting the relationship between owner-occupied housing and home value, as see below:

```{r map_sdf_owners}
ggplot(gw_model_results, aes(fill = percent_ooh)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c() + 
  theme_void() + 
  labs(fill = "Local β for \npercent_ooh")
```
This seems to indicate that the relationship between percent homeowners and home value is much stronger in suburban areas outside of downtown Miami. This is very similar to the book's DFW example.

Now, let's examine the strength of the relationship between structure age and home value:

```{r map_sdf_age}
ggplot(gw_model_results, aes(fill = median_structure_age)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c() + 
  theme_void() + 
  labs(fill = "Local β for \nmedian_structure_age")
```

Similar to in Dallas, building age is a stronger indicator of home value in downtown Miami.

It's important to note that DFW has some significant weaknesses when done on the local level. Because it lacks a lot of the context of census data outside of the area being studied, it often misleadingly indicates strong correlations between different factors. DFW is a useful tool to keep in mind when studying an area, but it shouldn't be presented as proof of a strong correlation between different factors.

### OK here is where the actual work for question 3 starts:

K-clusters are census tracts that are demographically similar to one another but different from the other clusters.
In order to generate them in a reproducible (reproductible?) way, we use a seed. 

I will continue to use Miami.

```{r k_means}
set.seed(1234)

miami_kmeans <- miami_pca %>%
  st_drop_geometry() %>%
  select(PC1:PC8) %>%
  kmeans(centers = 6)

table(miami_kmeans$cluster)
```
The smallest cluster is 42 census blocks large, the largest is 138.
Let's map those clusters.

```{r map_clusters}
miami_clusters <- miami_pca %>%
  mutate(cluster = as.character(miami_kmeans$cluster))

ggplot(miami_clusters, aes(fill = cluster)) + 
  geom_sf(size = 0.1) + 
  scale_fill_brewer(palette = "Set2") + 
  theme_void() + 
  labs(fill = "Cluster ")
```

The clusters look somewhat cohesive actually, especially compared to the book's DFW map. With plotly, we can now plot each tract as a point on a graph, where larger values on the x-axis (PC1) represent wealthier, older, and whiter areas, whereas larger values on the y-axis (PC3) represent more rural areas.

```{r plotly_library, warning=FALSE, message=FALSE}
library(plotly)
```


```{r plot_clusters}
cluster_plot <- ggplot(miami_clusters, 
                       aes(x = PC1, y = PC3, color = cluster)) + 
  geom_point() + 
  scale_color_brewer(palette = "Set2") + 
  theme_minimal()

ggplotly(cluster_plot) %>%
  layout(legend = list(orientation = "h", y = -0.15, 
                       x = 0.2, title = "Cluster"))
```

This is surprisingly quite different from the plot from DFW. In Dallas, the wealthiest, oldest, and whitest neighborhoods were also the most urban, whereas in Miami, they are actually very rural (comparatively to the rest of the tracts). However, the k-clusters clearly did a good job of grouping together similar neighborhoods according to these demographic characteristics.


Now let's use the SKATER algorithm in order to create clusters that are contiguous. SKATER works by pruning tracts that do not touch the rest of the group, or that have demographic qualities that differ too much from the rest in its cluster.

"costs" are the difference in demographic factors between tracts.



