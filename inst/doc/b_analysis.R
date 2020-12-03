## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.height = 5,
  fig_width = 8,
  warning = FALSE
)

## ----setup--------------------------------------------------------------------
library(ialiquor)

data("liquor_sales")

liquor <- liquor_sales

## ----preview_liquor-----------------------------------------------------------

dplyr::glimpse(liquor)


## ----usecase-1----------------------------------------------------------------

library(dplyr)
library(ggplot2)

liquor %>%
  group_by(year, county) %>%
  summarize(
    total_revenue = sum(state_revenue),
    .groups = 'drop'
  ) %>%
  ungroup() %>%
  group_by(year) %>%
  slice(which.max(total_revenue))


## -----------------------------------------------------------------------------
liquor %>%
  group_by(year, county) %>%
  summarize(
    population = max(population),
    .groups = 'drop'
  ) %>%
  ungroup() %>%
  group_by(year) %>%
  slice(which.max(population))

## -----------------------------------------------------------------------------
liquor %>%
  mutate(
    rev_per_person = round(state_revenue / population , 2)
  ) %>%
  group_by(year, county) %>%
  summarize(
    rev_per_person = round(sum(rev_per_person),2),
    .groups = 'drop'
  ) %>%
  ungroup() %>%
  group_by(year) %>%
  slice(which.max(rev_per_person))
  

## ---- message=FALSE, warning = FALSE, fig.height=5, fig.width=8---------------
liquor %>%
  filter(!type %in% c('beer','other','unknown')) %>%
  mutate(
    profit = state_revenue - state_cost
  ) %>%
  group_by(year, type) %>%
  summarize(
    total_profit = sum(profit) / 1000000,
    .groups = 'drop'
  ) %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = total_profit, col = type)) +
  labs(
    x = "Year Month", 
    y = "Profit in Million US$ (not adjusted for Inflation)", 
    title = "Annual Profit from Class E Liquor Sales by Liquor Type",
    caption = 'Source: Iowa Data Portal',
    color = 'Liquor Type'
  )

## ---- fig.height=5, fig.width=8-----------------------------------------------
profit <- liquor %>%
  filter(!type %in% c('beer','other','unknown')) %>%
  mutate(
    profit = state_revenue - state_cost
  ) %>%
  filter(year == 2019) %>%
  group_by(type) %>%
  summarize(
    total_profit = round(sum(profit) / 1000000 , 0),
    .groups = 'drop'
  ) %>%
  ungroup() %>%
  mutate(
    pct = paste0((round(total_profit / sum(total_profit) , 2)) * 100,'%')
  ) %>%
  arrange(desc(type)) %>%
  mutate(lab.ypos = cumsum(total_profit) - 0.5*total_profit)

profit %>%
  ggplot(aes(x = "", y = total_profit, fill = type)) +
  geom_bar(width = 1, stat = 'identity', color = 'white') +
  coord_polar("y", start = 0) +
  geom_text(aes(y = lab.ypos, label = pct), color = 'white') +
  theme_void() +
  labs(
    fill = 'Liquor Type',
    title = '2019 Profit Percentage by Liquor Type',
    caption = 'Source: Iowa Data Portal'
  )


## ---- fig.height=5, fig.width=8-----------------------------------------------
liquor_sales %>%
  filter(!type %in% c('beer','other','unknown')) %>%
  group_by(year_month, type) %>%
  summarize(total_cost = sum(state_cost), .groups = 'drop') %>%
  ungroup() %>%
  group_by(year_month) %>%
  mutate(
    cost_pct = round(total_cost / sum(total_cost) , 2)
  ) %>%
  ggplot(aes(x = year_month, y = cost_pct, fill = type)) +
  geom_area(alpha = 0.8, color = 'steelblue') +
  labs(y = 'Proportion of Cost (US$ - Not Adjusted for Inflation)',
       x = 'Year/Month',
       title = 'Proportion of State Costs of Purchasing Liquor From Vendors',
       caption = 'Source: Iowa Data Portal')

