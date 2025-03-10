# Setup
library(pacman)
p_load(tidyverse, fastverse, here, fixest)

# Load the individual datasets
gdp = here('problem-sets', '002', 'data-raw', 'GDPA.csv') |> fread()
cpi = here('problem-sets', '002', 'data-raw', 'CPIAUCSL.csv') |> fread()
bir = here('problem-sets', '002', 'data-raw', 'USAbirthbymonth.txt') |> fread()
pop = here('problem-sets', '002', 'data-raw', 'POP.csv') |> fread()

# Collapse birth data to annual
bir = bir[, .(births = fsum(Births)), by = .(year = Year)]

# Clean other datasets
gdp =
  gdp[, .(
    year = year(observation_date),
    gdp = GDPA
  )]
cpi =
  cpi[,
    .(cpi = fmean(CPIAUCSL)),
    by = .(year = year(observation_date))
  ]
pop =
  pop[,
    .(pop = fmean(POP * 1000) |> round(0)),
    by = .(year = year(observation_date))
  ]

# Calculate the inflation rate
cpi = cpi |> mutate(inf = (cpi - lag(cpi)) / lag(cpi) * 100)

# Join the datasets
ps =
  bir |>
  inner_join(pop, by = 'year') |>
  inner_join(gdp, by = 'year') |>
  inner_join(cpi, by = 'year')

# Save the dataset
fwrite(
  x = ps,
  file = here('problem-sets', '002', 'data-births.csv')
)

