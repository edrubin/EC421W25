# Notes ----------------------------------------------------------------------------------
#   Goal: Clean the data for the 000 problem set.
#   Time: Short

# Data notes -----------------------------------------------------------------------------
#   Source: IPMUS USA (2023 ACS)

# Setup ----------------------------------------------------------------------------------
  # Packages
  library(pacman)
  p_load(data.table, collapse, fixest, here)

# Load dataset ---------------------------------------------------------------------------
  # Load the raw data
  acs_dt = here('problem-sets/000/data-raw/usa_00006.csv') |> fread()

# Clean data -----------------------------------------------------------------------------
  # Names to lowercase
  setnames(acs_dt, tolower(names(acs_dt)))
  # Recode sex
  acs_dt[, `:=`(
    sex = fcase(
      sex == 1, 'Male',
      sex == 2, 'Female',
      default = NA
    )
  )]
  # Drop 'fertyr' (mostly missing)
  acs_dt[, fertyr := NULL]
  # Recode 'race'
  acs_dt[, `:=`(
    race = fcase(
      race == 1, 'White',
      race == 2, 'Black',
      race == 3, 'AIAN',
      race == 4, 'Chinese',
      race == 5, 'Japanese',
      default = 'Other'
    ),
    raced = NULL
  )]
  # Recode Hispanic origin
  acs_dt[, `:=`(
    hispan = fcase(
      hispan == 0, 'Non-Hispanic',
      hispan %in% 1:4, 'Hispanic',
      default = NA
    ),
    hispand = NULL
  )]
  setnames(acs_dt, 'hispan', 'hispanic')
  # Recode 'educ'
  acs_dt[, `:=`(
    educ = fcase(
      educ == 1, 4,
      educ == 2, 8,
      educ == 3, 9,
      educ == 4, 10,
      educ == 5, 11,
      educ == 6, 12,
      educ == 7, 13,
      educ == 8, 14,
      educ == 9, 15,
      educ == 10, 16,
      educ == 11, 17,
      default = NA
    )
  )]
  acs_dt[, deg_bachelors := 1 * (educd >= 101 & educd < 999)]
  acs_dt[, deg_masters := 1 * (educd == 114)]
  acs_dt[, deg_profession := 1 * (educd == 115)]
  acs_dt[, deg_phd := 1 * (educd == 116)]
  acs_dt[, educd := NULL]
  # Clean employment status
  acs_dt[, `:=`(
    empstat = fcase(
      empstat == 1, 'Employed',
      empstat == 2, 'Unemployed',
      empstat == 3, 'Not in labor force',
      default = NA
    ),
    empstatd = NULL
  )]
  # Rename hours worked per week
  setnames(acs_dt, 'uhrswork', 'hrs_wk')
  # Clean income
  acs_dt[inctot == 9999999, inctot := NA]
  setnames(acs_dt, 'inctot', 'income')

# Subset ---------------------------------------------------------------------------------
  # Take a 10,000-obseration sample of individuals older than 18
  set.seed(42)
  sub_dt = acs_dt[age >= 18][sample(.N, 1e4)]
  # Create desired indicator variables
  sub_dt[, `:=`(
    i_female = 1 * (sex == 'Female'),
    i_black = 1 * (race == 'Black'),
    i_white = 1 * (race == 'White'),
    i_hispanic = 1 * (hispanic == 'Hispanic'),
    i_workforce = 1 * (empstat %in% c('Employed', 'Unemployed')),
    i_employed = 1 * (empstat == 'Employed')
  )]

# Save data ------------------------------------------------------------------------------
  # Save the cleaned dataset
  fwrite(
    x = sub_dt,
    file = here('problem-sets', '000', 'data-acs.csv')
  )
