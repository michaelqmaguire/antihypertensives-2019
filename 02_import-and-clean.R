#-----------------------------------------------------------------------------------------------------------------#
#                                                                                                                 #
# PROJECT: ANTIHYPERTENSIVES 2019                                                                                 #
# AUTHOR: MICHAEL MAGUIRE, MS, DMA II                                                                             #
# INSTITUTION: UNIVERSITY OF FLORIDA, COLLEGE OF PHARMACY                                                         #
# DEPARTMENT: PHARMACEUTICAL OUTCOMES AND POLICY                                                                  #
# SUPERVISORS: AMIE GOODIN, PHD, MPP | JOSHUA BROWN, PHARMD, PHD                                                  #
# SCRIPT: 02_import-and-clean.R                                                                			              #
#                                                                                                                 #
#-----------------------------------------------------------------------------------------------------------------#

library(tidyverse)
library(data.table)
library(ggplot2)
library(readxl)

# Read in antihypertensive NDC file.

ah <- readxl::read_xlsx(path = "./data/raw/ndc_antihtn.xlsx") |>
  janitor::clean_names() |>
  select(gennme, atc4, atc5, ndc) |>
  arrange(ndc) |>
  as.data.table()

# Create a file containing duplicates for transparency.

# First group by all variables and create a counter variable.

dups <- ah[
  ,
  j = id := 1:.N,
  by = c('gennme', 'atc4', 'atc5', 'ndc')
  ]

# Remove duplicates across every field.

ndNDC <- dups[
  i = (id == 1)
]

# Dump the duplicates into one file.

dupNDC <-
  dups[
    i = (id > 1)
  ]

# Store the NDC's in a vector

ndcVec <- ndNDC$ndc

# Read in SDUD file. `file_location` is hidden.

sdud <- fread(
  file_location,
  colClasses = c("proper_ndc" = "character")
  )

# Extract year of interest and drugs of interest.

yr <- sdud[
  i = year == 2019 & proper_ndc %in% ndcVec,
  j = .(year, proper_ndc, quarter, state, suppression, numberrx, prodnme, gennme)
]

yrSV <- ndNDC[yr, on = .(ndc = proper_ndc)]

# test <- ndNDC[yr, on = .(ndc = proper_ndc)]

# Just a check on the structure of the data set.

unique(
  yrSV[, c('quarter', 'year')],
  by = c('quarter', 'year')
  )

# --------------------
# General aggregations
# --------------------

# Not quite sure what they want, so I'll just sum everything for now.

drugOverall <- yrSV[
  i = ,
  j = .(totalRX = sum(numberrx)),
  by = c('year', 'quarter', 'suppression')
]

setorder(drugOverall, year, quarter, suppression)

# Break it down by drug

drugOverallGeneric <- yrSV[
  i = ,
  j = .(totalRX = sum(numberrx)),
  by = c('year', 'quarter', 'suppression', 'gennme', 'atc4', 'atc5')
]

setorder(drugOverallGeneric, year, quarter, gennme, suppression)

# Break it down by brand name drug

drugOverallGenericBrand <- yrSV[
  i = ,
  j = .(totalRX = sum(numberrx)),
  by = c('year', 'quarter', 'suppression', 'gennme', 'prodnme', 'atc4', 'atc5')
]

setorder(drugOverallGenericBrand, year, quarter, gennme, prodnme, suppression)

# ------------------
# State aggregations
# ------------------

drugAggState <- yr[
  i = ,
  j = .(totalRX = sum(numberrx)),
  by = c('year', 'state', 'quarter', 'suppression')
]

setorder(drugAggState, year, state, quarter, suppression)

# Break down drug counts and generic name by state.

drugAggStateGeneric <- yr[
  i = ,
  j = .(totalRX = sum(numberrx)),
  by = c('year', 'state', 'quarter', 'gennme', 'suppression')
]

setorder(drugAggStateGeneric, year, state, quarter, gennme, suppression)

# Break down drug counts and generic name by state.

drugAggStateGenericBrand <- yr[
  i = ,
  j = .(totalRX = sum(numberrx)),
  by = c('year', 'state', 'quarter', 'gennme', 'prodnme', 'suppression')
]

setorder(drugAggStateGenericBrand, year, state, quarter, gennme, prodnme, suppression)

files <- list(drugOverall, drugOverallGeneric, drugOverallGenericBrand, drugAggState, drugAggStateGeneric, drugAggStateGenericBrand)
fileNames <- c('drugOverall', 'drugOverallGeneric', 'drugOverallGenericBrand', 'drugAggState', 'drugAggStateGeneric', 'drugAggStateGenericBrand')

map2(
  .x = files,
  .y = fileNames,
  .f = ~fwrite(.x, file = paste0("./data/clean/", format(Sys.Date(), "%Y%m%d"), "_", .y, ".csv"))
)

