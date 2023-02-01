## // TODO

# processing TODO
  # convert date.# to julian
  # new days to... (from hatching) columns for all instars

# stats
  # avg weight at instar
  # avg length to instar from hatching

# subsetting (in order of priority)
# (mostly bc weight at instar is affected by timing of the measurement)
  # 1. aggregate (no addtl considerations)
  # 2. hcs stage
    # 2a. keep p/h only
    # 2b. keep p/h/hm only
  # etc. initial temperature considerations
    ## (not too sure about this one bc N is pretty small?)
    # 267: 0, 1 (1 is pretty similar)
    # 330: 0, 1 (ptnlly worth filtering)
    # 337: 1, 2 (ptnlly worth filtering)


# setup -------------------------------------------------------------------

library(dplyr)
library(ggplot2)

#data <- 