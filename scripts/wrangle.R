# wrangle mod dev data
# 2026-05-05

# summary fns and etc for mod data

## load math fns
source(here("scripts/math_utils.R"))


# tdt summaries ---------------------------------------------------------------

PrepTDTSS <- function(wide_df){
  wide_df %>%
    #filter(trt.type == "tdt") %>% 
    select(c("cohort", starts_with("trt"), contains(c("pmd", "0h", "24h", "48h")))) %>%
    pivot_longer(cols = ends_with(c("0h", "24h", "48h")),
                 names_to = c(".value", "instar"),
                 names_sep = "\\.") %>%
    rename(exit = tt.pmd) %>%
    group_by(across(c(starts_with("trt"), "instar", #"cohort" 
                      ))) %>% 
    summarise(n = n(),
              across(.cols = c(mass, dmass, rmass, exit),
                     .fns = list(avg = ~ mean(.x, na.rm = TRUE),
                                 se = se),
                     .names = "{.fn}.{.col}")
              # avg.delta = mean(mass.change, na.rm = TRUE),
              # se.delta = se(mass.change),
              # avg.exit = mean(tt.pmd, na.rm = TRUE),
              # se.exit = se(tt.pmd)
              ) %>%
    ungroup()
}

# dev summaries -----------------------------------------------------------

PrepDevSS <- function(long_df){
  long_df %>%
    group_by(across(c(starts_with("trt"), instar))) %>%
    mutate(logmass = log(mass),
           # mass = case_when(instar %in% c("pupa", "eclose") ~ mass/1000,
           #                  TRUE ~ mass),
           devrate = 1/tt
           ) %>%
    summarise(n = n(),
              across(.cols = c(mass, logmass, tt, devrate),
                     .fns = list(avg = ~ mean(.x, na.rm = TRUE),
                                 se = se),
                     .names = "{.fn}.{.col}")
              ) %>%
    ungroup()
}

PrepSurvSS <- function(wide_df){
  wide_df %>%
    group_by(across(starts_with("trt"))) %>%
    summarise(n = n(),
              prop.pup = sum(is.pup == 1)/n,
              se.pup = seprop(prop.pup, n)) %>%
    ungroup()
    
}
