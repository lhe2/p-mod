# wrangle mod dev data
# 2026-05-05

# summary fns and etc for mod data

## load math fns
source(here("scripts/math_utils.R"))

# dev summaries -----------------------------------------------------------

CalcDevSS <- function(long_df){
  long_df %>%
    filter(trt.type == "dev") %>%
    group_by(across(c(starts_with("trt"), instar, is.sup))) %>%
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

CalcSurvSS <- function(wide_df){
  wide_df %>%
    filter(trt.type == "dev") %>%
    group_by(across(starts_with("trt"))) %>%
    summarise(n = n(),
              prop.pup = sum(is.pup > 0)/n,
              se.pup = seprop(prop.pup, n)) %>%
    ungroup()
}

CalcOutcomesSS <- function(wide_df){
  wide_df %>%
    filter(trt.type == "dev",
           !is.na(is.pup) # omit in-progress bugs
           ) %>%
    group_by(across(starts_with(c("trt")))) %>%
    summarise(tot_N = n(),
              N_pmd = sum(is.pup == 0 #& is.sup == 0
                          , na.rm = TRUE),
              # TODO fix wonky counting for the N_pup/sup..
              N_pup = sum(is.pup > 0 & is.sup == 0, na.rm = TRUE),
              N_sup = sum(is.pup > 0 & is.sup == 1, na.rm = TRUE),
              tot_sup = sum(is.sup == 1, na.rm = TRUE),
              sup_pmd = sum(is.sup == 1 & is.pup == 0, na.rm = TRUE),
              sup_pup = sum(is.sup == 1 & is.pup == 1, na.rm = TRUE),
              sup_LPI = sum(is.sup == 1 & is.pup == 2, na.rm = TRUE),
              ) %>%
    pivot_longer(cols = starts_with(c("N_", "sup_")),
                 names_to = c("type", "outcome"),
                 values_to = "n",
                 names_sep = "_") %>%
    mutate(prop = case_when(type == "N" ~ n/tot_N,
                            type == "sup" ~ n/tot_sup),
           type = factor(type, levels = c("N", "sup")),
           outcome = factor(outcome, levels = c("sup", "pup", "LPI", "pmd")))
    
    # ## this is what i envision, count-wise
    # group_by(across(starts_with(c("trt", "is.")))) %>%
    # summarise(N = n()) %>%
    # drop_na(is.pup) 
    
    # ## old...
    # group_by(across(starts_with(c("trt", "is.")))) %>%
    # summarise(N_all = n(),
    #           N_pmd = sum(is.pup == 0, na.rm = TRUE),
    #           not_sup = sum(is.sup == 0, na.rm = TRUE),
    #           is_sup = sum(is.sup == 1, na.rm = TRUE),
    #           is_pup = sum(is.pup == 1, na.rm = TRUE),
    #           is_LPI = sum(is.LPI == 1, na.rm = TRUE),
    #           ) %>%
    # ungroup() %>%
    # drop_na(is.pup) #%>%
    # # pivot_longer(cols = starts_with(c("not_", "is_")),
    # #              names_to = c("status", "outcome"),
    # #              values_to = "n",
    # #              names_sep = "_")
}


# tdt summaries ---------------------------------------------------------------

CalcTDTSS <- function(wide_df){
  wide_df %>%
    # TODO filter trt.type here or elsewhere?
    filter(trt.type == "tdt") %>% 
    select(c("cohort", starts_with("trt"), contains(c("pmd", "0h", "24h", "48h")))) %>%
    pivot_longer(cols = ends_with(c("0h", "24h", "48h")),
                 names_to = c(".value", "instar"),
                 names_sep = "\\.") %>%
    rename(exit = tt.pmd) %>%
    # TODO group by cohort?
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