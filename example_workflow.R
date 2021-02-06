### ideal workflow
source('launch_sequenchr.R')
library(TraMineR)
data(mvad)
seqstatl(mvad[, 17:86])
mvad.alphabet <- c("employment", "FE", "HE", "joblessness", "school",
                   "training")
mvad.labels <- c("employment", "further education", "higher education",
                 "joblessness", "school", "training")
mvad.scodes <- c("EM", "FE", "HE", "JL", "SC", "TR")
mvad.seq <- seqdef(mvad, 17:86, alphabet = mvad.alphabet, states = mvad.scodes,
                   labels = mvad.labels, xtstep = 6)


launch_sequenchr(mvad.seq)


# library(tidyverse)
# library(TraMineR)
# library(fastcluster)
# library(NbClust)
# library(dendextend)
# source("Plots/ggplot_settings.R")
set.seed(44)

# read in ATUS data
atus_raw <- read_tsv("example_data/atus.tsv")

# read in the demographics data
demographics <- read_delim(file = "example_data/demographic.tsv",
                           delim = "\t",
                           escape_double = FALSE,
                           trim_ws = TRUE)


# filter to only include respondents who logged on weekdays and non holidays
IDs <- demographics %>% 
  filter(day_of_week %in% 2:6,
         holiday == 0) %>% 
  dplyr::select(ID, survey_weight)

# filter to just include weekends, pivot wider, and sample with weights
n_sample <- 1000
atus_sampled <- atus_raw %>% 
  pivot_wider(values_from = description, names_from = period, names_prefix = "p_") %>% 
  right_join(IDs, by = "ID") %>% 
  slice_sample(n = n_sample, weight_by = survey_weight) %>% 
  dplyr::select(-survey_weight)

# define alphabet as all unique states
alphabet <- atus_sampled[,-1] %>% unlist() %>% unique() %>% sort()
# states <- c("CHH")
labels <- c("Care_HH", "Care_NHH", "Cons_Pur", "Eat_drink", "Edu", 
            "HH_activ", "Other", "Prsl_care", "Care_svcs", "Rel_spirit", 
            "Sleep", "Leisure", "Recreation", "Volunteer", "Work")

# create state sequence object
atus_seq <- seqdef(data = atus_sampled[, -1], 
                   alphabet = alphabet, 
                   # states = mvad.scodes,
                   id = atus_sampled$ID,
                   labels = labels,
                   xtstep = 1)

# launch shiny app
launch_sequenchr(atus_seq)


atus_seq %>%
  as_tibble() %>% 
  setNames(1:ncol(atus_seq)) %>% 
  mutate(sequenchr_seq_id = row_number()) %>%
  pivot_longer(cols = setdiff(colnames(.), "sequenchr_seq_id")) %>%
  mutate(period = as.numeric(name)) %>% 
  ggplot(aes(x=period, y=sequenchr_seq_id, fill=value)) +
  geom_tile()


# blacklist
# sequenchr_period