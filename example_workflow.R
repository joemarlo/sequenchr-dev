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


# atus --------------------------------------------------------------------

atus <- readr::read_csv('example_data/atus.csv')

# define alphabet as all unique states
alphabet <- sort(unique(unlist(atus[,-1])))
# states <- c("CHH")
labels <- c("Care_HH", "Care_NHH", "Cons_Pur", "Eat_drink", "Edu", 
            "HH_activ", "Other", "Prsl_care", "Care_svcs", "Rel_spirit", 
            "Sleep", "Leisure", "Recreation", "Volunteer", "Work")

# create state sequence object
atus_seq <- seqdef(data = atus[, -1], 
                   alphabet = alphabet, 
                   # states = mvad.scodes,
                   id = atus$ID,
                   labels = labels,
                   xtstep = 1)

# launch shiny app
launch_sequenchr(atus_seq)


# scratch work ------------------------------------------------------------

tidy_data <- atus_seq %>%
  as_tibble() %>% 
  setNames(1:ncol(atus_seq)) %>% 
  mutate(sequenchr_seq_id = row_number()) %>%
  pivot_longer(cols = setdiff(colnames(.), "sequenchr_seq_id")) %>% 
  mutate(period = as.numeric(name)) %>% 
  dplyr::select(-name)

color_mapping <- viridis::viridis_pal()(length(alphabet(atus_seq)))
names(color_mapping) <- alphabet(atus_seq)

tidy_data %>% 
  ggplot(aes(x = period, y = sequenchr_seq_id, fill = value)) +
  geom_tile() +
  scale_fill_manual(values = color_mapping)

tidy_data %>% 
  group_by(sequenchr_seq_id) %>% 
  summarize(seq_collapsed = paste0(value, collapse = 'SE3P'),
            .groups = 'drop') %>% 
  count(seq_collapsed) %>% 
  arrange(desc(n)) %>%
  slice_head(n = 10) %>% 
  separate(seq_collapsed, into = paste0('p', 1:48), sep = "SE3P") %>% 
  mutate(sequenchr_seq_id = row_number()) %>%
  pivot_longer(cols = setdiff(colnames(.), c('n', "sequenchr_seq_id"))) %>% 
  mutate(name = as.numeric(gsub('p', '', name))) %>% 
  rename(period = name) %>% 
  ggplot(aes(x = period, y = sequenchr_seq_id, fill = value)) +
  geom_tile() +
  scale_fill_manual(values = color_mapping)

tidy_data %>% 
  left_join(data.frame(cluster = sub("  \\|.*", "", cluster_assignments), 
                       sequenchr_seq_id = 1:length(cluster_assignments))) %>% 
  group_by(sequenchr_seq_id, cluster) %>% 
  summarize(seq_collapsed = paste0(value, collapse = 'SE3P'),
            .groups = 'drop') %>% 
  count(cluster, seq_collapsed) %>%
  group_by(cluster) %>% 
  arrange(desc(n)) %>%
  slice_head(n = 10) %>% 
  ungroup() %>% 
  separate(seq_collapsed, into = paste0('p', 1:48), sep = "SE3P") %>% 
  mutate(id = row_number()) %>%
  pivot_longer(cols = setdiff(colnames(.), c('n', "id", 'cluster'))) %>% 
  mutate(name = as.numeric(gsub('p', '', name))) %>% 
  rename(period = name) %>%
  ggplot(aes(x = period, y = id, fill = value)) +
  geom_tile() +
  scale_fill_manual(values = color_mapping) +
  facet_wrap(~cluster, scales = 'free_y')


tidy_data %>% 
  # count(value, period) %>% 
  ggplot(aes(x = period, fill = value)) +
  geom_bar(width = 1) +
  scale_fill_manual(values = color_mapping)

tidy_data %>% 
  count(value, period) %>% 
  group_by(period) %>% 
  filter(n == max(n)) %>% 
  ggplot(aes(x = period, y = n, fill = value)) +
  geom_col() +
  scale_fill_manual(values = color_mapping) +
  labs(title = "Modal activity per period",
       x = "Period",
       y = 'Frequency',
       fill = NULL)



# clustering --------------------------------------------------------------


# compute optimal matching distances
dist_om_TRATE <- seqdist(atus_seq, method = "OM", indel = 1, sm = "TRATE")
# dist_om_DHD <- seqdist(atus_seq, method = "DHD")

# cluster the data
clusters <- fastcluster::hclust(as.dist(dist_om_TRATE), method = "ward.D2")

# get optimal cluster sizes by calculating silhouette width
s_width <- NbClust::NbClust(
  data = NULL,
  diss = as.dist(dist_om_TRATE),
  distance = NULL,
  method = 'ward.D2',
  max.nc = 12,
  min.nc = 6,
  index = 'silhouette'
)

widths <- as.data.frame(s_width$All.index)
widths$name <- rownames(widths)
  

# plot the silhouette width
s_width$All.index %>% 
  tibble::enframe() %>% 
  widths %>% 
  rename(value = `s_width$All.index`) %>% 
  mutate(name = as.numeric(name)) %>% 
  ggplot(aes(x = name, y = value)) +
  geom_line(color = 'grey30') +
  geom_area(alpha = 0.4) +
  geom_point(color = 'grey30') +
  scale_x_continuous(breaks = 6:12) +
  labs(title = "Silhouette width",
       subtitle = 'Greater width is better',
       x = 'n clusters',
       y = 'Silhouette width')
# ggsave("Plots/silhouette_width.svg", width = 7, height = 4)


# dendrograms -------------------------------------------------------------

hcl_ward <- clusters
hcl_k <- 7 #s_width$Best.nc[['Number_clusters']] # need a balance of optimal width and enough clusters to be interesting to the user
dend <- as.dendrogram(hcl_ward) %>% set("branches_k_color", k = hcl_k) %>% set("labels_colors")
dend <- cut(dend, h = 50)$upper # cut off bottom of dendogram for computation performance
ggd1 <- as.ggdend(dend)

# set dashed line for non-cluster segments
ggd1$segments$linetype <- 'solid'
ggd1$segments$linetype[which(is.na(ggd1$segments$col))] <- 'dashed'

# set connecting lines to grey
ggd1$segments$col[is.na(ggd1$segments$col)] <- 'grey50'

# plot the dendrograms
ggd1$segments %>% 
  ggplot() + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend),
               color = ggd1$segments$col, linetype = ggd1$segments$linetype,
               lwd = 0.6, alpha = 0.7) +
  scale_x_continuous(labels = NULL) +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(title = "Dendrogram of edit distance with Ward (D2) linkage",
       subtitle = paste0("Weighted sample of ", 
                         scales::comma_format()(n_sample),
                         " respondents"),
       x = NULL,
       y = NULL) +
  theme(axis.ticks = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = 'none')
# ggsave("Plots/dendrogram.png", width = 7, height = 4)

# sequence plots ----------------------------------------------------------

cluster_assignments <- cutree(clusters, k = hcl_k)
cluster_ns <- table(cluster_assignments)
cluster_assignments <- factor(cluster_assignments, 
                              labels = paste("Cluster", 1:hcl_k, " | n = ", cluster_ns))

tibble(cluster = cluster_assignments,
       sequenchr_seq_id = 1:length(cluster_assignments)) %>% 
  right_join(tidy_data, by = 'sequenchr_seq_id') %>% 
  group_by(sequenchr_seq_id) %>% 
  mutate(entropy = DescTools::Entropy(table(value))) %>%
  ungroup() %>% 
  ggplot(aes(x = period, y = reorder(sequenchr_seq_id, entropy), fill = value)) +
  # ggplot(aes(x = period, y = sequenchr_seq_id, fill = value)) +
  geom_tile() +
  scale_fill_manual(values = color_mapping) +
  scale_y_discrete(labels = NULL) +
  facet_wrap(~cluster, scales = 'free_y')



# chord plot --------------------------------------------------------------

library(chorddiag)

# add NA filler rows after each group before calculating transition matrix
freq_data <- tidy_data %>% 
  mutate(value = as.character(value)) %>% 
  group_by(sequenchr_seq_id) %>% 
  group_split() %>% 
  map_dfr(.f = function(df){
    df %>% add_row(sequenchr_seq_id = NA, value = NA, period = NA)
  })

# transition matrix
n <- nrow(freq_data)  
TRATE_mat <- table(tibble(previous = freq_data$value[1:(n-1)],
             current = freq_data$value[2:n]))
TRATE_mat <- TRATE_mat / sum(TRATE_mat)

# plot the chord diagram
chorddiag(TRATE_mat, groupColors = as.vector(color_mapping), groupnamePadding = 20)



# other -------------------------------------------------------------------

tidy_data %>%  
  left_join(tibble(cluster = cluster_assignments,
                   sequenchr_seq_id = 1:length(cluster_assignments)),
            by = 'sequenchr_seq_id') %>%
  group_by(sequenchr_seq_id, cluster) %>% 
  summarize(seq_collapsed = paste0(value, collapse = 'SE3P'),
            .groups = 'drop') %>% 
  count(seq_collapsed, cluster) %>% 
  group_by(cluster) %>% 
  arrange(desc(n)) %>%
  slice_head(n = 10) %>% 
  separate(seq_collapsed, into = paste0('p', 1:48), sep = "SE3P") %>% 
  mutate(id = row_number()) %>%
  pivot_longer(cols = setdiff(colnames(.), c('n', "id"))) %>% 
  mutate(name = as.numeric(stringr::str_remove(name, 'p'))) %>% 
  rename(period = name) %>% 
  group_by(sequenchr_seq_id) %>%
  mutate(entropy = DescTools::Entropy(table(value))) %>%
  ungroup() %>%
  ggplot(aes(x = period, y = reorder(sequenchr_seq_id, entropy), fill = value)) +
  # ggplot(aes(x = period, y = sequenchr_seq_id, fill = value)) +
  geom_tile() +
  scale_fill_manual(values = color_mapping) +
  scale_y_discrete(breaks = 1:10) +
  facet_wrap(~cluster, scale = 'free_y') +
  labs(title = "Top 10 most common sequences",
       x = 'Period',
       y = 'Sequence (ranked)',
       fill = NULL)

# blacklist
# sequenchr_period