source('launch_sequenchr.R')
library(TraMineR)
data(mvad)
seqstatl(mvad[, 17:86])
mvad.alphabet <- c("employment", "FE", "HE", "joblessness", "school",
                   "training")
mvad.labels <- c("employment", "further education", "higher education",
                 "joblessness", "school", "training")
# mvad.scodes <- c("EM", "FE", "HE", "JL", "SC", "TR")
mvad.seq <- seqdef(mvad, 17:86, alphabet = mvad.alphabet, # states = mvad.scodes,
                   labels = mvad.labels, xtstep = 6)

launch_sequenchr(mvad.seq)


# atus --------------------------------------------------------------------

atus <- readr::read_csv('example_data/atus.csv')

# define alphabet as all unique states
alphabet <- sort(unique(unlist(atus[,-1])))

# create state sequence object
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

# create random covariate data
covars_df <- tibble(cov1 = rnorm(1000), cov2 = runif(1000)) #, cov3 = sample(c("yes", "no", 'maybe'), size = 1000, replace = TRUE))

# launch shiny app
launch_sequenchr(atus_seq, covars_df)


# scratch work ------------------------------------------------------------

# atus_seq <- mvad.seq
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
  scale_fill_manual(values = color_mapping) +
  scale_y_discrete(labels = NULL, breaks = NULL)

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
  index =  'silhouette' #'mcclain'
)

widths <- as.data.frame(s_width$All.index)
widths$name <- rownames(widths)
  

# plot the silhouette width
s_width$All.index %>% 
  tibble::enframe() %>% 
  mutate(name = as.numeric(name)) %>% 
  ggplot(aes(x = name, y = value)) +
  geom_line(color = 'grey30') +
  geom_area(alpha = 0.4) +
  geom_point(color = 'grey30') +
  scale_x_continuous(breaks = 6:12) +
  labs(title = "Silhouette width",
       subtitle = 'Greater width is better',
       x = 'n clusters',
       y = 'Silhouette width',)
# ggsave("Plots/silhouette_width.svg", width = 7, height = 4)


cluster_stats <- fpc::cluster.stats(d = as.dist(dist_om_TRATE), 
                   clustering = cutree(clusters, k = 6),
                   silhouette = TRUE)
cluster_stats$ch
cluster_stats$avg.silwidth

cluster_stats <- function(diss_matrix, clusters, k_min, k_max){
  all_stats <- lapply(k_min:k_max, function(k){
    c_stats <- fpc::cluster.stats(
      d = diss_matrix,
      clustering = cutree(clusters, k = k),
      silhouette = TRUE
    )
    return(tibble(k = k, ch = c_stats$ch, silhouette = c_stats$avg.silwidth))
  })
  
  all_stats <- bind_rows(all_stats)
  scale_01 <- function(x) (x - min(x)) / diff(range(x))
  all_stats$ch_norm <- scale_01(all_stats$ch)
  all_stats$silhouette_norm <- scale_01(all_stats$silhouette)
  
  return(all_stats)
}

tmp <- cluster_stats(as.dist(dist_om_TRATE), clusters, 2, 10)

tmp %>% 
  pivot_longer(cols = ends_with('norm')) %>% 
  ggplot(aes(x = k, y = value, group = name, color = name)) +
  geom_line() +
  labs(title = "Silhouette width",
       subtitle = 'Greater width is better',
       x = 'n clusters',
       y = 'Normalized index',
       caption = "Optimal: minimum CH, maximum silhouette width") 

# s_widths <- cluster::silhouette(dmatrix = dist_om_TRATE,
#                     x = cutree(clusters, k = 6))
# mean(s_widths[,3])



# dendrograms -------------------------------------------------------------

hcl_ward <- clusters
hcl_k <- 5 #s_width$Best.nc[['Number_clusters']] # need a balance of optimal width and enough clusters to be interesting to the user
dend <- as.dendrogram(hcl_ward) %>% 
  dendextend::set("branches_k_color", k = hcl_k) %>% 
  dendextend::set("labels_colors")
dend_cut <- cut(dend, h = 50)$upper # cut off bottom of dendogram for computation performance
ggd1 <- dendextend::as.ggdend(dend_cut)
ggd1_full <- dendextend::as.ggdend(dend)

# set dashed line for non-cluster segments
ggd1$segments$linetype <- 'solid'
ggd1$segments$linetype[which(is.na(ggd1$segments$col))] <- 'dashed'

# set connecting lines to grey
ggd1$segments$col[is.na(ggd1$segments$col)] <- 'grey50'

# label positions
cluster_labels <- ggd1$segments %>% 
  filter(col != 'grey50') %>% 
  group_by(col) %>% 
  summarize(x = mean(x), #((max(x) - min(x)) / 2) + min(x),
            .groups = 'drop') %>% 
  arrange(x) %>% 
  mutate(label = paste0("Cluster ", 1:hcl_k))

# plot the dendrograms
ggd1$segments %>% 
  ggplot() + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend),
               color = ggd1$segments$col, linetype = ggd1$segments$linetype,
               lwd = 0.6, alpha = 0.7) +
  scale_x_continuous(labels = cluster_labels$label,
                     breaks = cluster_labels$x) +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(title = "Dendrogram of edit distance with Ward (D2) linkage",
       x = NULL,
       y = NULL) +
  theme(axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 45),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = 'none')
# ggsave("Plots/dendrogram.png", width = 7, height = 4)


# map the clusters to the order on the dendrogram -------------------------

cluster_assignments <- cutree(clusters, k = hcl_k)

# think the $order attribute determines the order of the points in the dendrogram
# if this can be mapped to the original datapoints then will have "clusters per dendrogram"
# and then have "clusters per regular process" then merge the two
# atus_seq %>% as_tibble() %>% .[clusters$order, ] %>% View('asd')

# cluster_assignments
# cluster_assignments[clusters$order]

cluster_to_dend_mapping <- dplyr::tibble(cluster = cluster_assignments[clusters$order]) %>% 
  nest(-cluster) %>% 
  mutate(cluster_dend = row_number()) %>% 
  unnest(data) %>% 
  distinct()

cluster_sorted <- dplyr::tibble(cluster = cluster_assignments) %>% 
  left_join(cluster_to_dend_mapping, by = 'cluster') %>% 
  pull(cluster_dend)


cluster_ns <- table(cluster_sorted)
cluster_assignments <- factor(cluster_sorted, 
                              labels = paste("Cluster", 1:hcl_k, " | n = ", cluster_ns))


# sequence plots ----------------------------------------------------------


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
  # filter(period > 40) %>%
  mutate(value = as.character(value)) %>% 
  group_by(sequenchr_seq_id) %>% 
  group_split() %>% 
  purrr::map_dfr(.f = function(df){
    df %>% add_row(sequenchr_seq_id = NA, value = NA, period = NA)
  })

# transition matrix
n <- nrow(freq_data)  
TRATE_mat <- table(tibble(previous = freq_data$value[1:(n-1)],
             current = freq_data$value[2:n]))
TRATE_mat <- TRATE_mat / sum(TRATE_mat)

unique_states <- unique(tidy_data$value) %>% as.vector()
TRATE_filled <- crossing(previous = unique_states, current = unique_states) %>% 
  left_join(as_tibble(TRATE_mat),
            by = c('previous', 'current')) %>% 
  replace_na(list(n = 0)) %>% 
  pivot_wider(names_from = previous, values_from = n)
TRATE_filled_mat <- as.matrix(TRATE_filled[, -1])
rownames(TRATE_filled_mat) <- TRATE_filled[[1]]
# TRATE_filled_mat <- as.table(TRATE_filled_mat, names = c('previous', 'current'))
# names(TRATE_filled_mat) <- c('previous', 'current')

# plot the chord diagram
chorddiag::chorddiag(TRATE_filled_mat, groupColors = as.vector(color_mapping), groupnamePadding = 20)

# corplot
dplyr::as_tibble(TRATE_mat) %>% 
  ggplot(aes(x = previous, y = current, fill = n)) +
  geom_tile() +
  labs(title = "Correlation matrix",
       subtitle = "A helpful subttile",
       x = "From state",
       y = 'To state',
       fill = 'Transition rate') +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

dplyr::as_tibble(TRATE_filled_mat, n = 'n', ) .name_repair = c('previous', 'current'))

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