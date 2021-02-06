# sequenchr

Sequence analysis tool for applied researchers. Designed for faster analysis iterations or for whom just prefer a point-and-click interface.

```
source('launch_sequenchr.R')
library(TraMineR)

# load data and
data(mvad)
seqstatl(mvad[, 17:86])
mvad.alphabet <- c("employment", "FE", "HE", "joblessness", "school",
                   "training")
mvad.labels <- c("employment", "further education", "higher education",
                 "joblessness", "school", "training")
mvad.scodes <- c("EM", "FE", "HE", "JL", "SC", "TR")
mvad.seq <- seqdef(mvad, 17:86, alphabet = mvad.alphabet, states = mvad.scodes,
                   labels = mvad.labels, xtstep = 6)

# launch the shiny app
launch_sequenchr(mvad.seq)
```
