
rm(list = ls()) ; graphics.off()

pck <- c('tidyverse', 'tictoc',
         'ggplot2', 'reshape2',
         'MultiVarSel', 'doParallel')

install.packages(setdiff(pck, rownames(installed.packages())))

require(tidyverse)

require(ggplot2)
require(reshape2)

require(MultiVarSel)
require(doParallel)

require(tictoc)


