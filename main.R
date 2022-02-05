
pck <- c('tidyverse', 'ggplot2', 'reshape2')
install.packages(setdiff(pck, rownames(installed.packages())))

require(tidyverse)

require(ggplot2)
require(reshape2)


data <- read.csv2('table_proteome_FH_all.csv') %>% 
  select(-rep) %>% #rep représente le numero de réparition de l'exp ... pas utile
  mutate_if(is.character, as.factor)

str(data)
View(data)

not_var <- c('sample', 'group', 'temperature', 'imbibition')

#juste pour le fun
data[,1:20] %>% melt(id = not_var) %>% ggplot(aes(variable, value, fill = group)) +
  geom_bar(position="dodge", stat="identity") +
  theme(legend.position = "none")


