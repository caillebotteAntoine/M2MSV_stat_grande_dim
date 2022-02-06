
rm(list = ls()) ; graphics.off()

pck <- c('tidyverse', 'tictoc',
         'ggplot2', 'reshape2',
         'MultiVarSel', 'doMC')

install.packages(setdiff(pck, rownames(installed.packages())))

require(tidyverse)

require(ggplot2)
require(reshape2)

require(MultiVarSel)
require(doMC)

require(tictoc)


data <- read.csv2('table_proteome_FH_all.csv') %>% 
  select(-rep) %>% #rep représente le numero de réparition de l'exp ... pas utile
  mutate_if(is.character, as.factor)

str(data)
#View(data)
dim(data)

not_var <- c('sample', 'group', 'temperature', 'imbibition')

#juste pour le fun
data[,1:20] %>% melt(id = not_var) %>% ggplot(aes(variable, value, fill = group)) +
  geom_bar(position="dodge", stat="identity") +
  theme(legend.position = "none")

# DS (Dry seed),
# EI (Early imbibition) 
#     après 6h correspondant à la fin de la prise d’eau, 
# LI (Late imbibition)
#     après 20h

#================================#
# --- Construction de Y et X --- #
#================================#
#data %>% select(c('sample', 'group', 'temperature', 'imbibition'))

Y <- data %>% select(-c('sample', 'group', 'temperature', 'imbibition')) %>%
  as.matrix

Y %>% dim #27x494

#Matrice des prédicteurs
X <- model.matrix(lm(Y ~ group + 0, data = data) )
colnames(X) <- colnames(X) %>% str_replace('group','')

X %>% dim #27x9

# p <- ncol(X) # 9 var
# n <- nrow(X) # 27 indv
# print(paste0('n = ',n, ', p = ',p))


which(colMeans(Y) == 0)
which(Y %>% apply(2,sd) == 0)

Y <- scale(Y)

lm.res <- lm(Y ~ X + 0 )
#terms(lm.res)

residus <- lm.res$residual


#Teste du bruit blanc sans blanchissement
#Teste du porte manteau
white.pvalue <- whitening_test(residus)
white.pvalue #0.833


whitening_choice(residus, c("AR1", "nonparam", "ARMA"), 
                 pAR = 2, qMA = 0)


## => We will use the nonparametric modelling to see.
covar.sqare.hat <- whitening(residus,"nonparam")



# require(BlockCov)
# res_block <- Sigma_estimation(residus,
#                            reorder = TRUE, inv_12 = TRUE)
# 
# ########################################
# ##### Choix du blanchiment #############
# ########################################
# cor_residus <- cor(residus)
# mat_residus <- cor_residus[upper.tri(cor_residus)]
# 
# cor_res_blanch1 <- cor(residus %*% as.matrix(covar.sqare.hat))
# mat_res_blanch1 <- cor_res_blanch1[upper.tri(cor_res_blanch1)]
# 
# cor_res_blanch2 <- cor(residus %*% as.matrix(res_block$S_inv_12))
# mat_res_blanch2 <- cor_res_blanch2[upper.tri(cor_res_blanch2)]
# 
# res_blanch_data <- data.frame(sans_blanch = cor_residus %>% as.numeric,
#                               blanch_toeplitz = cor_res_blanch1 %>% as.numeric,
#                               banch_bloc = cor_res_blanch2 %>% as.numeric)
# boxplot(res_blanch_data)
# abline(h=0,col='red')












tic()

if(Sys.info()["sysname"] != "Windows")
{
  cores <- parallel::detectCores() -2 
}else{
  cores <- 1
}

registerDoMC(cores=cores)

freqs <- variable_selection(Y,X,covar.sqare.hat, nb_repli = 5000,
                            parallel=TRUE, nb.cores = cores)

stopCluster(cl)

saveRDS(freqs, 'freqs.rds')
freqs <- readRDS('freqs.rds')

toc()






