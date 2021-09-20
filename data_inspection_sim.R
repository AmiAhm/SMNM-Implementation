setwd("~/GitHub/SMNM-Implementation/")
source("R-utility/extra-utility.R")


load("output/main-simulation-data/simulation-known-10k.RData")
df <- as.data.frame(result)
df$risk_ratio_visu_st <- df$risk_visu_st / df$risk_oracle_st
df$risk_ratio_ss <- df$risk_ss / df$risk_oracle_st
df$risk_ratio_itses_st <- df$risk_iterative_st / df$risk_oracle_st

df$risk_ratio_visu_ht <- df$risk_visu_ht / df$risk_oracle_ht
df$risk_ratio_itses_ht <- df$risk_iterative_ht / df$risk_oracle_ht

plot(df$mean.length, df$risk_ratio_itses_st)
plot(df$sparsity, df$risk_ratio_itses_st)

plot(df$sparsity, df$risk_ratio_itses_st - df$risk_ratio_ss, ylim = c(-30, 30))

plot(df$sd2, df$risk_ratio_itses_st - df$risk_ratio_ss, ylim = c(-30, 30))

boxplot(df[df$risk_ratio_itses_st>2,]$sparsity, df[df$risk_ratio_itses_st<2,]$sparsity)

boxplot(df[df$risk_ratio_itses_st>2,]$sd2, df[df$risk_ratio_itses_st<2,]$sd2)