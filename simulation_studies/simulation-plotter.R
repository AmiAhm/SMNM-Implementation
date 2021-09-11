
plot.simulation.result <- function(data.path, out.path,
                                    mean.lengths = c(0.5,1,2),
                                   ns = c(10, 50, 500, 5000),
                                   ratio.max = 5,
                                   full.a4 = T
) {
  load(data.path)
  df <- as.data.frame(result)

  if(full.a4){
    pdf(out.path, paper="a4", height = 13, width = 12.2)
  }else{
    pdf(out.path, height = 19*length(ns)/4, width = 12.2*length(mean.lengths)/3)
  }
  par(mfrow=c(length(ns),length(mean.lengths)))
  for(n in ns) {
    for(l in mean.lengths) {
      df.t <- df[df$n == n,]
      df.t <- df.t[df.t$mean.length == l,]

      df.t$risk_ratio_visu_st <- df.t$risk_visu_st / df.t$risk_oracle_st
      df.t$risk_ratio_ss <- df.t$risk_ss / df.t$risk_oracle_st
      df.t$risk_ratio_itses_st <- df.t$risk_iterative_st / df.t$risk_oracle_st

      df.t$risk_ratio_visu_ht <- df.t$risk_visu_ht / df.t$risk_oracle_ht
      df.t$risk_ratio_itses_ht <- df.t$risk_iterative_ht / df.t$risk_oracle_ht


      df.t <- df.t[,c("risk_ratio_visu_st",
                      "risk_ratio_ss",
                      "risk_ratio_itses_st",
                      "risk_ratio_visu_ht",
                      "risk_ratio_itses_ht")]

      names(df.t) <- c("Univ-ST", "SS-ST", "ITSES-ST", "Univ-HT", "ITSES-HT")

      boxplot(df.t,
              main = paste0("n=",n,", l=", l, ",\n ST-SS: ",
                           round(median(df.t[,2], na.rm = T),digits = 4),
                           ", ITSES-ST: ",round(median(df.t[,3], na.rm = T),digits = 4),
                           "\n Univ-HT: ", round(median(df.t[,4], na.rm = T),digits = 4),
                           ", ITSES-HT: ",round(median(df.t[,5], na.rm = T),digits = 4)),
              las = 2,
              ylim = c(1,ratio.max),
              xlab = "",
              ylab = "Risk-ratio",
              col = c("indianred1", "indianred1", "skyblue","indianred1", "skyblue")
              )
      }}

dev.off()
}
