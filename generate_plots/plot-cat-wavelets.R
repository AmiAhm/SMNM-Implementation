setwd("~/GitHub/SMNM-Implementation")
source("R-utility/extra-utility.R")
source("R-utility/itses-result-plotter.R")
library(latex2exp)

sd <- 0.05
main.path <- "output/image-de-noising/wavelets-st/"
files <- list.files(main.path)
thetas <- read.csv("output/image-de-noising/original_wavelets.csv", header = F)


v <- unlist(thetas)/sd
mean.length <- sum(abs(thetas/sd))/length(unlist(thetas))
print(mean.length)

files <- files[grepl(nrow(thetas), files, fixed = TRUE)]

cha <- "a"

pdf("output/figures/cat_plots_st.pdf",width = 9, height = 3)

layout.matrix <- matrix(1:4, nrow = 1, ncol = 4)

layout(mat = layout.matrix,
       heights = 4, # Heights of the two rows
       widths =c(2,2,2,1)) # Widths of the two columns

par(mar=c(5, 3.4, 4, 0),
    mgp = c(2.3,1,0.5)
)

band_names <- list(
  "aa" = "LL",
  "ad" ="LH",
  "da" = "HL",
  "dd" = "HH"
)

for(j in 1:length(files)) {
  print(j)
  file <- files[j]
  level <- sub('\\.RData$', '', file) 
  level <- gsub(".*_","",level)
  load(paste0(main.path, file))
  level <- gsub('[0-9]+', '', level)
  level <- band_names[[level]]
  cha <- plot.fun(iterative.res = result, theta =  thetas[,j],
                  sd.true = sd, show.n.its = F, show.method = F,show.loss.fun = F,
                  universal = F, y.max = 0.6, theta2 = T,
                  name = paste0("Subband: ",level ),cha = cha)


}

par(mar=c(0, 0, 0, 0))
  plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
  get.legend1("ST", loss.threshold = F, show.loss.fun = F, universal = F, pos = "center")

dev.off()