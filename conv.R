library(R.matlab)
library(tidyverse)
library(cowplot)
library(pheatmap)
library(viridis)

data <- readMat("RetinaData.mat")
responses <- data$responses
stimulus <- data$stimulus
N <- 2000
cells <- 8
pres <- 2

## 100 per presentation x T presentations
T <- 5



inputs <- lapply(1:cells, function(x) matrix(0, ncol = T * 10 * 10, nrow = (N - T) * pres))
outputs <- lapply(1:cells, function(x) matrix(0, nrow = (N - T) * pres, ncol = 1))


for(p in 1:pres){
  # for each cell
  for(i in 1:cells) {
    # for each response
    for(j in 1:(N-T)) {
      # for each square in presentation
      for(t in 1:T)
      {
        for(k in 1:10)
        {
          for(l in 1:10)
          {
            inputs[[i]][(p-1) * (N-T) + j, (t-1)*100 + 10*(k-1) + l] <- stimulus[k, l, j + t - 1]
          }
        }
        outputs[[i]][(p-1) * (N-T) + j] <- responses[j + T, p, i]
      }
    }
  }
}


cell.models <- lapply(1:cells,
                      function(x)
                        lm(outputs[[x]] ~ inputs[[x]])
                      )
r.squares <- tibble(Cell = 1:8, R2 =  sapply(1:cells, function(x)
  paste("R2 =", round(summary(cell.models[[x]])$r.squared, 3))))

bind_rows(lapply(1:cells,
                 function(x) tibble(Predicted = fitted(cell.models[[x]]),
                                    Observed = outputs[[x]],
                                    Cell = x))) %>% ggplot(aes(x=Predicted, y=Observed)) +
  geom_point(alpha=0.1) + geom_smooth(method = "lm") +
  geom_text(aes(x=0.4, y=-0.55, label = R2), data=r.squares) +
  facet_wrap(~Cell, nrow = 4, ncol = 2) + theme_bw() +
  labs(x="Predicted response", y = "Observed response", title = "Model fit per RGC")

ggsave(paste0("T_", T, ".pdf"))



for(model in 1:cells) {
  rf.matrices <- lapply(1:T,
                        function(t){
                          weights <- cell.models[[model]]$coefficients[-1]
                          rf.m <- matrix(0, nrow=10, ncol=10)
                          for(k in 1:10){
                            for(l in 1:10){
                              rf.m[k, l] <- weights[(t-1)*100 + 10*(k-1) + l]
                            }
                          }
                          rf.m
                        })
  p <- plot_grid(plotlist=lapply(rf.matrices,
                                 function(m) pheatmap(m, cluster_rows = F, cluster_cols = F, color = viridis(20),
                                                      silent = TRUE)[[4]]), labels = paste("t =", 1:T),
                 nrow = 3, ncol = 2, label_size = 8, align="vh", scale = 0.8)

  save_plot(paste0("RF_RGC_", model, ".pdf"), p)

}

ggplot() + geom_line(aes(x=1:1995, y=fitted(cell.models[[1]])[1:1995]), color="blue") + geom_line(aes(x=1:1995, y=outputs[[1]][1:1995]), color="red") + theme_bw()
