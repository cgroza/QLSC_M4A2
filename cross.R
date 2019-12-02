library(R.matlab)
library(tidyverse)
library(cowplot)
library(pheatmap)
library(viridis)
library(glmnet)

data <- readMat("RetinaData.mat")
responses <- data$responses
stimulus <- data$stimulus
N <- 2000
cells <- 8

## 100 per presentation x T presentations
Ts <- list(2, 2, 2, 4, 3, 3, 2, 1)



inputs <- lapply(1:cells, function(x) matrix(0, ncol = Ts[[x]] * 10 * 10, nrow = N - Ts[[x]]))
outputs <- lapply(1:cells, function(x) matrix(0, nrow = N - Ts[[x]], ncol = 1))
val.outputs <- lapply(1:cells, function(x) matrix(0, nrow = N - Ts[[x]], ncol = 1))

                                        # for each cell
for(i in 1:cells) {
  T <- Ts[[i]]
  print(T)
                                        # for each response
  for(j in 1:(N-T)) {
                                        # for each square in presentation
    for(t in 1:T)
    {
      for(k in 1:10)
      {
        for(l in 1:10)
        {
          inputs[[i]][j, (t-1)*100 + 10*(k-1) + l] <- stimulus[k, l, j + t - 1]/255
        }
      }
      outputs[[i]][j] <- responses[j + T, 1, i]
      val.outputs[[i]][j] <- responses[j + T, 2, i]
    }
  }
}

cell.models <- lapply(1:cells,
                      function(x)
                        glmnet(y = outputs[[x]], x = inputs[[x]], alpha = 0.5)
                      )

cv.cell.models <- lapply(1:cells,
                      function(x)
                        cv.glmnet(y = outputs[[x]], x = inputs[[x]], alpha = 0.5)
                      )
pdf("glmnet_crossval_cell1.pdf")
plot(cv.cell.models[[1]])
dev.off()

r.squares <- tibble(T = unlist(Ts),
                    Cell = 1:8,
                    Lambda = sapply(1:8, function(x) cv.cell.models[[x]]$lambda.min),
                    fit.R2 =  sapply(1:cells,
                                     function(x)
                                       paste("R2 =", round(summary(lm(outputs[[x]] ~ predict(cv.cell.models[[x]], inputs[[x]], cv.cell.models[[x]]$lambda.min)[, 1] ))$r.squared, 3))
                                     ),
                        test.R2 = sapply(1:cells,
                                         function(x)
                                           paste("R2 =",
                                                 round(summary(lm(val.outputs[[x]] ~ predict(cv.cell.models[[x]], inputs[[x]], cv.cell.models[[x]]$lambda.min)[, 1]))$r.squared, 3)))

                        )


bind_rows(lapply(1:cells,
                 function(x) tibble(Predicted = predict(cv.cell.models[[x]], inputs[[x]],
                                                        cv.cell.models[[x]]$lambda.min)[, 1],
                                    Observed = val.outputs[[x]],
                                    Cell = x))) %>% ggplot(aes(x=Predicted, y=Observed)) +
  geom_point(alpha=0.1) + geom_smooth(method = "lm") +
  geom_text(aes(x=0.3, y=-0.55, label = test.R2), data=r.squares) +
  facet_wrap(~Cell, nrow = 3, ncol = 3) + theme_bw() +
  labs(x="Predicted response", y = "Observed response", title = "Model validation per RGC")
ggsave("cross_test.pdf")

bind_rows(lapply(1:cells,
                 function(x) tibble(Predicted = predict(cv.cell.models[[x]], inputs[[x]],
                                                        cv.cell.models[[x]]$lambda.min)[, 1],
                                    Observed = outputs[[x]],
                                    Cell = x))) %>% ggplot(aes(x=Predicted, y=Observed)) +
  geom_point(alpha=0.1) + geom_smooth(method = "lm") +
  geom_text(aes(x=0.3, y=-0.55, label = fit.R2), data=r.squares) +
  facet_wrap(~Cell, nrow = 3, ncol = 3) + theme_bw() +
  labs(x="Predicted response", y = "Observed response", title = "Model fit per RGC")
ggsave("cross_fit.pdf")



for(model in c(1,4,5,6,7,8)) {
  rf.matrices <- lapply(1:Ts[[model]],
                        function(t){
                          weights <- coef(cv.cell.models[[model]], s = cv.cell.models[[model]]$lambda.min)[-1]
                          rf.m <- matrix(0, nrow=10, ncol=10)
                          for(k in 1:10){
                            for(l in 1:10){
                              rf.m[k, l] <- weights[(t-1)*100 + 10*(k-1) + l]
                            }
                          }
                          rf.m
                        })
  p <- plot_grid(plotlist=lapply(rf.matrices,
                                 function(m) pheatmap(m, cellwidth = 10, cellheight = 10, cluster_rows = F, cluster_cols = F, color=viridis(50),
                                                      silent = TRUE)[[4]]), labels = paste("t =", 1:Ts[[model]]),
                 ncol =  floor(Ts[[model]]/2) + (Ts[[model]] %% 2), nrow = 2, label_size = 8, align="vh", scale = 0.8)

  save_plot(paste0("cross_RF_RGC_", model, ".pdf"), p)
}
