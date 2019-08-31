## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 8,
  fig.align = 'center',
  out.width = '100%'
)
library(JointAI)
options(width = 80)

## ---- fig.height = 8----------------------------------------------------------
op <- par(mar = c(2.5, 3, 2.5, 1), mgp = c(2, 0.8, 0))
plot_all(NHANES)
par(op)

## ---- fig.width = 8, fig.height = 10------------------------------------------
par(mar = c(2.5, 3, 2.5, 1), mgp = c(2, 0.8, 0))
plot_all(NHANES, allNA = TRUE, fill = '#e30f41', border = '#34111b', ncol = 3, nclass = 30)

## ---- fig.width = 8, fig.height = 10------------------------------------------
par(mar = c(2.5, 3, 2.5, 1), mgp = c(2, 0.8, 0))
plot_all(simLong, allNA = TRUE, fill = '#e30f41', border = '#34111b',
         ncol = 3, nclass = 30, use_level = TRUE, idvar = "ID")

## ---- fig.height = 6----------------------------------------------------------
md_pattern(NHANES)

## -----------------------------------------------------------------------------
md_pattern(NHANES, pattern = T, plot = F)

## ---- fig.height = 5----------------------------------------------------------
md_pattern(NHANES, color = c('#34111b', '#e30f41'), border = 'white',
           legend.position = 'none', print_xaxis = FALSE, 
           ylab = 'Alternative axis title')

## ---- fig.height = 9----------------------------------------------------------
md_pattern(simLong, color = c('#34111b', '#e30f41'))

