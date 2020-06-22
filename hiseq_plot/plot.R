
# create plots




##--------------##
## Venn-diagram ##
## ggvenn
## ggupset
## 
# 
# x = list(
#   A = 1:20,
#   B = 10:50,
#   C = 16:80
# )
# 
# vennplot(x)

library(ggvenn)

vennplot <- function(x, labels = NULL) {
  p <- ggvenn(x, show_percentage = FALSE, stroke_size = 0.7, text_size = 5)
  return(p)
}
