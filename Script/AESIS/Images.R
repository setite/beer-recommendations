png(filename="images/finalmodel.png")


png(filename="images/uglycolors.png")
semPaths( aesismodel_sensitive2.sem, data = aesis_data_core,
          what = "model",
          rotation = 2, residuals=F,
          whatLabels = "std", ask = FALSE,
          groups = "latents", curveAdjacent = FALSE, curvePivot = TRUE,
          nCharNodes = 10, nCharEdges = 6, sizeMan = 8, sizeLat = 12,
          edge.label.cex=0.8,
          #style = "lisrel",
          mar = c(10, 5, 10, 5))
dev.off()

png(filename="images/finalmodel.png", width = 600, height = 600, res = 1200)
semPaths( aesismodel_sensitive2.sem, data = aesis_data_core, layout = "tree2",
          rotation = 1, residuals=F,
          what = "col",
          whatLabels = "std", ask = FALSE, curve = 1.0,
          groups = "latents", curveAdjacent = TRUE, curvePivot = FALSE,
          nCharNodes = 0, nCharEdges = 5,
          sizeMan  = 10, sizeLat  = 10, sizeInt = 10,
          sizeMan2 = 4, sizeLat2 = 5, sizeInt2 = 10,
          thresholds = TRUE,
          edge.label.cex=0.8,
          style = "lisrel", shapeLat = "circle",
          #mar = c(bottom, left, top, right)
          mar = c(3, 3, 3, 5),
          title = TRUE
)
dev.off()
semPaths(aesismodel_sensitive2, "std", title = FALSE)
title("Lavaan model 2", line = 3)

# First Correlogram Example
library(corrgram)
png(filename="images/corrgram.png")
corrgram(aesis_data_core[c(6:10,14:15)], order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Corrgram Test")
dev.off()
