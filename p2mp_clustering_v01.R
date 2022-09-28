# napoli

library("gplots")
library("factoextra")


set.seed(1234)


dev.off()

v1 = c(0.179, 0.141, 0.09, 0.051, 0.038, 0.013, 0.000, 0.002, 0.256, 0.641, 
       0.846, 0.949, 1.000, 0.923, 0.692,	0.667, 0.769, 0.667, 0.564, 0.436, 
       0.282, 0.180, 0.154, 0.154); v1 = v1/max(v1) # business
v2 = c(0.739, 0.251, 0.063, 0.024, 0.027, 0.026, 0.066, 0.193, 0.322, 0.383, 
       0.330, 0.222, 0.224, 0.356, 0.480, 0.480, 0.414, 0.385, 0.420, 0.546,
       0.694, 0.844, 1.000, 0.960); v2 = v2/max(v2) # residential





profiles = rbind(v1,v2)


plot(c(0:23), v1, type = "b", frame = FALSE, pch = 19,
     col = "red", xlab = "Hour", ylab = "Norm. Traffic", 
     lty = 1, lwd = 1)

# 3. Add a second line
lines(c(0:23), v2, , pch = 18, col = "blue", type = "b", 
      lty = 2, lwd = 1)

# 4. Add a legend to the plot and set legend lty
legend("topleft", legend = c("Business", "Residential"),
       col = c("red", "blue"), lty = 1:2, cex = 0.8)



nnodes = 100


mm = c()
for (ii in c(1:nnodes)) {
  peak = runif(1,min=10,max=100)
  aux = abs(peak*profiles[sample(c(1:dim(profiles)[1]),size=1),] + 
    0.8*peak*rnorm(dim(profiles)[2]))
  mm = rbind(mm,aux)
}

rownames(mm) = paste0("hl4_",c(1:dim(mm)[1]))


heatmap.2(mm, scale = "row", col = bluered(100), 
          trace = "none", density.info = "none",
          #Colv = as.dendrogram(hclust(as.dist(1-cols.cor))),
          Rowv = as.dendrogram(hclust(as.dist(rows.cor))))


# con un bucle for a fuerza bruta


mm_bloques = ceiling(mm/25)
mm2 = mm_bloques

rows.cor2 <- 0.5+0.5*cor(t(mm), use = "pairwise.complete.obs", method = "pearson")

#dist_mat <- dist(rows.cor2, method = 'pearson')
hclust.row <- hclust(as.dist(rows.cor2))




# Con mi algoritmo
traff_hl4_aux = rowSums(mm_bloques)
oorder = hclust.row$order 
# oorder = sample(c(1:nrow(mm_bloques)),size=100,replace=F) #random asignment
clusters_final = list()
kk=1; winner = oorder[kk]; cc_aux = mm2[winner,]
while (kk<=nrow(mm)) {
  cc_aux =c()
  for (ii in c(kk:nrow(mm))) {
    winner = oorder[ii]; 
    cc_aux = rbind(cc_aux,mm2[winner,])
    #print(cc_aux)
    if (max(colSums(cc_aux))>16) {
      cc_aux = cc_aux[1:(dim(cc_aux)[1]-1),]
      kk =ii
      clusters_final = append(clusters_final,list(cc_aux))
      break
    }
    if (ii==nrow(mm)) {
      kk=nrow(mm)+1
      break
    }
  }
}
cat("asignacion por orden")
print(length(clusters_final))


nclustershow = 4
plot(c(0:23),rep(16,1,24),type = "b", frame = FALSE, pch = 19,
     col = "green", xlab = "Hour", ylab = "Norm. Traffic", 
     lty = 1, lwd = 1, xlim = c(0,23), ylim = c(0,20))
lines(c(0:23), colSums(clusters_final[[nclustershow]]),
      pch = 18, col = "red", type = "b", lty = 2, lwd = 1)
for (cc in 1:nrow(clusters_final[[nclustershow]])) {
  lines(c(0:23), clusters_final[[nclustershow]][cc,],
        pch = 18, col = "blue", type = "b", lty = 2, lwd = 1)
  print(which(rowSums(matrix(rep(clusters_final[[nclustershow]][cc,],100),nrow=100,byrow=T)-mm_bloques)==0))
}
legend("topleft", legend = c("Total aggregated", "Individual"),
       col = c("red", "blue"), lty = 1:2, cex = 0.8)



print(which(rowSums(matrix(rep(clusters_final[[nclustershow]][cc,],100),nrow=100,byrow=T)-mm_bloques)==0))




