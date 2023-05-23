# P2MP clustering algorithm

library("gplots")
library("factoextra")
library(proxy)

rm(list=ls())


set.seed(1234)


dev.off()




# TRAFFIC PROFILES

resident_fig_12 = c(0.548991935, 0.428015148, 0.280163077, 0.187894396, 
                    0.139545246, 0.129623817,0.189793138 ,0.338735226 ,
                    0.429277684 ,0.502130586 ,0.562305397 ,0.589313393 ,
                    0.592820885 ,0.592387172 ,0.611429119 ,0.62762653 ,
                    0.650912165 ,0.714977492 ,0.842662993 ,0.918683315 ,
                    0.963793572 ,0.992060767 ,0.981278074 ,0.860828409)
office_fig_12 = c(0.205724508 ,0.148966909 ,0.101625686 ,0.073814059 ,
                  0.054084557 ,0.054643942 ,0.085573929 ,0.194938462 ,
                  0.514704556 ,0.860634805 ,0.988734689 ,0.99222402 ,
                  0.980664941 ,0.983035822 ,0.982771482 ,0.958110238 ,
                  0.858561588 ,0.673190464 ,0.509924853 ,0.416267575 ,
                  0.391016002 ,0.374660534 ,0.345787174 ,0.296721115)
transport_fig_12 = c(0.010728113 ,0.006045965 ,0.01190015 ,0.011915025 ,
                     0.015400033 ,0.029975634 ,0.086658812 ,0.517964957 ,
                     0.965412651 ,0.728379646 ,0.435605404 ,0.352884198 ,
                     0.358444071 ,0.376623315 ,0.376484578 ,0.394920474 ,
                     0.520303876 ,0.708243986 ,0.862575765 ,0.665197845 ,
                     0.449659501 ,0.351744188 ,0.246387594 ,0.110125757)
entertainment_fig_12 = c(0.504347826 ,0.433485584 ,0.260185074 ,0.167829398 ,
                         0.13776988 ,0.133195019 ,0.234069382 ,0.366327726 ,
                         0.633154772 ,0.824226417 ,0.927273541 ,0.982282696 ,
                         0.972822501 ,0.963979893 ,0.977263795 ,0.991648198 ,
                         0.975470586 ,0.939116517 ,0.93070927 ,0.946142154 ,
                         0.973148633 ,0.991927826 ,0.957496444 ,0.84548412)
#all_fig_12 = [0.483478261 ,0.379098439 ,0.253187422 ,0.181990996 ,0.137334408 ,0.14945749 ,0.186133957 ,0.363498701 ,0.567904806 ,0.799219137 ,0.941691931 ,0.969132082 ,0.977054964 ,0.987005596 ,0.989352582 ,0.980225597 ,0.952913981 ,0.907681173 ,0.892724723 ,0.888548026 ,0.912223949 ,0.929476658 ,0.895311986 ,0.785927218 ,0.600556493]


# The two baseline profiles, normalised
v1 = c(0.179, 0.141, 0.09, 0.051, 0.038, 0.013, 0.000, 0.002, 0.256, 0.641, 
       0.846, 0.949, 1.000, 0.923, 0.692,	0.667, 0.769, 0.667, 0.564, 0.436, 
       0.282, 0.180, 0.154, 0.154); v1 = v1/max(v1) # business
v2 = c(0.739, 0.251, 0.063, 0.024, 0.027, 0.026, 0.066, 0.193, 0.322, 0.383, 
       0.330, 0.222, 0.224, 0.356, 0.480, 0.480, 0.414, 0.385, 0.420, 0.546,
       0.694, 0.844, 1.000, 0.960); v2 = v2/max(v2) # residential
profiles_basic = rbind(v1,v2)


profiles = rbind(resident_fig_12, office_fig_12, 
                 transport_fig_12, entertainment_fig_12)
profiles_weigths = c(0.2422, 0.3707, 0.4672, 0.3881); 
profiles_weigths = profiles_weigths/sum(profiles_weigths)

plot(c(0:23), profiles[1,], type = "b", frame = FALSE, pch = 19,
     col = "red", xlab = "Hour", ylab = "Norm. Traffic", 
     lty = 1, lwd = 1, xlim = c(0,25), ylim=c(0,1.1))
lines(c(0:23), profiles[2,], pch = 18, col = "blue", type = "b", 
      lty = 2, lwd = 1)
lines(c(0:23), profiles[3,], pch = 18, col = "green", type = "b", 
      lty = 2, lwd = 1)
lines(c(0:23), profiles[4,], pch = 18, col = "brown", type = "b", 
      lty = 2, lwd = 1)


legend("topleft", legend = c("Residential", "Office",
                             "Transport","Entertainment"),
       col = c("red", "blue","green","brown"), lty = 1:2, cex = 0.68)



# EXPERIMENT 1: ALG CA1, without GPS distance


nnodes = 1000

# traffic peak and variability
traffic_peak = c(10,150); traffic_var = 0.3

mm = c()
for (ii in c(1:nnodes)) {
  peak = runif(1,min=traffic_peak[1],max=traffic_peak[2])
  aux = abs(peak*profiles[sample(c(1:dim(profiles)[1]),size=1,prob=profiles_weigths),] + 
    traffic_var*peak*rnorm(dim(profiles)[2]))
  mm = rbind(mm,aux)
}

rownames(mm) = paste0("hl4_",c(1:dim(mm)[1]))




plot(c(0:23), mm[5,], type = "b", frame = FALSE, pch = 19,
     col = "red", xlab = "Hour", ylab = "Traffic (Gb/s)", 
     lty = 1, lwd = 1, xlim = c(0,25), ylim=c(0,200))
lines(c(0:23), mm[2,], pch = 18, col = "blue", type = "b", 
      lty = 2, lwd = 1)
lines(c(0:23), mm[3,], pch = 18, col = "green", type = "b", 
      lty = 2, lwd = 1)
lines(c(0:23), mm[4,], pch = 18, col = "brown", type = "b", 
      lty = 2, lwd = 1)


legend("topleft", legend = c("Node 1", "Node 2",
                             "Node 3","Node 4"),
       col = c("red", "blue","green","brown"), lty = 1:2, cex = 0.68)





mm = c()
for (ii in c(1:nnodes)) {
  peak = runif(1,min=traffic_peak[1],max=traffic_peak[2])
  aux = abs(peak*profiles[sample(c(1:dim(profiles)[1]),size=1),] + 
              traffic_var*peak*rnorm(dim(profiles)[2]))
  mm = rbind(mm,aux)
}

rownames(mm) = paste0("hl4_",c(1:dim(mm)[1]))

mm_bloques = ceiling(mm/25)
mm2 = mm_bloques



max_distance = 100 #km2
location_mm = data.frame(x=runif(nnodes,min=0,max=max_distance),
                         y=runif(nnodes,min=0,max=max_distance))
print("Distance Matrix ")
# creating a distance matrix
geodistance = dist(location_mm, location_mm, method="euclidean")
geodistance = geodistance/max(geodistance)
diag(geodistance)=1



mm_bloques = ceiling(mm/25)
mm2 = mm_bloques

rows.cor2 <- 0.5+0.5*cor(t(mm), use = "pairwise.complete.obs", method = "pearson")

geodist2 = matrix(0,nrow=nnodes,ncol=nnodes)
for (ii in c(1:nnodes)) {
  for (jj in c(1:nnodes)) {
    geodist2[ii,jj] = geodistance[ii,jj]
  }
}

rows.cor2 = as.matrix(rows.cor2)
geodistance = as.matrix(geodistance)


alpha = 1 # porcentaje importancia patrones
dist_matrix = alpha * rows.cor2 + (1-alpha)*geodist2

#object_name <- as.dist(object_name)
#hclust(object_name, method = "single")

hclust.row <- hclust(as.dist(dist_matrix))




# P2MP Clustering Algorithm: Hierarchical clustering of nodes
traff_hl4_aux = rowSums(mm_bloques)
oorder = hclust.row$order 
#oorder = sample(c(1:nrow(mm_bloques)), size=nrow(mm_bloques), replace=F) #random asignment
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
cat("1000 nodes: number of clusters CA1 = ")
print(length(clusters_final))




# Largest-First Fit Algorithm
traff_hl4_aux = rowSums(mm_bloques)
oorder = order(rowSums(mm_bloques),decreasing=TRUE)
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

cat("1000 nodes: number of clusters LargestFit = ")
print(length(clusters_final))







# Show a cluster
nclustershow = 1

for (nclustershow in c(1:length(clusters_final))) {
  if (!is.null(dim(clusters_final[[nclustershow]])[1])) {
    break
  }
}


plot(c(0:23),rep(16,1,24),type = "b", frame = FALSE, pch = 19,
     col = "green", xlab = "Hour", ylab = "Norm. Traffic", 
     lty = 1, lwd = 1, xlim = c(0,25), ylim = c(0,20))
lines(c(0:23), colSums(clusters_final[[nclustershow]]),
      pch = 18, col = "red", type = "b", lty = 2, lwd = 1)
for (cc in c(1:nrow(clusters_final[[nclustershow]]))) {
  lines(c(0:23), clusters_final[[nclustershow]][cc,],
        pch = 18, col = "blue", type = "b", lty = 2, lwd = 1)
  print(which(rowSums(matrix(rep(clusters_final[[nclustershow]][cc,],
                                 dim(mm)[1]),nrow=dim(mm)[1],byrow=T)-mm_bloques)==0))
}
legend("topleft", legend = c("Total aggregated", "Individual"),
       col = c("red", "blue"), lty = 1:2, cex = 0.8)


print(which(rowSums(matrix(rep(clusters_final[[nclustershow]][cc,],
                               dim(mm)[1]),nrow=dim(mm)[1],byrow=T)-mm_bloques)==0))




# EXPERIMENT 2: ALG CA2, including GPS distance

nnodes = 1000

mm = c()
for (ii in c(1:nnodes)) {
  peak = runif(1,min=0,max=100)
  aux = abs(peak*profiles[sample(c(1:dim(profiles)[1]),size=1),] + 
              0.3*peak*rnorm(dim(profiles)[2]))
  mm = rbind(mm,aux)
}

rownames(mm) = paste0("hl4_",c(1:dim(mm)[1]))

mm_bloques = ceiling(mm/25)
mm2 = mm_bloques



max_distance = 500 #km2
location_mm = data.frame(x=runif(nnodes,min=0,max=max_distance),
                         y=runif(nnodes,min=0,max=max_distance))
print("Distance Matrix ")
# creating a distance matrix
geodistance = dist(location_mm, location_mm, method="euclidean")
geodistance = geodistance/max(geodistance)
diag(geodistance)=1



mm_bloques = ceiling(mm/25)
mm2 = mm_bloques

rows.cor2 <- 0.5+0.5*cor(t(mm), use = "pairwise.complete.obs", method = "pearson")

geodist2 = matrix(0,nrow=nnodes,ncol=nnodes)
for (ii in c(1:nnodes)) {
  for (jj in c(1:nnodes)) {
    geodist2[ii,jj] = geodistance[ii,jj]
  }
}

rows.cor2 = as.matrix(rows.cor2)
geodistance = as.matrix(geodistance)


alpha = 1 # porcentaje importancia distancia
dist_matrix = alpha *rows.cor2 + (1-alpha)*geodist2

#object_name <- as.dist(object_name)
#hclust(object_name, method = "single")

hclust.row <- hclust(as.dist(dist_matrix))

#hclust.row <- hclust(as.dist(rows.cor2))


# P2MP Clustering Algorithm: Hierarchical clustering of nodes
traff_hl4_aux = rowSums(mm_bloques)
oorder = hclust.row$order 
#oorder = sample(c(1:nrow(mm_bloques)), size=nrow(mm_bloques), replace=F) #random asignment
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
cat("P2MP Alg. Number of clusters = ", length(clusters_final), "\n") 

opt_no_clusters = length(clusters_final)


# Random-Fit Algorithm
traff_hl4_aux = rowSums(mm_bloques)
oorder = sample(c(1:nrow(mm_bloques)), size=nrow(mm_bloques), replace=F) #random asignment
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
cat("Random Fit Alg. Number of clusters = ", length(clusters_final), "\n") 


# Smallest-First Fit Algorithm
traff_hl4_aux = rowSums(mm_bloques)
oorder = order(rowSums(mm_bloques),decreasing=FALSE)
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
cat("Smallest-First Fit Alg. Number of clusters = ", 
    length(clusters_final), "\n") 



# Largest-First Fit Algorithm
traff_hl4_aux = rowSums(mm_bloques)
oorder = order(rowSums(mm_bloques),decreasing=TRUE)
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
cat("Largest-First Fit Alg. Number of clusters = ", 
    length(clusters_final), "\n") 
cat("Savings = ",(length(clusters_final)-opt_no_clusters)/length(clusters_final),"\n")



# EXPERIMENT 3: CAPEX COMPARISON AGAINST P2MP


# P2MP Clustering Algorithm: Hierarchical clustering of nodes
traff_hl4_aux = rowSums(mm_bloques)
oorder = hclust.row$order 
#oorder = sample(c(1:nrow(mm_bloques)), size=nrow(mm_bloques), replace=F) #random asignment
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
cat("P2MP Alg. Number of clusters = ", length(clusters_final), "\n") 

opt_no_clusters = length(clusters_final)



# How many 100G P2MP transceivers?

tt400G = 0; tt100G = 0

for (ii in c(1:length(clusters_final))) {
  print(sum(as.numeric(apply(clusters_final[[ii]],1,max)>=4)))
  tt400G = tt400G + sum(as.numeric(apply(clusters_final[[ii]],1,max)>4))
  tt100G = tt100G + sum(as.numeric(apply(clusters_final[[ii]],1,max)<=4))
}
#print(tt400G)
#print(tt100G)



# How many P2P transceivers?
peak_traffic = NA*rep(1,nrow(mm))
for (ii in c(1:nrow(mm))) {
  peak_traffic[ii] = max(mm[ii,])
}

No_400Gplus_transceivers = sum(peak_traffic>400)
cat("No. of 400Gplus P2P transceivers", No_400Gplus_transceivers, "\n") 

No_400G_transceivers = sum(peak_traffic>200) - No_400Gplus_transceivers
cat("No. of 400G P2P transceivers", No_400G_transceivers, "\n") 

No_200G_transceivers = sum(peak_traffic>100) - No_400G_transceivers - 
  No_400Gplus_transceivers
cat("No. of 200G P2P transceivers", No_200G_transceivers, "\n") 

No_100G_transceivers = sum(peak_traffic>10) - No_400G_transceivers - 
  No_400Gplus_transceivers - No_200G_transceivers
cat("No. of 100G P2P transceivers", No_100G_transceivers, "\n") 

No_10G_transceivers = length(which(peak_traffic<10))
cat("No. of 10G P2P transceivers", No_10G_transceivers, "\n") 






