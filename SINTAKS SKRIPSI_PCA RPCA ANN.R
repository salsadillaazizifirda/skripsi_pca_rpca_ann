#--------------------library-----------------------
library(readxl)
library(openxlsx)
library(psych)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(writexl)
library(ggplot2)
library(rrcov)
library(psych)
library(caret)
library(DiagrammeR)

#--------------------DATA----------------------
data <- read_excel("D:/Skripsi/SKRIPSI SALSA/data_R.xlsx")
View(data)
str(data)
#Statistik deskriptif
statdesk<-describe(data)
statdesk
data.std <- as.data.frame(statdesk)
write_xlsx(statdesk, "D:/Skripsi/SKRIPSI SALSA/statdesk.xlsx")

#mengecualikan variabel kabupaten/kota dan 
#variabel target (Y) untuk analisis PCA &RPCA
data.numeric <- subset(data, select = -c(Kabupaten_Kota, Y))

#----------------STANDARISASI DATA------------------
data.std <- scale(data.numeric)
head(data.std)
dim(data.std)
# Boxplot data standar
boxplot(data.std,
        las = 2,
        col = "lightgreen",
        main = "Boxplot Data Setelah Standarisasi",
        ylab = "Z-score")
# Ekspor ke Excel
data.std <- as.data.frame(data.std)
write_xlsx(data.std, "D:/Skripsi/SKRIPSI SALSA/data_standarisasi.xlsx")

#----------------MATRIKS KORELASI-------------------
R <- cor(data.std)
R
R.df <- as.data.frame(R)
write_xlsx(R.df, "D:/Skripsi/SKRIPSI SALSA/data_korelasi.xlsx")
#Visualisasi korelasi
corrplot(R, method = "color", type = "upper")

#---------------UKURAN KELAYAKAN DATA---------------
#KMO & MSA
kelayakan_data <- KMO(R)
kelayakan_data
#uji bartlet
bartlett <- cortest.bartlett(R, n = nrow(data.std))
bartlett

#--------------------PCA----------------------------
#1.EIGENVALUE & EIGENVECTOR
eigen.pca <- eigen(R)
eigen.pca
#Eigenvalue
eigenvalue <- eigen.pca$values
eigenvalue
eigen_value <- as.data.frame(eigenvalue)
write_xlsx(eigen_value, "D:/Skripsi/SKRIPSI SALSA/data_eigenvalue.xlsx")
#Eigenvector / loading
eigenvector <- eigen.pca$vectors
eigenvector
eigen_vector <- as.data.frame(eigenvector)
write_xlsx(eigen_vector, "D:/Skripsi/SKRIPSI SALSA/data_eigenvector.xlsx")

#2.MENENTUKAN JUMLAH KOMPONEN UTAMA
#berdasarkan nilai eigenvalue>1
jumlah.pc <- sum(eigenvalue > 1)
jumlah.pc
#berdasarkan persentase kumulatif variansi 
proporsi <- eigenvalue/sum(eigenvalue)
kumulatif <- cumsum(proporsi)
hasil <- data.frame(
  Eigenvalue = eigenvalue,
  Proporsi = proporsi,
  Kumulatif = kumulatif
)
hasil
hasil <- as.data.frame(hasil)
write_xlsx(hasil, "D:/Skripsi/SKRIPSI SALSA/data_hasil eigenvalue & proporsi.xlsx")
#berdasar scree plot
plot(eigenvalue,
     type = "b",
     xlab = "Komponen Utama",
     ylab = "Eigenvalue",
     main = "Scree Plot CPCA")

#3.PERSAMAAN KOMPONEN UTAMA (PC)
eigenvector[,1] #Koefisien PC1
eigenvector[,2] #Koefisien PC2
eigenvector[,3] #Koefisien PC3
eigenvector[,4] #Koefisien PC4
eigenvector[,5] #Koefisien PC5

#4.SKOR KOMPONEN UTAMA
score.pca <- as.data.frame(data.std
          %*% eigenvector[,1:jumlah.pc])
colnames(score.pca) <- paste0("PC",1:jumlah.pc)
View(score.pca)
score.pca <- as.data.frame(score.pca)
write_xlsx(score.pca, "D:/Skripsi/SKRIPSI SALSA/data_skorpca.xlsx")


#-------------------------RPCA_RMCD----------------------
#1.MEMBENTUK SUBSET H
set.seed(123)
mcd <- CovMcd(data.std)
subsetH <- mcd$best;subsetH
subset_data <- data.std[mcd$best, ];subset_data 
hasil_subsetH <- data.frame(
  Observasi = subsetH,
  Kabupaten_Kota = data$Kabupaten_Kota[subsetH],
  data.std[subsetH, ]);hasil_subsetH
write_xlsx(hasil_subsetH, "D:/Skripsi/SKRIPSI SALSA/hasil_subsetH.xlsx")

#2.MENGHITUNG MEANS & KOVARIANS MCD
#mean MCD
mean.robust <- mcd$center
mean_robust <-data.frame(mean.robust )
write_xlsx(mean_robust, "D:/Skripsi/SKRIPSI SALSA/hasil_meanMCD.xlsx")
#kovarian MCD
cov.robust <- mcd$cov
cov_robust <-data.frame(cov.robust)
write_xlsx(cov_robust, "D:/Skripsi/SKRIPSI SALSA/hasil_KovarianMCD.xlsx")

#3.MENGHITUNG ROBUST MAHALANOBIS DISTANCE
rmd <- mahalanobis(data.std,
                   center = mean.robust,
                   cov = cov.robust)
cutoff <- qchisq(0.975,
                 df = ncol(data.std))
# jumlah outlier & inlier
sum(rmd > cutoff)   # outlier
sum(rmd <= cutoff)  # inlier
# status outlier/inlier
status <- ifelse(rmd > cutoff, "Outlier", "Inlier")
#tabel berdasarkan nama kabupaten/kota
hasil_rmd <- data.frame(
  Kabupaten_Kota = data$Kabupaten_Kota,
  RMD = rmd,
  Status = status)
head(hasil_rmd)
write_xlsx(hasil_rmd,"D:/Skripsi/SKRIPSI SALSA/hasil_RMD.xlsx")

#4.MENENTUKAN BOBOT TIAP OBSERVASI
bobot <- ifelse(rmd <= cutoff, 1, 0)
outlier <- which(bobot == 0)
data.inlier <- data.std[bobot == 1, ]
#Tabel observasi dan bobot
tabel_bobot <- data.frame(
  Kabupaten_Kota = data$Kabupaten_Kota,
  RMD = rmd,
  Bobot = bobot)
head(tabel_bobot)
write_xlsx(tabel_bobot,"D:/Skripsi/SKRIPSI SALSA/hasil_bobotobservasi.xlsx")

#5.ESTIMASI ULANG MEANS & KOVARIANS RMCD
mean.rmcd <- colMeans(data.inlier);mean.rmcd
mean_rmcd <-data.frame(mean.rmcd)
write_xlsx(mean_rmcd, "D:/Skripsi/SKRIPSI SALSA/hasil_meanRMCD.xlsx")
cov.rmcd <- cov(data.inlier);cov.rmcd
cov_rmcd <-data.frame(cov.rmcd)
write_xlsx(_, "D:/Skripsi/SKRIPSI SALSA/hasil_kovariansRMCD.xlsx")

#6.MENGHITUNG EIGENVALUE & EIGENVECTOR
eigen.robust <- eigen(cov.rmcd)
eigenvalue.robust <- eigen.robust$values;eigenvalue.robust 
eigenvector.robust <- eigen.robust$vectors;eigenvector.robust 
eigenvector_robust <-data.frame(eigenvector.robust )
write_xlsx(eigenvector_robust , "D:/Skripsi/SKRIPSI SALSA/hasil_eigenvector RPCA-RMCD.xlsx")

#7.MENGHITUNG JUMLAH KOMPONEN UTAMA RPCA RMCD
#berdasarkan eigenvalue
jumlah.pc.robust <- sum(eigenvalue.robust > 1)
jumlah.pc.robust
#berdasarkan proporsi
proporsi.robust <- eigenvalue.robust /
  sum(eigenvalue.robust)
kumulatif.robust <- cumsum(proporsi.robust)
hasil.robust <- data.frame(
  Komponen = paste("RPC",1:length(eigenvalue.robust),sep=""),
  Eigenvalue = eigenvalue.robust,
  Proporsi = proporsi.robust,
  Kumulatif = kumulatif.robust
)
hasil.robust
hasil.robust <-data.frame(hasil.robust)
write_xlsx(hasil.robust, "D:/Skripsi/SKRIPSI SALSA/hasil_eigenvarue dan proporsi RPCA RMCD.xlsx")
#Scree plot
plot(eigenvalue.robust,
     type = "b",
     xlab = "Komponen Utama",
     ylab = "Eigenvalue",
     main = "Scree Plot RPCA-RMCD")

#PERSAMAAN RPCA RMCD
eigenvector.robust[,1]
eigenvector.robust[,2]
eigenvector.robust[,3]
eigenvector.robust[,4]

#SKOR KOMPONEN UTAMA
score.rpca <- as.data.frame(
  data.std %*% eigenvector.robust[,1:4]
)
colnames(score.rpca) <- paste0("RPC",1:4)
View(score.rpca)
score.rpca<-data.frame(score.rpca)
write_xlsx(score.rpca, "D:/Skripsi/SKRIPSI SALSA/hasil_skor RPCA-RMCD.xlsx")



#----------------------------PCA ANN------------------------
#1.LABELLING VARIABEL TARGET
#Membentuk kelas biner
data$Y <- ifelse(data$Y >= median(data$Y),1, 0)
table(data$Y)
#kemiskinan tinggi = 1
#kemiskinan rendah = 0
data.pca.ann <- cbind(
  Kabupaten_Kota = data$Kabupaten_Kota,
  Y = data$Y,
  score.pca
)
head(data.pca.ann)
dataset.pca.ann<-data.frame(data.pca.ann)
write_xlsx(data.pca.ann, "D:/Skripsi/SKRIPSI SALSA/Dataset CPCA ANN.xlsx")

#2.PEMBAGIAN DATA TRAINING TESTING
set.seed(123)
n <- nrow(data.pca.ann)
index.train <- sample(
  1:n,
  size = 0.8*n)
train.pca <- data.pca.ann[index.train, ]
head(train.pca)
nrow(train.pca)
write_xlsx(train.pca, "D:/Skripsi/SKRIPSI SALSA/data training pca ann.xlsx")
test.pca  <- data.pca.ann[-index.train, ]
nrow(test.pca)
write_xlsx(test.pca, "D:/Skripsi/SKRIPSI SALSA/data testing pca ann.xlsx")

#3. ARSITEKTUR JARINGAN
#TRIAL HIDDEN LAYER
#Input layer = 5 (komponen utama)
#Output layer = 1 (klasifikasi biner)
library(neuralnet)
library(caret)
hidden <- c(1,2,3,4,5)
hasil.hidden <- data.frame()
set.seed(123)
for(i in hidden){
  model <- neuralnet(
    Y ~ PC1 + PC2 + PC3 + PC4 + PC5,
    data = train.pca,
    hidden = i,
    learningrate = 0.01,
    threshold = 0.01,
    linear.output = FALSE)
  # Prediksi
  pred <- compute(model, test.pca[,3:7])$net.result
  prediksi <- ifelse(pred > 0.5, 1, 0)
    # Confusion Matrix
  cm <- confusionMatrix(
    factor(prediksi),
    factor(test.pca$Y))
  # Simpan hasil
  hasil.hidden <- rbind(
    hasil.hidden,
    data.frame(
      Hidden = i,
      Accuracy = round(cm$overall["Accuracy"],4),
      Precision = round(cm$byClass["Precision"],4),
      Recall = round(cm$byClass["Recall"],4)))
}
hasil.hidden

#TRIAL LEARNING RATE
lr <- c(0.001,
        0.005,
        0.01,
        0.05,
        0.1)
hasil.lr <- data.frame()
set.seed(123)
for(i in lr){
  model <- neuralnet(
    Y ~ PC1 + PC2 + PC3 + PC4 + PC5,
    data = train.pca,
    hidden = 3,
    learningrate = i,
    threshold = 0.01,
    linear.output = FALSE)
  # Prediksi
  pred <- compute(model, test.pca[,3:7])$net.result
  prediksi <- ifelse(pred > 0.5, 1, 0)
  # Confusion Matrix
  cm <- confusionMatrix(
    factor(prediksi),
    factor(test.pca$Y))
  # Simpan hasil
  hasil.lr <- rbind(
    hasil.lr,
    data.frame(
      learning.rate = i,
      Accuracy = round(cm$overall["Accuracy"],4),
      Precision = round(cm$byClass["Precision"],4),
      Recall = round(cm$byClass["Recall"],4)))
}
hasil.lr

#4. PENENTUAN PARAMETER & HYPERPARAMETER
input_neuron<-5
hidden_neuron<-3
output_neuron<-1
max_epoch <- 1000
threshold <- 0.01
learning.rate<-0.1

#4. INISIASI BOBOT & BIAS
set.seed(123)
#bobot dan bias input-hidden
W <- matrix(runif(15, -0.5, 0.5), nrow = 5)
b_hidden <- runif(3, -0.5, 0.5)
#bobot dan bias hidden-output
V <- runif(3, -0.5, 0.5)
b_output <- runif(1, -0.5, 0.5)
#Membuat tabel
input_hidden <- data.frame(
  Variabel = c("Bias","PC1","PC2","PC3","PC4","PC5"),
  H1 = c(b_hidden[1], W[,1]),
  H2 = c(b_hidden[2], W[,2]),
  H3 = c(b_hidden[3], W[,3]))
hidden_output <- data.frame(
  Variabel = c("Bias","H1","H2","H3"),
  Output = c(b_output, V))
write.xlsx(
  list("Input-Hidden" = input_hidden,
       "Hidden-Output" = hidden_output),
  file = "D:/Skripsi/SKRIPSI SALSA/Bobot_dan_Bias_PCA_ANN.xlsx",
  overwrite = TRUE)

#Observasi pertama data training
x <- as.numeric(train.pca[1, c("PC1","PC2","PC3","PC4","PC5")])
target <- train.pca$Y[1]

#5. FEEDFORWARD
sigmoid <- function(x){
    1/(1+exp(-x))}
net_hidden <- x %*% W + b_hidden;net_hidden
hidden <- sigmoid(net_hidden);hidden
net_output <- hidden %*% V + b_output;net_output
output <- sigmoid(net_output);output

#6. BINARYCROSS ENTROPY
loss <- -(target*log(output)+
  (1-target)*log(1-output))
loss

#7. BACKPROPOGATION
# Delta output
delta_output <- as.numeric(output - target)
delta_output
#Delta hidden
delta_output <- as.numeric(delta_output)
delta_hidden <-hidden*(1-hidden)*
    (delta_output*V)
delta_hidden

#8. UPDATE BOBOT & BIAS
#update bobot hidden-ouput
V_baru <- numeric(3)
for(j in 1:3){
  V_baru[j] <-
    V[j] -
    learning.rate *
    delta_output *
    hidden[j]}
V_baru
#update bias ouput
b_output_baru <-b_output-learning.rate*delta_output;b_output_baru
#update bobot input-hidden
W_baru <- W
for(i in 1:5){
    for(j in 1:3){
    W_baru[i,j] <-W[i,j]-learning.rate*delta_hidden[j]*x[i]}}
W_baru
#update bias hidden
b_hidden_baru <-b_hidden-learning.rate*delta_hidden;b_hidden_baru

#PLOT FEEDFORWARD
grViz("
digraph ANN {
graph[
layout = dot
rankdir = LR
splines = true
nodesep = 0.4
ranksep = 1
]
node[
shape = circle
style = filled
fontname = Helvetica
fontsize = 12
width = 1.3
fixedsize = true
color = black
fontcolor = black
]
PC1[label='PC1\n0.1149',fillcolor='#B7DFF5']
PC2[label='PC2\n1.1236',fillcolor='#B7DFF5']
PC3[label='PC3\n0.3497',fillcolor='#B7DFF5']
PC4[label='PC4\n-0.0318',fillcolor='#B7DFF5']
PC5[label='PC5\n-0.0889',fillcolor='#B7DFF5']
H1[label='H1\nNet=0.6162\nOut=0.6494',fillcolor='#B7DFF5']
H2[label='H2\nNet=-0.1351\nOut=0.4663',fillcolor='#B7DFF5']
H3[label='H3\nNet=-0.3628\nOut=0.4103',fillcolor='#B7DFF5']
Output[
label='Output\nNet=0.4528\nOut=0.6113'
fillcolor='#98FB98'
fontcolor='black'
]
edge[
fontsize=10
fontname=Helvetica
color=black
penwidth=1.2
]
PC1->H1[label='-0.2124']
PC1->H2[label='-0.4544']
PC1->H3[label='0.4568']
PC2->H1[label='0.2883']
PC2->H2[label='0.0281']
PC2->H3[label='-0.0467']
PC3->H1[label='-0.0910']
PC3->H2[label='0.3924']
PC3->H3[label='0.1776']
PC4->H1[label='0.3830']
PC4->H2[label='0.0514']
PC4->H3[label='0.0726']
PC5->H1[label='0.4405']
PC5->H2[label='-0.0434']
PC5->H3[label='-0.3971']
H1->Output[label='-0.1721']
H2->Output[label='0.4545']
H3->Output[label='0.3895']
}")


#PLOT BACKPROPOGATION
grViz("
digraph BP {
graph[
layout = dot
rankdir = RL
splines = true
nodesep = 0.4
ranksep = 1
]
node[
shape = circle
style = filled
fontname = Helvetica
fontsize = 12
width = 1.3
fixedsize = true
color = black
fontcolor = black
]
Output[
label='Output\nδ=-0.3887'
fillcolor='#98FB98'
]
H1[
label='H1\nδ=0.0036'
fillcolor='#F7C6C7'
]
H2[
label='H2\nδ=-0.0104'
fillcolor='#F7C6C7'
]
H3[
label='H3\nδ=-0.0087'
fillcolor='#F7C6C7'
]
PC1[label='PC1',fillcolor='#B7DFF5']
PC2[label='PC2',fillcolor='#B7DFF5']
PC3[label='PC3',fillcolor='#B7DFF5']
PC4[label='PC4',fillcolor='#B7DFF5']
PC5[label='PC5',fillcolor='#B7DFF5']
edge[
fontsize=10
fontname=Helvetica
color=red
penwidth=1.5
]
Output->H1[label='δ']
Output->H2[label='δ']
Output->H3[label='δ']
H1->PC1
H1->PC2
H1->PC3
H1->PC4
H1->PC5
H2->PC1
H2->PC2
H2->PC3
H2->PC4
H2->PC5
H3->PC1
H3->PC2
H3->PC3
H3->PC4
H3->PC5
}")

#9. TRAINING ANN
X_train <- as.matrix(train.pca[,c("PC1","PC2","PC3","PC4","PC5")])
Y_train <- train.pca$Y
loss_history <- numeric(max_epoch)
for(epoch in 1:max_epoch){
  loss_epoch <- 0
  for(obs in 1:nrow(X_train)){
    
    #input
    x <- X_train[obs,]
        target <- Y_train[obs]
        
    #Feedforward
    net_hidden <- x %*% W + b_hidden
    hidden <- sigmoid(net_hidden)
    net_output <- hidden %*% V + b_output
    output <- sigmoid(net_output)
    
    #Binary cross entropy (BCE)
    loss <-
      -(target*log(output+1e-15)+
          (1-target)*log(1-output+1e-15))
    loss_epoch <- loss_epoch + loss
    
    #Backpropogation
    delta_output <- as.numeric(output-target)
    hidden <- as.numeric(hidden)
    delta_hidden <-hidden*(1-hidden)*(V*delta_output)
    
    #update bobot hidden-output
    for(j in 1:3){
      
      V[j] <-
        V[j]-
        learning.rate*
        delta_output*
        hidden[j]
      }
    #Update bias output 
    b_output <-
      b_output-
      learning.rate*
      delta_output
    
    #Update bobot Input-hidden
    for(i in 1:5){
      for(j in 1:3){
          W[i,j] <-
          W[i,j]-
          learning.rate*
          delta_hidden[j]*
          x[i]
        }
      }
    #update bias hidden
    b_hidden <-
      b_hidden-
      learning.rate*
      delta_hidden
    }
  
  #Loss Tiap Epoch
  loss_epoch <- loss_epoch/nrow(X_train)
  loss_history[epoch] <- loss_epoch
  cat("Epoch =",epoch,
      " Loss =",loss_epoch,"\n")
  if(loss_epoch<threshold){
    cat("Training berhenti pada epoch", epoch,"\n")
    break
    }
}

# Membuat data frame
loss_df <- data.frame(
  Epoch = 1:length(loss_history),
  Loss = loss_history
)

# Menyimpan ke Excel
write.xlsx(
  loss_df,
  file = "D:/Skripsi/SKRIPSI SALSA/Loss_Training_ANN.xlsx",
  rowNames = FALSE,
  overwrite = TRUE
)

#Plot Loss Epoch
plot(loss_history[loss_history!=0],
     type="l",
     xlab="Epoch",
     ylab="Binary Cross Entropy")

#Bobot dan Bias untuk model
W
b_hidden
V
b_output
data_all <- list(
  W = W,
  b_hidden = b_hidden,
  V = V,
  b_output = b_output
)

write.xlsx(data_all, "D:/Skripsi/SKRIPSI SALSA/hasil_Bobot_PCAANN.xlsx")

#TESTING
X_test <- as.matrix(test.pca[,c("PC1","PC2","PC3","PC4","PC5")])
#prediksi data testing
prediksi_prob <- numeric(nrow(X_test))
prediksi <- numeric(nrow(X_test))
for(i in 1:nrow(X_test)){
    hidden <-
    sigmoid(X_test[i,] %*% W + b_hidden)
    output <-
    sigmoid(hidden %*% V + b_output)
    prediksi_prob[i] <- output
    prediksi[i] <-
    ifelse(output>=0.5,1,0)
  }
confusion <- table(
    Aktual=test.pca$Y,
    Prediksi=prediksi
)
confusion

#EVALUASI MODEL
accuracy <-sum(diag(confusion))/sum(confusion)
accuracy
precision <-confusion[2,2]/(confusion[2,2]+confusion[1,2])
precision
recall <-confusion[2,2]/(confusion[2,2]+confusion[2,1])
recall


#PLOT PCA ANN
grViz(sprintf("
digraph ANN{
graph[
layout = dot
rankdir = LR
splines = true
nodesep = 0.6
ranksep = 1.2
]
node[
shape = circle
style = filled
fillcolor = LightBlue
color = black
fontcolor = black
fontsize = 12
fontname = Arial
width = 0.8
]
PC1
PC2
PC3
PC4
PC5
H1
H2
H3
Output[
label='Y'
shape=circle
style=filled
fillcolor=PaleGreen2
color=DarkGreen
fontcolor=black
penwidth=2
]
BiasH1[
shape=box
style=filled
fillcolor=LightGray
label='Bias\\n%.3f'
]
BiasH2[
shape=box
style=filled
fillcolor=LightGray
label='Bias\\n%.3f'
]
BiasH3[
shape=box
style=filled
fillcolor=LightGray
label='Bias\\n%.3f'
]
BiasO[
shape=box
style=filled
fillcolor=LightGray
label='Bias\\n%.3f'
]
edge[
fontsize=10
fontname=Arial
color=black
]
PC1->H1[label='%.2f']
PC2->H1[label='%.2f']
PC3->H1[label='%.2f']
PC4->H1[label='%.2f']
PC5->H1[label='%.2f']
PC1->H2[label='%.2f']
PC2->H2[label='%.2f']
PC3->H2[label='%.2f']
PC4->H2[label='%.2f']
PC5->H2[label='%.2f']
PC1->H3[label='%.2f']
PC2->H3[label='%.2f']
PC3->H3[label='%.2f']
PC4->H3[label='%.2f']
PC5->H3[label='%.2f']
BiasH1->H1
BiasH2->H2
BiasH3->H3
H1->Output[label='%.2f']
H2->Output[label='%.2f']
H3->Output[label='%.2f']
BiasO->Output
}
",
b_hidden[1], b_hidden[2], b_hidden[3], b_output,
W[1,1], W[2,1], W[3,1], W[4,1], W[5,1],
W[1,2], W[2,2], W[3,2], W[4,2], W[5,2],
W[1,3], W[2,3], W[3,3], W[4,3], W[5,3],
V[1], V[2], V[3]
))


#----------------------------RPCA ANN------------------------
#1.LABELLING VARIABEL TARGET
#Membentuk kelas biner
data$Y <- ifelse(data$Y >= median(data$Y),1, 0)
table(data$Y)
#kemiskinan tinggi = 1
#kemiskinan rendah = 0
data.rpca.ann <- cbind(
  Kabupaten_Kota = data$Kabupaten_Kota,
  Y = data$Y,
  score.rpca
)
head(data.rpca.ann)
dataset.rpca.ann<-data.frame(data.rpca.ann)
write_xlsx(data.rpca.ann, "D:/Skripsi/SKRIPSI SALSA/Dataset RPCA ANN.xlsx")

#2.PEMBAGIAN DATA TRAINING TESTING
set.seed(123)
n <- nrow(data.rpca.ann)
index.train <- sample(
  1:n,
  size = 0.8*n)
train.rpca <- data.rpca.ann[index.train, ]
head(train.rpca)
nrow(train.rpca)
write_xlsx(train.rpca, "D:/Skripsi/SKRIPSI SALSA/data training rpca ann.xlsx")
test.rpca  <- data.rpca.ann[-index.train, ]
nrow(test.rpca)
write_xlsx(test.rpca, "D:/Skripsi/SKRIPSI SALSA/data testing rpca ann.xlsx")

#3. ARSITEKTUR JARINGAN
#TRIAL HIDDEN LAYER
#Input layer = 4 (komponen utama)
#Output layer = 1 (klasifikasi biner)
library(neuralnet)
library(caret)
hidden <- c(1,2,3,4)
hasil.hidden <- data.frame()
set.seed(123)
for(i in hidden){
  model <- neuralnet(
    Y ~ RPC1 + RPC2 + RPC3 + RPC4,
    data = train.rpca,
    hidden = i,
    learningrate = 0.01,
    threshold = 0.01,
    linear.output = FALSE)
  # Prediksi
  pred <- compute(model, test.rpca[,3:6])$net.result
  prediksi <- ifelse(pred > 0.5, 1, 0)
  # Confusion Matrix
  cm <- confusionMatrix(
    factor(prediksi),
    factor(test.rpca$Y))
  # Simpan hasil
  hasil.hidden <- rbind(
    hasil.hidden,
    data.frame(
      Hidden = i,
      Accuracy = round(cm$overall["Accuracy"],4),
      Precision = round(cm$byClass["Precision"],4),
      Recall = round(cm$byClass["Recall"],4)))
}
hasil.hidden

#TRIAL LEARNING RATE
lr <- c(0.001,
        0.005,
        0.01,
        0.05,
        0.1)
hasil.lr <- data.frame()
set.seed(123)
for(i in lr){
  model <- neuralnet(
    Y ~ RPC1 + RPC2 + RPC3 + RPC4,
    data = train.rpca,
    hidden = 2,
    learningrate = i,
    threshold = 0.01,
    linear.output = FALSE)
  # Prediksi
  pred <- compute(model, test.rpca[,3:6])$net.result
  prediksi <- ifelse(pred > 0.5, 1, 0)
  # Confusion Matrix
  cm <- confusionMatrix(
    factor(prediksi),
    factor(test.rpca$Y))
  # Simpan hasil
  hasil.lr <- rbind(
    hasil.lr,
    data.frame(
      learning.rate = i,
      Accuracy = round(cm$overall["Accuracy"],4),
      Precision = round(cm$byClass["Precision"],4),
      Recall = round(cm$byClass["Recall"],4)))
}
hasil.lr

#4. PENENTUAN PARAMETER & HYPERPARAMETER
input_neuron<-5
hidden_neuron<-2
output_neuron<-1
max_epoch <- 1000
threshold <- 0.01
learning.rate<-0.001

#4. INISIASI BOBOT & BIAS
set.seed(123)
#bobot dan bias input-hidden
W <- matrix(runif(8, -0.5, 0.5), nrow = 4)
b_hidden <- runif(2, -0.5, 0.5)
#bobot dan bias hidden-output
V <- runif(2, -0.5, 0.5)
b_output <- runif(1, -0.5, 0.5)
#Membuat tabel
input_hidden <- data.frame(
  Variabel = c("Bias","RPC1","RPC2","RPC3","RPC4"),
  H1 = c(b_hidden[1], W[,1]),
  H2 = c(b_hidden[2], W[,2]))
hidden_output <- data.frame(
  Variabel = c("Bias","H1","H2"),
  Output = c(b_output, V))
write.xlsx(
  list("Input-Hidden" = input_hidden,
       "Hidden-Output" = hidden_output),
  file = "D:/Skripsi/SKRIPSI SALSA/Bobot_dan_Bias_RPCA_ANN.xlsx",
  overwrite = TRUE)

#Observasi pertama data training
x <- as.numeric(train.rpca[1, c("RPC1","RPC2","RPC3","RPC4")])
target <- train.rpca$Y[1]

#5. FEEDFORWARD
sigmoid <- function(x){
  1/(1+exp(-x))}
net_hidden <- x %*% W + b_hidden;net_hidden
hidden <- sigmoid(net_hidden);hidden
net_output <- hidden %*% V + b_output;net_output 
output <- sigmoid(net_output);output

#6. BINARYCROSS ENTROPY
loss <- -(target*log(output)+
            (1-target)*log(1-output))
loss

#7. BACKPROPOGATION
# Delta output
delta_output <- as.numeric(output - target)
delta_output
#Delta hidden
delta_output <- as.numeric(delta_output)
delta_hidden <-hidden*(1-hidden)*
  (delta_output*V)
delta_hidden

#8. UPDATE BOBOT & BIAS
#update bobot hidden-ouput
V_baru <- numeric(2)
for(j in 1:2){
  V_baru[j] <-
    V[j] -
    learning.rate *
    delta_output *
    hidden[j]}
V_baru
#update bias ouput
b_output_baru <-b_output-learning.rate*delta_output;b_output_baru
#update bobot input-hidden
W_baru <- W
for(i in 1:4){
  for(j in 1:2){
    W_baru[i,j] <-W[i,j]-learning.rate*delta_hidden[j]*x[i]}}
W_baru
#update bias hidden
b_hidden_baru <-b_hidden-learning.rate*delta_hidden;b_hidden_baru


#PLOT FEEDFORWARD
library(DiagrammeR)
x <- as.numeric(x)
hidden <- as.numeric(hidden)
output <- as.numeric(sigmoid(net_output))
W <- as.matrix(W)
V <- as.numeric(V)
b_hidden <- as.numeric(b_hidden)
b_output <- as.numeric(b_output)
grViz(sprintf("
digraph ANN{
graph[
layout=dot,
rankdir=LR,
nodesep=0.6,
ranksep=1
]
node[
shape=circle,
style=filled,
fillcolor=LightBlue,
color=black,
fontcolor=black,
fontsize=12,
width=1.2,
fixedsize=true
]
edge[
fontsize=11
]
RPC1[label='RPC1\\n%.4f']
RPC2[label='RPC2\\n%.4f']
RPC3[label='RPC3\\n%.4f']
RPC4[label='RPC4\\n%.4f']
H1[label='H1\\n%.4f']
H2[label='H2\\n%.4f']
Output[
label='Output\\n%.4f'
fillcolor=PaleGreen
]
bH1[
shape=box
fillcolor=khaki
label='bH1\\n%.4f'
]
bH2[
shape=box
fillcolor=khaki
label='bH2\\n%.4f'
]
bO[
shape=box
fillcolor=khaki
label='bO\\n%.4f'
]
RPC1 -> H1 [label='%.4f']
RPC2 -> H1 [label='%.4f']
RPC3 -> H1 [label='%.4f']
RPC4 -> H1 [label='%.4f']
RPC1 -> H2 [label='%.4f']
RPC2 -> H2 [label='%.4f']
RPC3 -> H2 [label='%.4f']
RPC4 -> H2 [label='%.4f']
H1 -> Output [label='%.4f']
H2 -> Output [label='%.4f']
bH1 -> H1
bH2 -> H2
bO  -> Output
}
",
x[1],
x[2],
x[3],
x[4],
hidden[1],
hidden[2],
output,
b_hidden[1],
b_hidden[2],
b_output,
W[1,1],
W[2,1],
W[3,1],
W[4,1],
W[1,2],
W[2,2],
W[3,2],
W[4,2],
V[1],
V[2]
))

#PLOT BACKPROPOGATION
grViz(sprintf("
digraph BackPropagation{
graph[
layout=dot
rankdir=RL
nodesep=0.7
ranksep=1
]
node[
shape=circle
style=filled
fillcolor=LightBlue
color=black
fontcolor=black
fontsize=12
width=1.1
fixedsize=true
]
Output[
label='Output\\nδ = %.4f'
fillcolor=PaleGreen
]
Error[
shape=box
style=filled
fillcolor=tomato
label='δ Output\\n%.4f'
]
H1[
label='H1\\nδ = %.4f'
]
H2[
label='H2\\nδ = %.4f'
]
RPC1[label='RPC1']
RPC2[label='RPC2']
RPC3[label='RPC3']
RPC4[label='RPC4']
Output -> Error
Error -> H1[
label='%.4f'
]
Error -> H2[
label='%.4f'
]
H1 -> RPC1[label='%.4f']
H1 -> RPC2[label='%.4f']
H1 -> RPC3[label='%.4f']
H1 -> RPC4[label='%.4f']
H2 -> RPC1[label='%.4f']
H2 -> RPC2[label='%.4f']
H2 -> RPC3[label='%.4f']
H2 -> RPC4[label='%.4f']
}
",
output,
delta_output,
delta_hidden[1],
delta_hidden[2],
V[1],
V[2],
W[1,1],
W[2,1],
W[3,1],
W[4,1],
W[1,2],
W[2,2],
W[3,2],
W[4,2]
))

#9. TRAINING ANN
X_train <- as.matrix(train.rpca[,c("RPC1","RPC2","RPC3","RPC4")])
Y_train <- train.rpca$Y
loss_history <- numeric(max_epoch)
for(epoch in 1:max_epoch){
  loss_epoch <- 0
  for(obs in 1:nrow(X_train)){
    
    #input
    x <- X_train[obs,]
    target <- Y_train[obs]
    
    #Feedforward
    net_hidden <- x %*% W + b_hidden
    hidden <- sigmoid(net_hidden)
    net_output <- hidden %*% V + b_output
    output <- sigmoid(net_output)
    
    #Binary cross entropy (BCE)
    loss <-
      -(target*log(output+1e-15)+
          (1-target)*log(1-output+1e-15))
    loss_epoch <- loss_epoch + loss
    
    #Backpropogation
    delta_output <- as.numeric(output-target)
    hidden <- as.numeric(hidden)
    delta_hidden <-hidden*(1-hidden)*(V*delta_output)
    
    #update bobot hidden-output
    for(j in 1:2){
      
      V[j] <-
        V[j]-
        learning.rate*
        delta_output*
        hidden[j]
    }
    #Update bias output 
    b_output <-
      b_output-
      learning.rate*
      delta_output
    
    #Update bobot Input-hidden
    for(i in 1:4){
      for(j in 1:2){
        W[i,j] <-
          W[i,j]-
          learning.rate*
          delta_hidden[j]*
          x[i]
      }
    }
    #update bias hidden
    b_hidden <-
      b_hidden-
      learning.rate*
      delta_hidden
  }
  
  #Loss Tiap Epoch
  loss_epoch <- loss_epoch/nrow(X_train)
  loss_history[epoch] <- loss_epoch
  cat("Epoch =",epoch,
      " Loss =",loss_epoch,"\n")
  if(loss_epoch<threshold){
    cat("Training berhenti pada epoch", epoch,"\n")
    break
  }
}

# Membuat data frame
loss_df <- data.frame(
  Epoch = 1:length(loss_history),
  Loss = loss_history
)

# Menyimpan ke Excel
write.xlsx(
  loss_df,
  file = "D:/Skripsi/SKRIPSI SALSA/Loss_Training_ANN.xlsx",
  rowNames = FALSE,
  overwrite = TRUE
)

#Plot Loss Epoch
plot(loss_history[loss_history!=0],
     type="l",
     xlab="Epoch",
     ylab="Binary Cross Entropy")

#Bobot dan Bias untuk model
W
b_hidden
V
b_output
data_all <- list(
  W = W,
  b_hidden = b_hidden,
  V = V,
  b_output = b_output
)
write.xlsx(data_all, "D:/Skripsi/SKRIPSI SALSA/hasil_Bobot_RPCA-ANN.xlsx")

#TESTING
X_test <- as.matrix(test.rpca[,c("RPC1","RPC2","RPC3","RPC4")])
#prediksi data testing
prediksi_prob <- numeric(nrow(X_test))
prediksi <- numeric(nrow(X_test))
for(i in 1:nrow(X_test)){
  hidden <-
    sigmoid(X_test[i,] %*% W + b_hidden)
  output <-
    sigmoid(hidden %*% V + b_output)
  prediksi_prob[i] <- output
  prediksi[i] <-
    ifelse(output>=0.5,1,0)
}
confusion <- table(
  Aktual=test.rpca$Y,
  Prediksi=prediksi
)
confusion

#EVALUASI MODEL
accuracy <-sum(diag(confusion))/sum(confusion)
accuracy
precision <-confusion[2,2]/(confusion[2,2]+confusion[1,2])
precision
recall <-confusion[2,2]/(confusion[2,2]+confusion[2,1])
recall

#DIAGRAM RPCA ANN
grViz(sprintf("
digraph ANN{
graph[
layout=dot
rankdir=LR
nodesep=0.8
ranksep=1
]

node[
shape=circle
style=filled
fillcolor=LightBlue
color=black
fontcolor=black
fontsize=12
width=1.1
fixedsize=true
]
RPC1[label='RPC1']
RPC2[label='RPC2']
RPC3[label='RPC3']
RPC4[label='RPC4']
H1[label='H1']
H2[label='H2']
Y[
label='Y'
fillcolor=PaleGreen
]
bH1[
shape=box
style=filled
fillcolor=khaki
label='bH1\\n%.4f'
]
bH2[
shape=box
style=filled
fillcolor=khaki
label='bH2\\n%.4f'
]

bO[
shape=box
style=filled
fillcolor=khaki
label='bO\\n%.4f'
]
RPC1 -> H1[label='%.4f']
RPC2 -> H1[label='%.4f']
RPC3 -> H1[label='%.4f']
RPC4 -> H1[label='%.4f']
RPC1 -> H2[label='%.4f']
RPC2 -> H2[label='%.4f']
RPC3 -> H2[label='%.4f']
RPC4 -> H2[label='%.4f']
H1 -> Y[label='%.4f']
H2 -> Y[label='%.4f']
bH1 -> H1
bH2 -> H2
bO -> Y

}
",

# Bias Hidden
b_hidden[1],
b_hidden[2],

# Bias Output
b_output,

# W menuju H1
W[1,1],
W[2,1],
W[3,1],
W[4,1],

# W menuju H2
W[1,2],
W[2,2],
W[3,2],
W[4,2],

# V
V[1],
V[2]

))
