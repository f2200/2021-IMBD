#----------訓練集----------#
data <- read.table("C:/Users/User/Desktop/train20210817v2.csv",header = TRUE,sep = ",",fill=TRUE)
dim(data)
View(data)

str(data)#查看資料結構(型態)
data1<-data[,-1]
summary(data1)
sum(is.na(data1))
View(data1)

#F_1正規化
data1[,1] <- (data1[,1]-mean(data1[,1]))/(sd(data1[,1]))

#----------特徵工程----------#
##連續變數取指數&根號&平方&log&倒數
library(stringr)
Continuous_feature_engineering <- function(dataset){
  Var <- c("F_1","F_2","F_3","F_4","F_5","F_6","F_7","F_8","F_9","F_10","F_11","F_12","F_13") 
  Exp_Var <- str_c(Var,"exp",sep="_")
  Square_Var <- str_c(Var,"square",sep = "_")
  Cube_Var <-str_c(Var,"cube",sep="_")
  Root_Var <- str_c(Var[2:13],"root",sep = "_") 
  Log_Var <- str_c(Var[2:13],"log",sep = "_") 
  reciprocal_Var <- str_c(Var,"reciprocal",sep="_")
  exp_matrix<-exp(dataset[Var])
  square_matrix <- dataset[Var]**2 
  cube_matrix <- dataset[Var]**3
  root_matrix <- sqrt(dataset[Var[2:13]])
  log_matrix <- log(dataset[Var[2:13]])
  reciprocal_matrix<-1/dataset[Var]
  colnames(exp_matrix) <- Exp_Var
  colnames(square_matrix) <- Square_Var
  colnames(cube_matrix)<-Cube_Var
  colnames(root_matrix) <- Root_Var
  colnames(log_matrix) <- Log_Var
  colnames(reciprocal_matrix) <- reciprocal_Var
  
  dataset <- cbind(dataset, exp_matrix,square_matrix,cube_matrix, root_matrix, log_matrix,reciprocal_matrix)
  return(dataset)
}
data1<-Continuous_feature_engineering(data1)
dim(data1)
sum(is.na(data1))
View(data1)

#F_1對其他變數做交互作用
library(stringr)
Continuous_F_1 <- function(dataset1){
  Var <- c("F_2","F_3","F_4","F_5","F_6","F_7","F_8","F_9","F_10","F_11","F_12","F_13",
           "F_2_exp","F_3_exp","F_4_exp","F_5_exp","F_6_exp","F_7_exp","F_8_exp","F_9_exp","F_10_exp","F_11_exp","F_12_exp","F_13_exp",
           "F_2_square","F_3_square","F_4_square","F_5_square","F_6_square","F_7_square","F_8_square","F_9_square","F_10_square","F_11_square","F_12_square","F_13_square",
           "F_2_cube","F_3_cube","F_4_cube","F_5_cube","F_6_cube","F_7_cube","F_8_cube","F_9_cube","F_10_cube","F_11_cube","F_12_cube","F_13_cube",
           "F_2_root","F_3_root","F_4_root","F_5_root","F_6_root","F_7_root","F_8_root","F_9_root","F_10_root","F_11_root","F_12_root","F_13_root",
           "F_2_log","F_3_log","F_4_log","F_5_log","F_6_log","F_7_log","F_8_log","F_9_log","F_10_log","F_11_log","F_12_log","F_13_log",
           "F_2_reciprocal","F_3_reciprocal","F_4_reciprocal","F_5_reciprocal","F_6_reciprocal","F_7_reciprocal","F_8_reciprocal","F_9_reciprocal","F_10_reciprocal","F_11_reciprocal","F_12_reciprocal","F_13_reciprocal",
           "F_1","F_1_exp","F_1_square","F_1_cube","F_1_reciprocal") 
  
  F_1 <- str_c("F_1",Var[1:84],sep = "_x_")
  A = matrix(0 ,ncol = 1,nrow = dim(dataset1)[1])
  for (i in 1:84){
    B<-dataset1["F_1"]*dataset1[(Var[i])]
    A<-cbind(A,B)
  }
  A<-A[2:85]
  colnames(A) <- F_1
  
  F_1_exp <- str_c("F_1_exp",Var[1:85],sep = "_x_")
  aa= matrix(0 ,ncol = 1,nrow = dim(dataset1)[1])
  for (j in 1:85){
    bb<-dataset1["F_1_exp"]*dataset1[(Var[j])]
    aa<-cbind(aa,bb)
  }
  aa<-aa[2:86]
  colnames(aa) <- F_1_exp
  
  F_1_square <- str_c("F_1_square",Var[1:86],sep = "_x_")
  AA= matrix(0 ,ncol = 1,nrow = dim(dataset1)[1])
  for (l in 1:86){
    BB<-dataset1["F_1_square"]*dataset1[(Var[l])]
    AA<-cbind(AA,BB)
  }
  AA<-AA[2:87]
  colnames(AA) <- F_1_square
  
  F_1_cube <- str_c("F_1_cube",Var[1:87],sep = "_x_")
  D= matrix(0 ,ncol = 1,nrow = dim(dataset1)[1])
  for (l in 1:87){
    DD<-dataset1["F_1_cube"]*dataset1[(Var[l])]
    D<-cbind(D,DD)
  }
  D<-D[2:88]
  colnames(D) <- F_1_cube
  
  F_1_reciprocal <- str_c("F_1_reciprocal",Var[1:88],sep = "_x_")
  cc= matrix(0 ,ncol = 1,nrow = dim(dataset1)[1])
  for (m in 1:88){
    dd<-dataset1["F_1_reciprocal"]*dataset1[(Var[m])]
    cc<-cbind(cc,dd)
  }
  cc<-cc[2:89]
  colnames(cc) <- F_1_reciprocal
  
  dataset1 <- cbind(A,aa,AA,D,cc)
  return(dataset1)
}
F1<-Continuous_F_1(data1)
dim(F1)

#F_2對其他變數做交互作用
library(stringr)
Continuous_F_2 <- function(dataset2){
  Var <- c("F_3","F_4","F_5","F_6","F_7","F_8","F_9","F_10","F_11","F_12","F_13",
           "F_3_exp","F_4_exp","F_5_exp","F_6_exp","F_7_exp","F_8_exp","F_9_exp","F_10_exp","F_11_exp","F_12_exp","F_13_exp",
           "F_3_square","F_4_square","F_5_square","F_6_square","F_7_square","F_8_square","F_9_square","F_10_square","F_11_square","F_12_square","F_13_square",
           "F_3_cube","F_4_cube","F_5_cube","F_6_cube","F_7_cube","F_8_cube","F_9_cube","F_10_cube","F_11_cube","F_12_cube","F_13_cube",
           "F_3_root","F_4_root","F_5_root","F_6_root","F_7_root","F_8_root","F_9_root","F_10_root","F_11_root","F_12_root","F_13_root",
           "F_3_log","F_4_log","F_5_log","F_6_log","F_7_log","F_8_log","F_9_log","F_10_log","F_11_log","F_12_log","F_13_log",
           "F_3_reciprocal","F_4_reciprocal","F_5_reciprocal","F_6_reciprocal","F_7_reciprocal","F_8_reciprocal","F_9_reciprocal","F_10_reciprocal","F_11_reciprocal","F_12_reciprocal","F_13_reciprocal",
           "F_2","F_2_exp","F_2_square","F_2_cube","F_2_root","F_2_log","F_2_reciprocal") 
  
  F_2 <- str_c("F_2",Var[1:77],sep = "_x_")
  A = matrix(0 ,ncol = 1,nrow = dim(dataset2)[1])
  for (i in 1:77){
    B<-dataset2["F_2"]*dataset2[(Var[i])]
    A<-cbind(A,B)
  }
  A<-A[2:78]
  colnames(A) <- F_2
  
  F_2_exp <- str_c("F_2_exp",Var[1:78],sep = "_x_")
  aa= matrix(0 ,ncol = 1,nrow = dim(dataset2)[1])
  for (j in 1:78){
    bb<-dataset2["F_2_exp"]*dataset2[(Var[j])]
    aa<-cbind(aa,bb)
  }
  aa<-aa[2:79]
  colnames(aa) <- F_2_exp
  
  F_2_square <- str_c("F_2_square",Var[1:79],sep = "_x_")
  AA= matrix(0 ,ncol = 1,nrow = dim(dataset2)[1])
  for (l in 1:79){
    BB<-dataset2["F_2_square"]*dataset2[(Var[l])]
    AA<-cbind(AA,BB)
  }
  AA<-AA[2:80]
  colnames(AA) <- F_2_square
  
  F_2_cube <- str_c("F_2_cube",Var[1:80],sep = "_x_")
  D= matrix(0 ,ncol = 1,nrow = dim(dataset2)[1])
  for (l in 1:80){
    DD<-dataset2["F_2_cube"]*dataset2[(Var[l])]
    D<-cbind(D,DD)
  }
  D<-D[2:81]
  colnames(D) <- F_2_cube
  
  F_2_root <- str_c("F_2_root",Var[1:81],sep = "_x_")
  c= matrix(0 ,ncol = 1,nrow = dim(dataset2)[1])
  for (l in 1:81){
    d<-dataset2["F_2_root"]*dataset2[(Var[l])]
    c<-cbind(c,d)
  }
  c<-c[2:82]
  colnames(c) <- F_2_root
  
  F_2_log <- str_c("F_2_log",Var[1:82],sep = "_x_")
  C= matrix(0 ,ncol = 1,nrow = dim(dataset2)[1])
  for (v in 1:82){
    B<-dataset2["F_2_log"]*dataset2[(Var[v])]
    C<-cbind(C,B)
  }
  C<-C[2:83]
  colnames(C) <- F_2_log
  
  F_2_reciprocal <- str_c("F_2_reciprocal",Var[1:83],sep = "_x_")
  cc= matrix(0 ,ncol = 1,nrow = dim(dataset2)[1])
  for (m in 1:83){
    dd<-dataset2["F_2_reciprocal"]*dataset2[(Var[m])]
    cc<-cbind(cc,dd)
  }
  cc<-cc[2:84]
  colnames(cc) <- F_2_reciprocal
  
  dataset2 <- cbind(A,aa,AA,D,c,C,cc)
  return(dataset2)
}
F2<-Continuous_F_2(data1)
dim(F2)

#F_3對其他變數做交互作用
library(stringr)
Continuous_F_3 <- function(dataset3){
  Var <- c("F_4","F_5","F_6","F_7","F_8","F_9","F_10","F_11","F_12","F_13",
           "F_4_exp","F_5_exp","F_6_exp","F_7_exp","F_8_exp","F_9_exp","F_10_exp","F_11_exp","F_12_exp","F_13_exp",
           "F_4_square","F_5_square","F_6_square","F_7_square","F_8_square","F_9_square","F_10_square","F_11_square","F_12_square","F_13_square",
           "F_4_cube","F_5_cube","F_6_cube","F_7_cube","F_8_cube","F_9_cube","F_10_cube","F_11_cube","F_12_cube","F_13_cube",
           "F_4_root","F_5_root","F_6_root","F_7_root","F_8_root","F_9_root","F_10_root","F_11_root","F_12_root","F_13_root",
           "F_4_log","F_5_log","F_6_log","F_7_log","F_8_log","F_9_log","F_10_log","F_11_log","F_12_log","F_13_log",
           "F_4_reciprocal","F_5_reciprocal","F_6_reciprocal","F_7_reciprocal","F_8_reciprocal","F_9_reciprocal","F_10_reciprocal","F_11_reciprocal","F_12_reciprocal","F_13_reciprocal",
           "F_3","F_3_exp","F_3_square","F_3_cube","F_3_root","F_3_log","F_3_reciprocal") 
  
  F_3 <- str_c("F_3",Var[1:70],sep = "_x_")
  A = matrix(0 ,ncol = 1,nrow = dim(dataset3)[1])
  for (i in 1:70){
    B<-dataset3["F_3"]*dataset3[(Var[i])]
    A<-cbind(A,B)
  }
  A<-A[2:71]
  colnames(A) <- F_3
  
  F_3_exp <- str_c("F_3_exp",Var[1:71],sep = "_x_")
  aa= matrix(0 ,ncol = 1,nrow = dim(dataset3)[1])
  for (j in 1:71){
    bb<-dataset3["F_3_exp"]*dataset3[(Var[j])]
    aa<-cbind(aa,bb)
  }
  aa<-aa[2:72]
  colnames(aa) <- F_3_exp
  
  F_3_square <- str_c("F_3_square",Var[1:72],sep = "_x_")
  AA= matrix(0 ,ncol = 1,nrow = dim(dataset3)[1])
  for (l in 1:72){
    BB<-dataset3["F_3_square"]*dataset3[(Var[l])]
    AA<-cbind(AA,BB)
  }
  AA<-AA[2:73]
  colnames(AA) <- F_3_square
  
  F_3_cube <- str_c("F_3_cube",Var[1:73],sep = "_x_")
  D= matrix(0 ,ncol = 1,nrow = dim(dataset3)[1])
  for (l in 1:73){
    DD<-dataset3["F_3_cube"]*dataset3[(Var[l])]
    D<-cbind(D,DD)
  }
  D<-D[2:74]
  colnames(D) <- F_3_cube
  
  F_3_root <- str_c("F_3_root",Var[1:74],sep = "_x_")
  c= matrix(0 ,ncol = 1,nrow = dim(dataset3)[1])
  for (l in 1:74){
    d<-dataset3["F_3_root"]*dataset3[(Var[l])]
    c<-cbind(c,d)
  }
  c<-c[2:75]
  colnames(c) <- F_3_root
  
  F_3_log <- str_c("F_3_log",Var[1:75],sep = "_x_")
  C= matrix(0 ,ncol = 1,nrow = dim(dataset3)[1])
  for (v in 1:75){
    B<-dataset3["F_3_log"]*dataset3[(Var[v])]
    C<-cbind(C,B)
  }
  C<-C[2:76]
  colnames(C) <- F_3_log
  
  F_3_reciprocal <- str_c("F_3_reciprocal",Var[1:76],sep = "_x_")
  cc= matrix(0 ,ncol = 1,nrow = dim(dataset3)[1])
  for (m in 1:76){
    dd<-dataset3["F_3_reciprocal"]*dataset3[(Var[m])]
    cc<-cbind(cc,dd)
  }
  cc<-cc[2:77]
  colnames(cc) <- F_3_reciprocal
  
  dataset3 <- cbind(A,aa,AA,D,c,C,cc)
  return(dataset3)
}
F3<-Continuous_F_3(data1)
dim(F3)

#F_4對其他變數做交互作用
library(stringr)
Continuous_F_4 <- function(dataset4){
  Var <- c("F_5","F_6","F_7","F_8","F_9","F_10","F_11","F_12","F_13",
           "F_5_exp","F_6_exp","F_7_exp","F_8_exp","F_9_exp","F_10_exp","F_11_exp","F_12_exp","F_13_exp",
           "F_5_square","F_6_square","F_7_square","F_8_square","F_9_square","F_10_square","F_11_square","F_12_square","F_13_square",
           "F_5_cube","F_6_cube","F_7_cube","F_8_cube","F_9_cube","F_10_cube","F_11_cube","F_12_cube","F_13_cube",
           "F_5_root","F_6_root","F_7_root","F_8_root","F_9_root","F_10_root","F_11_root","F_12_root","F_13_root",
           "F_5_log","F_6_log","F_7_log","F_8_log","F_9_log","F_10_log","F_11_log","F_12_log","F_13_log",
           "F_5_reciprocal","F_6_reciprocal","F_7_reciprocal","F_8_reciprocal","F_9_reciprocal","F_10_reciprocal","F_11_reciprocal","F_12_reciprocal","F_13_reciprocal",
           "F_4","F_4_exp","F_4_square","F_4_cube","F_4_root","F_4_log","F_4_reciprocal") 
  
  F_4 <- str_c("F_4",Var[1:63],sep = "_x_")
  A = matrix(0 ,ncol = 1,nrow = dim(dataset4)[1])
  for (i in 1:63){
    B<-dataset4["F_4"]*dataset4[(Var[i])]
    A<-cbind(A,B)
  }
  A<-A[2:64]
  colnames(A) <- F_4
  
  F_4_exp <- str_c("F_4_exp",Var[1:64],sep = "_x_")
  aa= matrix(0 ,ncol = 1,nrow = dim(dataset4)[1])
  for (j in 1:64){
    bb<-dataset4["F_4_exp"]*dataset4[(Var[j])]
    aa<-cbind(aa,bb)
  }
  aa<-aa[2:65]
  colnames(aa) <- F_4_exp
  
  F_4_square <- str_c("F_4_square",Var[1:65],sep = "_x_")
  AA= matrix(0 ,ncol = 1,nrow = dim(dataset4)[1])
  for (l in 1:65){
    BB<-dataset4["F_4_square"]*dataset4[(Var[l])]
    AA<-cbind(AA,BB)
  }
  AA<-AA[2:66]
  colnames(AA) <- F_4_square
  
  F_4_cube <- str_c("F_4_cube",Var[1:66],sep = "_x_")
  D= matrix(0 ,ncol = 1,nrow = dim(dataset4)[1])
  for (l in 1:66){
    DD<-dataset4["F_4_cube"]*dataset4[(Var[l])]
    D<-cbind(D,DD)
  }
  D<-D[2:67]
  colnames(D) <- F_4_cube
  
  F_4_root <- str_c("F_4_root",Var[1:67],sep = "_x_")
  c= matrix(0 ,ncol = 1,nrow = dim(dataset4)[1])
  for (l in 1:67){
    d<-dataset4["F_4_root"]*dataset4[(Var[l])]
    c<-cbind(c,d)
  }
  c<-c[2:68]
  colnames(c) <- F_4_root
  
  F_4_log <- str_c("F_4_log",Var[1:68],sep = "_x_")
  C= matrix(0 ,ncol = 1,nrow = dim(dataset4)[1])
  for (v in 1:68){
    B<-dataset4["F_4_log"]*dataset4[(Var[v])]
    C<-cbind(C,B)
  }
  C<-C[2:69]
  colnames(C) <- F_4_log
  
  F_4_reciprocal <- str_c("F_4_reciprocal",Var[1:69],sep = "_x_")
  cc= matrix(0 ,ncol = 1,nrow = dim(dataset4)[1])
  for (m in 1:69){
    dd<-dataset4["F_4_reciprocal"]*dataset4[(Var[m])]
    cc<-cbind(cc,dd)
  }
  cc<-cc[2:70]
  colnames(cc) <- F_4_reciprocal
  
  dataset4 <- cbind(A,aa,AA,D,c,C,cc)
  return(dataset4)
}
F4<-Continuous_F_4(data1)
dim(F4)

#F_5對其他變數做交互作用
library(stringr)
Continuous_F_5 <- function(dataset5){
  Var <- c("F_6","F_7","F_8","F_9","F_10","F_11","F_12","F_13",
           "F_6_exp","F_7_exp","F_8_exp","F_9_exp","F_10_exp","F_11_exp","F_12_exp","F_13_exp",
           "F_6_square","F_7_square","F_8_square","F_9_square","F_10_square","F_11_square","F_12_square","F_13_square",
           "F_6_cube","F_7_cube","F_8_cube","F_9_cube","F_10_cube","F_11_cube","F_12_cube","F_13_cube",
           "F_6_root","F_7_root","F_8_root","F_9_root","F_10_root","F_11_root","F_12_root","F_13_root",
           "F_6_log","F_7_log","F_8_log","F_9_log","F_10_log","F_11_log","F_12_log","F_13_log",
           "F_6_reciprocal","F_7_reciprocal","F_8_reciprocal","F_9_reciprocal","F_10_reciprocal","F_11_reciprocal","F_12_reciprocal","F_13_reciprocal",
           "F_5","F_5_exp","F_5_square","F_5_cube","F_5_root","F_5_log","F_5_reciprocal") 
  
  F_5 <- str_c("F_5",Var[1:56],sep = "_x_")
  A = matrix(0 ,ncol = 1,nrow = dim(dataset5)[1])
  for (i in 1:56){
    B<-dataset5["F_5"]*dataset5[(Var[i])]
    A<-cbind(A,B)
  }
  A<-A[2:57]
  colnames(A) <- F_5
  
  F_5_exp <- str_c("F_5_exp",Var[1:57],sep = "_x_")
  aa= matrix(0 ,ncol = 1,nrow = dim(dataset5)[1])
  for (j in 1:57){
    bb<-dataset5["F_5_exp"]*dataset5[(Var[j])]
    aa<-cbind(aa,bb)
  }
  aa<-aa[2:58]
  colnames(aa) <- F_5_exp
  
  F_5_square <- str_c("F_5_square",Var[1:58],sep = "_x_")
  AA= matrix(0 ,ncol = 1,nrow = dim(dataset5)[1])
  for (l in 1:58){
    BB<-dataset5["F_5_square"]*dataset5[(Var[l])]
    AA<-cbind(AA,BB)
  }
  AA<-AA[2:59]
  colnames(AA) <- F_5_square
  
  F_5_cube <- str_c("F_5_cube",Var[1:59],sep = "_x_")
  D= matrix(0 ,ncol = 1,nrow = dim(dataset5)[1])
  for (l in 1:59){
    DD<-dataset5["F_5_cube"]*dataset5[(Var[l])]
    D<-cbind(D,DD)
  }
  D<-D[2:60]
  colnames(D) <- F_5_cube
  
  F_5_root <- str_c("F_5_root",Var[1:60],sep = "_x_")
  c= matrix(0 ,ncol = 1,nrow = dim(dataset5)[1])
  for (l in 1:60){
    d<-dataset5["F_5_root"]*dataset5[(Var[l])]
    c<-cbind(c,d)
  }
  c<-c[2:61]
  colnames(c) <- F_5_root
  
  F_5_log <- str_c("F_5_log",Var[1:61],sep = "_x_")
  C= matrix(0 ,ncol = 1,nrow = dim(dataset5)[1])
  for (v in 1:61){
    B<-dataset5["F_5_log"]*dataset5[(Var[v])]
    C<-cbind(C,B)
  }
  C<-C[2:62]
  colnames(C) <- F_5_log
  
  F_5_reciprocal <- str_c("F_5_reciprocal",Var[1:62],sep = "_x_")
  cc= matrix(0 ,ncol = 1,nrow = dim(dataset5)[1])
  for (m in 1:62){
    dd<-dataset5["F_5_reciprocal"]*dataset5[(Var[m])]
    cc<-cbind(cc,dd)
  }
  cc<-cc[2:63]
  colnames(cc) <- F_5_reciprocal
  
  dataset5 <- cbind(A,aa,AA,D,c,C,cc)
  return(dataset5)
}
F5<-Continuous_F_5(data1)
dim(F5)

#F_6對其他變數做交互作用
library(stringr)
Continuous_F_6 <- function(dataset6){
  Var <- c("F_7","F_8","F_9","F_10","F_11","F_12","F_13",
           "F_7_exp","F_8_exp","F_9_exp","F_10_exp","F_11_exp","F_12_exp","F_13_exp",
           "F_7_square","F_8_square","F_9_square","F_10_square","F_11_square","F_12_square","F_13_square",
           "F_7_cube","F_8_cube","F_9_cube","F_10_cube","F_11_cube","F_12_cube","F_13_cube",
           "F_7_root","F_8_root","F_9_root","F_10_root","F_11_root","F_12_root","F_13_root",
           "F_7_log","F_8_log","F_9_log","F_10_log","F_11_log","F_12_log","F_13_log",
           "F_7_reciprocal","F_8_reciprocal","F_9_reciprocal","F_10_reciprocal","F_11_reciprocal","F_12_reciprocal","F_13_reciprocal",
           "F_6","F_6_exp","F_6_square","F_6_cube","F_6_root","F_6_log","F_6_reciprocal") 
  
  F_6 <- str_c("F_6",Var[1:49],sep = "_x_")
  A = matrix(0 ,ncol = 1,nrow = dim(dataset6)[1])
  for (i in 1:49){
    B<-dataset6["F_6"]*dataset6[(Var[i])]
    A<-cbind(A,B)
  }
  A<-A[2:50]
  colnames(A) <- F_6
  
  F_6_exp <- str_c("F_6_exp",Var[1:50],sep = "_x_")
  aa= matrix(0 ,ncol = 1,nrow = dim(dataset6)[1])
  for (j in 1:50){
    bb<-dataset6["F_6_exp"]*dataset6[(Var[j])]
    aa<-cbind(aa,bb)
  }
  aa<-aa[2:51]
  colnames(aa) <- F_6_exp
  
  F_6_square <- str_c("F_6_square",Var[1:51],sep = "_x_")
  AA= matrix(0 ,ncol = 1,nrow = dim(dataset6)[1])
  for (l in 1:51){
    BB<-dataset6["F_6_square"]*dataset6[(Var[l])]
    AA<-cbind(AA,BB)
  }
  AA<-AA[2:52]
  colnames(AA) <- F_6_square
  
  F_6_cube <- str_c("F_6_cube",Var[1:52],sep = "_x_")
  D= matrix(0 ,ncol = 1,nrow = dim(dataset6)[1])
  for (l in 1:52){
    DD<-dataset6["F_6_cube"]*dataset6[(Var[l])]
    D<-cbind(D,DD)
  }
  D<-D[2:53]
  colnames(D) <- F_6_cube
  
  F_6_root <- str_c("F_6_root",Var[1:53],sep = "_x_")
  c= matrix(0 ,ncol = 1,nrow = dim(dataset6)[1])
  for (l in 1:53){
    d<-dataset6["F_6_root"]*dataset6[(Var[l])]
    c<-cbind(c,d)
  }
  c<-c[2:54]
  colnames(c) <- F_6_root
  
  F_6_log <- str_c("F_6_log",Var[1:54],sep = "_x_")
  C= matrix(0 ,ncol = 1,nrow = dim(dataset6)[1])
  for (v in 1:54){
    B<-dataset6["F_6_log"]*dataset6[(Var[v])]
    C<-cbind(C,B)
  }
  C<-C[2:55]
  colnames(C) <- F_6_log
  
  F_6_reciprocal <- str_c("F_6_reciprocal",Var[1:55],sep = "_x_")
  cc= matrix(0 ,ncol = 1,nrow = dim(dataset6)[1])
  for (m in 1:55){
    dd<-dataset6["F_6_reciprocal"]*dataset6[(Var[m])]
    cc<-cbind(cc,dd)
  }
  cc<-cc[2:56]
  colnames(cc) <- F_6_reciprocal
  
  dataset6 <- cbind(A,aa,AA,D,c,C,cc)
  return(dataset6)
}
F6<-Continuous_F_6(data1)
dim(F6)

#F_7對其他變數做交互作用
library(stringr)
Continuous_F_7 <- function(dataset7){
  Var <- c("F_8","F_9","F_10","F_11","F_12","F_13",
           "F_8_exp","F_9_exp","F_10_exp","F_11_exp","F_12_exp","F_13_exp",
           "F_8_square","F_9_square","F_10_square","F_11_square","F_12_square","F_13_square",
           "F_8_cube","F_9_cube","F_10_cube","F_11_cube","F_12_cube","F_13_cube",
           "F_8_root","F_9_root","F_10_root","F_11_root","F_12_root","F_13_root",
           "F_8_log","F_9_log","F_10_log","F_11_log","F_12_log","F_13_log",
           "F_8_reciprocal","F_9_reciprocal","F_10_reciprocal","F_11_reciprocal","F_12_reciprocal","F_13_reciprocal",
           "F_7","F_7_exp","F_7_square","F_7_cube","F_7_root","F_7_log","F_7_reciprocal") 
  
  F_7 <- str_c("F_7",Var[1:42],sep = "_x_")
  A = matrix(0 ,ncol = 1,nrow = dim(dataset7)[1])
  for (i in 1:42){
    B<-dataset7["F_7"]*dataset7[(Var[i])]
    A<-cbind(A,B)
  }
  A<-A[2:43]
  colnames(A) <- F_7
  
  F_7_exp <- str_c("F_7_exp",Var[1:43],sep = "_x_")
  aa= matrix(0 ,ncol = 1,nrow = dim(dataset7)[1])
  for (j in 1:43){
    bb<-dataset7["F_7_exp"]*dataset7[(Var[j])]
    aa<-cbind(aa,bb)
  }
  aa<-aa[2:44]
  colnames(aa) <- F_7_exp
  
  F_7_square <- str_c("F_7_square",Var[1:44],sep = "_x_")
  AA= matrix(0 ,ncol = 1,nrow = dim(dataset7)[1])
  for (l in 1:44){
    BB<-dataset7["F_7_square"]*dataset7[(Var[l])]
    AA<-cbind(AA,BB)
  }
  AA<-AA[2:45]
  colnames(AA) <- F_7_square
  
  F_7_cube <- str_c("F_7_cube",Var[1:45],sep = "_x_")
  D= matrix(0 ,ncol = 1,nrow = dim(dataset7)[1])
  for (l in 1:45){
    DD<-dataset7["F_7_cube"]*dataset7[(Var[l])]
    D<-cbind(D,DD)
  }
  D<-D[2:46]
  colnames(D) <- F_7_cube
  
  F_7_root <- str_c("F_7_root",Var[1:46],sep = "_x_")
  c= matrix(0 ,ncol = 1,nrow = dim(dataset7)[1])
  for (l in 1:46){
    d<-dataset7["F_7_root"]*dataset7[(Var[l])]
    c<-cbind(c,d)
  }
  c<-c[2:47]
  colnames(c) <- F_7_root
  
  F_7_log <- str_c("F_7_log",Var[1:47],sep = "_x_")
  C= matrix(0 ,ncol = 1,nrow = dim(dataset7)[1])
  for (v in 1:47){
    B<-dataset7["F_7_log"]*dataset7[(Var[v])]
    C<-cbind(C,B)
  }
  C<-C[2:48]
  colnames(C) <- F_7_log
  
  F_7_reciprocal <- str_c("F_7_reciprocal",Var[1:48],sep = "_x_")
  cc= matrix(0 ,ncol = 1,nrow = dim(dataset7)[1])
  for (m in 1:48){
    dd<-dataset7["F_7_reciprocal"]*dataset7[(Var[m])]
    cc<-cbind(cc,dd)
  }
  cc<-cc[2:49]
  colnames(cc) <- F_7_reciprocal
  
  dataset7 <- cbind(A,aa,AA,D,c,C,cc)
  return(dataset7)
}
F7<-Continuous_F_7(data1)
dim(F7)

#F_8對其他變數做交互作用
library(stringr)
Continuous_F_8 <- function(dataset8){
  Var <- c("F_9","F_10","F_11","F_12","F_13",
           "F_9_exp","F_10_exp","F_11_exp","F_12_exp","F_13_exp",
           "F_9_square","F_10_square","F_11_square","F_12_square","F_13_square",
           "F_9_cube","F_10_cube","F_11_cube","F_12_cube","F_13_cube",
           "F_9_root","F_10_root","F_11_root","F_12_root","F_13_root",
           "F_9_log","F_10_log","F_11_log","F_12_log","F_13_log",
           "F_9_reciprocal","F_10_reciprocal","F_11_reciprocal","F_12_reciprocal","F_13_reciprocal",
           "F_8","F_8_exp","F_8_square","F_8_cube","F_8_root","F_8_log","F_8_reciprocal") 
  
  F_8 <- str_c("F_8",Var[1:35],sep = "_x_")
  A = matrix(0 ,ncol = 1,nrow = dim(dataset8)[1])
  for (i in 1:35){
    B<-dataset8["F_8"]*dataset8[(Var[i])]
    A<-cbind(A,B)
  }
  A<-A[2:36]
  colnames(A) <- F_8
  
  F_8_exp <- str_c("F_8_exp",Var[1:36],sep = "_x_")
  aa= matrix(0 ,ncol = 1,nrow = dim(dataset8)[1])
  for (j in 1:36){
    bb<-dataset8["F_8_exp"]*dataset8[(Var[j])]
    aa<-cbind(aa,bb)
  }
  aa<-aa[2:37]
  colnames(aa) <- F_8_exp
  
  F_8_square <- str_c("F_8_square",Var[1:37],sep = "_x_")
  AA= matrix(0 ,ncol = 1,nrow = dim(dataset8)[1])
  for (l in 1:37){
    BB<-dataset8["F_8_square"]*dataset8[(Var[l])]
    AA<-cbind(AA,BB)
  }
  AA<-AA[2:38]
  colnames(AA) <- F_8_square
  
  F_8_cube <- str_c("F_8_cube",Var[1:38],sep = "_x_")
  D= matrix(0 ,ncol = 1,nrow = dim(dataset8)[1])
  for (l in 1:38){
    DD<-dataset8["F_8_cube"]*dataset8[(Var[l])]
    D<-cbind(D,DD)
  }
  D<-D[2:39]
  colnames(D) <- F_8_cube
  
  F_8_root <- str_c("F_8_root",Var[1:39],sep = "_x_")
  c= matrix(0 ,ncol = 1,nrow = dim(dataset8)[1])
  for (l in 1:39){
    d<-dataset8["F_8_root"]*dataset8[(Var[l])]
    c<-cbind(c,d)
  }
  c<-c[2:40]
  colnames(c) <- F_8_root
  
  F_8_log <- str_c("F_8_log",Var[1:40],sep = "_x_")
  C= matrix(0 ,ncol = 1,nrow = dim(dataset8)[1])
  for (v in 1:40){
    B<-dataset8["F_8_log"]*dataset8[(Var[v])]
    C<-cbind(C,B)
  }
  C<-C[2:41]
  colnames(C) <- F_8_log
  
  F_8_reciprocal <- str_c("F_8_reciprocal",Var[1:41],sep = "_x_")
  cc= matrix(0 ,ncol = 1,nrow = dim(dataset8)[1])
  for (m in 1:41){
    dd<-dataset8["F_8_reciprocal"]*dataset8[(Var[m])]
    cc<-cbind(cc,dd)
  }
  cc<-cc[2:42]
  colnames(cc) <- F_8_reciprocal
  
  dataset8 <- cbind(A,aa,AA,D,c,C,cc)
  return(dataset8)
}
F8<-Continuous_F_8(data1)
dim(F8)

#F_9對其他變數做交互作用
library(stringr)
Continuous_F_9 <- function(dataset9){
  Var <- c("F_10","F_11","F_12","F_13",
           "F_10_exp","F_11_exp","F_12_exp","F_13_exp",
           "F_10_square","F_11_square","F_12_square","F_13_square",
           "F_10_cube","F_11_cube","F_12_cube","F_13_cube",
           "F_10_root","F_11_root","F_12_root","F_13_root",
           "F_10_log","F_11_log","F_12_log","F_13_log",
           "F_10_reciprocal","F_11_reciprocal","F_12_reciprocal","F_13_reciprocal",
           "F_9","F_9_exp","F_9_square","F_9_cube","F_9_root","F_9_log","F_9_reciprocal") 
  
  F_9 <- str_c("F_9",Var[1:28],sep = "_x_")
  A = matrix(0 ,ncol = 1,nrow = dim(dataset9)[1])
  for (i in 1:28){
    B<-dataset9["F_9"]*dataset9[(Var[i])]
    A<-cbind(A,B)
  }
  A<-A[2:29]
  colnames(A) <- F_9
  
  F_9_exp <- str_c("F_9_exp",Var[1:29],sep = "_x_")
  aa= matrix(0 ,ncol = 1,nrow = dim(dataset9)[1])
  for (j in 1:29){
    bb<-dataset9["F_9_exp"]*dataset9[(Var[j])]
    aa<-cbind(aa,bb)
  }
  aa<-aa[2:30]
  colnames(aa) <- F_9_exp
  
  F_9_square <- str_c("F_9_square",Var[1:30],sep = "_x_")
  AA= matrix(0 ,ncol = 1,nrow = dim(dataset9)[1])
  for (l in 1:30){
    BB<-dataset9["F_9_square"]*dataset9[(Var[l])]
    AA<-cbind(AA,BB)
  }
  AA<-AA[2:31]
  colnames(AA) <- F_9_square
  
  F_9_cube <- str_c("F_9_cube",Var[1:31],sep = "_x_")
  D= matrix(0 ,ncol = 1,nrow = dim(dataset9)[1])
  for (l in 1:31){
    DD<-dataset9["F_9_cube"]*dataset9[(Var[l])]
    D<-cbind(D,DD)
  }
  D<-D[2:32]
  colnames(D) <- F_9_cube
  
  F_9_root <- str_c("F_9_root",Var[1:32],sep = "_x_")
  c= matrix(0 ,ncol = 1,nrow = dim(dataset9)[1])
  for (l in 1:32){
    d<-dataset9["F_9_root"]*dataset9[(Var[l])]
    c<-cbind(c,d)
  }
  c<-c[2:33]
  colnames(c) <- F_9_root
  
  F_9_log <- str_c("F_9_log",Var[1:33],sep = "_x_")
  C= matrix(0 ,ncol = 1,nrow = dim(dataset9)[1])
  for (v in 1:33){
    B<-dataset9["F_9_log"]*dataset9[(Var[v])]
    C<-cbind(C,B)
  }
  C<-C[2:34]
  colnames(C) <- F_9_log
  
  F_9_reciprocal <- str_c("F_9_reciprocal",Var[1:34],sep = "_x_")
  cc= matrix(0 ,ncol = 1,nrow = dim(dataset9)[1])
  for (m in 1:34){
    dd<-dataset9["F_9_reciprocal"]*dataset9[(Var[m])]
    cc<-cbind(cc,dd)
  }
  cc<-cc[2:35]
  colnames(cc) <- F_9_reciprocal
  
  dataset9 <- cbind(A,aa,AA,D,c,C,cc)
  return(dataset9)
}
F9<-Continuous_F_9(data1)
dim(F9)

#F_10對其他變數做交互作用
library(stringr)
Continuous_F_10 <- function(dataset10){
  Var <- c("F_11","F_12","F_13",
           "F_11_exp","F_12_exp","F_13_exp",
           "F_11_square","F_12_square","F_13_square",
           "F_11_cube","F_12_cube","F_13_cube",
           "F_11_root","F_12_root","F_13_root",
           "F_11_log","F_12_log","F_13_log",
           "F_11_reciprocal","F_12_reciprocal","F_13_reciprocal",
           "F_10","F_10_exp","F_10_square","F_10_cube","F_10_root","F_10_log","F_10_reciprocal") 
  
  F_10 <- str_c("F_10",Var[1:21],sep = "_x_")
  A = matrix(0 ,ncol = 1,nrow = dim(dataset10)[1])
  for (i in 1:21){
    B<-dataset10["F_10"]*dataset10[(Var[i])]
    A<-cbind(A,B)
  }
  A<-A[2:22]
  colnames(A) <- F_10
  
  F_10_exp <- str_c("F_10_exp",Var[1:22],sep = "_x_")
  aa= matrix(0 ,ncol = 1,nrow = dim(dataset10)[1])
  for (j in 1:22){
    bb<-dataset10["F_10_exp"]*dataset10[(Var[j])]
    aa<-cbind(aa,bb)
  }
  aa<-aa[2:23]
  colnames(aa) <- F_10_exp
  
  F_10_square <- str_c("F_10_square",Var[1:23],sep = "_x_")
  AA= matrix(0 ,ncol = 1,nrow = dim(dataset10)[1])
  for (l in 1:23){
    BB<-dataset10["F_10_square"]*dataset10[(Var[l])]
    AA<-cbind(AA,BB)
  }
  AA<-AA[2:24]
  colnames(AA) <- F_10_square
  
  F_10_cube <- str_c("F_10_cube",Var[1:24],sep = "_x_")
  D= matrix(0 ,ncol = 1,nrow = dim(dataset10)[1])
  for (l in 1:24){
    DD<-dataset10["F_10_cube"]*dataset10[(Var[l])]
    D<-cbind(D,DD)
  }
  D<-D[2:25]
  colnames(D) <- F_10_cube
  
  F_10_root <- str_c("F_10_root",Var[1:25],sep = "_x_")
  c= matrix(0 ,ncol = 1,nrow = dim(dataset10)[1])
  for (l in 1:25){
    d<-dataset10["F_10_root"]*dataset10[(Var[l])]
    c<-cbind(c,d)
  }
  c<-c[2:26]
  colnames(c) <- F_10_root
  
  F_10_log <- str_c("F_10_log",Var[1:26],sep = "_x_")
  C= matrix(0 ,ncol = 1,nrow = dim(dataset10)[1])
  for (v in 1:26){
    B<-dataset10["F_10_log"]*dataset10[(Var[v])]
    C<-cbind(C,B)
  }
  C<-C[2:27]
  colnames(C) <- F_10_log
  
  F_10_reciprocal <- str_c("F_10_reciprocal",Var[1:27],sep = "_x_")
  cc= matrix(0 ,ncol = 1,nrow = dim(dataset10)[1])
  for (m in 1:27){
    dd<-dataset10["F_10_reciprocal"]*dataset10[(Var[m])]
    cc<-cbind(cc,dd)
  }
  cc<-cc[2:28]
  colnames(cc) <- F_10_reciprocal
  
  dataset10 <- cbind(A,aa,AA,D,c,C,cc)
  return(dataset10)
}
F10<-Continuous_F_10(data1)
dim(F10)

#F_11對其他變數做交互作用
library(stringr)
Continuous_F_11 <- function(dataset11){
  Var <- c("F_12","F_13",
           "F_12_exp","F_13_exp",
           "F_12_square","F_13_square",
           "F_12_cube","F_13_cube",
           "F_12_root","F_13_root",
           "F_12_log","F_13_log",
           "F_12_reciprocal","F_13_reciprocal",
           "F_11","F_11_exp","F_11_square","F_11_cube","F_11_root","F_11_log","F_11_reciprocal") 
  
  F_11 <- str_c("F_11",Var[1:14],sep = "_x_")
  A = matrix(0 ,ncol = 1,nrow = dim(dataset11)[1])
  for (i in 1:14){
    B<-dataset11["F_11"]*dataset11[(Var[i])]
    A<-cbind(A,B)
  }
  A<-A[2:15]
  colnames(A) <- F_11
  
  F_11_exp <- str_c("F_11_exp",Var[1:15],sep = "_x_")
  aa= matrix(0 ,ncol = 1,nrow = dim(dataset11)[1])
  for (j in 1:15){
    bb<-dataset11["F_11_exp"]*dataset11[(Var[j])]
    aa<-cbind(aa,bb)
  }
  aa<-aa[2:16]
  colnames(aa) <- F_11_exp
  
  F_11_square <- str_c("F_11_square",Var[1:16],sep = "_x_")
  AA= matrix(0 ,ncol = 1,nrow = dim(dataset11)[1])
  for (l in 1:16){
    BB<-dataset11["F_11_square"]*dataset11[(Var[l])]
    AA<-cbind(AA,BB)
  }
  AA<-AA[2:17]
  colnames(AA) <- F_11_square
  
  F_11_cube <- str_c("F_11_cube",Var[1:17],sep = "_x_")
  D= matrix(0 ,ncol = 1,nrow = dim(dataset11)[1])
  for (l in 1:17){
    DD<-dataset11["F_11_cube"]*dataset11[(Var[l])]
    D<-cbind(D,DD)
  }
  D<-D[2:18]
  colnames(D) <- F_11_cube
  
  F_11_root <- str_c("F_11_root",Var[1:18],sep = "_x_")
  c= matrix(0 ,ncol = 1,nrow = dim(dataset11)[1])
  for (l in 1:18){
    d<-dataset11["F_11_root"]*dataset11[(Var[l])]
    c<-cbind(c,d)
  }
  c<-c[2:19]
  colnames(c) <- F_11_root
  
  F_11_log <- str_c("F_11_log",Var[1:19],sep = "_x_")
  C= matrix(0 ,ncol = 1,nrow = dim(dataset11)[1])
  for (v in 1:19){
    B<-dataset11["F_11_log"]*dataset11[(Var[v])]
    C<-cbind(C,B)
  }
  C<-C[2:20]
  colnames(C) <- F_11_log
  
  F_11_reciprocal <- str_c("F_11_reciprocal",Var[1:20],sep = "_x_")
  cc= matrix(0 ,ncol = 1,nrow = dim(dataset11)[1])
  for (m in 1:20){
    dd<-dataset11["F_11_reciprocal"]*dataset11[(Var[m])]
    cc<-cbind(cc,dd)
  }
  cc<-cc[2:21]
  colnames(cc) <- F_11_reciprocal
  
  dataset11 <- cbind(A,aa,AA,D,c,C,cc)
  return(dataset11)
}
F11<-Continuous_F_11(data1)
dim(F11)

#F_12對其他變數做交互作用
library(stringr)
Continuous_F_12 <- function(dataset12){
  Var <- c("F_13","F_13_exp","F_13_square","F_13_cube","F_13_root","F_13_log","F_13_reciprocal",
           "F_12","F_12_exp","F_12_square","F_12_cube","F_12_root","F_12_log","F_12_reciprocal") 
  
  F_12 <- str_c("F_12",Var[1:7],sep = "_x_")
  A = matrix(0 ,ncol = 1,nrow = dim(dataset12)[1])
  for (i in 1:7){
    B<-dataset12["F_12"]*dataset12[(Var[i])]
    A<-cbind(A,B)
  }
  A<-A[2:8]
  colnames(A) <- F_12
  
  F_12_exp <- str_c("F_12_exp",Var[1:8],sep = "_x_")
  aa= matrix(0 ,ncol = 1,nrow = dim(dataset12)[1])
  for (j in 1:8){
    bb<-dataset12["F_12_exp"]*dataset12[(Var[j])]
    aa<-cbind(aa,bb)
  }
  aa<-aa[2:9]
  colnames(aa) <- F_12_exp
  
  F_12_square <- str_c("F_12_square",Var[1:9],sep = "_x_")
  AA= matrix(0 ,ncol = 1,nrow = dim(dataset12)[1])
  for (l in 1:9){
    BB<-dataset12["F_12_square"]*dataset12[(Var[l])]
    AA<-cbind(AA,BB)
  }
  AA<-AA[2:10]
  colnames(AA) <- F_12_square
  
  F_12_cube <- str_c("F_12_cube",Var[1:10],sep = "_x_")
  D= matrix(0 ,ncol = 1,nrow = dim(dataset12)[1])
  for (l in 1:10){
    DD<-dataset12["F_12_cube"]*dataset12[(Var[l])]
    D<-cbind(D,DD)
  }
  D<-D[2:11]
  colnames(D) <- F_12_cube
  
  F_12_root <- str_c("F_12_root",Var[1:11],sep = "_x_")
  c= matrix(0 ,ncol = 1,nrow = dim(dataset12)[1])
  for (l in 1:11){
    d<-dataset12["F_12_root"]*dataset12[(Var[l])]
    c<-cbind(c,d)
  }
  c<-c[2:12]
  colnames(c) <- F_12_root
  
  F_12_log <- str_c("F_12_log",Var[1:12],sep = "_x_")
  C= matrix(0 ,ncol = 1,nrow = dim(dataset12)[1])
  for (v in 1:12){
    B<-dataset12["F_12_log"]*dataset12[(Var[v])]
    C<-cbind(C,B)
  }
  C<-C[2:13]
  colnames(C) <- F_12_log
  
  F_12_reciprocal <- str_c("F_12_reciprocal",Var[1:13],sep = "_x_")
  cc= matrix(0 ,ncol = 1,nrow = dim(dataset12)[1])
  for (m in 1:13){
    dd<-dataset12["F_12_reciprocal"]*dataset12[(Var[m])]
    cc<-cbind(cc,dd)
  }
  cc<-cc[2:14]
  colnames(cc) <- F_12_reciprocal
  
  dataset12 <- cbind(A,aa,AA,D,c,C,cc)
  return(dataset12)
}
F12<-Continuous_F_12(data1)
dim(F12)

#F_13對其他變數做交互作用
library(stringr)
Continuous_F_13 <- function(dataset13){
  Var <- c("F_13","F_13_exp","F_13_square","F_13_cube","F_13_root","F_13_log","F_13_reciprocal")
  
  F_13_exp <- str_c("F_13_exp",Var[1],sep = "_x_")
  aa= matrix(0 ,ncol = 1,nrow = dim(dataset13)[1])
  bb<-dataset13["F_13_exp"]*dataset13[(Var[1])]
  aa<-cbind(bb,aa)
  colnames(aa) <- F_13_exp
  
  F_13_square <- str_c("F_13_square",Var[1:2],sep = "_x_")
  AA= matrix(0 ,ncol = 1,nrow = dim(dataset13)[1])
  for (l in 1:2){
    BB<-dataset13["F_13_square"]*dataset13[(Var[l])]
    AA<-cbind(AA,BB)
  }
  AA<-AA[2:3]
  colnames(AA) <- F_13_square
  
  F_13_cube <- str_c("F_13_cube",Var[1:3],sep = "_x_")
  D= matrix(0 ,ncol = 1,nrow = dim(dataset13)[1])
  for (l in 1:3){
    DD<-dataset13["F_13_cube"]*dataset13[(Var[l])]
    D<-cbind(D,DD)
  }
  D<-D[2:4]
  colnames(D) <- F_13_cube
  
  F_13_root <- str_c("F_13_root",Var[1:4],sep = "_x_")
  c= matrix(0 ,ncol = 1,nrow = dim(dataset13)[1])
  for (l in 1:4){
    d<-dataset13["F_13_root"]*dataset13[(Var[l])]
    c<-cbind(c,d)
  }
  c<-c[2:5]
  colnames(c) <- F_13_root
  
  F_13_log <- str_c("F_13_log",Var[1:5],sep = "_x_")
  C= matrix(0 ,ncol = 1,nrow = dim(dataset13)[1])
  for (v in 1:5){
    B<-dataset13["F_13_log"]*dataset13[(Var[v])]
    C<-cbind(C,B)
  }
  C<-C[2:6]
  colnames(C) <- F_13_log
  
  F_13_reciprocal <- str_c("F_13_reciprocal",Var[1:6],sep = "_x_")
  cc= matrix(0 ,ncol = 1,nrow = dim(dataset13)[1])
  for (m in 1:6){
    dd<-dataset13["F_13_reciprocal"]*dataset13[(Var[m])]
    cc<-cbind(cc,dd)
  }
  cc<-cc[2:7]
  colnames(cc) <- F_13_reciprocal
  
  dataset13 <- cbind(aa,AA,D,c,C,cc)
  return(dataset13)
}
F13<-Continuous_F_13(data1)
F13<-F13[,-2]
dim(F13)

data2<-cbind(data1,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13)#將全部的特工工程新增出來的資料跟原始訓練資料做合併
dim(data2)
Train_data <- data2
#--------------lasso篩選變數-------------#
# install.packages("glmnet")
library(glmnet)
lasso = glmnet(x = as.matrix(data2[, -14]), y = data2[, 14], alpha = 1,family = "gaussian")
plot(lasso, xvar='lambda', main="Lasso")
cv.lasso = cv.glmnet(x = as.matrix(data2[, -14]), y = data2[, 14], alpha = 1, family = "gaussian")
best.lambda = cv.lasso$lambda.min
best.lambda
plot(lasso, xvar='lambda', main="Lasso")
abline(v=log(best.lambda), col="blue", lty=5.5 )
coef(cv.lasso, s = "lambda.min")
select.ind = which(coef(cv.lasso, s = "lambda.min") != 0)
select.ind = select.ind[-1]-1 
select.ind 
select.varialbes = colnames(data2)[select.ind]
select.varialbes

#--------------建模-------------#
model<-lm(O ~ ., data2[, c(select.varialbes, "O")])
summary(model)

#----------測試集----------#
data3 <- read.table("C:/Users/User/Desktop/2021test0831.csv",header = TRUE,sep = ",",fill=TRUE)
dim(data3)
View(data3)

str(data3)#查看資料結構(型態)
data4<-data3[,-1]
summary(data4)
sum(is.na(data4))
View(data4)

#F_1正規化
data4[,1] <- (data4[,1]-mean(data4[,1]))/(sd(data4[,1]))

#--------------特徵工程-------------#
##連續變數取指數&根號&平方&log&倒數
library(stringr)
Continuous_feature_engineering1 <- function(dataset){
  Var <- c("F_1","F_2","F_3","F_4","F_5","F_6","F_7","F_8","F_9","F_10","F_11","F_12","F_13") 
  Exp_Var <- str_c(Var,"exp",sep="_")
  Square_Var <- str_c(Var,"square",sep = "_")
  Cube_Var <-str_c(Var,"cube",sep="_")
  Root_Var <- str_c(Var[2:13],"root",sep = "_") 
  Log_Var <- str_c(Var[2:13],"log",sep = "_") 
  reciprocal_Var <- str_c(Var,"reciprocal",sep="_")
  exp_matrix<-exp(dataset[Var])
  square_matrix <- dataset[Var]**2 
  cube_matrix <- dataset[Var]**3
  root_matrix <- sqrt(dataset[Var[2:13]])
  log_matrix <- log(dataset[Var[2:13]])
  reciprocal_matrix<-1/dataset[Var]
  colnames(exp_matrix) <- Exp_Var
  colnames(square_matrix) <- Square_Var
  colnames(cube_matrix)<-Cube_Var
  colnames(root_matrix) <- Root_Var
  colnames(log_matrix) <- Log_Var
  colnames(reciprocal_matrix) <- reciprocal_Var
  
  dataset <- cbind(dataset, exp_matrix,square_matrix,cube_matrix, root_matrix, log_matrix,reciprocal_matrix)
  return(dataset)
}
data4<-Continuous_feature_engineering1(data4)
dim(data4)
sum(is.na(data4))
View(data4)

#F_1對其他變數做交互作用
library(stringr)
Continuous1_F_1 <- function(dataset1){
  Var <- c("F_2","F_3","F_4","F_5","F_6","F_7","F_8","F_9","F_10","F_11","F_12","F_13",
           "F_2_exp","F_3_exp","F_4_exp","F_5_exp","F_6_exp","F_7_exp","F_8_exp","F_9_exp","F_10_exp","F_11_exp","F_12_exp","F_13_exp",
           "F_2_square","F_3_square","F_4_square","F_5_square","F_6_square","F_7_square","F_8_square","F_9_square","F_10_square","F_11_square","F_12_square","F_13_square",
           "F_2_cube","F_3_cube","F_4_cube","F_5_cube","F_6_cube","F_7_cube","F_8_cube","F_9_cube","F_10_cube","F_11_cube","F_12_cube","F_13_cube",
           "F_2_root","F_3_root","F_4_root","F_5_root","F_6_root","F_7_root","F_8_root","F_9_root","F_10_root","F_11_root","F_12_root","F_13_root",
           "F_2_log","F_3_log","F_4_log","F_5_log","F_6_log","F_7_log","F_8_log","F_9_log","F_10_log","F_11_log","F_12_log","F_13_log",
           "F_2_reciprocal","F_3_reciprocal","F_4_reciprocal","F_5_reciprocal","F_6_reciprocal","F_7_reciprocal","F_8_reciprocal","F_9_reciprocal","F_10_reciprocal","F_11_reciprocal","F_12_reciprocal","F_13_reciprocal",
           "F_1","F_1_exp","F_1_square","F_1_cube","F_1_reciprocal") 
  
  F_1 <- str_c("F_1",Var[1:84],sep = "_x_")
  A = matrix(0 ,ncol = 1,nrow = dim(dataset1)[1])
  for (i in 1:84){
    B<-dataset1["F_1"]*dataset1[(Var[i])]
    A<-cbind(A,B)
  }
  A<-A[2:85]
  colnames(A) <- F_1
  
  F_1_exp <- str_c("F_1_exp",Var[1:85],sep = "_x_")
  aa= matrix(0 ,ncol = 1,nrow = dim(dataset1)[1])
  for (j in 1:85){
    bb<-dataset1["F_1_exp"]*dataset1[(Var[j])]
    aa<-cbind(aa,bb)
  }
  aa<-aa[2:86]
  colnames(aa) <- F_1_exp
  
  F_1_square <- str_c("F_1_square",Var[1:86],sep = "_x_")
  AA= matrix(0 ,ncol = 1,nrow = dim(dataset1)[1])
  for (l in 1:86){
    BB<-dataset1["F_1_square"]*dataset1[(Var[l])]
    AA<-cbind(AA,BB)
  }
  AA<-AA[2:87]
  colnames(AA) <- F_1_square
  
  F_1_cube <- str_c("F_1_cube",Var[1:87],sep = "_x_")
  D= matrix(0 ,ncol = 1,nrow = dim(dataset1)[1])
  for (l in 1:87){
    DD<-dataset1["F_1_cube"]*dataset1[(Var[l])]
    D<-cbind(D,DD)
  }
  D<-D[2:88]
  colnames(D) <- F_1_cube
  
  F_1_reciprocal <- str_c("F_1_reciprocal",Var[1:88],sep = "_x_")
  cc= matrix(0 ,ncol = 1,nrow = dim(dataset1)[1])
  for (m in 1:88){
    dd<-dataset1["F_1_reciprocal"]*dataset1[(Var[m])]
    cc<-cbind(cc,dd)
  }
  cc<-cc[2:89]
  colnames(cc) <- F_1_reciprocal
  
  dataset1 <- cbind(A,aa,AA,D,cc)
  return(dataset1)
}
continuous1_F1<-Continuous1_F_1(data4)

#F_2對其他變數做交互作用
library(stringr)
Continuous1_F_2 <- function(dataset2){
  Var <- c("F_3","F_4","F_5","F_6","F_7","F_8","F_9","F_10","F_11","F_12","F_13",
           "F_3_exp","F_4_exp","F_5_exp","F_6_exp","F_7_exp","F_8_exp","F_9_exp","F_10_exp","F_11_exp","F_12_exp","F_13_exp",
           "F_3_square","F_4_square","F_5_square","F_6_square","F_7_square","F_8_square","F_9_square","F_10_square","F_11_square","F_12_square","F_13_square",
           "F_3_cube","F_4_cube","F_5_cube","F_6_cube","F_7_cube","F_8_cube","F_9_cube","F_10_cube","F_11_cube","F_12_cube","F_13_cube",
           "F_3_root","F_4_root","F_5_root","F_6_root","F_7_root","F_8_root","F_9_root","F_10_root","F_11_root","F_12_root","F_13_root",
           "F_3_log","F_4_log","F_5_log","F_6_log","F_7_log","F_8_log","F_9_log","F_10_log","F_11_log","F_12_log","F_13_log",
           "F_3_reciprocal","F_4_reciprocal","F_5_reciprocal","F_6_reciprocal","F_7_reciprocal","F_8_reciprocal","F_9_reciprocal","F_10_reciprocal","F_11_reciprocal","F_12_reciprocal","F_13_reciprocal",
           "F_2","F_2_exp","F_2_square","F_2_cube","F_2_root","F_2_log","F_2_reciprocal") 
  
  F_2 <- str_c("F_2",Var[1:77],sep = "_x_")
  A = matrix(0 ,ncol = 1,nrow = dim(dataset2)[1])
  for (i in 1:77){
    B<-dataset2["F_2"]*dataset2[(Var[i])]
    A<-cbind(A,B)
  }
  A<-A[2:78]
  colnames(A) <- F_2
  
  F_2_exp <- str_c("F_2_exp",Var[1:78],sep = "_x_")
  aa= matrix(0 ,ncol = 1,nrow = dim(dataset2)[1])
  for (j in 1:78){
    bb<-dataset2["F_2_exp"]*dataset2[(Var[j])]
    aa<-cbind(aa,bb)
  }
  aa<-aa[2:79]
  colnames(aa) <- F_2_exp
  
  F_2_square <- str_c("F_2_square",Var[1:79],sep = "_x_")
  AA= matrix(0 ,ncol = 1,nrow = dim(dataset2)[1])
  for (l in 1:79){
    BB<-dataset2["F_2_square"]*dataset2[(Var[l])]
    AA<-cbind(AA,BB)
  }
  AA<-AA[2:80]
  colnames(AA) <- F_2_square
  
  F_2_cube <- str_c("F_2_cube",Var[1:80],sep = "_x_")
  D= matrix(0 ,ncol = 1,nrow = dim(dataset2)[1])
  for (l in 1:80){
    DD<-dataset2["F_2_cube"]*dataset2[(Var[l])]
    D<-cbind(D,DD)
  }
  D<-D[2:81]
  colnames(D) <- F_2_cube
  
  F_2_root <- str_c("F_2_root",Var[1:81],sep = "_x_")
  c= matrix(0 ,ncol = 1,nrow = dim(dataset2)[1])
  for (l in 1:81){
    d<-dataset2["F_2_root"]*dataset2[(Var[l])]
    c<-cbind(c,d)
  }
  c<-c[2:82]
  colnames(c) <- F_2_root
  
  F_2_log <- str_c("F_2_log",Var[1:82],sep = "_x_")
  C= matrix(0 ,ncol = 1,nrow = dim(dataset2)[1])
  for (v in 1:82){
    B<-dataset2["F_2_log"]*dataset2[(Var[v])]
    C<-cbind(C,B)
  }
  C<-C[2:83]
  colnames(C) <- F_2_log
  
  F_2_reciprocal <- str_c("F_2_reciprocal",Var[1:83],sep = "_x_")
  cc= matrix(0 ,ncol = 1,nrow = dim(dataset2)[1])
  for (m in 1:83){
    dd<-dataset2["F_2_reciprocal"]*dataset2[(Var[m])]
    cc<-cbind(cc,dd)
  }
  cc<-cc[2:84]
  colnames(cc) <- F_2_reciprocal
  
  dataset2 <- cbind(A,aa,AA,D,c,C,cc)
  return(dataset2)
}
continuous1_F2<-Continuous1_F_2(data4)

#F_3對其他變數做交互作用
library(stringr)
Continuous1_F_3 <- function(dataset3){
  Var <- c("F_4","F_5","F_6","F_7","F_8","F_9","F_10","F_11","F_12","F_13",
           "F_4_exp","F_5_exp","F_6_exp","F_7_exp","F_8_exp","F_9_exp","F_10_exp","F_11_exp","F_12_exp","F_13_exp",
           "F_4_square","F_5_square","F_6_square","F_7_square","F_8_square","F_9_square","F_10_square","F_11_square","F_12_square","F_13_square",
           "F_4_cube","F_5_cube","F_6_cube","F_7_cube","F_8_cube","F_9_cube","F_10_cube","F_11_cube","F_12_cube","F_13_cube",
           "F_4_root","F_5_root","F_6_root","F_7_root","F_8_root","F_9_root","F_10_root","F_11_root","F_12_root","F_13_root",
           "F_4_log","F_5_log","F_6_log","F_7_log","F_8_log","F_9_log","F_10_log","F_11_log","F_12_log","F_13_log",
           "F_4_reciprocal","F_5_reciprocal","F_6_reciprocal","F_7_reciprocal","F_8_reciprocal","F_9_reciprocal","F_10_reciprocal","F_11_reciprocal","F_12_reciprocal","F_13_reciprocal",
           "F_3","F_3_exp","F_3_square","F_3_cube","F_3_root","F_3_log","F_3_reciprocal") 
  
  F_3 <- str_c("F_3",Var[1:70],sep = "_x_")
  A = matrix(0 ,ncol = 1,nrow = dim(dataset3)[1])
  for (i in 1:70){
    B<-dataset3["F_3"]*dataset3[(Var[i])]
    A<-cbind(A,B)
  }
  A<-A[2:71]
  colnames(A) <- F_3
  
  F_3_exp <- str_c("F_3_exp",Var[1:71],sep = "_x_")
  aa= matrix(0 ,ncol = 1,nrow = dim(dataset3)[1])
  for (j in 1:71){
    bb<-dataset3["F_3_exp"]*dataset3[(Var[j])]
    aa<-cbind(aa,bb)
  }
  aa<-aa[2:72]
  colnames(aa) <- F_3_exp
  
  F_3_square <- str_c("F_3_square",Var[1:72],sep = "_x_")
  AA= matrix(0 ,ncol = 1,nrow = dim(dataset3)[1])
  for (l in 1:72){
    BB<-dataset3["F_3_square"]*dataset3[(Var[l])]
    AA<-cbind(AA,BB)
  }
  AA<-AA[2:73]
  colnames(AA) <- F_3_square
  
  F_3_cube <- str_c("F_3_cube",Var[1:73],sep = "_x_")
  D= matrix(0 ,ncol = 1,nrow = dim(dataset3)[1])
  for (l in 1:73){
    DD<-dataset3["F_3_cube"]*dataset3[(Var[l])]
    D<-cbind(D,DD)
  }
  D<-D[2:74]
  colnames(D) <- F_3_cube
  
  F_3_root <- str_c("F_3_root",Var[1:74],sep = "_x_")
  c= matrix(0 ,ncol = 1,nrow = dim(dataset3)[1])
  for (l in 1:74){
    d<-dataset3["F_3_root"]*dataset3[(Var[l])]
    c<-cbind(c,d)
  }
  c<-c[2:75]
  colnames(c) <- F_3_root
  
  F_3_log <- str_c("F_3_log",Var[1:75],sep = "_x_")
  C= matrix(0 ,ncol = 1,nrow = dim(dataset3)[1])
  for (v in 1:75){
    B<-dataset3["F_3_log"]*dataset3[(Var[v])]
    C<-cbind(C,B)
  }
  C<-C[2:76]
  colnames(C) <- F_3_log
  
  F_3_reciprocal <- str_c("F_3_reciprocal",Var[1:76],sep = "_x_")
  cc= matrix(0 ,ncol = 1,nrow = dim(dataset3)[1])
  for (m in 1:76){
    dd<-dataset3["F_3_reciprocal"]*dataset3[(Var[m])]
    cc<-cbind(cc,dd)
  }
  cc<-cc[2:77]
  colnames(cc) <- F_3_reciprocal
  
  dataset3 <- cbind(A,aa,AA,D,c,C,cc)
  return(dataset3)
}
continuous1_F3<-Continuous1_F_3(data4)

#F_4對其他變數做交互作用
library(stringr)
Continuous1_F_4 <- function(dataset4){
  Var <- c("F_5","F_6","F_7","F_8","F_9","F_10","F_11","F_12","F_13",
           "F_5_exp","F_6_exp","F_7_exp","F_8_exp","F_9_exp","F_10_exp","F_11_exp","F_12_exp","F_13_exp",
           "F_5_square","F_6_square","F_7_square","F_8_square","F_9_square","F_10_square","F_11_square","F_12_square","F_13_square",
           "F_5_cube","F_6_cube","F_7_cube","F_8_cube","F_9_cube","F_10_cube","F_11_cube","F_12_cube","F_13_cube",
           "F_5_root","F_6_root","F_7_root","F_8_root","F_9_root","F_10_root","F_11_root","F_12_root","F_13_root",
           "F_5_log","F_6_log","F_7_log","F_8_log","F_9_log","F_10_log","F_11_log","F_12_log","F_13_log",
           "F_5_reciprocal","F_6_reciprocal","F_7_reciprocal","F_8_reciprocal","F_9_reciprocal","F_10_reciprocal","F_11_reciprocal","F_12_reciprocal","F_13_reciprocal",
           "F_4","F_4_exp","F_4_square","F_4_cube","F_4_root","F_4_log","F_4_reciprocal") 
  
  F_4 <- str_c("F_4",Var[1:63],sep = "_x_")
  A = matrix(0 ,ncol = 1,nrow = dim(dataset4)[1])
  for (i in 1:63){
    B<-dataset4["F_4"]*dataset4[(Var[i])]
    A<-cbind(A,B)
  }
  A<-A[2:64]
  colnames(A) <- F_4
  
  F_4_exp <- str_c("F_4_exp",Var[1:64],sep = "_x_")
  aa= matrix(0 ,ncol = 1,nrow = dim(dataset4)[1])
  for (j in 1:64){
    bb<-dataset4["F_4_exp"]*dataset4[(Var[j])]
    aa<-cbind(aa,bb)
  }
  aa<-aa[2:65]
  colnames(aa) <- F_4_exp
  
  F_4_square <- str_c("F_4_square",Var[1:65],sep = "_x_")
  AA= matrix(0 ,ncol = 1,nrow = dim(dataset4)[1])
  for (l in 1:65){
    BB<-dataset4["F_4_square"]*dataset4[(Var[l])]
    AA<-cbind(AA,BB)
  }
  AA<-AA[2:66]
  colnames(AA) <- F_4_square
  
  F_4_cube <- str_c("F_4_cube",Var[1:66],sep = "_x_")
  D= matrix(0 ,ncol = 1,nrow = dim(dataset4)[1])
  for (l in 1:66){
    DD<-dataset4["F_4_cube"]*dataset4[(Var[l])]
    D<-cbind(D,DD)
  }
  D<-D[2:67]
  colnames(D) <- F_4_cube
  
  F_4_root <- str_c("F_4_root",Var[1:67],sep = "_x_")
  c= matrix(0 ,ncol = 1,nrow = dim(dataset4)[1])
  for (l in 1:67){
    d<-dataset4["F_4_root"]*dataset4[(Var[l])]
    c<-cbind(c,d)
  }
  c<-c[2:68]
  colnames(c) <- F_4_root
  
  F_4_log <- str_c("F_4_log",Var[1:68],sep = "_x_")
  C= matrix(0 ,ncol = 1,nrow = dim(dataset4)[1])
  for (v in 1:68){
    B<-dataset4["F_4_log"]*dataset4[(Var[v])]
    C<-cbind(C,B)
  }
  C<-C[2:69]
  colnames(C) <- F_4_log
  
  F_4_reciprocal <- str_c("F_4_reciprocal",Var[1:69],sep = "_x_")
  cc= matrix(0 ,ncol = 1,nrow = dim(dataset4)[1])
  for (m in 1:69){
    dd<-dataset4["F_4_reciprocal"]*dataset4[(Var[m])]
    cc<-cbind(cc,dd)
  }
  cc<-cc[2:70]
  colnames(cc) <- F_4_reciprocal
  
  dataset4 <- cbind(A,aa,AA,D,c,C,cc)
  return(dataset4)
}
continuous1_F4<-Continuous1_F_4(data4)

#F_5對其他變數做交互作用
library(stringr)
Continuous1_F_5 <- function(dataset5){
  Var <- c("F_6","F_7","F_8","F_9","F_10","F_11","F_12","F_13",
           "F_6_exp","F_7_exp","F_8_exp","F_9_exp","F_10_exp","F_11_exp","F_12_exp","F_13_exp",
           "F_6_square","F_7_square","F_8_square","F_9_square","F_10_square","F_11_square","F_12_square","F_13_square",
           "F_6_cube","F_7_cube","F_8_cube","F_9_cube","F_10_cube","F_11_cube","F_12_cube","F_13_cube",
           "F_6_root","F_7_root","F_8_root","F_9_root","F_10_root","F_11_root","F_12_root","F_13_root",
           "F_6_log","F_7_log","F_8_log","F_9_log","F_10_log","F_11_log","F_12_log","F_13_log",
           "F_6_reciprocal","F_7_reciprocal","F_8_reciprocal","F_9_reciprocal","F_10_reciprocal","F_11_reciprocal","F_12_reciprocal","F_13_reciprocal",
           "F_5","F_5_exp","F_5_square","F_5_cube","F_5_root","F_5_log","F_5_reciprocal") 
  
  F_5 <- str_c("F_5",Var[1:56],sep = "_x_")
  A = matrix(0 ,ncol = 1,nrow = dim(dataset5)[1])
  for (i in 1:56){
    B<-dataset5["F_5"]*dataset5[(Var[i])]
    A<-cbind(A,B)
  }
  A<-A[2:57]
  colnames(A) <- F_5
  
  F_5_exp <- str_c("F_5_exp",Var[1:57],sep = "_x_")
  aa= matrix(0 ,ncol = 1,nrow = dim(dataset5)[1])
  for (j in 1:57){
    bb<-dataset5["F_5_exp"]*dataset5[(Var[j])]
    aa<-cbind(aa,bb)
  }
  aa<-aa[2:58]
  colnames(aa) <- F_5_exp
  
  F_5_square <- str_c("F_5_square",Var[1:58],sep = "_x_")
  AA= matrix(0 ,ncol = 1,nrow = dim(dataset5)[1])
  for (l in 1:58){
    BB<-dataset5["F_5_square"]*dataset5[(Var[l])]
    AA<-cbind(AA,BB)
  }
  AA<-AA[2:59]
  colnames(AA) <- F_5_square
  
  F_5_cube <- str_c("F_5_cube",Var[1:59],sep = "_x_")
  D= matrix(0 ,ncol = 1,nrow = dim(dataset5)[1])
  for (l in 1:59){
    DD<-dataset5["F_5_cube"]*dataset5[(Var[l])]
    D<-cbind(D,DD)
  }
  D<-D[2:60]
  colnames(D) <- F_5_cube
  
  F_5_root <- str_c("F_5_root",Var[1:60],sep = "_x_")
  c= matrix(0 ,ncol = 1,nrow = dim(dataset5)[1])
  for (l in 1:60){
    d<-dataset5["F_5_root"]*dataset5[(Var[l])]
    c<-cbind(c,d)
  }
  c<-c[2:61]
  colnames(c) <- F_5_root
  
  F_5_log <- str_c("F_5_log",Var[1:61],sep = "_x_")
  C= matrix(0 ,ncol = 1,nrow = dim(dataset5)[1])
  for (v in 1:61){
    B<-dataset5["F_5_log"]*dataset5[(Var[v])]
    C<-cbind(C,B)
  }
  C<-C[2:62]
  colnames(C) <- F_5_log
  
  F_5_reciprocal <- str_c("F_5_reciprocal",Var[1:62],sep = "_x_")
  cc= matrix(0 ,ncol = 1,nrow = dim(dataset5)[1])
  for (m in 1:62){
    dd<-dataset5["F_5_reciprocal"]*dataset5[(Var[m])]
    cc<-cbind(cc,dd)
  }
  cc<-cc[2:63]
  colnames(cc) <- F_5_reciprocal
  
  dataset5 <- cbind(A,aa,AA,D,c,C,cc)
  return(dataset5)
}
continuous1_F5<-Continuous1_F_5(data4)

#F_6對其他變數做交互作用
library(stringr)
Continuous1_F_6 <- function(dataset6){
  Var <- c("F_7","F_8","F_9","F_10","F_11","F_12","F_13",
           "F_7_exp","F_8_exp","F_9_exp","F_10_exp","F_11_exp","F_12_exp","F_13_exp",
           "F_7_square","F_8_square","F_9_square","F_10_square","F_11_square","F_12_square","F_13_square",
           "F_7_cube","F_8_cube","F_9_cube","F_10_cube","F_11_cube","F_12_cube","F_13_cube",
           "F_7_root","F_8_root","F_9_root","F_10_root","F_11_root","F_12_root","F_13_root",
           "F_7_log","F_8_log","F_9_log","F_10_log","F_11_log","F_12_log","F_13_log",
           "F_7_reciprocal","F_8_reciprocal","F_9_reciprocal","F_10_reciprocal","F_11_reciprocal","F_12_reciprocal","F_13_reciprocal",
           "F_6","F_6_exp","F_6_square","F_6_cube","F_6_root","F_6_log","F_6_reciprocal") 
  
  F_6 <- str_c("F_6",Var[1:49],sep = "_x_")
  A = matrix(0 ,ncol = 1,nrow = dim(dataset6)[1])
  for (i in 1:49){
    B<-dataset6["F_6"]*dataset6[(Var[i])]
    A<-cbind(A,B)
  }
  A<-A[2:50]
  colnames(A) <- F_6
  
  F_6_exp <- str_c("F_6_exp",Var[1:50],sep = "_x_")
  aa= matrix(0 ,ncol = 1,nrow = dim(dataset6)[1])
  for (j in 1:50){
    bb<-dataset6["F_6_exp"]*dataset6[(Var[j])]
    aa<-cbind(aa,bb)
  }
  aa<-aa[2:51]
  colnames(aa) <- F_6_exp
  
  F_6_square <- str_c("F_6_square",Var[1:51],sep = "_x_")
  AA= matrix(0 ,ncol = 1,nrow = dim(dataset6)[1])
  for (l in 1:51){
    BB<-dataset6["F_6_square"]*dataset6[(Var[l])]
    AA<-cbind(AA,BB)
  }
  AA<-AA[2:52]
  colnames(AA) <- F_6_square
  
  F_6_cube <- str_c("F_6_cube",Var[1:52],sep = "_x_")
  D= matrix(0 ,ncol = 1,nrow = dim(dataset6)[1])
  for (l in 1:52){
    DD<-dataset6["F_6_cube"]*dataset6[(Var[l])]
    D<-cbind(D,DD)
  }
  D<-D[2:53]
  colnames(D) <- F_6_cube
  
  F_6_root <- str_c("F_6_root",Var[1:53],sep = "_x_")
  c= matrix(0 ,ncol = 1,nrow = dim(dataset6)[1])
  for (l in 1:53){
    d<-dataset6["F_6_root"]*dataset6[(Var[l])]
    c<-cbind(c,d)
  }
  c<-c[2:54]
  colnames(c) <- F_6_root
  
  F_6_log <- str_c("F_6_log",Var[1:54],sep = "_x_")
  C= matrix(0 ,ncol = 1,nrow = dim(dataset6)[1])
  for (v in 1:54){
    B<-dataset6["F_6_log"]*dataset6[(Var[v])]
    C<-cbind(C,B)
  }
  C<-C[2:55]
  colnames(C) <- F_6_log
  
  F_6_reciprocal <- str_c("F_6_reciprocal",Var[1:55],sep = "_x_")
  cc= matrix(0 ,ncol = 1,nrow = dim(dataset6)[1])
  for (m in 1:55){
    dd<-dataset6["F_6_reciprocal"]*dataset6[(Var[m])]
    cc<-cbind(cc,dd)
  }
  cc<-cc[2:56]
  colnames(cc) <- F_6_reciprocal
  
  dataset6 <- cbind(A,aa,AA,D,c,C,cc)
  return(dataset6)
}
continuous1_F6<-Continuous1_F_6(data4)

#F_7對其他變數做交互作用
library(stringr)
Continuous1_F_7 <- function(dataset7){
  Var <- c("F_8","F_9","F_10","F_11","F_12","F_13",
           "F_8_exp","F_9_exp","F_10_exp","F_11_exp","F_12_exp","F_13_exp",
           "F_8_square","F_9_square","F_10_square","F_11_square","F_12_square","F_13_square",
           "F_8_cube","F_9_cube","F_10_cube","F_11_cube","F_12_cube","F_13_cube",
           "F_8_root","F_9_root","F_10_root","F_11_root","F_12_root","F_13_root",
           "F_8_log","F_9_log","F_10_log","F_11_log","F_12_log","F_13_log",
           "F_8_reciprocal","F_9_reciprocal","F_10_reciprocal","F_11_reciprocal","F_12_reciprocal","F_13_reciprocal",
           "F_7","F_7_exp","F_7_square","F_7_cube","F_7_root","F_7_log","F_7_reciprocal") 
  
  F_7 <- str_c("F_7",Var[1:42],sep = "_x_")
  A = matrix(0 ,ncol = 1,nrow = dim(dataset7)[1])
  for (i in 1:42){
    B<-dataset7["F_7"]*dataset7[(Var[i])]
    A<-cbind(A,B)
  }
  A<-A[2:43]
  colnames(A) <- F_7
  
  F_7_exp <- str_c("F_7_exp",Var[1:43],sep = "_x_")
  aa= matrix(0 ,ncol = 1,nrow = dim(dataset7)[1])
  for (j in 1:43){
    bb<-dataset7["F_7_exp"]*dataset7[(Var[j])]
    aa<-cbind(aa,bb)
  }
  aa<-aa[2:44]
  colnames(aa) <- F_7_exp
  
  F_7_square <- str_c("F_7_square",Var[1:44],sep = "_x_")
  AA= matrix(0 ,ncol = 1,nrow = dim(dataset7)[1])
  for (l in 1:44){
    BB<-dataset7["F_7_square"]*dataset7[(Var[l])]
    AA<-cbind(AA,BB)
  }
  AA<-AA[2:45]
  colnames(AA) <- F_7_square
  
  F_7_cube <- str_c("F_7_cube",Var[1:45],sep = "_x_")
  D= matrix(0 ,ncol = 1,nrow = dim(dataset7)[1])
  for (l in 1:45){
    DD<-dataset7["F_7_cube"]*dataset7[(Var[l])]
    D<-cbind(D,DD)
  }
  D<-D[2:46]
  colnames(D) <- F_7_cube
  
  F_7_root <- str_c("F_7_root",Var[1:46],sep = "_x_")
  c= matrix(0 ,ncol = 1,nrow = dim(dataset7)[1])
  for (l in 1:46){
    d<-dataset7["F_7_root"]*dataset7[(Var[l])]
    c<-cbind(c,d)
  }
  c<-c[2:47]
  colnames(c) <- F_7_root
  
  F_7_log <- str_c("F_7_log",Var[1:47],sep = "_x_")
  C= matrix(0 ,ncol = 1,nrow = dim(dataset7)[1])
  for (v in 1:47){
    B<-dataset7["F_7_log"]*dataset7[(Var[v])]
    C<-cbind(C,B)
  }
  C<-C[2:48]
  colnames(C) <- F_7_log
  
  F_7_reciprocal <- str_c("F_7_reciprocal",Var[1:48],sep = "_x_")
  cc= matrix(0 ,ncol = 1,nrow = dim(dataset7)[1])
  for (m in 1:48){
    dd<-dataset7["F_7_reciprocal"]*dataset7[(Var[m])]
    cc<-cbind(cc,dd)
  }
  cc<-cc[2:49]
  colnames(cc) <- F_7_reciprocal
  
  dataset7 <- cbind(A,aa,AA,D,c,C,cc)
  return(dataset7)
}
continuous1_F7<-Continuous1_F_7(data4)

#F_8對其他變數做交互作用
library(stringr)
Continuous1_F_8 <- function(dataset8){
  Var <- c("F_9","F_10","F_11","F_12","F_13",
           "F_9_exp","F_10_exp","F_11_exp","F_12_exp","F_13_exp",
           "F_9_square","F_10_square","F_11_square","F_12_square","F_13_square",
           "F_9_cube","F_10_cube","F_11_cube","F_12_cube","F_13_cube",
           "F_9_root","F_10_root","F_11_root","F_12_root","F_13_root",
           "F_9_log","F_10_log","F_11_log","F_12_log","F_13_log",
           "F_9_reciprocal","F_10_reciprocal","F_11_reciprocal","F_12_reciprocal","F_13_reciprocal",
           "F_8","F_8_exp","F_8_square","F_8_cube","F_8_root","F_8_log","F_8_reciprocal") 
  
  F_8 <- str_c("F_8",Var[1:35],sep = "_x_")
  A = matrix(0 ,ncol = 1,nrow = dim(dataset8)[1])
  for (i in 1:35){
    B<-dataset8["F_8"]*dataset8[(Var[i])]
    A<-cbind(A,B)
  }
  A<-A[2:36]
  colnames(A) <- F_8
  
  F_8_exp <- str_c("F_8_exp",Var[1:36],sep = "_x_")
  aa= matrix(0 ,ncol = 1,nrow = dim(dataset8)[1])
  for (j in 1:36){
    bb<-dataset8["F_8_exp"]*dataset8[(Var[j])]
    aa<-cbind(aa,bb)
  }
  aa<-aa[2:37]
  colnames(aa) <- F_8_exp
  
  F_8_square <- str_c("F_8_square",Var[1:37],sep = "_x_")
  AA= matrix(0 ,ncol = 1,nrow = dim(dataset8)[1])
  for (l in 1:37){
    BB<-dataset8["F_8_square"]*dataset8[(Var[l])]
    AA<-cbind(AA,BB)
  }
  AA<-AA[2:38]
  colnames(AA) <- F_8_square
  
  F_8_cube <- str_c("F_8_cube",Var[1:38],sep = "_x_")
  D= matrix(0 ,ncol = 1,nrow = dim(dataset8)[1])
  for (l in 1:38){
    DD<-dataset8["F_8_cube"]*dataset8[(Var[l])]
    D<-cbind(D,DD)
  }
  D<-D[2:39]
  colnames(D) <- F_8_cube
  
  F_8_root <- str_c("F_8_root",Var[1:39],sep = "_x_")
  c= matrix(0 ,ncol = 1,nrow = dim(dataset8)[1])
  for (l in 1:39){
    d<-dataset8["F_8_root"]*dataset8[(Var[l])]
    c<-cbind(c,d)
  }
  c<-c[2:40]
  colnames(c) <- F_8_root
  
  F_8_log <- str_c("F_8_log",Var[1:40],sep = "_x_")
  C= matrix(0 ,ncol = 1,nrow = dim(dataset8)[1])
  for (v in 1:40){
    B<-dataset8["F_8_log"]*dataset8[(Var[v])]
    C<-cbind(C,B)
  }
  C<-C[2:41]
  colnames(C) <- F_8_log
  
  F_8_reciprocal <- str_c("F_8_reciprocal",Var[1:41],sep = "_x_")
  cc= matrix(0 ,ncol = 1,nrow = dim(dataset8)[1])
  for (m in 1:41){
    dd<-dataset8["F_8_reciprocal"]*dataset8[(Var[m])]
    cc<-cbind(cc,dd)
  }
  cc<-cc[2:42]
  colnames(cc) <- F_8_reciprocal
  
  dataset8 <- cbind(A,aa,AA,D,c,C,cc)
  return(dataset8)
}
continuous1_F8<-Continuous1_F_8(data4)

#F_9對其他變數做交互作用
library(stringr)
Continuous1_F_9 <- function(dataset9){
  Var <- c("F_10","F_11","F_12","F_13",
           "F_10_exp","F_11_exp","F_12_exp","F_13_exp",
           "F_10_square","F_11_square","F_12_square","F_13_square",
           "F_10_cube","F_11_cube","F_12_cube","F_13_cube",
           "F_10_root","F_11_root","F_12_root","F_13_root",
           "F_10_log","F_11_log","F_12_log","F_13_log",
           "F_10_reciprocal","F_11_reciprocal","F_12_reciprocal","F_13_reciprocal",
           "F_9","F_9_exp","F_9_square","F_9_cube","F_9_root","F_9_log","F_9_reciprocal") 
  
  F_9 <- str_c("F_9",Var[1:28],sep = "_x_")
  A = matrix(0 ,ncol = 1,nrow = dim(dataset9)[1])
  for (i in 1:28){
    B<-dataset9["F_9"]*dataset9[(Var[i])]
    A<-cbind(A,B)
  }
  A<-A[2:29]
  colnames(A) <- F_9
  
  F_9_exp <- str_c("F_9_exp",Var[1:29],sep = "_x_")
  aa= matrix(0 ,ncol = 1,nrow = dim(dataset9)[1])
  for (j in 1:29){
    bb<-dataset9["F_9_exp"]*dataset9[(Var[j])]
    aa<-cbind(aa,bb)
  }
  aa<-aa[2:30]
  colnames(aa) <- F_9_exp
  
  F_9_square <- str_c("F_9_square",Var[1:30],sep = "_x_")
  AA= matrix(0 ,ncol = 1,nrow = dim(dataset9)[1])
  for (l in 1:30){
    BB<-dataset9["F_9_square"]*dataset9[(Var[l])]
    AA<-cbind(AA,BB)
  }
  AA<-AA[2:31]
  colnames(AA) <- F_9_square
  
  F_9_cube <- str_c("F_9_cube",Var[1:31],sep = "_x_")
  D= matrix(0 ,ncol = 1,nrow = dim(dataset9)[1])
  for (l in 1:31){
    DD<-dataset9["F_9_cube"]*dataset9[(Var[l])]
    D<-cbind(D,DD)
  }
  D<-D[2:32]
  colnames(D) <- F_9_cube
  
  F_9_root <- str_c("F_9_root",Var[1:32],sep = "_x_")
  c= matrix(0 ,ncol = 1,nrow = dim(dataset9)[1])
  for (l in 1:32){
    d<-dataset9["F_9_root"]*dataset9[(Var[l])]
    c<-cbind(c,d)
  }
  c<-c[2:33]
  colnames(c) <- F_9_root
  
  F_9_log <- str_c("F_9_log",Var[1:33],sep = "_x_")
  C= matrix(0 ,ncol = 1,nrow = dim(dataset9)[1])
  for (v in 1:33){
    B<-dataset9["F_9_log"]*dataset9[(Var[v])]
    C<-cbind(C,B)
  }
  C<-C[2:34]
  colnames(C) <- F_9_log
  
  F_9_reciprocal <- str_c("F_9_reciprocal",Var[1:34],sep = "_x_")
  cc= matrix(0 ,ncol = 1,nrow = dim(dataset9)[1])
  for (m in 1:34){
    dd<-dataset9["F_9_reciprocal"]*dataset9[(Var[m])]
    cc<-cbind(cc,dd)
  }
  cc<-cc[2:35]
  colnames(cc) <- F_9_reciprocal
  
  dataset9 <- cbind(A,aa,AA,D,c,C,cc)
  return(dataset9)
}
continuous1_F9<-Continuous1_F_9(data4)

#F_10對其他變數做交互作用
library(stringr)
Continuous1_F_10 <- function(dataset10){
  Var <- c("F_11","F_12","F_13",
           "F_11_exp","F_12_exp","F_13_exp",
           "F_11_square","F_12_square","F_13_square",
           "F_11_cube","F_12_cube","F_13_cube",
           "F_11_root","F_12_root","F_13_root",
           "F_11_log","F_12_log","F_13_log",
           "F_11_reciprocal","F_12_reciprocal","F_13_reciprocal",
           "F_10","F_10_exp","F_10_square","F_10_cube","F_10_root","F_10_log","F_10_reciprocal") 
  
  F_10 <- str_c("F_10",Var[1:21],sep = "_x_")
  A = matrix(0 ,ncol = 1,nrow = dim(dataset10)[1])
  for (i in 1:21){
    B<-dataset10["F_10"]*dataset10[(Var[i])]
    A<-cbind(A,B)
  }
  A<-A[2:22]
  colnames(A) <- F_10
  
  F_10_exp <- str_c("F_10_exp",Var[1:22],sep = "_x_")
  aa= matrix(0 ,ncol = 1,nrow = dim(dataset10)[1])
  for (j in 1:22){
    bb<-dataset10["F_10_exp"]*dataset10[(Var[j])]
    aa<-cbind(aa,bb)
  }
  aa<-aa[2:23]
  colnames(aa) <- F_10_exp
  
  F_10_square <- str_c("F_10_square",Var[1:23],sep = "_x_")
  AA= matrix(0 ,ncol = 1,nrow = dim(dataset10)[1])
  for (l in 1:23){
    BB<-dataset10["F_10_square"]*dataset10[(Var[l])]
    AA<-cbind(AA,BB)
  }
  AA<-AA[2:24]
  colnames(AA) <- F_10_square
  
  F_10_cube <- str_c("F_10_cube",Var[1:24],sep = "_x_")
  D= matrix(0 ,ncol = 1,nrow = dim(dataset10)[1])
  for (l in 1:24){
    DD<-dataset10["F_10_cube"]*dataset10[(Var[l])]
    D<-cbind(D,DD)
  }
  D<-D[2:25]
  colnames(D) <- F_10_cube
  
  F_10_root <- str_c("F_10_root",Var[1:25],sep = "_x_")
  c= matrix(0 ,ncol = 1,nrow = dim(dataset10)[1])
  for (l in 1:25){
    d<-dataset10["F_10_root"]*dataset10[(Var[l])]
    c<-cbind(c,d)
  }
  c<-c[2:26]
  colnames(c) <- F_10_root
  
  F_10_log <- str_c("F_10_log",Var[1:26],sep = "_x_")
  C= matrix(0 ,ncol = 1,nrow = dim(dataset10)[1])
  for (v in 1:26){
    B<-dataset10["F_10_log"]*dataset10[(Var[v])]
    C<-cbind(C,B)
  }
  C<-C[2:27]
  colnames(C) <- F_10_log
  
  F_10_reciprocal <- str_c("F_10_reciprocal",Var[1:27],sep = "_x_")
  cc= matrix(0 ,ncol = 1,nrow = dim(dataset10)[1])
  for (m in 1:27){
    dd<-dataset10["F_10_reciprocal"]*dataset10[(Var[m])]
    cc<-cbind(cc,dd)
  }
  cc<-cc[2:28]
  colnames(cc) <- F_10_reciprocal
  
  dataset10 <- cbind(A,aa,AA,D,c,C,cc)
  return(dataset10)
}
continuous1_F10<-Continuous1_F_10(data4)

#F_11對其他變數做交互作用
library(stringr)
Continuous1_F_11 <- function(dataset11){
  Var <- c("F_12","F_13",
           "F_12_exp","F_13_exp",
           "F_12_square","F_13_square",
           "F_12_cube","F_13_cube",
           "F_12_root","F_13_root",
           "F_12_log","F_13_log",
           "F_12_reciprocal","F_13_reciprocal",
           "F_11","F_11_exp","F_11_square","F_11_cube","F_11_root","F_11_log","F_11_reciprocal") 
  
  F_11 <- str_c("F_11",Var[1:14],sep = "_x_")
  A = matrix(0 ,ncol = 1,nrow = dim(dataset11)[1])
  for (i in 1:14){
    B<-dataset11["F_11"]*dataset11[(Var[i])]
    A<-cbind(A,B)
  }
  A<-A[2:15]
  colnames(A) <- F_11
  
  F_11_exp <- str_c("F_11_exp",Var[1:15],sep = "_x_")
  aa= matrix(0 ,ncol = 1,nrow = dim(dataset11)[1])
  for (j in 1:15){
    bb<-dataset11["F_11_exp"]*dataset11[(Var[j])]
    aa<-cbind(aa,bb)
  }
  aa<-aa[2:16]
  colnames(aa) <- F_11_exp
  
  F_11_square <- str_c("F_11_square",Var[1:16],sep = "_x_")
  AA= matrix(0 ,ncol = 1,nrow = dim(dataset11)[1])
  for (l in 1:16){
    BB<-dataset11["F_11_square"]*dataset11[(Var[l])]
    AA<-cbind(AA,BB)
  }
  AA<-AA[2:17]
  colnames(AA) <- F_11_square
  
  F_11_cube <- str_c("F_11_cube",Var[1:17],sep = "_x_")
  D= matrix(0 ,ncol = 1,nrow = dim(dataset11)[1])
  for (l in 1:17){
    DD<-dataset11["F_11_cube"]*dataset11[(Var[l])]
    D<-cbind(D,DD)
  }
  D<-D[2:18]
  colnames(D) <- F_11_cube
  
  F_11_root <- str_c("F_11_root",Var[1:18],sep = "_x_")
  c= matrix(0 ,ncol = 1,nrow = dim(dataset11)[1])
  for (l in 1:18){
    d<-dataset11["F_11_root"]*dataset11[(Var[l])]
    c<-cbind(c,d)
  }
  c<-c[2:19]
  colnames(c) <- F_11_root
  
  F_11_log <- str_c("F_11_log",Var[1:19],sep = "_x_")
  C= matrix(0 ,ncol = 1,nrow = dim(dataset11)[1])
  for (v in 1:19){
    B<-dataset11["F_11_log"]*dataset11[(Var[v])]
    C<-cbind(C,B)
  }
  C<-C[2:20]
  colnames(C) <- F_11_log
  
  F_11_reciprocal <- str_c("F_11_reciprocal",Var[1:20],sep = "_x_")
  cc= matrix(0 ,ncol = 1,nrow = dim(dataset11)[1])
  for (m in 1:20){
    dd<-dataset11["F_11_reciprocal"]*dataset11[(Var[m])]
    cc<-cbind(cc,dd)
  }
  cc<-cc[2:21]
  colnames(cc) <- F_11_reciprocal
  
  dataset11 <- cbind(A,aa,AA,D,c,C,cc)
  return(dataset11)
}
continuous1_F11<-Continuous1_F_11(data4)

#F_12對其他變數做交互作用
library(stringr)
Continuous1_F_12 <- function(dataset12){
  Var <- c("F_13","F_13_exp","F_13_square","F_13_cube","F_13_root","F_13_log","F_13_reciprocal",
           "F_12","F_12_exp","F_12_square","F_12_cube","F_12_root","F_12_log","F_12_reciprocal") 
  
  F_12 <- str_c("F_12",Var[1:7],sep = "_x_")
  A = matrix(0 ,ncol = 1,nrow = dim(dataset12)[1])
  for (i in 1:7){
    B<-dataset12["F_12"]*dataset12[(Var[i])]
    A<-cbind(A,B)
  }
  A<-A[2:8]
  colnames(A) <- F_12
  
  F_12_exp <- str_c("F_12_exp",Var[1:8],sep = "_x_")
  aa= matrix(0 ,ncol = 1,nrow = dim(dataset12)[1])
  for (j in 1:8){
    bb<-dataset12["F_12_exp"]*dataset12[(Var[j])]
    aa<-cbind(aa,bb)
  }
  aa<-aa[2:9]
  colnames(aa) <- F_12_exp
  
  F_12_square <- str_c("F_12_square",Var[1:9],sep = "_x_")
  AA= matrix(0 ,ncol = 1,nrow = dim(dataset12)[1])
  for (l in 1:9){
    BB<-dataset12["F_12_square"]*dataset12[(Var[l])]
    AA<-cbind(AA,BB)
  }
  AA<-AA[2:10]
  colnames(AA) <- F_12_square
  
  F_12_cube <- str_c("F_12_cube",Var[1:10],sep = "_x_")
  D= matrix(0 ,ncol = 1,nrow = dim(dataset12)[1])
  for (l in 1:10){
    DD<-dataset12["F_12_cube"]*dataset12[(Var[l])]
    D<-cbind(D,DD)
  }
  D<-D[2:11]
  colnames(D) <- F_12_cube
  
  F_12_root <- str_c("F_12_root",Var[1:11],sep = "_x_")
  c= matrix(0 ,ncol = 1,nrow = dim(dataset12)[1])
  for (l in 1:11){
    d<-dataset12["F_12_root"]*dataset12[(Var[l])]
    c<-cbind(c,d)
  }
  c<-c[2:12]
  colnames(c) <- F_12_root
  
  F_12_log <- str_c("F_12_log",Var[1:12],sep = "_x_")
  C= matrix(0 ,ncol = 1,nrow = dim(dataset12)[1])
  for (v in 1:12){
    B<-dataset12["F_12_log"]*dataset12[(Var[v])]
    C<-cbind(C,B)
  }
  C<-C[2:13]
  colnames(C) <- F_12_log
  
  F_12_reciprocal <- str_c("F_12_reciprocal",Var[1:13],sep = "_x_")
  cc= matrix(0 ,ncol = 1,nrow = dim(dataset12)[1])
  for (m in 1:13){
    dd<-dataset12["F_12_reciprocal"]*dataset12[(Var[m])]
    cc<-cbind(cc,dd)
  }
  cc<-cc[2:14]
  colnames(cc) <- F_12_reciprocal
  
  dataset12 <- cbind(A,aa,AA,D,c,C,cc)
  return(dataset12)
}
continuous1_F12<-Continuous1_F_12(data4)

#F_13對其他變數做交互作用
library(stringr)
Continuous1_F_13 <- function(dataset13){
  Var <- c("F_13","F_13_exp","F_13_square","F_13_cube","F_13_root","F_13_log","F_13_reciprocal")
  
  F_13_exp <- str_c("F_13_exp",Var[1],sep = "_x_")
  aa= matrix(0 ,ncol = 1,nrow = dim(dataset13)[1])
  bb<-dataset13["F_13_exp"]*dataset13[(Var[1])]
  aa<-cbind(bb,aa)
  colnames(aa) <- F_13_exp
  
  F_13_square <- str_c("F_13_square",Var[1:2],sep = "_x_")
  AA= matrix(0 ,ncol = 1,nrow = dim(dataset13)[1])
  for (l in 1:2){
    BB<-dataset13["F_13_square"]*dataset13[(Var[l])]
    AA<-cbind(AA,BB)
  }
  AA<-AA[2:3]
  colnames(AA) <- F_13_square
  
  F_13_cube <- str_c("F_13_cube",Var[1:3],sep = "_x_")
  D= matrix(0 ,ncol = 1,nrow = dim(dataset13)[1])
  for (l in 1:3){
    DD<-dataset13["F_13_cube"]*dataset13[(Var[l])]
    D<-cbind(D,DD)
  }
  D<-D[2:4]
  colnames(D) <- F_13_cube
  
  F_13_root <- str_c("F_13_root",Var[1:4],sep = "_x_")
  c= matrix(0 ,ncol = 1,nrow = dim(dataset13)[1])
  for (l in 1:4){
    d<-dataset13["F_13_root"]*dataset13[(Var[l])]
    c<-cbind(c,d)
  }
  c<-c[2:5]
  colnames(c) <- F_13_root
  
  F_13_log <- str_c("F_13_log",Var[1:5],sep = "_x_")
  C= matrix(0 ,ncol = 1,nrow = dim(dataset13)[1])
  for (v in 1:5){
    B<-dataset13["F_13_log"]*dataset13[(Var[v])]
    C<-cbind(C,B)
  }
  C<-C[2:6]
  colnames(C) <- F_13_log
  
  F_13_reciprocal <- str_c("F_13_reciprocal",Var[1:6],sep = "_x_")
  cc= matrix(0 ,ncol = 1,nrow = dim(dataset13)[1])
  for (m in 1:6){
    dd<-dataset13["F_13_reciprocal"]*dataset13[(Var[m])]
    cc<-cbind(cc,dd)
  }
  cc<-cc[2:7]
  colnames(cc) <- F_13_reciprocal
  
  dataset13 <- cbind(aa,AA,D,c,C,cc)
  return(dataset13)
}
continuous1_F13<-Continuous1_F_13(data4)
continuous1_F13<-continuous1_F13[,-2]

data5<-cbind(data4,continuous1_F1,continuous1_F2,continuous1_F3,continuous1_F4,
             continuous1_F5,continuous1_F6,continuous1_F7,continuous1_F8,continuous1_F9,
             continuous1_F10,continuous1_F11,continuous1_F12,continuous1_F13)
dim(data5)#將全部的特工工程新增出來的資料跟原始測試資料做合併
Test_data <- data5
#--------------預測測試資料的O值-------------#
lm.lasso.predict <- predict(model,data5[, c(select.varialbes)])
lm.lasso.predict
head(model1)

library(useful)
Formula <- O ~. -1
Train_X <- build.x(Formula, data = Train_data)
Train_y <- build.y(Formula, data = Train_data)
install.packages("xgboost")
library(xgboost)
xgb <- xgboost(data = Train_X,
               label = Train_y,
               max.depth = 3,
               eta = 0.1,
               nthread = 4,
               nrounds = 20,
               objective = "binary:logistic",
               eval_metric = "logloss")
