#要注意学生をロジスティックモデルを用いて分類するプログラム

library(logistf)

t = 0.018 #分類の閾値(0.018 ~ 0.50)

######データの前処理######################
df <- read.csv("C:/Users/taich/OneDrive/デスクトップ/研究室/data/卒研/ryunen.csv")#データ読み込み
ryunen <- df["留年判定"]


for(i in 8:15){  #因子データを整数型に変換
  df[,i] <- as.integer(df[,i])
  } 
df2 <- df[, -16]
pc <- prcomp(df2, scale=T) #主成分分析
pc_score <- pc$x[,1:6]

##########################################

#####ロジスティック分類モデルの構築(firthの方法を用いるためにlogistf()を使用)##############

#実データで分類モデルを構築
lo1 <- logistf(df[,16]~df[,1]+df[,2]+df[,3]+df[,4]+df[,5]+df[,6]+df[,7]
                        +df[,8]+df[,9]+df[,10]+df[,11]+df[,12]+df[,13]+df[,14]+df[,15], data=df, family=binomial(), firth=T)
#lo1をステップワイズ法で変数選択
lo1_step <- logistf(df[,16]~df[,1]+df[,5]+df[,7]+df[,10], data=df, family=binomial(), firth=T)

#主成分得点で分類モデルを構築
lo2 <- logistf(df[,16]~pc_score[,1]+pc_score[,2]+pc_score[,3]+pc_score[,4]+pc_score[,5]+pc_score[,6], family=binomial(), firth=T)
#lo2をステップワイズ法で変数選択
lo2_step <- logistf(df[,16]~pc_score[,3]+pc_score[,4]+pc_score[,6], family=binomial(), firth=T)

#############################################################################################


##########分類と精度の測定#################


lo1_predict <- lf_p(lo1, ryunen, t)#分類
lo1_step_predict <- lf_p(lo1_step, ryunen, t)
lo2_predict <- lf_p(lo2, ryunen, t)
lo2_predict <- lf_p(lo2_step, ryunen, t)

ryunen <- t(ryunen)

lo1_table <- table(lo1_predict, ryunen)#混同行列の作成
lo1_accuracy <- (lo1_table[1,1] + lo1_table[2,2]) / length(ryunen)#正解率
lo1_saigen <- lo1_table[1,1] / (lo1_table[1,1] + lo1_table[1,2])#再現率
lo1_tekigou <- lo1_table[1,1] / (lo1_table[1,1] + lo1_table[2,1]) #適合率
lo1_fscore <- (2 * lo1_table[1,1]) / (2 * lo1_table[1,1] + lo1_table[1,2] + lo1_table[2,1])#F値


lo1_value <- c("正解率"=lo1_accuracy, "再現率"=lo1_saigen, "適合率"=lo1_tekigou, "F値"=lo1_fscore)#４つの値を格納


lo1_table <- table(lo1_step_predict, ryunen)
lo1_step_accuracy <- (lo1_table[1,1] + lo1_table[2,2]) / length(ryunen)
lo1_step_saigen <- lo1_table[1,1] / (lo1_table[1,1] + lo1_table[1,2])
lo1_step_tekigou <- lo1_table[1,1] / (lo1_table[1,1] + lo1_table[2,1])
lo1_step_fscore <- (2 * lo1_table[1,1]) / (2 * lo1_table[1,1] + lo1_table[1,2] + lo1_table[2,1]) 

lo1_step_value <- c("正解率"=lo1_step_accuracy, "再現率"=lo1_step_saigen, "適合率"=lo1_step_tekigou, "F値"=lo1_step_fscore)

  
lo2_table <- table(lo2_predict, ryunen)
lo2_accuracy <- (lo2_table[1,1] + lo2_table[2,2]) / length(ryunen)
lo2_saigen <- lo2_table[1,1] / (lo2_table[1,1] + lo2_table[1,2])
lo2_tekigou <- lo2_table[1,1] / (lo2_table[1,1] + lo2_table[2,1])
lo2_fscore <- (2 * lo2_table[1,1]) / (2 * lo2_table[1,1] + lo2_table[1,2] + lo2_table[2,1])

lo2_value <- c("正解率"=lo2_accuracy, "再現率"=lo2_saigen, "適合率"=lo2_tekigou, "F値"=lo2_fscore)



lo2_table <- table(lo2_predict, ryunen)
lo2_step_accuracy <- (lo2_table[1,1] + lo2_table[2,2]) / length(ryunen)
lo2_step_saigen <- lo2_table[1,1] / (lo2_table[1,1] + lo2_table[1,2])
lo2_step_tekigou <- lo2_table[1,1] / (lo2_table[1,1] + lo2_table[2,1])
lo2_step_fscore <- (2 * lo2_table[1,1]) / (2 * lo2_table[1,1] + lo2_table[1,2] + lo2_table[2,1])

lo2_step_value <- c("正解率"=lo2_step_accuracy, "再現率"=lo2_step_saigen, "適合率"=lo2_step_tekigou, "F値"=lo2_step_fscore)


###########################################