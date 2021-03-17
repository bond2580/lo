#�v���ӊw�������W�X�e�B�b�N���f����p���ĕ��ނ���v���O����

library(logistf)

t = 0.018 #���ނ�臒l(0.018 ~ 0.50)

######�f�[�^�̑O����######################
df <- read.csv("C:/Users/taich/OneDrive/�f�X�N�g�b�v/������/data/����/ryunen.csv")#�f�[�^�ǂݍ���
ryunen <- df["���N����"]


for(i in 8:15){  #���q�f�[�^�𐮐��^�ɕϊ�
  df[,i] <- as.integer(df[,i])
  } 
df2 <- df[, -16]
pc <- prcomp(df2, scale=T) #�听������
pc_score <- pc$x[,1:6]

##########################################

#####���W�X�e�B�b�N���ރ��f���̍\�z(firth�̕��@��p���邽�߂�logistf()���g�p)##############

#���f�[�^�ŕ��ރ��f�����\�z
lo1 <- logistf(df[,16]~df[,1]+df[,2]+df[,3]+df[,4]+df[,5]+df[,6]+df[,7]
                        +df[,8]+df[,9]+df[,10]+df[,11]+df[,12]+df[,13]+df[,14]+df[,15], data=df, family=binomial(), firth=T)
#lo1���X�e�b�v���C�Y�@�ŕϐ��I��
lo1_step <- logistf(df[,16]~df[,1]+df[,5]+df[,7]+df[,10], data=df, family=binomial(), firth=T)

#�听�����_�ŕ��ރ��f�����\�z
lo2 <- logistf(df[,16]~pc_score[,1]+pc_score[,2]+pc_score[,3]+pc_score[,4]+pc_score[,5]+pc_score[,6], family=binomial(), firth=T)
#lo2���X�e�b�v���C�Y�@�ŕϐ��I��
lo2_step <- logistf(df[,16]~pc_score[,3]+pc_score[,4]+pc_score[,6], family=binomial(), firth=T)

#############################################################################################


##########���ނƐ��x�̑���#################


lo1_predict <- lf_p(lo1, ryunen, t)#����
lo1_step_predict <- lf_p(lo1_step, ryunen, t)
lo2_predict <- lf_p(lo2, ryunen, t)
lo2_predict <- lf_p(lo2_step, ryunen, t)

ryunen <- t(ryunen)

lo1_table <- table(lo1_predict, ryunen)#�����s��̍쐬
lo1_accuracy <- (lo1_table[1,1] + lo1_table[2,2]) / length(ryunen)#����
lo1_saigen <- lo1_table[1,1] / (lo1_table[1,1] + lo1_table[1,2])#�Č���
lo1_tekigou <- lo1_table[1,1] / (lo1_table[1,1] + lo1_table[2,1]) #�K����
lo1_fscore <- (2 * lo1_table[1,1]) / (2 * lo1_table[1,1] + lo1_table[1,2] + lo1_table[2,1])#F�l


lo1_value <- c("����"=lo1_accuracy, "�Č���"=lo1_saigen, "�K����"=lo1_tekigou, "F�l"=lo1_fscore)#�S�̒l���i�[


lo1_table <- table(lo1_step_predict, ryunen)
lo1_step_accuracy <- (lo1_table[1,1] + lo1_table[2,2]) / length(ryunen)
lo1_step_saigen <- lo1_table[1,1] / (lo1_table[1,1] + lo1_table[1,2])
lo1_step_tekigou <- lo1_table[1,1] / (lo1_table[1,1] + lo1_table[2,1])
lo1_step_fscore <- (2 * lo1_table[1,1]) / (2 * lo1_table[1,1] + lo1_table[1,2] + lo1_table[2,1]) 

lo1_step_value <- c("����"=lo1_step_accuracy, "�Č���"=lo1_step_saigen, "�K����"=lo1_step_tekigou, "F�l"=lo1_step_fscore)

  
lo2_table <- table(lo2_predict, ryunen)
lo2_accuracy <- (lo2_table[1,1] + lo2_table[2,2]) / length(ryunen)
lo2_saigen <- lo2_table[1,1] / (lo2_table[1,1] + lo2_table[1,2])
lo2_tekigou <- lo2_table[1,1] / (lo2_table[1,1] + lo2_table[2,1])
lo2_fscore <- (2 * lo2_table[1,1]) / (2 * lo2_table[1,1] + lo2_table[1,2] + lo2_table[2,1])

lo2_value <- c("����"=lo2_accuracy, "�Č���"=lo2_saigen, "�K����"=lo2_tekigou, "F�l"=lo2_fscore)



lo2_table <- table(lo2_predict, ryunen)
lo2_step_accuracy <- (lo2_table[1,1] + lo2_table[2,2]) / length(ryunen)
lo2_step_saigen <- lo2_table[1,1] / (lo2_table[1,1] + lo2_table[1,2])
lo2_step_tekigou <- lo2_table[1,1] / (lo2_table[1,1] + lo2_table[2,1])
lo2_step_fscore <- (2 * lo2_table[1,1]) / (2 * lo2_table[1,1] + lo2_table[1,2] + lo2_table[2,1])

lo2_step_value <- c("����"=lo2_step_accuracy, "�Č���"=lo2_step_saigen, "�K����"=lo2_step_tekigou, "F�l"=lo2_step_fscore)


###########################################