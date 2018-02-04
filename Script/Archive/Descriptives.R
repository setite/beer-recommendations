library(foreign)
#setwd("N:/Vivitrol_DOC/Suzanne/Vivitrol/Umit")
Base<-read.spss("NOMs_Baseline(10-19-17).sav",  use.value.labels = FALSE, to.data.frame = TRUE, reencode = NA)
FU6<-read.spss("NOMs_FU_6(10-19-17).sav",  use.value.labels = FALSE, to.data.frame = TRUE, reencode = NA)
FU12<-read.spss("NOMs_FU_12(10-19-17).sav",  use.value.labels = FALSE, to.data.frame = TRUE, reencode = NA)
Baseslt<-Base[,c("ID","Site","ParticipantType","Mean_V_NOMs_B916","Mean_V_NOMs_B1722")]
FU6slt<-FU6[,c("ID","Site","ServicesReceived_6","Mean_V_NOMs_FU6_916","Mean_V_NOMs_FU6_1722","Mean_V_NOMs_FU6_6280","Mean_V_NOMs_FU6_6274")]
FU12slt<-FU12[,c("ID","Site","ServicesReceived_12","Mean_V_NOMs_FU12_916","Mean_V_NOMs_FU12_1722","Mean_V_NOMs_FU12_6280","Mean_V_NOMs_FU12_6274")]
colnames(FU6slt)[2]<-"Site_6"
colnames(FU12slt)[2]<-"Site_12"
#colnames(FU6slt)[1]<-"ID_6"
#colnames(FU12slt)[1]<-"ID_12"
Bas.FU6 <-merge(Baseslt, FU6slt, by.x = "ID")
Bas.FU612<-merge(Bas.FU6, FU12slt, by.y = "ID")

#write.csv(Bas.FU612,"N:/Vivitrol_DOC/Suzanne/Vivitrol/Umit/Satisdata2.csv")

sink(file = "sink/desc_test.txt")
library(readr)

sapply(Base, mean, na.rm=T)
write_csvy(sapply(Base, mean, na.rm=T), "sink/sapply_test.csv")
write.csv(sapply(Base, mean, na.rm=T), file = "sink/sapply_test2.csv")

sink()
