#### Data Analysis:Local Fungal fungi Isolates against FAW and non target insect from Burkina Faso
Lepidopterian_Mortality_Def <- read.csv("~/Desktop/Lepidopterian_Mortality_Def.csv")
View(Lepidopterian_Mortality_Def)
is_dat<-Lepidopterian_Mortality_Def
View(is_dat)
str(is_dat)
unique(is_dat$Days.post.infection)
levels(is_dat$Replicates)
#Manipulations and Analysis
library(reshape2)
#melt(Data set, column names to keep, column names to restructure)
View(is_dat)
colnames(is_dat)
library(plyr)
colnames(is_dat)
is_dat_Surv<-is_dat[c(1,2,3,4,5,7)]
View(is_dat_Surv)
library(plyr)
colnames(is_dat_Surv)
dat1=ddply(is_dat_Surv, .(Replicates,Concentration,Treatment,Insect.Species), transform, nval=sum(Mortality, na.rm=TRUE))
View(dat1)
df2=ddply(dat1, .(Replicates,Concentration,Treatment,Insect.Species), transform, Dead=cumsum(Mortality))
View(df2)
df2$Survival=(1-df2$Dead/df2$nval)
View(df2)
colnames(df2)[3]="Days"
df3=subset(df2, Days!="Alives")
df3$Days=as.numeric(as.character(df3$Days))
View(df3)
df4=ddply(df3, .(Days,Concentration,Treatment,Insect.Species), summarize, mean=mean(Survival), replicates=length(Survival), se=sd(Survival)/sqrt(length(Survival)))
View(df4)
## Survival over 14 days
T_Survival=subset(df4, Days=="14")
View(T_Survival)
T_Survival$survival_percentage <- (T_Survival$mean) * 100
T_Survival$survival_percentage_se <- (T_Survival$se) * 100
View(T_Survival)
colnames(T_Survival)
TDf_Survival<-T_Survival[c(1,2,3,4,6,8,9)]
View(TDf_Survival)
Colnames(TDf_Survival)
colnames (TDf_Survival)
colnames(TDf_Survival)[1]="Days post fungal infection"
colnames(TDf_Survival)[5]="Biological Replicates"
colnames(TDf_Survival)[6]="Mean survival (%)"
colnames(TDf_Survival)[7]="SE Survival (%)"
View(TDf_Survival)
library(ggplot2)
library(scales)
library(MASS)
limits=aes(ymax=mean+se, ymin=mean-se)
theme = theme_bw()+theme(text = element_text(size=20),axis.title.x = element_text(size=20,hjust=1, vjust=.5,),axis.text.y = element_text(size=25,), title = element_text(size=20), legend.title = element_text(size=25), legend.text = element_text(size=20))
unique(df4$Treatment)
cbPalette <-c("#4484ff","#ff226e","#000000","#03ec13","#d48686")
Plt=ggplot(df4, aes(Days, mean, color=Treatment))+geom_line(size=1.5)+geom_errorbar(limits, width=.2, size=1.5)+theme+scale_colour_manual(values=cbPalette)+ylab("Mean Survival")+scale_y_continuous(labels=percent)+facet_wrap(Insect.Species~Concentration)
#Displays the plot
Plt
## Food consumption Analysis ###
View(is_dat)
colnames(is_dat)
is_Consum<-is_dat[c(1,2,3,4,6,7)]
View(is_Consum)
library(plyr)
colnames(is_Consum)
W_dat1=ddply(is_Consum, .(Replicates,Concentration,Treatment,Wastes), transform, nval=sum(Wastes, na.rm=TRUE))
View(W_dat1)
W_df2=ddply(W_dat1, .(Replicates,Concentration,Treatment,Wastes), transform, Wast_Cum=cumsum(Wastes))
View(W_df2)
T_Cum=subset(W_df2, Days.post.infection=="14")
View(T_Cum)
colnames(T_Cum)
T_Cum_df3=ddply(T_Cum, .(Concentration,Treatment,Insect.Species), summarize, mean=mean(Wast_Cum), replicates=length(Wast_Cum), se=sd(Wast_Cum)/sqrt(length(Wast_Cum)))
View(T_Cum_df3)
library(ggplot2)
library(scales)
library(MASS)
limits=aes(ymax=mean+se, ymin=mean-se)
theme = theme_bw()+theme(text = element_text(size=20),axis.title.x = element_text(size=20,hjust=1, vjust=.5,), axis.text.x = element_text(size=12,hjust=0,angle = 90,vjust=0),axis.text.y = element_text(size=20,), title = element_text(size=20), legend.title = element_text(size=25), legend.text = element_text(size=20))
colnames(T_Cum_df3)[2]="Treatments"
cbPalette <-c("#4484ff","#ff226e","#000000","#03ec13","#d48686")
# Fix and add necessary arguments to geom_bar and geom_errorbar
W_Plt = ggplot(T_Cum_df3, aes(x = Treatments, y = mean, fill = Treatments)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") + 
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2, size = 1.5) + 
  scale_fill_manual(values = cbPalette) + # define custom color palette
  ylab("Mean wastes produced (g)") + facet_wrap(Insect.Species ~ Concentration) + theme 
# Display the plot
W_Plt


