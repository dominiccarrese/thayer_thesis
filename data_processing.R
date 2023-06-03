#install.packages("stringr")
# install.packages("ggplot2")
# install.packages("ggpattern")
# install.packages("brunnermunzel")

library(stringr)
library(ggplot2)
library(ggpattern)
library(brunnermunzel)
library(boot)

data<- read.csv("~/Downloads/wdat8.csv")
# data$Name<-NULL
# write.csv(data, file="anon_data")

#loop through columns
for (i in 1:ncol(data)) {
  
  #loop through rows
  for (j in 1:nrow(data)) {
      col<-data[,i]
      num<-0
      #skip NAs
      if (is.na(col[j])){}
      
      #these if statements map likert scale words to numbers
      else if( stringr::str_detect(col[j],"^sa$") ){
        data[j,i]<-as.integer(5)
        num<-1
      }
      else if( stringr::str_detect(col[j],"^a$") ){
        data[j,i]<-as.integer(4)
        num<-1
      }
      else if( stringr::str_detect(col[j], "^n$") ){
        data[j,i]<-as.integer(3)
        num<-1
      }
      else if( stringr::str_detect(col[j], "^d$") ){
        data[j,i]<-as.integer(2)
        num<-1
      }
      else if( stringr::str_detect(col[j], "^sd$") ){
        data[j,i]<-as.integer(1)
        num<-1
      }
      else if( stringr::str_detect(col[j], "^a/n$") ){
        data[j,i]<-as.integer(3.5)
        num<-1
      }
      
  }
  
  if (num==1){
    data[i]<-lapply(data[i], as.numeric)
  }
    
  #if the column should map "sa" to 5 and "sd" to 1, flip mapping
  if( stringr::str_detect(names(data)[i], "A2$|A3$|A4$") ){
    data[,i]<-data[,i]*-1+6
    }
}

#select workshop group and construct pre and post scores
workshop<-data[data$Workshop>0,]
prework<-rowSums(workshop[,4:31])
postwork<-rowSums(workshop[,32:59])
deltawork<-postwork-prework

#select control group and construct pre and post scores
control<-data[data$Workshop==0,]
precontrol<-rowSums(control[,4:31])
postcontrol<-rowSums(control[,32:59])
deltacontrol<-postcontrol-precontrol

#alternative wdata, just from likert scores
likert_questions<-stringr::str_detect(names(data), "1$|2$|3$|4$|5$")
likert_control<-control[,likert_questions]
likert_precontrol<-rowSums(likert_control[1:14])
likert_postcontrol<-rowSums(likert_control[15:28])
likert_deltacontrol<-likert_postcontrol-likert_precontrol

likert_work<-workshop[,likert_questions]
likert_prework<-rowSums(likert_work[1:14])
likert_postwork<-rowSums(likert_work[15:28])
likert_deltawork<-likert_postwork-likert_prework


#isolate random variables A, K, E
A_control<-control[,stringr::str_detect(names(data), "PrA|PoA")]
A_precontrol<-rowSums(A_control[,1:9])
A_postcontrol<-rowSums(A_control[,10:ncol(A_control)])

A_work<-workshop[,stringr::str_detect(names(data), "PrA|PoA")]
A_prework<-rowSums(A_work[,1:9])
A_postwork<-rowSums(A_work[,10:ncol(A_work)])

K_control<-control[,stringr::str_detect(names(data), "PrK|PoK")]
K_precontrol<-rowSums(K_control[,1:9])
K_postcontrol<-rowSums(K_control[,10:ncol(K_control)])

K_work<-workshop[,stringr::str_detect(names(data), "PrK|PoK")]
K_prework<-rowSums(K_work[,1:9])
K_postwork<-rowSums(K_work[,10:ncol(K_work)])

E_control<-control[,stringr::str_detect(names(data), "PrE|PoE")]
E_precontrol<-rowSums(E_control[,1:10])
E_postcontrol<-rowSums(E_control[,11:ncol(E_control)])

E_work<-workshop[,stringr::str_detect(names(data), "PrE|PoE")]
E_prework<-rowSums(E_work[,1:10])
E_postwork<-rowSums(E_work[,11:ncol(E_work)])

summary_data<-data.frame(row.names = c(control$Name, workshop$Name),
                         Participant = c(1:32),
                         Workshop=c(control$Workshop, workshop$Workshop),
                         Delta=c(deltacontrol,deltawork),
                         Delta_Likert=c(likert_postcontrol-likert_precontrol, likert_postwork-likert_prework),
                         A_pre=c(A_precontrol, A_prework),
                         A_post=c(A_postcontrol, A_postwork),
                         K_pre=c(K_precontrol, K_prework),
                         K_post=c(K_postcontrol, K_postwork),
                         E_pre=c(E_precontrol, E_prework),
                         E_post=c(E_postcontrol, E_postwork), 
                         Likert_pre=c(likert_precontrol,likert_prework),
                         Likert_post=c(likert_postcontrol,likert_postwork))
summary_data$Pre_total<-summary_data$A_pre+summary_data$K_pre+summary_data$E_pre
summary_data$Post_total<-summary_data$A_post+summary_data$K_post+summary_data$E_post
summary_data$A_delta<-summary_data$A_post-summary_data$A_pre
summary_data$K_delta<-summary_data$K_post-summary_data$K_pre
summary_data$E_delta<-summary_data$E_post-summary_data$E_pre


short_summary_data<-data.frame(row.names = c("control", "workshop"),
                               Mean_Delta=c(mean(summary_data$Delta[summary_data$Workshop==0]), mean(summary_data$Delta[summary_data$Workshop>0])),
                               Median_Delta=c(median(summary_data$Delta[summary_data$Workshop==0]), median(summary_data$Delta[summary_data$Workshop>0])),
                               Mean_Likert_Delta=c(mean(summary_data$Delta_Likert[summary_data$Workshop==0]),mean(summary_data$Delta_Likert[summary_data$Workshop>0])),
                               Median_Likert_Delta=c(median(summary_data$Delta_Likert[summary_data$Workshop==0]),median(summary_data$Delta_Likert[summary_data$Workshop>0])),
                               Mean_A_Pre=c(mean(summary_data$A_pre[summary_data$Workshop==0]),mean(summary_data$A_pre[summary_data$Workshop>0])),
                               Mean_A_Post=c(mean(summary_data$A_post[summary_data$Workshop==0]),mean(summary_data$A_post[summary_data$Workshop>0])),
                               Mean_K_Pre=c(mean(summary_data$K_pre[summary_data$Workshop==0]),mean(summary_data$K_pre[summary_data$Workshop>0])),
                               Mean_K_Post=c(mean(summary_data$K_post[summary_data$Workshop==0]),mean(summary_data$K_post[summary_data$Workshop>0])),
                               Mean_E_Pre=c(mean(summary_data$A_pre[summary_data$Workshop==0]),mean(summary_data$E_pre[summary_data$Workshop>0])),
                               Mean_E_Post=c(mean(summary_data$A_post[summary_data$Workshop==0]),mean(summary_data$E_post[summary_data$Workshop>0]))
                               )

init_highperformers<-summary_data[summary_data$A_pre>22,]

outliers_data<-data.frame(row.names = c("control high outlier", "control low outlier", "workshop high outlier", "workshop low outlier"),
                          Delta=c(summary_data$Delta[summary_data$Participant==1], summary_data$Delta[summary_data$Participant==11], 
                                  summary_data$Delta[summary_data$Participant==18], summary_data$Delta[summary_data$Participant==21]),
                          Likert_Delta=c(summary_data$Delta_Likert[summary_data$Participant==1], summary_data$Delta_Likert[summary_data$Participant==11], 
                                         summary_data$Delta_Likert[summary_data$Participant==18], summary_data$Delta_Likert[summary_data$Participant==21]),
                          A_Pre=c(summary_data$A_pre[summary_data$Participant==1], summary_data$A_pre[summary_data$Participant==11], 
                                       summary_data$A_pre[summary_data$Participant==18], summary_data$A_pre[summary_data$Participant==21]),
                          A_Post=c(summary_data$A_post[summary_data$Participant==1], summary_data$A_post[summary_data$Participant==11], 
                                   summary_data$A_post[summary_data$Participant==18], summary_data$A_post[summary_data$Participant==21]),
                          K_Pre=c(summary_data$K_pre[summary_data$Participant==1], summary_data$K_pre[summary_data$Participant==11], 
                                       summary_data$K_pre[summary_data$Participant==18], summary_data$K_pre[summary_data$Participant==21]),
                          K_Post=c(summary_data$K_post[summary_data$Participant==1], summary_data$K_post[summary_data$Participant==11], 
                                   summary_data$K_post[summary_data$Participant==18], summary_data$K_post[summary_data$Participant==21]),
                          E_Pre=c(summary_data$E_pre[summary_data$Participant==1], summary_data$E_pre[summary_data$Participant==11], 
                                  summary_data$E_pre[summary_data$Participant==18], summary_data$E_pre[summary_data$Participant==21]),
                          E_Post=c(summary_data$E_post[summary_data$Participant==1], summary_data$E_post[summary_data$Participant==11], 
                                   summary_data$E_post[summary_data$Participant==18], summary_data$E_post[summary_data$Participant==21])
                          )

sum_table <- data.frame(t(short_summary_data))
outlier_table <- data.frame(t(outliers_data))

write.csv2(sum_table, file="summary_table")
write.csv2(outlier_table, file="outlier_table")


#figures: overlapping histograms and boxplot
dc<-data.frame(deltacontrol)
dc$group<-"control"
dw<-data.frame(deltawork)
dw$group<-"workshop"
colnames(dc)[1]<-"delta"
colnames(dw)[1]<-"delta"

png("summ_histogram.png", units="in", width=3.5, height=3, res=300)
ggplot( rbind(dc, dw), aes(x=delta, fill=group, pattern=group) ) + 
  geom_histogram_pattern(data = dc, alpha = 0.4, binwidth = 6,
                         colour = 'black', 
                         ) + 
  geom_histogram_pattern(data = dw, alpha = 0.7, binwidth = 6,
                         colour = 'black', 
                        ) +
  scale_fill_grey()+
  guides(colour = guide_legend(override.aes = c(alpha = .1, alpha=.2)))+
  theme(legend.position=c(.8,.8), legend.key.size = unit(.3,"inch"))+
  labs(x="Change in Survey Score (Post-Pre)", y = "Count")
dev.off()

png("summ_boxplot.png", units="in", width=3.5, height=3, res=300)
ggplot( rbind(dc, dw), aes(x=group, y=delta, color=group), fill=group, color=group) + 
  geom_boxplot(data = dc) + 
  geom_boxplot(data = dw) +
  scale_color_manual(values=c("black", "black")) +
  labs(y = "Change in Survey Score (Post-Pre)", x="Group") +
  theme(legend.position = "none")
dev.off()

#histograms, boxplots, and variance
hist(deltacontrol)
hist(deltawork)

boxplot(deltacontrol)
boxplot(deltawork)

hist(prework)
hist(postwork)
range(deltacontrol)
range(deltawork)

hist

hist(likert_deltacontrol)
hist(likert_deltawork)
range(likert_deltacontrol)
range(likert_deltawork)


##HYPOTHESIS TESTING
# for Brunner Munzel test, adding the argument est="difference" produces a
# different sample estimate, confidence interval, and way to interpret the test.
# we go with the "original" P(X<Y)+.5*P(X=Y) estimate

#hypothesis 1. Are postworkshop scores higher than preworkshop scores?
brunnermunzel.test(prework, postwork)
brunnermunzel.test(A_prework, A_postwork)
brunnermunzel.test(K_prework, K_postwork)
brunnermunzel.test(E_prework, E_postwork)

#simulation of situation where A_prework and A_postwork are not significantly 
# different, but (A_postwork-A_prework) is significantly different from 
# (A_postcontrol-A_precontrol)

# sim_wpre<-round(runif(n=16, min=20, max=32), 0)
# sim_wpost<-round(runif(n=16, min=22, max=34), 0)
# sim_cpre<-round(runif(n=16, min=20, max=30), 0)
# sim_cpost<-round(runif(n=16, min=18, max=29), 0)

sim_wpre<-c(31, 22, 25, 23, 31, 20, 26, 25, 25, 22, 28, 27, 26, 31, 25, 21)
sim_wpost<-c(31, 28, 28, 25, 29, 26, 26, 25, 30, 23, 22, 26, 26, 28, 25, 30)
sim_cpre<-c(30, 29, 29, 26, 26, 22, 26, 23, 23, 22, 24, 28, 20, 28, 27, 21)
sim_cpost<-c(26, 28, 24, 28, 24, 27, 26, 20, 23, 21, 20, 23, 25, 18, 21, 25)
sim_wdelta<-sim_wpost-sim_wpre
sim_cdelta<-sim_cpost-sim_cpre

brunnermunzel.test(sim_wpre, sim_wpost) #insig
brunnermunzel.test(sim_cpre, sim_cpost) #insig
brunnermunzel.test(sim_wpre, sim_cpost) #insig
brunnermunzel.test(sim_wdelta,sim_cdelta) #sig
brunnermunzel.test(sim_wpost, sim_cpost) #sig


#hypothesis 2. Are control group attitude scores higher 
# on the second survey as compared to the first?
brunnermunzel.test(A_precontrol, A_postcontrol)

#hypothesis 3. Are score changes higher for workshop group than control group?
brunnermunzel.test(deltacontrol, deltawork)
brunnermunzel.test(likert_deltacontrol, likert_deltawork)


#bootstrapping, from https://data.library.virginia.edu/the-wilcoxon-rank-sum-test/
# med.diff <- function(d, i) {
#   tmp <- d[i,] 
#   median(tmp$Delta[tmp$Workshop==0]) - 
#     median(tmp$Delta[tmp$Workshop>0])
# }
# 
# boot.out <- boot(data = summary_data, statistic = med.diff, R = 1000)
# median(boot.out$t)
# boot.ci(boot.out, type = "perc")




### Supplementary/Exploratory Code

likdeltbyq<-likert_control[15:28]-likert_control[1:14]
colSums(likdeltbyq)

n<-c(1:100)
political_empowerment<-1/n
png("pe.png", units="in", width=4, height=4, res=300)
plot(n,political_empowerment, 
     xlab = "n (number of sytem causes)",
     ylab = "PE (political empowerment)")
text(50, .5, substitute(paste(italic(PE(n)==frac(1,n)))))
dev.off()

find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}

find_mode(data$PrA1)
min(data$PrA1)


x<-c(1:1000)
y<-x+(sample(10000,1000)-5000)/100
uncertainty<-data.frame(x,y)

png("uncertainty.png", units="in", width=3, height=2.4, res=300)
ggplot(data=uncertainty, aes(x=x, y=y)) + geom_line(aes(x)) + theme_classic() +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank())+
  geom_ribbon(data=uncertainty, aes( ymin=y^1.2*-2+3*x, ymax=y^1.2*2+x), linetype=2, alpha=0.1)+
  geom_ribbon(data=uncertainty, aes( ymin=y^1.1*-2+3*x, ymax=y^1.1*2+x), linetype=2, alpha=0.1)+
  labs(y = "a system effect", x="time")
dev.off()
