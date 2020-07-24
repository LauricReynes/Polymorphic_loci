# Display the percentage of polymorphic loci (%P) within populations

* %P was initially computed using a random sampling approach implemented in a custom bash script
  * Available at https://github.com/rebecca-cj/Revegetation/blob/master/RevegPaper/polym_resample.sh
* %P was evaluated for each sample size (n = 3 to n = 10) from 100 random resamples within populations
* Raw data are available (in the repository) for *L. rodriguezii* (**'polymorphic_loci_lr.txt'**) and *L. digitata* (**'polymorphic_loci_ld.txt'**)

#### Download R packages
```
library(ggplot2)
library(reshape)
library(RColorBrewer)
```

#### 1. Compute average values and standard deviation (S.D) from 100 random resamples
```
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

setwd("file path")
P_lr <-read.table("polymorphic_loci_lr.txt",header = TRUE)
P_ld <-read.table("polymorphic_loci_ld.txt",header = TRUE)

P_data_lr <-data_summary(P_lr,varname ="P",groupnames = c("POP",'n'))
P_data_ld <-data_summary(P_ld,varname ="P",groupnames = c("POP",'n'))
```
#### 2. Display the results as vertical intervals for both species 
```
color <- brewer.pal(n = 4, name = "Set1")
color1 <- brewer.pal(n = 5, name = "Set1")

lr <-ggplot(P_data_lr, aes(x=n, y=P, group=POP, color=POP)) + 
  geom_line(linetype = "dashed",size=1) +
  geom_pointrange(aes(ymin=P-sd, ymax=P+sd),size = 1)+
  scale_fill_manual(values=color)+
  scale_y_continuous(breaks=seq(10,80,10),limits= c(0,80))+
  labs(title="",x="Sample size", y = "%P")+
  scale_x_discrete(limits=c(3,6,8,10))+
  theme_bw() +
  theme(axis.line = element_line(size=1, colour = "black"),
        legend.title = element_blank(),
        legend.position=c(0.19, 0.85),
        legend.text = element_text(size=14,face="bold"),
        panel.grid.major = element_line(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        text=element_text(family="Tahoma"),
        axis.title = element_text(size = 12),
        axis.text.x=element_text(angle = 0,colour="black", size = 12),
        axis.text.y=element_text(colour="black", size = 10))
  
ld <-ggplot(P_data_ld, aes(x=n, y=P, group=POP, color=POP)) + 
  geom_line(linetype = "dashed",size=1) +
  geom_pointrange(aes(ymin=P-sd, ymax=P+sd),size = 1)+
  scale_fill_manual(values=color1)+
  scale_y_continuous(breaks=seq(10,80,10),limits= c(0,80))+
  labs(title="",x="Sample size", y = "%P")+
  scale_x_discrete(limits=c(3,6,8,10))+
  theme_bw() +
  theme(axis.line = element_line(size=1, colour = "black"),
        legend.title = element_blank(),
        legend.position=c(0.19, 0.85),
        legend.text = element_text(size=14,face="bold"),
        panel.grid.major = element_line(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        text=element_text(family="Tahoma"),
        axis.title = element_text(size = 12),
        axis.text.x=element_text(angle = 0,colour="black", size = 12),
        axis.text.y=element_text(colour="black", size = 10))

plot_grid(lr,ld,
          labels =c("A","B"),label_fontface = "bold", ncol=2, nrow = 1,
          label_size = 16,
          label_x = 0.12, label_y = 0.95,
          hjust = 1, vjust = 1,
          label_fontfamily = "Tahoma")
          
ggsave("FigureS2.jpg",width=40 ,height=15,dpi=300,units="cm")
```








