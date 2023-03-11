# load packadges 
library(readxl)
library(reshape2)
library(dplyr)
library(ggplot2)

#load dataset 
df=read_excel("C:/Users/Saidi/Downloads/data_design_2023_creatortrack_data.xlsx", 
     sheet = "urban_growth")
df2=read_excel("C:/Users/Saidi/Downloads/data_design_2023_creatortrack_data.xlsx", 
               sheet = "full_variables")
#transform wide to long format 
df_long= melt(df,
          id.vars = "countrywb",
          valiable.name="Year",
          value.name = "urb_gro")
# Data manupulation 

abg=df_long %>% rename(Year=variable)

abg$urb_gro=as.numeric(abg$urb_gro)
 
df_ab=abg %>% group_by(countrywb) %>% 
  summarise(sum_urb=mean(urb_gro)) %>% arrange(desc(sum_urb))

df_ab%>%  ggplot(aes(reorder(x= countrywb,-sum_urb),y=sum_urb))+
  geom_point(size=4)+
  
           geom_segment(
             aes(x=countrywb, xend=countrywb, y=0, yend=sum_urb))+
  
               xlab("Country")+ylab("Average growth rate")+
           coord_flip()+theme_bw()+
  ggtitle("Average urban growth rate per country 2000-2021")
 #geom_label(aes(countrywb,sum_urb, label = signif(sum_urb)), 
              #colour = "darkred", nudge_x = 0.35, size = 2)

 
 