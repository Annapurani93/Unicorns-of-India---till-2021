library(tidyverse)
library(readxl)
library(ggtext)
read_excel("unicorn 2021.xlsx")->data
data%>%
  group_by(Year)%>%
  count()->yearwise

data%>%
  group_by(Sector)%>%
  count()%>%
  arrange(desc(n))->sectorwise

data%>%
  group_by(Location)%>%
  count()%>%
  arrange(desc(n))->citywise


ggplot(yearwise,aes(x=Year,y=n,label=n))+
  geom_col(width=0.5, fill="#fe9c8f",colour="white")+
  scale_x_continuous(breaks=c(2011,2012,2013,2014,2015,
                              2016,2017,2018,2019,2020,2021))+
  geom_text(size=4.5,colour="white",hjust=-1)+
  coord_flip()+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(colour="white",face = "bold"),
        axis.text.x = element_blank(),
        axis.line=element_blank(),
        panel.background = element_rect(fill="black"),
        plot.background=element_rect(fill="black"),
        panel.grid = element_blank(),
        plot.margin = unit(c(0.5,1,0.5,1),"cm"),
        strip.background = element_rect(fill="gray10"),
        strip.text = element_text(colour="white",size=10,face="bold",margin = margin(t=5,b=5)),
        legend.position = "none",
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title = element_text(size=14,face="bold",color="white",margin=margin(b=15)),
        plot.subtitle = element_text(size=12, colour="white",margin=margin(b=21)),
        plot.caption = element_markdown(size=10, colour="white",hjust=0,margin=margin(t=25)))+  
  theme(panel.spacing = unit(1, "lines"))+
  labs(title=str_wrap("2021 SAW THE HIGHEST NUMBER OF UNICORNS CREATED IN INDIA...",30),
       subtitle = str_wrap("The below visualization shows the number of unicorns created each year in India",108))->plot1

sectorwise

ggplot(sectorwise,aes(x=n,y=reorder(Sector,n, decreasing=TRUE),label=n))+
  geom_col(width=0.5, fill="#fe9c8f",colour="white")+
  geom_text(size=4.5,colour="white",hjust=-1.2)+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(colour="white",face = "bold"),
        axis.text.x = element_blank(),
        axis.line=element_blank(),
        panel.background = element_rect(fill="black"),
        plot.background=element_rect(fill="black"),
        panel.grid = element_blank(),
        plot.margin = unit(c(0.5,1,0.5,1),"cm"),
        strip.background = element_rect(fill="gray10"),
        strip.text = element_text(colour="white",size=10,face="bold",margin = margin(t=5,b=5)),
        legend.position = "none",
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title = element_text(size=14,face="bold",color="white",margin=margin(b=15)),
        plot.subtitle = element_text(size=12, colour="white",margin=margin(b=21)),
        plot.caption = element_markdown(size=10, colour="white",hjust=0,margin=margin(t=25)))+  
  theme(panel.spacing = unit(1, "lines"))+
  labs(title="...AND AMONG SECTORS, FINTECH TOPS THE CHARTS",
       subtitle = str_wrap("The below visualization shows the number of unicorns created in each sector, in India",108))->plot2

citywise
ggplot(citywise,aes(x=n,y=reorder(Location,n, decreasing=TRUE),label=n))+
  geom_col(width=0.5, fill="#fe9c8f",colour="white")+
  geom_text(size=4.5,colour="white",hjust=-1.2)+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(colour="white",face = "bold"),
        axis.text.x = element_blank(),
        axis.line=element_blank(),
        panel.background = element_rect(fill="black"),
        plot.background=element_rect(fill="black"),
        panel.grid = element_blank(),
        plot.margin = unit(c(0.5,1,0.5,1),"cm"),
        strip.background = element_rect(fill="gray10"),
        strip.text = element_text(colour="white",size=10,face="bold",margin = margin(t=5,b=5)),
        legend.position = "none",
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title = element_text(size=14,face="bold",color="white",margin=margin(b=15)),
        plot.subtitle = element_text(size=12, colour="white",margin=margin(b=21)),
        plot.caption = element_markdown(size=10, colour="white",hjust=0,margin=margin(t=25)))+  
  theme(panel.spacing = unit(1, "lines"))+
  labs(title=str_wrap("BANGALORE HAS PRODUCED THE HIGHEST NUMBER OF UNICORNS AMONG CITIES...",40),
       subtitle = str_wrap("The below visualization shows the number of unicorns created in various cities in India",108))->plot3

data%>%
  select(Company,`Valuation ($B)`)%>%
  arrange(desc(`Valuation ($B)`))%>%
  slice(1:15)->top10

top10[-c(1,3,12),]->top110

top110[-c(11,12),]->top

ggplot(top,aes(x=`Valuation ($B)`,y=reorder(Company,`Valuation ($B)`, decreasing=TRUE),label= paste0("$",`Valuation ($B)`," Billion")))+
  geom_col(width=0.5, fill="#fe9c8f",colour="white")+
  geom_text(size=4.5,colour="white",hjust=-0.1)+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(colour="white",face = "bold"),
        axis.text.x = element_blank(),
        axis.line=element_blank(),
        panel.background = element_rect(fill="black"),
        plot.background=element_rect(fill="black"),
        panel.grid = element_blank(),
        plot.margin = unit(c(0.5,1,0.5,1),"cm"),
        strip.background = element_rect(fill="gray10"),
        strip.text = element_text(colour="white",size=10,face="bold",margin = margin(t=5,b=5)),
        legend.position = "none",
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title = element_text(size=14,face="bold",color="white",margin=margin(b=15)),
        plot.subtitle = element_text(size=12, colour="white",margin=margin(b=21)),
        plot.caption = element_markdown(size=10, colour="white",hjust=0,margin=margin(t=25)))+  
  theme(panel.spacing = unit(1, "lines"))+
  labs(title="...AND BYJU'S LEADS THE CHARTS IN VALUATION",
       subtitle = str_wrap("The below visualization shows the top 10 Indian unicorns with the highest valuation (valuation in billion dollars)",108))->plot4


library(patchwork)

((plot1+plot4)/(plot3+plot2))

plot1+plot4+plot3+plot2+
  plot_layout(nrow=2)&
  theme(panel.grid = element_blank(),
        plot.margin = unit(c(0.5,1,0.5,1),"cm"),
        panel.background = element_rect(fill="black"),
        plot.background=element_rect(fill="black"),
        plot.title = element_text(size=14,face="bold",color="white",margin=margin(b=15)),
        plot.subtitle = element_text(size=12, face="bold",colour="white",margin=margin(b=21)),
        plot.caption = element_markdown(size=10, colour="white",hjust=0,margin=margin(t=25)))&
  plot_annotation(
    title = "UNICORNS OF INDIA",
    subtitle = str_wrap("India has thE highest number of unicorns in the world after the US and China. The country has produced 81 unicorns so far, of which 7 have gone public,
                        and 4 have lost their unicorn status due to a dip in valuation. 2021 saw a whopping 44 unicorns created in India, the highest so far for the country.",120),
    caption = "Source: Venture Intelligence|Design and Analysis: @annapurani93",
    )->plot


ggsave("unicorns.png",plot,height=16,width=18)
