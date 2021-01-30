library(readxl)
library(forestplot)
women_data<-read_xlsx("/Users/DELL/Documents/GH Intern/Covid-19/interaction_women.xlsx")
women_used<-cbind(c(". Groups","\n",women_data$.Groups),
                  c("Women\n HR(95% CI)","\n",women_data$`HR (95% CI)`),
                  c("P Value","\n",women_data$`P value`))
women_data$HR<-as.numeric(women_data$HR)
women_data$LEFT <-as.numeric(women_data$LEFT)
women_data$RIGHT <-as.numeric(women_data$RIGHT)
forestplot(women_used,new_page = T,mean=c(NA,NA,women_data$HR),
           lower=c(NA,NA,women_data$LEFT),upper=c(NA,NA,women_data$RIGHT), 
           graph.pos=3,graphwidth=unit(30,"mm"),
           is.summary=c(T,T,T,F,F,F,F,T,F,F,T,F,F,F,F,T,F,F,T,F,F,F,F,T,F,F),
           txt_gp=fpTxtGp(ticks=gpar(cex=0.5),summary=gpar(cex=0.8),cex=0.8),
           line.margin=unit(20,"mm"),lineheight=unit(5.0,"mm"),
           col=fpColors(box="darkblue",line="darkblue",summary="royalblue"),
           xlog=T,colgap=unit(2,"mm"),zero=1,xticks=c(0.9,1,1.2),boxsize=0.5
           ) #colgap is the gap between each columns