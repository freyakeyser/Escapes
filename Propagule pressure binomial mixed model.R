
prop.press_atl_byyear$detect <- 0
prop.press_atl_byyear$detect[prop.press_atl_byyear$TotalFarmed > 0] <- 1
prop.press_atl_byyear$detect[is.na(prop.press_atl_byyear$TotalFarmed)] <- NA

prop.press_atl_byyear$sProp.press <- scale(prop.press_atl_byyear$prop.press)

scaleList <- list(scale = attr(prop.press_atl_byyear$sProp.press, "scaled:scale"),
                  center = attr(prop.press_atl_byyear$sProp.press, "scaled:center"))

prop.press_atl_byyear$sProp.press <- as.numeric(prop.press_atl_byyear$sProp.press)

mod <- glmer(detect~sProp.press+(1|River),data=prop.press_atl_byyear,family=binomial(link="logit"))

sdata <-  scale(seq(from=0,to=0.0015,by=0.000001),scale=scaleList$scale,center=scaleList$center)
newdata <- data.frame(expand.grid(unique(as.character(prop.press_atl_byyear$River)),sdata),stringsAsFactors = F)
newdata <- data.frame(River="BigSalmon",sdata,stringsAsFactors = F)

colnames(newdata) <- c("River","sProp.press")

newdata <- cbind(newdata,AICcmodavg::predictSE(mod,newdata,se.fit=T,type="response")%>%data.frame())
newdata$fixedprop.press <- newdata$sProp.press * (as.numeric(scaleList$scale) + as.numeric(scaleList$center))

#mod_coef=as.data.frame(coef(mod)$River)
#int <- ((log(p/(1-p)) - mod_coef[1,1]) / mod_coef[1,2])*scaleList$scale + scaleList$center

Intercept <- mean(newdata[which(round(newdata$fit,2)==0.5),"fixedprop.press"]) #there are two values, answer is in between

newdata <- filter(newdata,fixedprop.press<0.0015)

p1 <- ggplot()+
  geom_segment(aes(x=Intercept,xend=Intercept,y=-Inf,yend=0.5),lty=2,colour="grey40")+
  geom_segment(aes(x=-Inf,xend=Intercept,y=0.5,yend=0.5),lty=2,colour="grey40")+
  geom_line(data=newdata,aes(x=fixedprop.press,y=fit,group=River),lwd=1.12)+
  geom_line(data=newdata,aes(x=fixedprop.press,y=fit+se.fit,group=River),lty=2,lwd=1.05)+
  geom_line(data=newdata,aes(x=fixedprop.press,y=fit-se.fit,group=River),lty=2,lwd=1.05)+
  geom_point(data=prop.press_atl_byyear,aes(x=prop.press,y=detect),pch=21,fill="white")+
  labs(x="Propagule pressure",y=expression(paste("Probability of farmed presence " %+-% " se",sep=" ")))+
  theme_bw();p1


ggsave("c:/Users/StanleyR/Documents/MPAs/Analysis Help/Freya/BinomialMod.png",p1)
