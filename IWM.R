###############################
# Long-Term IWM Study 
# Coded by Rodger Farr
##############################

#This is a split-split-split plot study

#model:
#lmer(Block(B)+Year(A)+EUw
# +C+AC+EUs
# +D+AD+DC+ADC+EUss
# +E+AE+EC+ED+EAC+EAD+ECD+EACD+EUsss
# +F+FA+FC+FD+FE+FAC+FAD+FAE+FCD+FCE+FDE+FACD+FACE+FCDE+FACDE+EUssss,data=iwm)

#how do I make the EU's

iwm=read.csv("iwm.csv",header=T)
summary(iwm)

iwm$Block[iwm$block==1]="B1"
iwm$Block[iwm$block==2]="B2"
iwm$Block[iwm$block==3]="B3"
iwm$Block[iwm$block==4]="B4"

iwm$Year[iwm$year==1]="Y1"
iwm$Year[iwm$year==2]="Y2"

library(lme4)
library(agricolae)
library(Matrix)

iwm$a=factor(iwm$Year)
iwm$c=factor(iwm$hand.weed)
iwm$d=factor(iwm$plow)
iwm$e=factor(iwm$cover.crop)  
iwm$f=factor(iwm$herb)

iwm$trt=factor(iwm$TRT)

mod1=lm(pa.acre~trt+Block+a,data=iwm)
anova(mod1)

mod2=lm(pa.acre~c*d*e*f+a+Block,data=iwm)
anova(mod2)

#plow was significant in the factorial
plow=HSD.test(mod2,"d");plow

iwm$EUw=paste(iwm$Block,iwm$a, sep="-")
iwm$EUs=paste(iwm$Block,iwm$a,iwm$c, sep="-")
iwm$EUss=paste(iwm$Block,iwm$a,iwm$c,iwm$d, sep="-")
iwm$EUsss=paste(iwm$Block,iwm$a,iwm$c,iwm$d,iwm$e, sep="-")
iwm$EUssss=paste(iwm$Block,iwm$a,iwm$c,iwm$d,iwm$e,iwm$f, sep="-")

#model:
mod3=lmer(pa.acre~Block+a+(1|EUw)+c+a:c+(1|EUs)
     +d+a:d+d:c+a:d:c+(1|EUss)
     +e+a:e+e:c+e:d+e:a:c+e:a:d+e:c:d+e:a:c:d+(1|EUsss)
     +f+f:a+f:c+f:d+f:e+f:a:c+f:a:d+f:a:e+f:c:d+f:c:e+f:d:e+f:a:c:d+f:a:c:e+f:c:d:e+f:a:c:d:e+(1|EUssss),data=iwm)

