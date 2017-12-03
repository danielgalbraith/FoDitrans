library(dplyr)
library(tibble)
library(tidyr)
library(ggplot2)
library(purrr)
library(lme4)
theme_set(theme_bw())

source("helpers.R")
dd = read.csv(file="faroese_data.csv")
d = dd %>%
  gather(Speaker,Response, -X., -Sentence, -Gloss, -Context, -Voice, -Word.order, -Aux.agr, -Part.agr.target, -Part.case, -Part.gend, -Part.num, -IO.case, -IO.gend, -IO.num, -DO.case, -DO.gend, -DO.num) %>%
  filter(Response != "-") %>%
  mutate(Response = as.numeric(Response))
passive = d %>%
  filter(Voice == "pass")
agr = passive %>%
  group_by(Word.order, DO.case, Part.num) %>%
  summarise(Mean = mean(Response), CILow = ci.low(Response), CIHigh = ci.high(Response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh)

dodge = position_dodge(.9)

ggplot(agr[agr$DO.case != "nom/acc",], aes(x=Word.order, y=Mean, fill=DO.case)) +
  geom_bar(stat="identity", position=dodge) +
  geom_errorbar(aes(ymin=YMin, ymax=YMax),position=dodge,width=.25) +
  facet_wrap(~Part.num)

passive.noneuter = droplevels(passive[passive$DO.case != "nom/acc",])

library(lmerTest)

centered = cbind(passive.noneuter, myCenter(passive.noneuter[,c("DO.case","Part.num")]))
contrasts(centered$Word.order) = cbind("io.first.vs.obj.shift" = c(1,0,0,0), "io.first.vs.expl"=c(0,0,1,0),"io.first.vs.do.first" = c(0,1,0,0))

cm = lmer(Response ~ cDO.case * cPart.num * Word.order + (1|Speaker) + (1|X.), data = centered, REML=F)
summary(cm)
