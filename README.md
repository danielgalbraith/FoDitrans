This repository contains an analysis of the results of a survey on Faroese ditransitive passives.

* <b>analysis.r</b>: R script containing code for plot using ggplot and model using lmer4
* <b>faroese_data.csv</b>: Contains sentence judgement data from the Faroese survey
* <b>word_order_plot.png</b>: Plot generated by analysis.r

The linear mixed-effects model is implemented in lmer4, and can be summarised thus:

acceptability ~ case of theme * number value of participle * word order + (1|speaker) + (1|item) 

Thanks to Judith Degen for assistance in understanding the model and R tips.
