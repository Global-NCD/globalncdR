
chap.forestplot = function(data = data,
                           var = "variable",
                           visit = "visit",
                           arm = "arm",
                           title = "Total EQ5D"
                           ){

  frm = as.formula(paste0(var, "~factor(",visit, ") + ", arm))

  model = lme(frm, random=~1|id, data = eval(parse(text = data)), na.action=na.exclude, subset=visit > 0 & age_rand>=50)
  agege50= c(matrix(unlist(intervals(model,which="fixed")),ncol=3) [ 3,c(2,1,3) ], summary(model)$tTable[3,5])

  model = lme(frm, random=~1|id, data = eval(parse(text = data)), na.action=na.exclude, subset=visit > 0 & age_rand<50)
  agelt50 = c(matrix(unlist(intervals(model,which="fixed")),ncol=3) [ 3,c(2,1,3) ], summary(model)$tTable[3,5])

  model = lme(frm, random=~1|id, data = eval(parse(text = data)), na.action=na.exclude, subset=visit > 0 & lpg == 1)
  lpg1 = c(matrix(unlist(intervals(model,which="fixed")),ncol=3) [ 3,c(2,1,3) ], summary(model)$tTable[3,5])

  model = lme(frm, random=~1|id, data = eval(parse(text = data)), na.action=na.exclude, subset=visit > 0 & lpg == 0)
  lpg0 = c(matrix(unlist(intervals(model,which="fixed")),ncol=3) [ 3,c(2,1,3) ], summary(model)$tTable[3,5])

  model = lme(frm, random=~1|id, data = eval(parse(text = data)), na.action=na.exclude, subset=visit > 0 & sbp > 120)
  sbp1= c(matrix(unlist(intervals(model,which="fixed")),ncol=3) [ 3,c(2,1,3) ], summary(model)$tTable[3,5])

  model = lme(frm, random=~1|id, data = eval(parse(text = data)), na.action=na.exclude, subset=visit > 0 & sbp <= 120)
  sbp0 = c(matrix(unlist(intervals(model,which="fixed")),ncol=3) [ 3,c(2,1,3) ], summary(model)$tTable[3,5])


  tab = rbind(agege50, agelt50 , lpg1, lpg0, sbp1, sbp0)
  colnames(tab) = c("between", "lb","ub", "p")
  tab = data.frame(tab)
  tab$y = c( "Age >= 50 y", "Age < 50 y","Owns LPG: Yes", "Owns LPG: No", "SBP >= 120mm Hg", "SBP < 120mm Hg")

  tab$label = paste0(sprintf("%3.2f",tab$between,1)," (",
                     sprintf("%3.2f",tab$lb,1), " to ",
                     sprintf("%3.2f",tab$ub,1),")")
  tab$y = factor(tab$y, levels = tab$y)

  p = ggplot(data=tab,
             aes(x=between,
                 y = y)) +
    geom_point(shape=23, fill="black",size=4.5)+
    geom_errorbarh(aes(xmin = lb, xmax = ub), height=0, size=1)+
    scale_x_continuous("\n Between-arm difference",
                       expand = c(0,0))+
    coord_cartesian(xlim=c(min(tab$lb) - (abs(min(tab$lb))/5),
                           max(tab$ub) + (abs(max(tab$ub))/1.2)))+
    geom_vline(xintercept = 0, linetype=2)+
    theme_bw()+
    ggtitle(title)+
    annotate("text",
             x = Inf,#max(tab$ub) + max(tab$ub)/10,
             y=tab$y,
             label=tab$label,
             hjust=1.1,
             size=5)+
    theme(plot.title = element_text(hjust = 0.5, size = 19),
          axis.text = element_text(size = 14))
  return(p)
}
