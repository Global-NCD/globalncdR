
chap.forestplot = function(datalist, var, y.lab, y.headings, title = "", ...){

  if (!("ggplot2" %in% rownames(installed.packages()))) {
    message("Please install package ggplot2")
  }
  if (!("nlme" %in% rownames(installed.packages()))) {
    message("Please install package nlme")
  }
  library(ggplot2);library(nlme)

frm = as.formula(paste0(var, "~factor(visit) + arm"))

tab = NULL
for(i in datalist) {
  model = lme(frm, random=~1|id, data = eval(parse(text = i)), na.action=na.exclude)
  yy = c(matrix(unlist(intervals(model,which="fixed")),ncol=3) [ 3,c(2,1,3) ], summary(model)$tTable[3,5])
  tab = rbind(tab, yy)
}

  colnames(tab) = c("between", "lb","ub", "p")
  tab = data.frame(tab)
  tab$y = y.lab[!(y.lab %in% y.headings)]
  tab$label = paste0(sprintf("%3.2f",tab$between,1)," (",
                     sprintf("%3.2f",tab$lb,1), " to ",
                     sprintf("%3.2f",tab$ub,1),")")

tab2 = NULL
for(i in y.lab){
if(!i %in% tab$y){tab2 = rbind(tab2, rep(NA, ncol(tab)))
  colnames(tab2) = colnames(tab)}
if(i %in% tab$y){tab2 = rbind(tab2, tab[tab$y == i,])}
}
tab2$y = y.lab
tab2$y = factor(tab2$y, levels = rev(tab2$y))

tab2$face = ifelse(tab2$y %in% y.headings, "bold", "plain")

  p = ggplot(data=tab2,
             aes(x=between,
                 y = y)) +
    geom_point(shape=23, fill="black",size=4.5)+
    geom_errorbarh(aes(xmin = lb, xmax = ub), height=0, size=1)+
    scale_x_continuous("\n Between-arm difference",
                       expand = c(0,0))+
    coord_cartesian(xlim=c(min(tab2$lb, na.rm = T) - (abs(min(tab2$lb, na.rm = T))/5),
                           max(tab2$ub, na.rm = T) + (abs(max(tab2$ub, na.rm = T))/1.5)))+
    geom_vline(xintercept = 0, linetype=2)+
    theme_bw()+
    ggtitle(title)+
    annotate("text",
             x = Inf,#max(tab$ub) + max(tab$ub)/10,
             y=tab2$y,
             label=tab2$label,
             hjust=1.1,
             size=5)+
    theme(plot.title = element_text(hjust = 0.5, size = 19),
          axis.text = element_text(size = 14),
          axis.text.y = element_text(face = rev(tab2$face)),
          axis.title = element_text(size = 16),
          axis.title.y = element_blank())

  return(p)
}

# data1 = dat %>% filter(visit > 0 & age_rand>=50)
# data2 = dat %>% filter(visit > 0 & age_rand<50)
# data3 = dat %>% filter(visit > 0 & lpg == 1)
# data4 = dat %>% filter(visit > 0 & lpg == 0)
# data5 = dat %>% filter(visit > 0 & sbp > 120)
# data6 = dat %>% filter(visit > 0 & sbp <= 120)

# tiff('CHAP_Healthstate_EQ5D.tiff', units="in", width=10, height=8, res=100, compression = 'lzw')
# chap.forestplot(datalist = c("data1", "data2", "data3", "data4", "data5", "data6"),
#                 var = "healthstate_eq5d",
#                 y.lab = c("Age", " >= 50 y", "< 50 y",
#                           "Owns LPG", "Yes", "No",
#                           "SBP", ">= 120mm Hg", "< 120mm Hg"),
#                 y.headings = c("Age", "Owns LPG", "SBP"),
#                 title = "Healthstate EQ5D")
# dev.off()





