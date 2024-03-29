
# datalist = datalist
# var = "healthstate_eq5d"
# y.lab = y.lab
# y.headings = y.headings
# title = "EQ5D: Healthstate"
# maindata = dat


chap.forestplot = function(datalist, var, y.lab, y.headings, title = "", ann.lab = NULL, maindata, ...){

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

#assign(dt, eval(parse(text = maindata)))

frm.age = as.formula(paste0(var, "~ factor(visit) + arm*ifelse(age_rand>=50,1,0)"))
mod.age = lme(frm.age, random=~1|id, eval(parse(text = maindata)), na.action=na.exclude, subset=visit>0)

frm.lpg = as.formula(paste0(var, "~ factor(visit) + arm*ifelse(lpg == 1,1,0)"))
mod.lpg = lme(frm.lpg, random=~1|id, eval(parse(text = maindata)), na.action=na.exclude, subset=visit>0)

frm.sbp = as.formula(paste0(var, "~ factor(visit) + arm*ifelse(sbp >= 100,1,0)"))
mod.sbp = lme(frm.sbp, random=~1|id, eval(parse(text = maindata)), na.action=na.exclude, subset=visit>0)

frm.cooktime = as.formula(paste0(var, "~ factor(visit) + arm*ifelse(totalcook_avgmins >= 190,1,0)"))
mod.cooktime = lme(frm.cooktime, random=~1|id, eval(parse(text = maindata)), na.action=na.exclude, subset=visit>0)

frm.svcooktime = as.formula(paste0(var, "~ factor(visit) + arm*ifelse(cooktime_avgyr1 >= 180,1,0)"))
mod.svcooktime = lme(frm.svcooktime, random=~1|id, eval(parse(text = maindata)), na.action=na.exclude, subset=visit>0)

frm.edu = as.formula(paste0(var, "~ factor(visit) + arm*ifelse(yofeduc_bsc > 5,1,0)"))
mod.edu = lme(frm.edu, random=~1|id, eval(parse(text = maindata)), na.action=na.exclude, subset=visit>0)

frm.occupation = as.formula(paste0(var, "~ factor(visit) + arm*ifelse(employment == 'Farmer' | employment == 'Laborer',1,0)"))
mod.occupation = lme(frm.occupation, random=~1|id, eval(parse(text = maindata)), na.action=na.exclude, subset=visit>0)

a = nrow(summary(mod.age)$tTable)
b = ncol(summary(mod.age)$tTable)

p.list = sprintf("%3.2f",c(summary(mod.age)$tTable[a,b],
                           summary(mod.lpg)$tTable[a,b],
                           summary(mod.sbp)$tTable[a,b],
                           summary(mod.cooktime)$tTable[a,b],
                           summary(mod.svcooktime)$tTable[a,b],
                           summary(mod.edu)$tTable[a,b],
                           summary(mod.occupation)$tTable[a,b]))

#y.headings <- paste0(y.headings, " (p=", p.list, ")")

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

list1 = c(1, 4, 7, 10, 13, 16, 19)

for(i in 1:7){
  tab2$y[list1[i]] <- paste0(tab2$y[list1[i]], " (p=", p.list[i], ")")
}

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
                           max(tab2$ub, na.rm = T) + (abs(max(tab2$ub, na.rm = T))/0.8)))+
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
          axis.title.y = element_blank()) +
    annotate("text", x = -Inf, y = Inf, hjust = -0.5, vjust = 1.3, label = ann.lab, size = 10)

  return(p)
}

# data1 = dat %>% filter(visit > 0 & age_rand>=50)
# data2 = dat %>% filter(visit > 0 & age_rand<50)
# data3 = dat %>% filter(visit > 0 & lpg == 1)
# data4 = dat %>% filter(visit > 0 & lpg == 0)
# data5 = dat %>% filter(visit > 0 & sbp > 120)
# data6 = dat %>% filter(visit > 0 & sbp <= 120)
#
# tiff('CHAP_Healthstate_EQ5D.tiff', units="in", width=10, height=8, res=100, compression = 'lzw')
# chap.forestplot(datalist = c("data1", "data2", "data3", "data4", "data5", "data6"),
#                 var = "healthstate_eq5d",
#                 y.lab = c("Age", " >= 50 y", "< 50 y",
#                           "Owns LPG", "Yes", "No",
#                           "SBP", ">= 120mm Hg", "< 120mm Hg"),
#                 y.headings = c("Age", "Owns LPG", "SBP"),
#                 title = "Healthstate EQ5D")
#
# dev.off()





