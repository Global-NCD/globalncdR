forestplot = function(datalist = datalist,
                      frm = case ~ dosebin + strata(id),
                      stat = clogit,
                      y.lab = y.lab,
                      exp = 1,
                      rev = 1,
                      perc = 1,
                      round = 2,
                      sectionhead = NA,
                      title = "title",
                      errorbar.width = 0,
                      errorbar.size = 1,
                      x.min = -50,
                      x.max = 150,
                      x.break = 10,
                      xcoord.min = -50,
                      xcoord.max = 140,
                      lab.size = 16,
                      lab.hjust = 1.1,
                      vline.x = 0,
                      vline.type = "dashed"){
  frm <- as.formula(frm)

  if(deparse(substitute(stat)) == "clogit"){
    tab = NULL
    for(i in datalist){
      coef = coef(stat(frm, data = i))[1]
      conf = confint(stat(frm, data = i))[1,]
      tab = rbind(tab, c(coef, conf))
    }
  }
  tab = data.frame(tab)

  ######
  if(exp == "TRUE" | exp == "T"){tab[,c(1:3)] = exp(tab[,c(1:3)])}
  if(rev == "TRUE" | rev == "T"){
    tab[,c(1:3)] <- (1- tab[,c(1:3)])
    tab <- tab[,c(1, 3, 2)]
  }
  if(perc == "TRUE" | perc == "T"){tab[,c(1:3)] = tab[,c(1:3)]*100}
  tab$lab = paste0(sprintf(paste0("%3.",round,"f"),tab[[1]],1)," (",
                   sprintf(paste0("%3.",round,"f"),tab[[2]]), " to ",
                   sprintf(paste0("%3.",round,"f"),tab[[3]],1),")")

  tab2 = NULL
  for(i in c(1:length(y.lab))){
    if(y.lab[i] %in% sectionhead){
      tab2 = rbind(tab2, rep(NA, ncol(tab)))
      colnames(tab2) = colnames(tab)
    }
    if(!y.lab[i] %in% sectionhead){
      tab2 = rbind(tab2, tab[i - sum(is.na(tab2[[4]])),])
    }
  }
  tab2$y = y.lab
  tab2$serial = nrow(tab2):1
  tab2$serial = factor(tab2$serial, levels = tab2$serial[order(tab2$serial)])
  tab2$face = ifelse(tab2$y %in% sectionhead, "bold", "plain")
  tab2$lab = replace(tab2$lab, is.na(tab2$lab), "")

  p =
    ggplot(data = tab2) +
    geom_point(aes(x = tab2[[1]], y = tab2[[6]]), shape=23, fill="black", size = 6) +
    geom_errorbar(aes(xmin = tab2[[2]], xmax = tab2[[3]], y = tab2[[6]]), width = errorbar.width, size = errorbar.size) +
    scale_x_continuous("", breaks = seq(x.min, x.max, x.break)) +
    scale_y_discrete("", label = rev(tab2[[5]])) +
    coord_cartesian(xlim=c(xcoord.min, xcoord.max)) +
    geom_vline(x = vline.x, type = vline.type) +
    theme_bw()+
    ggtitle(title)+
    annotate("text", x = Inf, y = tab2[[6]], label =tab2[[4]], hjust = lab.hjust, size = lab.size) +
    theme(axis.ticks = element_blank(),
          panel.grid = element_blank(),
          plot.title = element_text(hjust=0.5,size=27),
          axis.text = element_text(size=17),
          axis.title = element_text(size=22),
          axis.text.y = element_text(face = rev(tab2$face)),
          legend.position = "none")

  return(list(tab2, p))
}
