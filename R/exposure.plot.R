exposure.plot = function(data, var, exposure , y.lab, x.lab, trimmed){

  frm = as.formula(paste0(var,"~", "s(",exposure,")","+ s(age_rand) + s(id,bs='re') +
            bmi + chimney + lpg + fise + pigs + dogs + incomecat + yearseduc + sleep"))
  
  model = gam(frm, data = eval(parse(text = data)), method = "REML", na.action = na.exclude)
  a1 = plot_smooth(model, view = exposure, rm.ranef=T)
  
data = eval(parse(text = data))
x.min = min(data[[exposure]], na.rm = T)
x.max = quantile(data[[exposure]],0.95,na.rm=T)

p =
  ggplot () +
  geom_line(aes(x = a1$fv[[exposure]], y = a1$fv$fit),linetype=1) +
  geom_line(aes(x = a1$fv[[exposure]], y = a1$fv$ul),linetype="longdash") +
  geom_line(aes(x = a1$fv[[exposure]], y = a1$fv$ll),linetype="longdash") +
  theme_bw() +
  scale_y_continuous(y.lab)
if(trimmed == 1){
  p = p + 
    scale_x_continuous(x.lab, limits = c(x.min, x.max)) +
    geom_rug(aes(x = data[[exposure]][data[[exposure]] < quantile(data[[exposure]], 0.945, na.rm=T)]),sides="b")
    }
if(trimmed != 1){
  p = p +
    scale_x_continuous(x.lab) + 
    geom_rug(aes(x = data[[exposure]]),sides="b")}
return(p)
}









