
exposure.plot = function(data, var, exposure , y.lab){

x.lab = expression(paste("Kitchen ",PM[2.5]," (",mu,g/m^3,")"))

frm = as.formula(paste0(var,"~", "s(",exposure,")","+ s(age_rand) + s(id,bs='re') +
            bmi + s(season) + chimney + lpg + fise + pigs + dogs + incomecat + yearseduc + sleep"))

model = gam(frm, data = eval(parse(text = data)), method = "REML", na.action = na.exclude)
a1 = plot_smooth(model, view = exposure, rm.ranef=T)

p = 
ggplot () + 
  geom_line(aes(x=a1$fv$pm_kit,y=a1$fv$fit),linetype=1)+
  geom_line(aes(x=a1$fv$pm_kit,y=a1$fv$ul),linetype="longdash")+
  geom_line(aes(x=a1$fv$pm_kit,y=a1$fv$ll),linetype="longdash")+
  theme_bw() +
  scale_y_continuous(y.lab) +
  scale_x_continuous(x. lab,
                     limits=c(min(data[[exposure]], na.rm = T),quantile(data[[exposure]],0.95,na.rm=T)))+
  geom_rug(aes(x = data[[exposure]][data[[exposure]] < quantile(data[[exposure]], 0.945, na.rm=T)]),sides="b")
return(p)
}









