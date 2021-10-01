exposure.plot = function(dat, var, exposure, y.min, y.max, y.lab, x.lab, trimmed){

  frm = as.formula(paste0(var,"~", "s(",exposure,")","+ s(age_rand) + s(id,bs='re') +
            bmi + chimney + lpg + fise + pigs + dogs + incomecat + yearseduc + sleep"))

  model = gam(frm, data = dat, method = "REML", na.action = na.exclude)
  a1 = plot_smooth(model, view = exposure, rm.ranef=T)

dat$exposure = dat[[exposure]]

x.min = min(dat$exposure, na.rm = T)
x.max = quantile(dat$exposure,0.95,na.rm=T)


p =
  ggplot () +
  geom_line(aes(x = a1$fv[[exposure]], y = a1$fv$fit),linetype=1) +
  geom_line(aes(x = a1$fv[[exposure]], y = a1$fv$ul),linetype="longdash") +
  geom_line(aes(x = a1$fv[[exposure]], y = a1$fv$ll),linetype="longdash") +
  theme_bw() +
  scale_y_continuous(y.lab, limits = c(y.min, y.max))
if(trimmed == 1){
  p = p +
    scale_x_continuous(x.lab, limits = c(x.min, x.max)) +
    geom_rug(aes(x = dat$exposure[dat$exposure < quantile(dat$exposure, 0.945, na.rm=T)]),sides="b")
    }
if(trimmed != 1){
  p = p +
    scale_x_continuous(x.lab) +
    geom_rug(aes(x = dat[[exposure]]),sides="b")}
return(p)
}

# dat = dat
# var = "total_eq5d"
# exposure = "pm_per"
# y.lab = "y"
# x.lab = "x"
# trimmed = 1








