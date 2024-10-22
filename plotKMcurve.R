survPlot <-function(fit, ncol=1, legend_position="top", break_time=100){
  survminer::ggsurvplot(
    fit,                     # survfit object with calculated statistics.
    risk.table = TRUE,       # show risk table.
    pval = TRUE,             # show p-value of log-rank test.
    conf.int = FALSE,         # show confidence intervals for 
    # point estimaes of survival curves.
    xlim = c(0,max(fit$time)),        # present narrower X axis, but not affect
    # survival estimates.
    break.time.by = break_time,     # break X axis in time intervals by 500.
    risk.table.y.text.col = T, # colour risk table text annotations.
    risk.table.y.text = F, # show bars instead of names in text annotations
    font.x = c('bold'),
    font.y = c('bold'),
    font.stickslab = c('bold'),
    surv.plot.height=0.65,
    risk.table.height=0.35,
    legend = legend_position
    # legend.labs=lab.legend
    # in legend of risk table
  ) +
    guides(colour=guide_legend(ncol=ncol))
}

# example: survPlot(survfit(Surv(OS,OS_cens)~idh_codel, data = OS_KM), break_time=20, legend_position="top", ncol=3)


