

# example: survPlot(survfit(Surv(OS,OS_cens)~idh_codel, data = OS_KM), break_time=20, legend_position="top", ncol=3)


survPlot <- function( fit, ncol=1, legend_position="top", break_time=100,
                            linetype=1, palette='jco') {

library(survminer)
library(ggplot2)
  
  # Custom ggplot for the survival plot with line type and color
p <-  ggsurvplot(
    fit,                        # survfit object with calculated statistics.
    risk.table = TRUE,          # show risk table.
    pval = TRUE,                # show p-value of log-rank test.
    conf.int = FALSE,           # show confidence intervals.
    xlim = c(0, max(fit$time)), # set X axis limit.
    break.time.by = break_time, # break X axis into intervals.
    risk.table.y.text.col = TRUE, # color risk table text annotations.
    risk.table.y.text = FALSE,  # show bars instead of text.
    font.x = c("bold"),
    font.y = c("bold"),
    font.tickslab = c("bold"),
    surv.plot.height = 0.65,
    risk.table.height = 0.35,
    legend = legend_position,
    linetype = linetype,
    palette = palette
  ) +
    guides(colour = guide_legend(ncol = ncol))           # Adjust legend column count

return(p)
}