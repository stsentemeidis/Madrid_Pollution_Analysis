
### THIS SCRIPT PLOTS THE CORRELATION MATRICES

# Bubble Correlation Matrix
# corrplot(cor(pollution_daily_h[, names(pollution_daily_h)[sapply(pollution_daily_h,is.numeric)]]), 
#          order = 'FPC',
#          type = 'upper',
#          diag = FALSE,
#          tl.srt = 45
#          )

# Save plot Bubble Correlation Matrix
png('plots/2. bubble_corr_matrix.png', width = 1500, height = 1000)
corrplot(cor(pollution_daily_h[, names(pollution_daily_h)[sapply(pollution_daily_h,is.numeric)]]),
         order = 'FPC',
         type = 'upper',
         diag = FALSE,
         tl.srt = 45,
         mar = c(2,2,2,2)
)
dev.off()

print(paste0('[', round(difftime(Sys.time(),start_time, units = 'secs'),1), 's]: ',
             'Bubble Correlation Matrix is generated and saved in /plots!'))


# Mixed Bubble Correlation Matrix
# corrplot.mixed(cor(pollution_daily_h[, names(pollution_daily_h)[sapply(pollution_daily_h,is.numeric)]]), order = 'FPC')

# Save plot Mixed Bubble Correlation Matrix
png('plots/3. mixed_bubble_corr_matrix.png', width = 1500, height = 1000)
corrplot.mixed(cor(pollution_daily_h[, names(pollution_daily_h)[sapply(pollution_daily_h,is.numeric)]]), order = 'FPC')
dev.off()

print(paste0('[', round(difftime(Sys.time(),start_time, units = 'secs'),1), 's]: ',
             'Mixed Bubble Correlation Matrix is generated and saved in /plots!'))

# GGpairs Correlation Matrix
# ggpairs(pollution_daily_h[, names(pollution_daily_h)[sapply(pollution_daily_h,is.numeric)]],
#         lower = list(continuous = wrap('points', alpha = 0.3, size = 0.1)))+
#   theme(panel.grid.major = element_blank())

# Save plot GGpairs Correlation Matrix
png('plots/4. ggpairs_corr_matrix.png', width = 1500, height = 1000)
print(ggpairs(pollution_daily_h[, names(pollution_daily_h)[sapply(pollution_daily_h,is.numeric)]],
        lower = list(continuous = wrap('points', alpha = 0.3, size = 0.1)))+
  theme(panel.grid.major = element_blank()))
dev.off()

print(paste0('[', round(difftime(Sys.time(),start_time, units = 'secs'),1), 's]: ',
             'GGpairs Correlation Matrix is generated and saved in /plots!'))

