
### THIS SCRIPT PLOTS THE TIME SERIES POLLUTANTS AND WEATHER PARAMETERS

# Generate plots for pollutants
plot_pollution <- list()
increment <- 1

for (i in c(list_pollutants,weather_param)){
  var_color <- ifelse(i %in% pollutants$pollutant, pollutants[pollutants$pollutant == i, 'color'], 'skyblue')
  plot_pollution[[increment]] <- ggplot(data=pollution_daily_v[pollution_daily_v$variable == i,],
                                        aes(x=date,y=value))+
                                  geom_line(color=var_color)+
                                  labs(title = i)+
                                  theme(legend.position="none", axis.title = element_blank(), plot.title = element_text(hjust = 0.5))
  increment <- increment + 1
}

# Display plots
# grid.arrange(grobs=plot_pollution, ncol=5, nrow=4)

# Save plots
png('plots/1. timeseries.png', width = 1500, height = 1000)
grid.arrange(grobs=plot_pollution, ncol=5, nrow=4)
dev.off()

print(paste0('[', round(difftime(Sys.time(),start_time, units = 'secs'),1), 's]: ',
             'Variables plot is generated and saved in /plots!'))



