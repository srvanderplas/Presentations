
theme_stimuli <- function(base_size = 12, base_family = ""){
  theme_grey(base_size = base_size, base_family = base_family) +
    theme(legend.background = element_blank(), 
          legend.key = element_blank(), 
          panel.background = element_rect(fill="white",color="black"), 
          panel.border = element_blank(), 
          axis.title = element_blank(),
          strip.background = element_rect(fill="grey80",color="black"), 
          plot.background = element_blank(),
          panel.grid.major = element_line(color="grey85"),
          axis.text=element_blank())
}