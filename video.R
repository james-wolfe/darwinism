vid = evoo2 %>% 
  select(!'...1') %>%
  mutate(time = rep(1:48, each = 99),
         y = rep(0:98, times = 48)) %>% 
  pivot_longer(cols = '0':'98', names_to = "x", values_to = "val") %>%
  ggplot(aes(x = as.numeric(x), y = y, fill = as.factor(val), group = 1)) + 
  scale_fill_manual(values = c("navyblue","firebrick4","gold","forestgreen"),
                      breaks = c(0,1,2,3)) +
  geom_raster() + 
  theme_void() +
  theme(legend.position = "none") +
  transition_time(time)

anim_save("output.gif", 
          animation = vid, 
          duration = 6, 
          fps = 20, 
          width = 800, 
          height = 800, 
          renderer = gifski_renderer())