######We want {tidyverse} as usual, but also {Cairo} for saving our plots####
library(tidyverse)
library(Cairo)
library(Hmisc)
####Load the data###
mydata <- readRDS("FALSE_DATA_2018_census_education_week4.rds")
####Exploring Categorical Data#####
mydata %>% 
  ggplot(aes(x=prop_abs_grp)) +
  geom_bar()
####Create a barchart of gender counts within each category#####
mydata %>% 
  ggplot(aes(x=prop_abs_grp, fill = gender)) + 
  geom_bar()
####A side-by-side barchart might be a better option here######
mydata %>%  
  filter(!is.na(gender) & !is.na(prop_abs_grp)) %>% 
  ggplot(aes(x = prop_abs_grp, fill = gender)) +
  geom_bar(position = "dodge")
#####Can you produce overlapping transparent bars?#####
mydata %>%  
  filter(!is.na(gender) & !is.na(prop_abs_grp)) %>% 
  ggplot(aes(x = prop_abs_grp, fill = gender)) +
  geom_bar(position = position_dodge(width = 0.5), alpha = 0.6)
#####Create a stacked barchart of gender proportions within prop_abs_grp on the x-axis by setting the position argument####
mydata %>%  
  filter(!is.na(gender) & !is.na(prop_abs_grp)) %>% 
  ggplot(aes(x = prop_abs_grp, fill = gender)) +
  geom_bar(position = "fill")
####Now create horizontal barplot plot (coord_flip)####
mydata %>%
  filter(!is.na(gender) & !is.na(prop_abs_grp)) %>%
  ggplot(aes(x = gender, fill = prop_abs_grp)) +
  geom_bar(position = "fill") +
  coord_flip()
####If you want side-by-side proportions you need to calculate them by hand##
mydata %>% 
  filter(!is.na(gender) & !is.na(prop_abs_grp)) %>%
  group_by(prop_abs_grp, gender) %>% 
  count() %>% 
  group_by(gender) %>%
  mutate(proportion = n / sum(n)) %>% 
  ggplot(aes(x = gender, y = proportion, fill = prop_abs_grp)) +
  geom_bar(stat = "identity", position = "dodge")
####Facet one of the barplots by e.g. freemeal####
mydata %>%
  filter(!is.na(gender) & !is.na(prop_abs_grp)) %>%
  ggplot(aes(x = gender, fill = prop_abs_grp)) +
  geom_bar(position = "fill") +
  facet_wrap(~ freemeal)
####Plot a histogram of exclusion duration (sum_duration)####
mydata %>% 
  ggplot(aes(x = sum_duration)) +
  geom_histogram() 
###Most of pupils have no exclusions, so it might be a good idea to exclude#####
###Most of pupils have no exclusions, so it might be a good idea to exclude those with sum_duration == 0###
mydata %>% 
  filter(sum_duration != 0) %>% 
  ggplot(aes(x = sum_duration)) +
  geom_histogram()
####Always good idea to check for different bins, so experiment with binwidth####
mydata %>% 
  filter(sum_duration != 0) %>%   
  ggplot(aes(x = sum_duration)) +
  geom_histogram(binwidth = 1)
###Create a density plot###
mydata %>%
  filter(sum_duration != 0) %>%    
  ggplot(aes(x = sum_duration)) +
  geom_density() 
####Change the smoothing bandwidth to be used####
default_bw <- density(na.omit(mydata$sum_duration))$bw
mydata %>% 
  filter(sum_duration != 0) %>%    
  ggplot(aes(x = sum_duration)) +
  geom_density(bw = 2 * default_bw)
#####Density by gender#####
mydata %>% 
  filter(sum_duration != 0) %>%  
  ggplot(aes(x = sum_duration, fill = gender)) +
  geom_density() 
# Default density plots are not very helpful as they get in the way of each 
# other. We can either change the transparency with alpha, facet or use colour 
# instead of fill

mydata %>% 
  filter(sum_duration != 0) %>%  
  ggplot(aes(x = sum_duration, fill = gender)) +
  geom_density(alpha = 0.4) 

mydata %>% 
  filter(sum_duration != 0) %>%  
  ggplot(aes(x = sum_duration, fill = gender)) +
  geom_density() +
  facet_wrap( ~ gender)

mydata %>% 
  filter(sum_duration != 0) %>%  
  ggplot(aes(x = sum_duration, col = gender)) +
  geom_density(lwd = 1)
# Scatterplots and boxplots#######
# Make a scatter plot with n_exclusions on the x-axis and sum_duration on the y-axis
mydata %>% 
  ggplot(aes(x = n_exclusions, y = sum_duration)) +
  geom_point()
####Seems likely that there's overplotting here####
mydata %>% 
  ggplot(aes(x = n_exclusions, y = sum_duration)) +
  geom_jitter(alpha = 0.3, width = 0.2)
####Plot the same variables (sum_duration by n_exclusions) using geom_boxplot() 
# and interpret the results.
mydata %>% 
  ggplot(aes(x = n_exclusions, y = sum_duration, group = n_exclusions)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3, col = "red") 
######Create now a boxplot for average exclusion duration by number of exclusions 
##### and facet by age and gender. Try using facet_grid() this time
mydata %>%
  filter(!is.na(gender)) %>%
  filter(agep0 %in% 6:9) %>%
  mutate(avg_duration = sum_duration / n_exclusions) %>%
  ggplot(aes(x = n_exclusions, y = avg_duration, group = n_exclusions)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3, col = "red") +
  facet_grid(gender ~ agep0)
#####Regression lines####
mydata %>% 
  ggplot(aes(x = as.numeric(slsdobmt), y = sum_duration)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm")
# Say we think this relationship between birthmonth and exclusion duration
# might vary by council area.
mydata %>% 
  ggplot(aes(x = as.numeric(slsdobmt), y = sum_duration)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ councilarea)
# To avoid "Aberdeen First!" let's reorder this plot by the number of children
# in each council area. We can call fct_reorder() from within our call to 
# ggplot2() to reorder council area by synid,
mydata %>% 
  ggplot(aes(x = as.numeric(slsdobmt), y = sum_duration)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ fct_reorder(councilarea, synid, .fun = NROW, .desc = TRUE))
####Exporting your plots###3
ca_duration_mnth_plot <- mydata %>% 
  ggplot(aes(x = as.numeric(slsdobmt), y = sum_duration)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm") +
  facet_wrap(~ fct_reorder(councilarea, synid, .fun = NROW, .desc = TRUE))
####then pass this object to ggsave() and specify the filename####
ggsave(plot = ca_duration_mnth_plot, 
       file = "exclusion-duration-month-council.png", 
       type = "cairo")
###Formatting your plot (e.g. for presentations or publications)####
###Create a plot of your choice####
myplot <- ggplot(mydata, aes(x = newmhutype0, fill = no_sibs_grp)) +
  geom_bar(position = "fill")
#####To change labels you can use labs()######
myplot_with_labs <- myplot + labs(title = "My plot title", 
                                  subtitle = "A subtitle",
                                  x = "X-axis label",
                                  y = "Y-axis label",
                                  caption = "Data source: SLS",
                                  fill = "Number of siblings")
myplot_with_labs
#####Let's change the font#####
myplot_with_labs + theme_bw(base_size = 14) 
####or you can change each element separately########
myplot_with_labs + theme(axis.text = element_text(size = 12, face = "bold"),
                         plot.title = element_text(colour = "red"))
##To rotate labels on X-axis use angle argument of element_text()######
####Adjust the horizontal justification (hjust) property######
myplot_with_labs + 
  theme(axis.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(angle = -30, hjust = 0))

###The same plot again but without the hjust setting on the x-axis text#####
myplot_with_labs + 
  theme(axis.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(angle = -30))
#####To draw legend at the bottom###
myplot + theme(legend.position = "bottom")
# or to remove it
myplot + theme(legend.position = "none")
# To change fill colour you can use e.g. scale_*_brewer() and different
# type options: seq (sequential), div (diverging) or qual (qualitative)
myplot + scale_fill_brewer(type = "qual")
myplot + scale_fill_brewer(type = "seq")
myplot + scale_fill_brewer(type = "div")
# and different palette
myplot + scale_fill_brewer(type = "qual", palette = 3)
####Advanced statistical summary in ggplot####
####we'll create a line graph#####
mydata %>%
  filter(!is.na(gender)) %>%
  filter(agep0 %in% 6:10) %>%  # arbitrarily pick a range of ages for simplicity
  ggplot(aes(x = agep0, y = n_exclusions)) +
  geom_line()
#An alternative using `stat_summary()` and specifying `mean` as the function####
mydata %>%
  filter(!is.na(gender)) %>%
  filter(agep0 %in% 6:10) %>%
  ggplot(aes(x = agep0, y = n_exclusions)) +
  stat_summary(geom = "line", fun = mean)
####Advanced: 'group' aesthetic in line charts####
mydata %>%
  filter(!is.na(gender)) %>%
  filter(agep0 %in% 6:10) %>%
  group_by(agep0, gender) %>%
  summarise(mean_exclusions = mean(n_exclusions), .groups = "drop") %>%
  ggplot(aes(x = agep0, y = mean_exclusions, shape = gender)) +
  geom_line() + 
  geom_point(size = rel(3)) 

##You can add multiple aesthetics using the same variable making the lines distinct##
mydata %>%
  filter(!is.na(gender)) %>%
  filter(agep0 %in% 6:10) %>%
  group_by(agep0, gender) %>%
  summarise(mean_exclusions = mean(n_exclusions), .groups = "drop") %>%
  ggplot(aes(
    x = agep0,
    y = mean_exclusions,
    shape = gender,
    colour = gender,
    linetype = gender,
    linewidth = gender
  ))+
  geom_line() + 
  geom_point(size = rel(3))  
###Like with the bar graph above, we can offset the points and lines to make them more distinct when they overlap
mydata %>%
  filter(!is.na(gender)) %>%
  filter(agep0 %in% 6:10) %>%
  group_by(agep0, gender) %>%
  summarise(mean_exclusions = mean(n_exclusions), .groups = "drop") %>%
  ggplot(aes(x = agep0, y = mean_exclusions, shape = gender)) +
  geom_line(position = position_dodge(width = 0.5)) + 
  geom_point(position = position_dodge(width = 0.5), size = rel(3))