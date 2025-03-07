##### SPECIFY THE LIBRARIES

library(ggsci)
library(dplyr)
library(tidyverse)
library(patchwork)
library(ggdag)
library(dagitty)
library(cobalt)

##### DEFINE THE TERMS
set.seed(123)  # set seed for reproducability
pre_perf = rnorm(100, mean = 3, sd = 0.2) # this is our baseline performance distribution
shock = 6 # this is the time confounder (sometimes called a shock)
effect = 3 # this is the actual effect of the program (100% increase; very good!)
t = 10

######################################################################################################################## 

# Figure 1

##### STEP 1. CREATE THE DATASETS

# Case 1A: pre / post data with no confounders (naive set up)
case1a =  data.frame(
  id = rep(1:100, each = 2),  # 100 employees, each appearing twice (pre / post)
  phase = rep(c("pre", "post"), 100) # the pre / post program phase identifier
) %>% 
  mutate(phase = factor(phase, levels = c("pre", "post"))) # order the variable

case1a$perf = ifelse(case1a$phase == "pre", 
                     pre_perf, 
                     pre_perf + effect) # defining the pre / post-program performance

# our data structure looks like this: 
#      id   phase     perf
# 1     1     pre 3.439762
# 2     1    post 6.262483
# 199 100     pre 3.004197
# 200 100    post 6.249983

# Case 1B: pre / post data with time confounder (i.e., shock)
case1b = data.frame(
  id = rep(1:100, each = t), # 100 employees repeated across 10 timepoints (t)
  time = rep(1:t, 100), # tracking time 1 vs time 10
  phase = c(rep('pre', 
                each = 5), 
            rep('post', 
                each = 5)) # everyone went through the program at time 6
)
case1b$perf = ifelse(case1b$time < 6, 
                     pre_perf, 
                     ifelse(case1b$time == 6, 
                            pre_perf + shock + effect, # time confounder @ t = 6
                            pre_perf + effect)) # regular program effect after
case1b$phase = as.factor(case1b$phase)

# our data structure looks like this: 
#       id time   phase     perf
# 1      1    1     pre 3.439762
# 2      1    2     pre 3.262483
# 999  100    9    post 6.004197
# 1000 100   10    post 6.249983

# Case 1C: pre / post data with shock and staggered adoption 
case1c = data.frame( 
  id = seq_along(1:100), 
  pre = round(runif(100, min = 3, max = 7), 
              0) 
)
case1c$post = case1c$pre + 1
case1c = case1c %>% 
  group_by(pre, post) %>% 
  mutate(cohort = paste0('cohort:', cur_group_id()) ) %>% 
  ungroup() %>% 
  pivot_longer(cols = c(pre, post), 
               names_to = "phase", 
               values_to = "time") %>%
  # lot of conditionals here: 
  ## if folks went through the program @ t = 6, they are exposed to the shock 
  ## if folks did not go through @ t = 6, they are not exposed to the shock
  ## prior to the program, there is no program effect but they could get a shock
  mutate(phase = factor(phase, levels = c("pre", "post")),
         perf = ifelse(phase == "post" & time == 6, 
                       pre_perf + shock + effect,
                       ifelse(phase == "pre" & time == 6, 
                              pre_perf + shock, 
                              ifelse(phase == 'pre' & time != 6, 
                                     pre_perf, 
                                     pre_perf + effect)))) 

# our data structure looks like this: 
#      id cohort   phase  time  perf
# 1     1 cohort:4 pre       6  9.44
# 2     1 cohort:4 post      7  6.26
# 3   100 cohort:2 pre       4  3.00
# 4   100 cohort:2 post      5  6.25

# Case 1D: pre / post data with shock and staggered adoption 
case1d = data.frame(
  id = rep(1:100, each = t), # 100 employees repeated across 10 timepoints (t)
  time = rep(1:t, 100), # tracking time 1 vs time 10
  phase = c(rep(0, each = 5), rep(1, each = 5)) # everyone went through the program at time 6
) %>%
  mutate(
    pre_perf = rnorm(n(), mean = 3, sd = 0.2), # we don't care about this
    perf = pre_perf + time + 3 * phase, # we only care about this perf variable
    phase = ifelse(phase == 0, 'pre', 'post'),
    phase = factor(phase, levels = c("pre", "post"))
)

# our data structure looks like this: 
#       id time phase pre_perf      perf
# 1      1    1   pre 2.797177  3.797177
# 2      1    2   pre 2.841737  4.841737
# 999  100    9  post 2.943852 14.943852
# 1000 100   10  post 3.146820 16.146820

##### STEP 2. ESTIMATE THE EFFECT
# Case 1A t-test
t.test(
  case1a$perf[case1a$phase == "post"],
  case1a$perf[case1a$phase == "pre"], 
  paired = TRUE
)
#   mean difference: 
# 2.991964 <---- estimated difference (very close!)

# Case 1B t-test
t.test(
  case1b$perf[case1b$time == 6],
  case1b$perf[case1b$time == 5], 
  paired = TRUE
)
#   mean difference: 
# 9.076507 <---- estimated difference (not close!)

# Case 1C t-test
# Loop over each unique cohort
unique_cohorts = unique(case1c$cohort)
for (cohort_name in unique_cohorts) {
  # Perform paired t-test
  test_result <- t.test(
    case1c$perf[case1c$cohort == cohort_name & case1c$phase == "post"],
    case1c$perf[case1c$cohort == cohort_name & case1c$phase == "pre"], 
    paired = TRUE
  )
  
  # Print results
  print(paste("T-test results for cohort:", cohort_name))
  print(test_result$estimate)
}

# "T-test results for cohort: cohort:4" = -3.079855 
# "T-test results for cohort: cohort:1" = 2.839101 
# "T-test results for cohort: cohort:3" = 8.938598 
# "T-test results for cohort: cohort:2" = 2.926431 
# "T-test results for cohort: cohort:5" = 3.139341 
# <---- most cohorts show modest improvement (closer to real effect)

# Case 1D t-test
t.test(
  case1d$perf[case1d$time == 6],
  case1d$perf[case1d$time == 5], 
  paired = TRUE
)
#   mean difference: 
# 4.008672 <---- estimated difference (biased!)

summary(lm(perf ~ time + phase, data = case1d))
# time        0.986960   0.004156   237.5   <2e-16 ***
# programpost 3.003796   0.023877   125.8   <2e-16 *** <---- estimated difference (very close!)
  

##### STEP 3. SUMMARY TABLES
summary1A = case1a %>% group_by(phase) %>% summarise(mean_performance = mean(perf, na.rm = T))
summary1B = case1b %>% group_by(time, phase) %>% summarise(mean_performance = mean(perf, na.rm = TRUE), .groups = "drop")
summary1C = case1c %>% group_by(cohort, phase) %>% summarise(mean_performance = mean(perf, na.rm = T))
summary1D = case1d %>% group_by(time, phase) %>% summarise(mean_performance = mean(perf, na.rm = TRUE), .groups = "drop")

##### STEP 4. SUMMARY PLOTS
p1a = ggplot(summary1A, aes(x = phase, y = mean_performance, fill = phase))  + 
  geom_col(position = "dodge", color = "black", width = .3) +  # Black border around bars
  scale_fill_manual(values = c("pre" = "#97bbf5", "post" = "#4269d0")) +  # Custom colors
  annotate("label", 
           x = .5, 
           y = max(case1a$perf) * .8, 
           label = "• pairwise t.test est. = 2.99", 
           fill = "lightyellow",  # Background color
           color = "black",  # Text color
           label.size = 0,  # Border thickness
           size = 3, 
           fontface = "bold",
           hjust = 0) + 
  labs(
    title = "a. naive",
    x = "time",
    y = "average performance"
  ) +
  scale_x_discrete(labels = c("1" = "pre-program", "2" = "post-program")) + 
  theme_classic() +
  theme(
    panel.grid.major.x = element_blank(),  # Grid alignment
    panel.grid.minor.x = element_blank(),  # Remove minor grid lines
    plot.title = element_text(hjust = 0, size = 10, face = "bold"),  
    axis.title = element_text(size = 10),
    legend.position = "none") 
  
p1a

p1b = ggplot(summary1B, aes(x = time, y = mean_performance, group = phase)) +
  geom_col(aes(fill = phase, alpha = ifelse(time < 5 | time > 6, 0.4, 1)), position = "dodge", color = "black", width = .5) + 
  scale_fill_manual(values = c("pre" = "#97bbf5", "post" = "#4269d0")) +
  annotate("label", 
           x = 1.2, 
           y = max(case1b$perf) * .6, 
           label = "• the actual effect is 3 but the t.test est. = 9.07", 
           fill = "lightyellow",
           color = "black",
           label.size = 0,
           size = 3,
           fontface = "bold",
           hjust = 0) + 
  geom_line(color = "black", 
            aes(alpha = ifelse(time < 5 | time > 6, .6, 1)), 
            size = 1) +
  geom_point(color = "black", 
             aes(alpha = ifelse(time < 5 | time > 6, .6, 1)), 
             size = 2) +  # Add points to the line
  geom_segment(aes(x = 5, xend = 6, 
                   y = mean_performance[time == 5], 
                   yend = mean_performance[time == 6]), 
               linetype = "dashed", color = "red", size = 1) +
  scale_x_continuous(breaks = 1:10, limits = c(1, 10)) +  
  labs(
    title = "b. shock",
    x = "time",
    y = "average performance",
    fill = "program phase"
  ) +
  theme_classic() +
  theme(
    panel.grid.major.x = element_blank(),  # Grid alignment
    panel.grid.minor.x = element_blank(),  # Remove minor grid lines
    plot.title = element_text(hjust = 0, size = 10, face = "bold"),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) +
  guides(alpha = "none")  # Remove alpha legend
p1b

p1c = ggplot(summary1C, aes(x = phase, y = mean_performance, group = phase)) +
  geom_col(aes(fill = phase), position = "dodge", color = "black", width = .5) +  
  scale_fill_manual(values = c("pre" = "#97bbf5", "post" = "#4269d0")) +
  # Labels and theme
  labs(
    title = "c. staggered adoption",
    x = "time",
    y = "average performance",
    fill = "program phase"
  ) +
  theme_classic() +
  theme(
    panel.grid.major.x = element_blank(),  # Grid alignment
    panel.grid.minor.x = element_blank(),  # Remove minor grid lines
    plot.title = element_text(hjust = 0, size = 10, face = "bold"),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
  ) + 
  guides(alpha = "none") + # Remove alpha legend
  facet_wrap(~cohort, ncol = 5) + 
  geom_label(data = summary1C %>% filter(cohort %in% c("cohort:5")), 
             aes(x = .5, 
                 y = max(summary1C$mean_performance) * 0.8, 
                 label = " • cohort 1 est.: 2.99 \n • cohort 2 est.: 3.06 \n • cohort 3 est.: 8.97 \n • cohort 4 est.: -3.04 \n • cohort 5 est.: 2.91 "), 
             size = 3, 
             hjust = 0, 
             fill = "lightyellow",  # Background color
             color = "black",  # Text color
             label.size = 0,  # Border thickness
             fontface = "bold",
             inherit.aes = FALSE)
p1c

p1d = ggplot(summary1D, aes(x = time, y = mean_performance, group = phase)) +
  geom_col(aes(fill = phase, alpha = ifelse(time < 5 | time > 6, 0.4, 1)), 
           position = "dodge", 
           color = "black", 
           width = .5) +  
  geom_line(color = "black", 
            aes(alpha = ifelse(time < 5 | time > 6, .6, 1)), 
            size = 1) +
  geom_point(color = "black", 
             aes(alpha = ifelse(time < 5 | time > 6, .6, 1)), 
             size = 2) +  # Add points to the line
  scale_fill_manual(values = c("pre" = "#97bbf5", "post" = "#4269d0")) +
  geom_segment(aes(x = 5, xend = 6, 
                   y = mean_performance[time == 5], 
                   yend = mean_performance[time == 6]), 
               linetype = "dashed", color = "red", size = 1) +
  scale_x_continuous(breaks = 1:10, limits = c(1, 10)) +  
  labs(
    title = "d. trend",
    x = "time",
    y = "average performance",
    fill = "program phase"
  ) +
  annotate("label", 
           x = 1, 
           y = max(case1d$perf) * 0.9, 
           label = " • est. w/o accounting for trend = 9.07 \n • est. after accounting for trend = 3.00", 
           size = 3, 
           hjust = 0, 
           fill = "lightyellow",  # Background color
           color = "black",  # Text color
           fontface = "bold",
           label.size = 0) +  # Border thickness
  theme_classic() +
  theme(
    panel.grid.major.x = element_blank(),  # Grid alignment
    panel.grid.minor.x = element_blank(),  # Remove minor grid lines
    plot.title = element_text(hjust = 0, size = 10, face = "bold"),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
  ) + guides(alpha = "none")  # Remove alpha legend
p1d



##### STEP 5. CREATE THE COMBINED PLOT

# Apply background color to each individual plot
p1a <- p1a + theme_wsj() + theme(plot.title = element_text(size = 14), axis.title = element_blank(), legend.position = "none")
p1b <- p1b + theme_wsj() + theme(plot.title = element_text(size = 14), axis.title = element_blank(), legend.position = "none")
p1c <- p1c + theme_wsj() + theme(plot.title = element_text(size = 14), axis.title = element_blank(), legend.position = "none")
p1d <- p1d + theme_wsj() + theme(plot.title = element_text(size = 14), axis.title = element_blank(), legend.position = "none")

combined_plot1 <- ggarrange(p1a, p1b, p1c, p1d, 
                           ncol = 2, nrow = 2,  
                           common.legend = TRUE,  
                           legend = "none")
combined_plot1


######################################################################################################################## 

# Figure 2: 

##### Plot DAG
p2a = confounder_triangle(x_y_associated = TRUE) %>% 
  tidy_dagitty() %>% 
  mutate(linetype = ifelse(name == "z", "dashed", "solid")) %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) + 
  geom_dag_point() + 
  geom_dag_text() + 
  geom_dag_edges(aes(edge_linetype = linetype), show.legend = FALSE) + 
  theme_dag() 

##### STEP 1. CREATE DATASET
case2 = data.frame( motivation = rnorm(500, .4, .1)) %>%
  mutate(performance = motivation * 2, 
         phase = rbinom(500, 1, motivation * .8))


##### STEP 2. ESTIMATE MODELS
summary(lm(performance ~ phase, data = case2)) # unadj value
#             Estimate    Std. Error   t    Pr(>|t|)    
# (Intercept)  0.76665    0.01028   74.61  < 2e-16 ***
# phase        0.08981    0.01800    4.99 8.34e-07 ***
  
m.out1 = matchit(phase ~ motivation, data = case2)
m.df = match_data(m.out1)

summary(lm(performance ~ phase, data = m.df)) # adj value
#             Estimate  Std. Error    t     Pr(>|t|)    
# (Intercept) 0.849022   0.013932  60.941   <2e-16 ***
# phase       0.007436   0.019703   0.377    0.706 


total_treated <- case2 %>% filter(phase == 1) %>% nrow()
matched_treated <- m.df %>% filter(phase == 1) %>% nrow()

##### STEP 3. CREATE SUMMARY TABLES
unadj = case2 %>% group_by(phase) %>% summarise(m_perf = mean(performance))  %>% mutate(type = 'unadj')
adj = m.df %>% group_by(phase) %>% summarise(m_perf = mean(performance))  %>% mutate(type = 'adj')

##### STEP 4. PLOT GRAPHS
plotdf = rbind(adj, unadj) %>% 
  mutate(phase = factor(ifelse(phase == 0, 'pre', 'post'), levels = c("pre", "post")),
         type = factor(type, levels = c('unadj', 'adj') ) )

p2b = ggplot(plotdf, aes(x = phase, y = m_perf, fill = phase)) +
  geom_col(position = "dodge", color = "black", width = .3) +  # Black border around bars
  scale_fill_manual(values = c("pre" = "#97bbf5", "post" = "#4269d0")) +  # Custom colors
  facet_wrap(~ type) + 
  labs(
    # title = "b. estimates",
    x = "time",
    y = "average performance",
    fill = "program phase"
  ) +
  labs(
    title = "a. mean differences",
    y = "average performance",
  ) +
  coord_cartesian(ylim = (c(.7, 1))) +
  theme_classic() +
  theme(
    panel.grid.major.x = element_blank(),  # Grid alignment
    panel.grid.minor.x = element_blank(),  # Remove minor grid lines
    plot.title = element_text(hjust = 0, size = 10, face = "bold"),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
  ) + guides(alpha = "none")  # Remove alpha legend


p2c = love.plot(m.out1, var.order = "unadjusted", abs = TRUE, line = FALSE, 
                thresholds = c(m = .1, ks = .05), colors = c("#FF725C", "#4269D0"), 
                shapes = c("triangle filled", "circle filled")) +
  annotate("text", x = 0.8, y = Inf, 
           label = paste("Matched Obs: ", matched_treated, "(", round((matched_treated / total_treated) * 100, 1), "% )"),
           hjust = 1, vjust = 1, size = 3, color = "black") + 
  labs(
    title = "b. covariate balance",
  )


p2b = p2b + theme_wsj() + theme(plot.title = element_text(size = 14), axis.title = element_blank(), legend.position = "none") 
p2c = p2c + theme_wsj() + theme(plot.title = element_text(size = 14), legend.title = element_blank())

combined_plot2 <- p2a | (p2b / p2c) + plot_layout(widths = c(2, 1))  
combined_plot2


######################################################################################################################## 

# Figure 3: 

##### STEP 1. CREATE DATASETS

# Case 3a. Program group has the same pre-program performance as control
case3a = data.frame(
  id = rep(1:100, each = t), # 100 employees repeated across 10 timepoints (t)
  time = rep(1:t, 100) # tracking time 1 vs time 10
) %>%
  left_join(., data.frame(id = seq_along(1:100), 
                          p = rbinom(100, 1, .5)), by = "id"
  ) %>%
  mutate(
    phase = ifelse(time > 5, 1, 0),
    pre_perf = rnorm(n(), mean = 3, sd = 0.2), # same as before
    perf = pre_perf + time + 3 * phase * p,
    phase = ifelse(phase == 0, 'pre', 'post'),
    phase = factor(phase, levels = c("pre", "post")),
    p = recode(p, 
               `0` = 'control',
               `1` = 'program')
  ) 

# Case 3b. Program group has higher pre-program performance compared to control
obs = data.frame(
  id = rep(1:100, each = t), # 100 employees repeated across 10 timepoints (t)
  time = rep(1:t, 100) # tracking time 1 vs time 10
) %>%
  left_join(., data.frame(id = seq_along(1:100), 
                          p = rbinom(100, 1, .5)), by = "id"
  ) %>%
  mutate(
    phase = ifelse(time > 5, 1, 0),
    pre_perf = rnorm(n(), mean = 3, sd = 0.2), # same as before
    perf = ifelse(p == 1, pre_perf + time + 3 * phase + p, pre_perf + time + p),
    phase = ifelse(phase == 0, 'pre', 'post'),
    phase = factor(phase, levels = c("pre", "post"))
  ) 

counter = obs %>% filter(p == 1) %>% mutate(perf = pre_perf + time + p,
                                            p = 2)
case3b = rbind(obs, counter) %>% mutate(p = recode(p, 
                                                   `0` = 'control',
                                                   `1` = 'program',
                                                   `2` = 'counterfactual'))

##### STEP 2. CREATE SUMMARY TABLES
summary3A = case3a %>% group_by(p, time, phase) %>% summarise(m_perf = mean(perf, na.rm = T))
summary3B = case3b %>% group_by(p, time, phase) %>% summarise(m_perf = mean(perf, na.rm = T))


##### STEP 3. CREATE PLOTS
p3a = ggplot(summary3A, aes(x = time, y = m_perf, group = p, color = p)) +
  geom_line(size = .5) +  # Line plot
  geom_point(size = 1) +  # Add points for clarity
  scale_color_manual(values = c("control" = "#97bbf5", "program" = "#4269d0")) + 
  scale_x_continuous(breaks = 1:10, limits = c(1, 10)) +  
  geom_segment(data = summary3A %>% filter(p %in% c("control", "program")) %>%
                 group_by(p) %>% slice_max(time, n = 1),  # Get last counterfactual & last program points
               aes(x = time, xend = time, 
                   y = min(m_perf), yend = max(m_perf)),  # Vertical line between the two points
               color = "red", linetype = "dashed", size = 0.8) +
  annotate("label", 
           x = 8, 
           y = max(summary3A$m_perf) * 0.9, 
           label = " DiD Effect", 
           size = 4, 
           hjust = 0, 
           fill = NA,
           color = "black",  # Text color
           fontface = "bold",
           label.size = 0) +  # Border thickness
  labs(
    x = "Program Phase",
    y = "Performance",
    title = "A. DiD w/o initial differences",
    color = "Group",
    linetype = "Group"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.position = "bottom"
  )

p3b = ggplot(summary3B, aes(x = time, y = m_perf, group = p, color = p, linetype = p)) +
  geom_line(size = .5) +  # Line plot
  geom_point(size = 1) +  # Add points for clarity
  scale_x_continuous(breaks = 1:10, limits = c(1, 10)) +  
  geom_segment(data = summary3B %>% filter(p %in% c("counterfactual", "program")) %>%
                 group_by(p) %>% slice_max(time, n = 1),  # Get last counterfactual & last program points
               aes(x = time, xend = time, 
                   y = min(m_perf), yend = max(m_perf)),  # Vertical line between the two points
               color = "red", linetype = "dashed", size = 0.8) +
  scale_linetype_manual(values = c("control" = "solid", "program" = "solid", "counterfactual" = "dotted")) + 
  scale_color_manual(values = c("control" = "#97bbf5", "program" = "#4269d0", "counterfactual" = "#9498A0")) + 
  labs(
    x = "Time",
    y = "Performance",
    title = "B. DiD w/ initial differences",
    color = "Group",
    linetype = "Group"
  )  +
  annotate("label", 
           x = 8, 
           y = max(summary3B$m_perf) * 0.9, 
           label = " DiD Effect", 
           size = 4, 
           hjust = 0, 
           color = "black",  # Text color
           fontface = "bold",
           fill = NA,
           label.size = 0) +  # Border thickness
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.position = "bottom"
  )

p3a <- p3a + theme_wsj() + theme(plot.title = element_text(size = 14), axis.title = element_blank(), legend.position = "none")
p3b <- p3b + theme_wsj() + theme(plot.title = element_text(size = 14), axis.title = element_blank(), legend.position = "none")

combined_plot3 <- ggarrange(p3a, p3b,
                            ncol = 2,  
                            common.legend = TRUE,  
                            legend = "none")
combined_plot3


combined_plot1
combined_plot2
combined_plot3


