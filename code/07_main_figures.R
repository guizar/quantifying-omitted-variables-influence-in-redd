library(patchwork)
library(ggbreak)
library(rnaturalearth)
library(rnaturalearthdata)
library(countrycode)
library(sf)

# ------------------------------------------------------------------------------------------------------------
#  Load details

# --- For consistency we will build for the objects constructed int he previous exploratory plots
source(file.path("code", "05_plot_ATEs.R"), echo = TRUE)
source(file.path("code", "06_sensitivity_analysis.R"), echo = TRUE)

# Loads Consbio2022
d_guizar = read_csv(file.path("data", "guizar_2022.csv"))
d_guizar %<>% mutate(guizar = 0-(rate_diff*100))

# list projects analysed in this study (all)
comp_meths$vcs_id <- str_extract(comp_meths$proj_id, "_.*")
comp_meths$vcs_id <- sub("_", "", comp_meths$vcs_id)
comp_meths %<>% left_join(d_guizar %>% select(vcs_id,guizar))

# compute additional metrics
comp_meths <-  comp_meths %>% left_join(proj_tab %>% select(vcs_id=project, area_ha))
comp_meths <-  comp_meths %>% left_join(claims %>% select(vcs_id,avoided_verra))
comp_meths <- comp_meths %>% mutate(ate_ha = area_ha*((ate_yr*0.01)*5), ate_se_ha = area_ha*((ate_yr_se*0.01)*5))
comp_meths <- comp_meths %>% mutate(guizar_ha = area_ha*(guizar*5)*0.01)

# Add 4C estimates
comp_meths <-  comp_meths %>% left_join(proj_comp %>% select(vcs_id=project, implied_ate))

# Loads project info
proj_info <- read_csv(file = file.path("data", "vcs-info.csv")) %>% filter(inclusion_status!= 'Unavailable\ngeospatial data') %>% 
mutate(proj_id = paste0(iso3, "_", vcs_id))

# Select the design table for the chosen distance metric and alpha
design_tab <- ordl[[dist_use]][[as.character(alpha_use)]]

# add matched prop info to proj_info
proj_info <- proj_info %>% left_join(design_tab)

# Generate a table with the projects included in the analysis (n=36)
# for further plotting/examination
comp_meths_sub <- comp_meths %>% filter(!proj_id %in% projects_with_low_matched_prop)

# --------------------------------------------------------------------------- 
# Generate palettes

# models cols
base_col= carto_pal(7, "Temps")[c(3,1,5,7)] # c("#38293", "#3B5249", "#519872", "#A4B494")
col_palette = generate_palette(base_col[1], modification = "go_darker", n_colours = length(levels(comp_meths$method)), view_palette = F)

# for the two panelmodels
col_palette = c("#de700fff", "#c78c58ff", col_palette[3:length(col_palette)])

# plot DR as black
col_palette[length(col_palette)] <- "#000000"

#  -- maps colors
# Loaded in 00_preamble.R: Declare project labels according to project's inclusion in this study
# scales::show_col(map_cols)
# scales::show_col(col_palette)

guizar_colour <- "#1c1c1c" 
grey_color <- "#1e1e1e"
red_color <- "#c34242"

dodge_width <- 0.7

#############################################
#  ----------- SI PLOT ---------
#############################################

# Output file params
filename_out <- "SI_effect_sizes_fig.png"
file_main_plot <- file.path("figures", filename_out)

# TOP -----------

# Create the plot: ATE estimates by method across projects
top_panel <- ggplot(comp_meths, aes(x = proj_id, y = ate_yr, color = method)) +
  geom_hline(lwd = 0.5, yintercept = 0) +  
  geom_errorbar(aes(ymin = ate_yr - 2 * ate_yr_se, ymax = ate_yr + 2 * ate_yr_se), 
                width = 0.2, position = position_dodge(width = dodge_width)) +  
geom_point(position = position_dodge(width = dodge_width)) +  

 geom_point(aes(proj_id,guizar), color=guizar_colour, shape=3, size=1, alpha=0.6) + # Adds Guizar
xlab ("") +
ylab("Diff. in forest loss (%"~yr^-1*')') +
  theme_bw() +  
  theme(panel.border = element_rect( fill=NA, colour = "black", size=1),
        axis.text.x = element_text(angle = 90, hjust = 1),  
        legend.title=element_blank(),
        axis.line = element_blank(), 
        legend.position = "bottom", 
        axis.title.x = element_text(margin = ggplot2::margin(t = 1)),
        axis.title.y = element_text(margin = ggplot2::margin(r = 1)),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "in"),
        legend.margin=ggplot2::margin(0.1,0.1,0.1,0.1),
        legend.box.spacing = unit(0.001, "in"),
        ) +  
  
  scale_color_manual(values=col_palette, name="Method") +

  # Custom colouring for the x-axis labels
  scale_x_discrete(limits = order_vcs,  # Maintain project order on x-axis
                   labels = function(proj_id) {
                     ifelse(proj_id %in% projects_with_low_matched_prop, 
                            paste0(proj_id), proj_id)  # Label with project ID
                   }) +
  
  # Define color for x-axis labels: Change for problematic sites
  theme(axis.text.x = element_text(color = ifelse(order_vcs %in% projects_with_low_matched_prop, axis_text_color_dimmed, "black"),
    face = ifelse(order_vcs %in% projects_with_low_matched_prop, "bold", "bold")
    )
  ) 

# --- Middle panel ---
order_vcs <- comp_meths %>%
  select(proj_id, ate_ha) %>%
  group_by(proj_id) %>%  summarise(ate_ha = mean(ate_ha)) %>%
  arrange(ate_ha) %>%
  pull(proj_id)  %>% as.character()

middle_panel <- ggplot(comp_meths, aes(x = proj_id, y = ate_ha, color = method)) +
  geom_hline(lwd = 0.5, yintercept = 0) +  
  geom_errorbar(aes(ymin = ate_ha - 2 * ate_se_ha, ymax = ate_ha + 2 * ate_se_ha), 
                width = 0.2, position = position_dodge(width = dodge_width)) +  
geom_point(position = position_dodge(width = dodge_width)) +  # Points for ATE

geom_point(aes(proj_id,guizar_ha), color=guizar_colour, shape=3, size=1, alpha=0.6) + # Adds Guizar
xlab ("") +
ylab("Diff. in forest loss (Ha)") +
  theme_bw() +  
  theme(panel.border = element_rect( fill=NA, colour = "black", size=1),
        axis.text.x = element_text(angle = 90, hjust = 1),  # Rotate x-axis labels
        legend.title=element_blank(),
        axis.line = element_blank(), 
        legend.position = "bottom", 
        axis.title.x = element_text(margin = ggplot2::margin(t = 1)),
        axis.title.y = element_text(margin = ggplot2::margin(r = 1)),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "in"),
        legend.margin=ggplot2::margin(0.1,0.1,0.1,0.1),
        legend.box.spacing = unit(0.001, "in"),
        ) +  
  
  scale_color_manual(values=col_palette, name="Method") +

  scale_x_discrete(limits = order_vcs,  # Maintain project order on x-axis
                   labels = function(proj_id) {
                     ifelse(proj_id %in% projects_with_low_matched_prop, 
                            paste0(proj_id), proj_id)  # Label with project ID
                   }) +
  
  # Define color for x-axis labels: Change for problematic sites
  theme(axis.text.x = element_text(color = ifelse(order_vcs %in% projects_with_low_matched_prop, axis_text_color_dimmed, "black"),
    face = ifelse(order_vcs %in% projects_with_low_matched_prop, "bold", "bold")
    )
) 

# Bottom LEFT ------
order_vcs <- comp_meths %>% 
  filter(method==methnames$lm_simple) %>%
  select(proj_id, ate_yr) %>%
  group_by(proj_id) %>%  summarise(ate_yr = mean(ate_yr)) %>%
  arrange(ate_yr) %>%
  pull(proj_id)  %>% as.character()

bottom_left <- ggplot(comp_meths %>% filter(method==methnames$lm_simple), aes(x = proj_id, y = ate_yr)) +
  geom_hline(lwd = 0.5, yintercept = 0) +  
  geom_point(position = position_dodge(width = dodge_width), color=grey_color) +  # Points for ATE
  geom_errorbar(aes(ymin = ate_yr - 2 * ate_yr_se, ymax = ate_yr + 2 * ate_yr_se), 
                width = 0.2, position = position_dodge(width = dodge_width), color=grey_color) +  
 geom_point(aes(proj_id,implied_ate), color=red_color, shape=19, size=1) + # Adds 4C
xlab ("") +
ylab("Diff. in forest loss (%"~yr^-1*')') +
# scale_y_break(c(-14, -95)) +

# Custom coloring for the x-axis labels
scale_x_discrete(limits = order_vcs,  # Maintain project order on x-axis
                   labels = function(proj_id) {
                     ifelse(proj_id %in% projects_with_low_matched_prop, 
                            paste0(proj_id), proj_id)  # Label with project ID
                   }) +
  
# Define color for x-axis labels: Change for problematic projects
theme_bw() + 
  theme(panel.border = element_rect( fill=NA, colour = "black", size=1),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "in"),
        legend.title=element_blank(),
        # axis.text.y = element_text(margin = ggplot2::margin(r = 1)),
        axis.line = element_blank(), 
        legend.position = "bottom", 
        axis.title.x = element_text(margin = ggplot2::margin(t = 1)),
        axis.title.y = element_text(margin = ggplot2::margin(r = 1)),
        axis.text.x.top = element_blank(),  # Remove top x-axis text
        axis.ticks.x.top = element_blank(),
        axis.text.x = element_text(
            color = ifelse(order_vcs %in% projects_with_low_matched_prop, axis_text_color_dimmed, "black"),
            face = ifelse(order_vcs %in% projects_with_low_matched_prop, "bold", "bold"),
            angle = 90, hjust = 1
            )
        ) 

# Bottom RIGHT ------
order_vcs <- comp_meths %>% 
  filter(method==methnames$lm_simple) %>%
  select(proj_id, ate_ha) %>%
  group_by(proj_id) %>% summarise(ate_ha = mean(ate_ha)) %>%
  arrange(ate_ha) %>%
  pull(proj_id)  %>% as.character()

bottom_right <- ggplot(comp_meths %>% filter(method==methnames$lm_simple), aes(x = proj_id, y = ate_ha)) +
  geom_hline(lwd = 0.5, yintercept = 0) +  
  geom_point(position = position_dodge(width = dodge_width), color=grey_color) +  # Points for ATE
  geom_errorbar(aes(ymin = ate_ha - 2 * ate_se_ha, ymax = ate_ha + 2 * ate_se_ha), 
                width = 0.2, position = position_dodge(width = dodge_width), color=grey_color) +  
 geom_point(aes(proj_id,avoided_verra), color=red_color, shape=19, size=1) + # Adds 4C
xlab ("") +
ylab("Diff. in forest loss (Ha)") +

# Custom coloring for the x-axis labels
scale_x_discrete(limits = order_vcs,  # Maintain project order on x-axis
                   labels = function(proj_id) {
                     ifelse(proj_id %in% projects_with_low_matched_prop, 
                            paste0(proj_id), proj_id)  # Label with project ID
                   }) +
  
# Define color for x-axis labels: Change for problematic projects
theme_bw() + 
  theme(panel.border = element_rect( fill=NA, colour = "black", size=1),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "in"),
        legend.title=element_blank(),
        # axis.text.y = element_text(margin = ggplot2::margin(r = 1)),
        axis.line = element_blank(), 
        legend.position = "bottom", 
        axis.title.x = element_text(margin = ggplot2::margin(t = 1)),
        axis.title.y = element_text(margin = ggplot2::margin(r = 1)),
        axis.text.x.top = element_blank(),  # Remove top x-axis text
        axis.ticks.x.top = element_blank(),
        axis.text.x = element_text(
            color = ifelse(order_vcs %in% projects_with_low_matched_prop, axis_text_color_dimmed, "black"),
            face = ifelse(order_vcs %in% projects_with_low_matched_prop, "bold", "bold"),
            angle = 90, hjust = 1
            )
        ) 

#  --------- PLOT OUT ---------
  gg_out <- top_panel / middle_panel / (bottom_left | bottom_right)   +  plot_layout(
    guides = "collect", 
    heights = c(0.8, 0.8, 1.5),
    ncol = 1,  
    nrow = 3   
  ) & 
  plot_annotation(tag_levels = 'a') &  # Add tags to panels
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5), 
    legend.position = "bottom",
    plot.margin = unit(c(0.01,0.01,0.01,0.01), "in"),
          legend.margin=ggplot2::margin(0.1,0.1,0.1,0.1),
          legend.box.spacing = unit(0.001, "in"),
          axis.text.x.top = element_blank(),  # Remove top x-axis text
          axis.ticks.x.top = element_blank()
  )

ggsave(file_main_plot, gg_out, width=12, height=12, units='in', dpi=300)

#############################################
#  ----------- MAIN PLOT ---------
#############################################

# Output file params
filename_out <- "Main_effect_sizes_fig.png"
file_main_plot <- file.path("figures", filename_out)

# TOP -----------
# Create the plot: ATE estimates by method across projects

top_panel <- ggplot(comp_meths_sub, aes(x = proj_id, y = ate_yr, color = method)) +
  geom_hline(lwd = 0.5, yintercept = 0) +  
  geom_errorbar(aes(ymin = ate_yr - 2 * ate_yr_se, ymax = ate_yr + 2 * ate_yr_se), 
                width = 0.2, position = position_dodge(width = dodge_width)) +  
geom_point(position = position_dodge(width = dodge_width)) +  # Points for ATE
 geom_point(aes(proj_id,guizar), color=guizar_colour, shape=3, size=1, alpha=0.6) + # Adds Guizar
xlab ("") +
ylab("Diff. in forest loss (%"~yr^-1*')') +
  theme_bw() +  
  theme(panel.border = element_rect( fill=NA, colour = "black", size=1),
        axis.text.x = element_text(angle = 90, hjust = 1, face = "bold"),  # Rotate x-axis labels
        legend.title=element_blank(),
        axis.line = element_blank(), 
        legend.position = "bottom", 
        axis.title.x = element_text(margin = ggplot2::margin(t = 1)),
        axis.title.y = element_text(margin = ggplot2::margin(r = 1)),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "in"),
        legend.margin=ggplot2::margin(0.1,0.1,0.1,0.1),
        legend.box.spacing = unit(0.001, "in"),
        ) +  
  
  scale_color_manual(values=col_palette, name="Method") +
  
  # Define color for x-axis labels: Change for problematic sites
  theme(face = "bold")

# --- Middle panel ---
order_vcs <- comp_meths %>%
  select(proj_id, ate_ha) %>%
  group_by(proj_id) %>%  summarise(ate_ha = mean(ate_ha)) %>%
  arrange(ate_ha) %>% mutate(proj_id = as.character(proj_id)) %>% filter(!proj_id %in% projects_with_low_matched_prop) %>%  pull(proj_id)  %>% as.character() 

middle_panel <- ggplot(comp_meths_sub, aes(x = proj_id, y = ate_ha, color = method)) +
  geom_hline(lwd = 0.5, yintercept = 0) +  
  geom_errorbar(aes(ymin = ate_ha - 2 * ate_se_ha, ymax = ate_ha + 2 * ate_se_ha), 
                width = 0.2, position = position_dodge(width = dodge_width)) +  
geom_point(position = position_dodge(width = dodge_width)) +  # Points for ATE
geom_point(aes(proj_id,guizar_ha), color=guizar_colour, shape=3, size=1, alpha=0.6) + # Adds Guizar
xlab ("") +
ylab("Diff. in forest loss (Ha)") +
  theme_bw() +  
  theme(panel.border = element_rect( fill=NA, colour = "black", size=1),
        axis.text.x = element_text(angle = 90, hjust = 1, face = "bold"),  # Rotate x-axis labels
        legend.title=element_blank(),
        axis.line = element_blank(), 
        legend.position = "bottom", 
        axis.title.x = element_text(margin = ggplot2::margin(t = 1)),
        axis.title.y = element_text(margin = ggplot2::margin(r = 1)),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "in"),
        legend.margin=ggplot2::margin(0.1,0.1,0.1,0.1),
        legend.box.spacing = unit(0.001, "in"),
        ) +  
  
  scale_color_manual(values=col_palette, name="Method") +
  scale_x_discrete(limits = order_vcs) +
  
  # Define color for x-axis labels: Change for problematic sites
  theme(face = "bold")

# Bottom LEFT ------
order_vcs <- comp_meths_sub %>% 
  filter(method==methnames$lm_simple) %>%
  select(proj_id, ate_yr) %>%
  group_by(proj_id) %>%  summarise(ate_yr = mean(ate_yr)) %>%
  arrange(ate_yr) %>% mutate(proj_id = as.character(proj_id)) %>% filter(!proj_id %in% projects_with_low_matched_prop) %>%  pull(proj_id)  %>% as.character() 

bottom_left <- ggplot(comp_meths_sub %>% filter(method==methnames$lm_simple), aes(x = proj_id, y = ate_yr)) +
  geom_hline(lwd = 0.5, yintercept = 0) +  
  geom_point(position = position_dodge(width = dodge_width), color=grey_color) +  # Points for ATE
  geom_errorbar(aes(ymin = ate_yr - 2 * ate_yr_se, ymax = ate_yr + 2 * ate_yr_se), 
                width = 0.2, position = position_dodge(width = dodge_width), color=grey_color) +  
 geom_point(aes(proj_id,implied_ate), color=red_color, shape=19, size=1) + # Adds 4C
xlab ("") +
ylab("Diff. in forest loss (%"~yr^-1*')') +
# scale_y_break(c(-14, -95)) +
  
scale_x_discrete(limits = order_vcs) +

# Define color for x-axis labels: Change for problematic projects
theme_bw() + 
  theme(panel.border = element_rect( fill=NA, colour = "black", size=1),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "in"),
        legend.title=element_blank(),
        # axis.text.y = element_text(margin = ggplot2::margin(r = 1)),
        axis.line = element_blank(), 
        legend.position = "bottom", 
        axis.title.x = element_text(margin = ggplot2::margin(t = 1)),
        axis.title.y = element_text(margin = ggplot2::margin(r = 1)),
        axis.text.x.top = element_blank(),  # Remove top x-axis text
        axis.ticks.x.top = element_blank(),
        axis.text.x = element_text(
            face = "bold",
            angle = 90, hjust = 1
            )
        ) 

# Bottom RIGHT ------
order_vcs <- comp_meths_sub %>% 
  filter(method==methnames$lm_simple) %>%
  select(proj_id, ate_ha) %>%
  group_by(proj_id) %>% summarise(ate_ha = mean(ate_ha)) %>%
  arrange(ate_ha) %>%
  pull(proj_id)  %>% as.character()

bottom_right <- ggplot(comp_meths_sub %>% filter(method==methnames$lm_simple), aes(x = proj_id, y = ate_ha)) +
  geom_hline(lwd = 0.5, yintercept = 0) +  
  geom_point(position = position_dodge(width = dodge_width), color=grey_color) +  # Points for ATE
  geom_errorbar(aes(ymin = ate_ha - 2 * ate_se_ha, ymax = ate_ha + 2 * ate_se_ha), 
                width = 0.2, position = position_dodge(width = dodge_width), color=grey_color) +  
 geom_point(aes(proj_id,avoided_verra), color=red_color, shape=19, size=1) + # Adds 4C
xlab ("") +
ylab("Diff. in forest loss (Ha)") +

scale_x_discrete(limits = order_vcs) +

# Define color for x-axis labels: Change for problematic projects
theme_bw() + 
  theme(panel.border = element_rect( fill=NA, colour = "black", size=1),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "in"),
        legend.title=element_blank(),
        # axis.text.y = element_text(margin = ggplot2::margin(r = 1)),
        axis.line = element_blank(), 
        legend.position = "bottom", 
        axis.title.x = element_text(margin = ggplot2::margin(t = 1)),
        axis.title.y = element_text(margin = ggplot2::margin(r = 1)),
        axis.text.x.top = element_blank(),  # Remove top x-axis text
        axis.ticks.x.top = element_blank(),
        axis.text.x = element_text(
            face = "bold",
            angle = 90, hjust = 1
            )
        ) 

#  --------- PLOT OUT ---------
  gg_out <- top_panel / middle_panel / (bottom_left | bottom_right)   +  plot_layout(
    guides = "collect", 
    heights = c(0.8, 0.8, 1.5),
    ncol = 1,  
    nrow = 3   
  ) & 
  plot_annotation(tag_levels = 'a') &  # Add tags to panels
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5), 
    legend.position = "bottom",
    plot.margin = unit(c(0.01,0.01,0.01,0.01), "in"),
          legend.margin=ggplot2::margin(0.1,0.1,0.1,0.1),
          legend.box.spacing = unit(0.001, "in"),
          axis.text.x.top = element_blank(),  # Remove top x-axis text
          axis.ticks.x.top = element_blank()
  )

ggsave(file_main_plot, gg_out, width=10, height=10, units='in', dpi=300)


# ---------------------------------------------------------------------------
#  Summaries for reporting
# ---------------------------------------------------------------------------


comp_meths %>% filter(method==methnames$lm_simple)  %>% filter(!is.na(avoided_verra)) %>% tally()
# n=26

comp_meths %>% 
  filter(method==methnames$lm_simple)  %>% 
  filter(!is.na(avoided_verra)) %>% 
  select(vcs_id) %>% distinct() %>%
  left_join(proj_info %>% select(vcs_id, inclusion_status)) %>% group_by(inclusion_status) %>% tally()

# 1 <80% of plots matched     3
# 2 Included                 23

proj_info %>% group_by(inclusion_status) %>% tally()
#   inclusion_status                      n
#   <chr>                             <int>
# "<80% of plots matched"               6
# "<80% undisturbed"                   22
# "Included"                           38
# "Insufficient temporal\n records"     5

# ---- Count of projects examined
proj_info %>% filter(inclusion_status %in% c(included_label,under_80_label)) %>% group_by(continent) %>% tally()
# # A tibble: 4 × 2
#   continent     n
#   <chr>     <int>
# 1 Africa        7
# 2 Americas     34
# 3 Asia          2
# 4 Oceania       1

# ---  Matched below optimal sample size
proj_info %>% filter(inclusion_status == under_80_label) %>% group_by(continent) %>% tally()
#   continent     n
#   <chr>     <int>
# 1 Africa        5
# 2 Americas      1
# 3 Asia          2

# --- Included
proj_info %>% filter(inclusion_status == included_label) %>% group_by(continent) %>% tally()
# 1 Africa        3
# 2 Americas     33
# 3 Asia          1
# 4 Oceania       1

# COMPARISON VS GUIZAR 2022 AND VERRA

# Corr with Guizar 2022
spearman_corr <- comp_meths_sub %>% filter(method==methnames$doubly_robust) %>%
  filter(!is.na(guizar)) %>%
  {cor.test(.$ate_yr, .$guizar, method = "spearman")}

# vera --
spearman_corr <- comp_meths_sub %>% filter(method==methnames$doubly_robust) %>%
  filter(!is.na(avoided_verra)) %>% 
  left_join(proj_info) %>%
  filter(inclusion_status == included_label) %>%
  {cor.test(.$ate_ha, .$avoided_verra, method = "spearman")}

# ------ Compare avoided deforestation HA between this study and estimated in VCS reports
comparison_verra = comp_meths_sub %>% filter(method==methnames$doubly_robust) %>%
  filter(!is.na(avoided_verra)) %>% 
  left_join(proj_info) %>%
  filter(inclusion_status == included_label) %>% 
  summarise (ate_ha = sum(ate_ha), avoided_verra = sum(avoided_verra)) %>% 
  mutate(ratio = avoided_verra/ate_ha) 
  
comparison_verra %>% kable(digits=2)

# |   ate_ha| avoided_verra| ratio|
# |--------:|-------------:|-----:|
# | -31822.6|     -123099.5|  3.87|

comparison_verra_top = comp_meths_sub %>% filter(method==methnames$doubly_robust) %>%
  left_join(proj_info) %>%
  filter(inclusion_status == included_label) %>% 
  filter(proj_id %in% c('BRA_1382','PER_985')) %>% 
  summarise (ate_ha = sum(ate_ha), avoided_verra = sum(avoided_verra)) 
 
ratio_diff_top = comparison_verra_top$avoided_verra / comparison_verra$ate_ha 
ratio_diff_top %>% round(digits=2)
# [1] 1.88

(ratio_diff_top/(comparison_verra$avoided_verra/comparison_verra$ate_ha)) %>% round(digits=2)
# [1] 0.49

# -- print overcrediting
comp_meths_sub %>% filter(method==methnames$doubly_robust) %>%
  left_join(proj_info) %>%
  filter(proj_id %in% c('BRA_1382','PER_985')) %>% 
  select(proj_id, avoided_verra) %>% kable(digits=2)

# ---- Bootstrap estimates (similar to guizar-coutino et al 2022)
d_mean_estimates <- comp_meths_sub %>% filter(method==methnames$doubly_robust) %>%
  left_join(proj_info) %>%
  filter(inclusion_status == included_label) %>% 
  mutate(perc_avoided = (abs(ate_ha)/area_ha)*100, perc_avoided_yr = perc_avoided/5)

# boot CI
estimate_ci <- boot(data = d_mean_estimates$perc_avoided_yr, statistic = meanD, R = 9999)  %>% boot.ci(.,type = c("bca")) 

d_mean_estimates %>%
  summarise(mean_estimate = mean(perc_avoided_yr)) %>% mutate(
    ci_low = round(estimate_ci[[4]][4],2),
    ci_upp = round(estimate_ci[[4]][5],2)
  ) %>% kable(digits=2)

# | mean_estimate| ci_low| ci_upp|
# |-------------:|------:|------:|
# |          0.26|   0.16|   0.42|

# Total avoided deforestation
comp_meths_sub %>% filter(method==methnames$doubly_robust) %>%
  left_join(proj_info) %>%
  filter(inclusion_status == included_label) %>% 
  summarise (ate_ha = sum(ate_ha), area_ha = sum(area_ha)) %>% 
  mutate(perc_avoided = (abs(ate_ha)/area_ha)*100, perc_avoided_yr = perc_avoided/5) %>%
  kable(digits=2)

# |    ate_ha| area_ha| perc_avoided| perc_avoided_yr|
# |---------:|-------:|------------:|---------------:|
# | -33948.21| 5464973|         0.62|            0.12|


# -----------------------------------------------------------------------------------
# Other figs
# -----------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------
# Map of proj locations 
# ---------------------------------------------------------------------------------


centroids = proj_info %>%
  filter(inclusion_status != 'Unavailable\ngeospatial data') %>%
  st_as_sf(., coords = c("lon", "lat"), crs = 4326, agr = "constant") %>% rename(pt_4326 = geometry) %>%
  mutate(inclusion_status = factor(inclusion_status,levels=names(map_cols)))

world <- ne_countries(scale = "medium", returnclass = "sf")
world = st_transform(world, crs = st_crs(4326))

proj_info$iso3 = countrycode(proj_info$country,origin = "country.name",destination = "iso3c")

# MAP
world.moll = st_transform(world, crs = "+proj=moll")
world.wintri = lwgeom::st_transform_proj(world, crs = "+proj=wintri")

roi = world.moll %>% filter(iso_a3 %in% unique(proj_info$iso3))

bbox = sf::st_bbox(roi)
bbox['ymax'] = 3000000 

mp = ggplot() +
    geom_sf(data = world.moll, fill='#dedede', lwd=0.1) +
    geom_sf(data = st_transform(centroids, crs = "+proj=moll"),aes(fill=inclusion_status), shape=21, color='black',alpha=0.7, size=2) + 
    coord_sf(xlim = c(bbox['xmin'], bbox['xmax']), ylim = c(bbox['ymin'], bbox['ymax']), expand = FALSE) +
    # scale_fill_viridis_d(option = "plasma","") +
    scale_fill_manual(values=map_cols, name="") +
    theme_classic() +
    theme(panel.border = element_rect(fill='transparent', color='#23212A'),
          axis.line = element_blank(),
          legend.box = "vertical",
          legend.position = "bottom",
          legend.text = element_text(angle = 0, size=11),
          plot.margin = unit(c(0.1,0.1,0.1,0.1), "in"),
          legend.margin=ggplot2::margin(0.1,0.1,0.1,0.1),
          legend.box.spacing = unit(0.001, "in"))

ggsave(filename=file.path("figures", "sites-map-moll.png"), plot=mp, width=10, dpi=300)

# --------------------------------------------------------------------------------------------------------
# Figure S2
# --------------------------------------------------------------------------------------------------------

# Bland Altman analysis
library(blandr)
comp_tab <- comp_meths_sub %>% filter(method==methnames$doubly_robust) %>%
  filter(!is.na(guizar)) %>%
  select(ate_yr, guizar)

stats_out <- blandr::blandr.statistics(comp_tab$ate_yr, comp_tab$guizar, sig.level = 0.95)

# blandr::blandr.draw(comp_tab$ate_yr, comp_tab$guizar)
cor(comp_tab$ate_yr, comp_tab$guizar, method = "sp")

# Compute means and differences
means <- (comp_tab$ate_yr + comp_tab$guizar) / 2
diffs <- comp_tab$ate_yr - comp_tab$guizar
mean_diff <- mean(diffs, na.rm = TRUE)
sd_diff <- sd(diffs, na.rm = TRUE)

# Calculate limits of agreement
loa_upper <- mean_diff + 1.96 * sd_diff
loa_lower <- mean_diff - 1.96 * sd_diff

# Create a data frame
bland_df <- data.frame(means = means, diffs = diffs)

# Create the ggplot
ggp_cor <- ggplot(bland_df, aes(x = means, y = diffs)) +
  geom_point(color = "black") +
  geom_hline(yintercept = mean_diff, color = "red", linetype = "dashed") +
  geom_hline(yintercept = mean_diff + 1.96 * sd_diff, color = "darkgreen", linetype = "dotted") +
  geom_hline(yintercept = mean_diff - 1.96 * sd_diff, color = "darkgreen", linetype = "dotted") +
  labs(
    x = "Mean of Measurements",
    y = "Difference Between Measurements"
  ) + theme_classic() +
    theme(panel.border = element_rect(fill='transparent', color='#23212A'),
          axis.line = element_blank(),
          legend.box = "vertical",
          legend.position = "bottom",
          legend.text = element_text(angle = 0, size=11),
          plot.margin = unit(c(0.1,0.1,0.1,0.1), "in"),
          legend.margin=ggplot2::margin(0.1,0.1,0.1,0.1),
          legend.box.spacing = unit(0.001, "in"))

ggsave(filename=file.path("figures", "bland-altman.png"), plot=ggp_cor, width=10, dpi=300)


# ---------------------------------------------------------------------
# SUPPLEMENTARY TABLES 
# ---------------------------------------------------------------------

#---------------------------------------------------------------------------
# Table S1
# --------------------------------------------------------------------------

# Load N samples
proj_samples <- read_csv(file.path("tables", "proj_treat_counts.csv"))
proj_samples <- proj_samples %>% mutate(samples_str = paste0(treat,'= ',n)) %>%
select(proj_id,samples_str) %>% 
group_by(proj_id) %>% summarise(samples_str = paste0(samples_str, collapse=', '))

# PREPARE TABLE
out_table <- proj_info %>% left_join(
  comp_meths %>% 
  filter(method==methnames$doubly_robust) %>%
  select(proj_id, ate_yr, ate_yr_se) %>%
  mutate (conf_low = ate_yr - 2 * ate_yr_se, conf_high = ate_yr + 2 * ate_yr_se)
  )

cls = c('Project ID','Project Name','Area (km^2^)','Effect size [95% CI] (%/yr^-1^), for projects where we could not estimate the effect size, reason for exclusion is give','Sample size','Percentage matched in treatment','Included in ​Guizar-Coutiño et al.,(2022)​', 'VCS Methodology (VM)')
out_print = out_table %>%
      mutate(
      estimate_char = paste0(round(ate_yr,2),' [',round(conf_low,2),' ', round(conf_high,2),']'),
      km = round(area_ha*0.01,2),
      id = paste0(vcs_id,' (',iso3,')'),
      perc_matched = eval(parse(text = paste0('prop_matched_alpha_', as.character(alpha_use)))) * 100,
      ) %>% 
      mutate(
        estimate_char = ifelse(!inclusion_status %in% c(included_label,under_80_label),inclusion_status,estimate_char),
        guizar_2022 = ifelse(vcs_id %in% d_guizar$vcs_id,'Y','X')
      ) %>%
      left_join(proj_samples) %>%
      arrange(country) %>%
    select(proj_id,project_name,km,estimate_char,samples_str,perc_matched,guizar_2022, vcs_methodology)
colnames(out_print) = cls



# Render table to MD
md_table <- capture.output(pandoc.table(out_print, style='multiline',split.table = Inf, digits=3))
writeLines(md_table, file.path("tables", "project_summaries.md"))

# Render table to Latex
# tex_table <- capture.output(kable(out_print, format = "latex", booktabs = TRUE, caption = ""))
# writeLines(tex_table, file.path("tables", "project_summaries.tex"))

# Render MD to Docx (requires Pandoc)
system(paste0("pandoc ", file.path("tables", "project_summaries.md"), " -o ", file.path("tables", "SuppTable_1.docx")))


#---------------------------------------------------------------------------
# Table S3
# --------------------------------------------------------------------------

proj_covar_sums <- read_csv(file.path("data", "proj_covar_sums.csv"))
proj_covar_sums <- proj_covar_sums %>% left_join(proj_info %>% select(proj_id,inclusion_status))

proj_covar_sums <- proj_covar_sums %>% 
  gather(var, val, -c(inclusion_status, proj_id)) %>%
  group_by(var) %>% nest() %>%
  mutate(
    mean_matched = mean(data[[1]]$val[data[[1]]$inclusion_status == included_label]),
    mean_unmatched = mean(data[[1]]$val[data[[1]]$inclusion_status == under_80_label]),
    mean_diff = mean_matched - mean_unmatched,
    test = map(data, ~ wilcox.test(val ~ inclusion_status, data = .x, conf.int = TRUE)))

# condense
proj_covar_sums <- proj_covar_sums %>% 
  mutate(
    # parameter = map_dbl(test, ~.x$parameter),
    p_value = map_dbl(test, ~ .x$p.value),
    W = map_dbl(test, ~ .x$statistic)
  ) %>% 
  separate(var, into = c("var", "statistic"), sep = "_(?=[^_]+$)") %>%
# reomve 'noise' tests
  filter(!statistic %in% c('sum','sd')) %>%
  select(-c(data, test, statistic))

# --- Export table
cls = c('Covariate','Mean\nmatched (n=36)','Mean\nunmatched (n=8)','Mean\ndifference')

var_fcts = c(
  dist_degra = 'Mean plot level\ndist. to recently defor. (m)',
  mean_slp = 'Mean plot level slope (degrees)',
  mean_access = 'Mean plot level\ntime travel to pop. centers (seconds)',
  mean_ele = 'Mean plot level elevation (m)',
  pre_short_term_trend= 'Proj level short-term\ndeforestation (prop.)',
  pre_long_term_trend = 'Proj level long-term\ndeforestation (prop.)')


out_table = proj_covar_sums %>%
      mutate(mean_matched = round(mean_matched,2), mean_unmatched = round(mean_unmatched,2),
      mean_diff = paste0(round(mean_diff,2), ' (',W, ', ',round(p_value,2),')')
      ) %>% 
      arrange(var) %>%
      mutate(var = recode(var, !!!var_fcts)) %>%
    select(var,mean_matched,mean_unmatched,mean_diff)
colnames(out_table) = cls


# render as md
md_table <- capture.output(pandoc.table(out_table, style='multiline',split.table = Inf, digits=3))
writeLines(md_table, file.path("tables", "Covariate_differences.md"))

# system(paste0("pandoc ", file.path("tables", "Covariate_differences.md"), " -o ", file.path("tables", "SuppTable_2.docx")))

# Render table to Latex
tex_table <- capture.output(kable(out_table, format = "latex", booktabs = TRUE, caption = ""))
writeLines(tex_table, file.path("tables", "Covariate_differences.tex"))


# --------------------------------------------------
# Table S4
# PROJECT SELECTION SUMMARIES
# ---------------------------------------------------

forest_classes <- proj_info %>% 
    select(proj_id, area_ha, syear, inclusion_status, undisturbed, degraded, deforested, regrowth, water, other) %>% 
    rowwise() %>% 
    mutate(total = sum(undisturbed, degraded, deforested, regrowth, water, other, na.rm = TRUE)) %>% 
    mutate(
        area_ha = round(area_ha, 2),
        Undisturbed = round(undisturbed/total * 100, 2), 
        Degraded = round(degraded/total * 100, 2), 
        Deforested = round(deforested/total * 100, 2), 
        Regrowth = round(regrowth/total * 100, 2), 
        Water = round(water/total * 100, 2), 
        Other = round(other/total * 100, 2),
    ) %>%
    select(-c(total,undisturbed,degraded,deforested,regrowth,water,other) ) %>%
    filter(inclusion_status != 'Insufficient temporal\n records') %>%
    mutate(inclusion_status = case_when(
        inclusion_status %in% c(under_80_label, included_label) ~ 'Included for matching',
        TRUE ~ inclusion_status
    )) %>%
    mutate(inclusion_status = factor(inclusion_status, levels = c(inclusion_lab[1],'Included for matching'))) %>%
    arrange(inclusion_status, area_ha)

cls = c('Project ID','Area (Ha)',"Starting year","Inclusion status", "Undisturbed %", "Degraded %", "Deforested %", "Regrowth %", "Water %", "Other %")
out_print = forest_classes
colnames(out_print) = cls

# Render table to MD
md_table <- capture.output(pandoc.table(out_print, style='multiline', split.table = Inf, digits=3, missing='-'))
writeLines(md_table, file.path("tables", "project_selection_summaries.md"))

tex_table <- capture.output(kable(out_print, format = "latex", booktabs = TRUE, caption = ""))
writeLines(tex_table, file.path("tables", "project_selection_summaries.tex"))

system(paste0("pandoc ", file.path("tables", "project_selection_summaries.md"), " -o ", file.path("tables", "SuppTable_4.docx")))



# --------------------------------------------------------------------------------------------
# Table S5
# Project summaries by filtering levels (area, ISO3, methodology)
# --------------------------------------------------------------------------------------------

proj_info <- read_csv(file = file.path("data", "vcs-info.csv")) %>% filter(inclusion_status!= 'Unavailable\ngeospatial data') %>% 
mutate(proj_id = paste0(iso3, "_", vcs_id))


# Helper to summarise a dataframe into one row
summarise_proj_group <- function(label, df) {
  # Mean and IQR of area_ha
  area_mean <- mean(df$area_ha, na.rm = TRUE)
  qs <- stats::quantile(df$area_ha, probs = c(0.25, 0.75), na.rm = TRUE, type = 7)
  area_str <- paste0(round(area_mean, 2), " (", round(qs[1], 2), "–", round(qs[2], 2), ")")

  # ISO3 counts
  iso_vals <- df$iso3
  iso_vals <- iso_vals[!is.na(iso_vals) & iso_vals != ""]
  iso_tab <- sort(table(iso_vals), decreasing = TRUE)
  iso_str <- if (length(iso_tab)) paste0(names(iso_tab), ": ", as.integer(iso_tab), collapse = ", ") else "-"

  # VCS methodology counts (robust to multiple codes separated by , ; / |)
  if ("vcs_methodology" %in% names(df)) {
    vcsm_split <- strsplit(df$vcs_methodology, "[,;/|]+")
    vcsm_flat <- unlist(lapply(vcsm_split, function(x) trimws(x)))
    vcsm_flat <- vcsm_flat[!is.na(vcsm_flat) & vcsm_flat != ""]
    vcsm_tab <- sort(table(vcsm_flat), decreasing = TRUE)
    vcsm_str <- if (length(vcsm_tab)) paste0(names(vcsm_tab), ": ", as.integer(vcsm_tab), collapse = ", ") else "-"
  } else {
    vcsm_str <- "-"
  }

  tibble::tibble(
    Filter = label,
    `Area (Ha) mean (IQR)` = area_str,
    `ISO3 (count)` = iso_str,
    `VCS methodology (count)` = vcsm_str
  )
}

# Define groups
summ_all <- summarise_proj_group(
  label = "All projects",
  df = proj_info
)

summ_no_temporal <- summarise_proj_group(
  label = "Excl. insufficient temporal records",
  df = proj_info %>% dplyr::filter(inclusion_status %in% c(inclusion_lab[1],inclusion_lab[4], inclusion_lab[3]))
)

summ_no_temporal_undist <- summarise_proj_group(
  label = "Excl. insufficient temporal and <80% undisturbed",
  df = proj_info %>% dplyr::filter(inclusion_status %in% c(inclusion_lab[4], inclusion_lab[3]))
)

proj_filter_summaries <- dplyr::bind_rows(summ_all, summ_no_temporal, summ_no_temporal_undist)

# Export table (MD and LaTeX), consistent with other exports above
md_table <- capture.output(pandoc.table(proj_filter_summaries, style='multiline', split.table = Inf, digits=3, missing='-'))
writeLines(md_table, file.path("tables", "project_filter_summaries.md"))
system(paste0("pandoc ", file.path("tables", "project_filter_summaries.md"), " -o ", file.path("tables", "SuppTable_5.docx")))


# ---------------------------------------------------
# Relabel projects according to their inclusion status in the paper 
# the outputs of this relabellig are already reflected in the 
# vcs-info.csv file 
# ---------------------------------------------------


#  -- label projects according to their inclusion in the analysis
# proj_in <-comp_meths %>% select(proj_id) %>% distinct() %>% .$proj_id %>% levels()
# proj_not_examined <- proj_info %>% filter(inclusion_status %in% inclusion_lab[1:2]) %>% .$proj_id

# proj_info <- proj_info %>% mutate(
#     inclusion_status = case_when(
#         !proj_id %in% projects_with_low_matched_prop & !proj_id %in% proj_not_examined & proj_id %in% proj_in ~ included_label,
#         proj_id %in% projects_with_low_matched_prop & !proj_id %in% proj_not_examined  ~ under_80_label,
#         TRUE ~ inclusion_status
#     )
# )

# -- Write updated file 
# cols_write <-c('vcs_id','lat','lon','inclusion_status','project_name','syear','country','iso3','continent','area_ha','proj_id','undisturbed','degraded','deforested','regrowth','water','other','vcs_methodology')

# write_csv(
#   proj_info %>% filter(!is.na(inclusion_status)) %>%
#   select(all_of(cols_write)), 
#   file = file.path("data", "vcs-info.csv"))
