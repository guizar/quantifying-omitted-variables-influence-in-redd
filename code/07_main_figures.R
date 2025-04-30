library(patchwork)
library(ggbreak)

# --- For consistency we will build for the objects constructed int he previous exploratory plots
source(file.path(pth, "code", "05_plot_ATEs.R"), echo=TRUE)
source(file.path(pth, "code", "06_sensitivity_analysis.R"), echo=TRUE)

setwd(file.path(pth,'figures')) # FOR TESTING

# Loads Consbio2022
d_guizar = read_csv(file.path(pth, "data", "guizar_2022.csv"))
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
proj_info <- read_csv(file = file.path(pth, "data", "vcs-info.csv")) %>% filter(inclusion_status!= 'Unavailable\ngeospatial data') %>% 
mutate(proj_id = paste0(iso3, "_", vcs_id))

# --- Generate palettes
# models cols
base_col= carto_pal(7, "Temps")[c(3,1,5,7)] # c("#38293", "#3B5249", "#519872", "#A4B494")
col_palette = generate_palette(base_col[1], modification = "go_darker", n_colours = length(levels(comp_meths$method)), view_palette = F)

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
file_main_plot <- file.path(pth, "figures", filename_out)

# TOP -----------

# Create the plot: ATE estimates by method across projects
top_panel <- ggplot(comp_meths, aes(x = proj_id, y = ate_yr, color = method)) +
  geom_hline(lwd = 0.5, yintercept = 0) +  
  geom_errorbar(aes(ymin = ate_yr - 2 * ate_yr_se, ymax = ate_yr + 2 * ate_yr_se), 
                width = 0.2, position = position_dodge(width = dodge_width)) +  
geom_point(position = position_dodge(width = dodge_width)) +  

 geom_point(aes(proj_id,guizar), color=guizar_colour, shape=3, size=1, alpha=0.6) + # Adds Guizar
xlab ("") +
ylab("Difference in forest loss (%"~yr^-1*')') +
  theme_bw() +  
  theme(panel.border = element_rect( fill=NA, colour = "black", size=1),
        axis.text.x = element_text(angle = 90, hjust = 1),  
        legend.title=element_blank(),
        axis.line = element_blank(), 
        legend.position = "bottom", 
        axis.title.x = element_text(margin = margin(t = 1)),
        axis.title.y = element_text(margin = margin(r = 1)),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "in"),
        legend.margin=margin(0.1,0.1,0.1,0.1),
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
  pull(proj_id)

middle_panel <- ggplot(comp_meths, aes(x = proj_id, y = ate_ha, color = method)) +
  geom_hline(lwd = 0.5, yintercept = 0) +  
  geom_errorbar(aes(ymin = ate_ha - 2 * ate_se_ha, ymax = ate_ha + 2 * ate_se_ha), 
                width = 0.2, position = position_dodge(width = dodge_width)) +  
geom_point(position = position_dodge(width = dodge_width)) +  # Points for ATE

geom_point(aes(proj_id,guizar_ha), color=guizar_colour, shape=3, size=1, alpha=0.6) + # Adds Guizar
xlab ("") +
ylab("Difference in forest loss (Ha)") +
  theme_bw() +  
  theme(panel.border = element_rect( fill=NA, colour = "black", size=1),
        axis.text.x = element_text(angle = 90, hjust = 1),  # Rotate x-axis labels
        legend.title=element_blank(),
        axis.line = element_blank(), 
        legend.position = "bottom", 
        axis.title.x = element_text(margin = margin(t = 1)),
        axis.title.y = element_text(margin = margin(r = 1)),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "in"),
        legend.margin=margin(0.1,0.1,0.1,0.1),
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
  pull(proj_id)

bottom_left <- ggplot(comp_meths %>% filter(method==methnames$lm_simple), aes(x = proj_id, y = ate_yr)) +
  geom_hline(lwd = 0.5, yintercept = 0) +  
  geom_point(position = position_dodge(width = dodge_width), color=grey_color) +  # Points for ATE
  geom_errorbar(aes(ymin = ate_yr - 2 * ate_yr_se, ymax = ate_yr + 2 * ate_yr_se), 
                width = 0.2, position = position_dodge(width = dodge_width), color=grey_color) +  
 geom_point(aes(proj_id,implied_ate), color=red_color, shape=19, size=1) + # Adds 4C
xlab ("") +
ylab("Difference in forest loss (%"~yr^-1*')') +
scale_y_break(c(-14, -95)) +

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
        # axis.text.y = element_text(margin = margin(r = 1)),
        axis.line = element_blank(), 
        legend.position = "bottom", 
        axis.title.x = element_text(margin = margin(t = 1)),
        axis.title.y = element_text(margin = margin(r = 1)),
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
  pull(proj_id)

bottom_right <- ggplot(comp_meths %>% filter(method==methnames$lm_simple), aes(x = proj_id, y = ate_ha)) +
  geom_hline(lwd = 0.5, yintercept = 0) +  
  geom_point(position = position_dodge(width = dodge_width), color=grey_color) +  # Points for ATE
  geom_errorbar(aes(ymin = ate_ha - 2 * ate_se_ha, ymax = ate_ha + 2 * ate_se_ha), 
                width = 0.2, position = position_dodge(width = dodge_width), color=grey_color) +  
 geom_point(aes(proj_id,avoided_verra), color=red_color, shape=19, size=1) + # Adds 4C
xlab ("") +
ylab("Difference in forest loss (Ha)") +

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
        # axis.text.y = element_text(margin = margin(r = 1)),
        axis.line = element_blank(), 
        legend.position = "bottom", 
        axis.title.x = element_text(margin = margin(t = 1)),
        axis.title.y = element_text(margin = margin(r = 1)),
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
          legend.margin=margin(0.1,0.1,0.1,0.1),
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
file_main_plot <- file.path(pth, "figures", filename_out)

# Subset file
comp_meths_sub <- comp_meths %>% filter(!proj_id %in% projects_with_low_matched_prop)

# TOP -----------
# Create the plot: ATE estimates by method across projects

top_panel <- ggplot(comp_meths_sub, aes(x = proj_id, y = ate_yr, color = method)) +
  geom_hline(lwd = 0.5, yintercept = 0) +  
  geom_errorbar(aes(ymin = ate_yr - 2 * ate_yr_se, ymax = ate_yr + 2 * ate_yr_se), 
                width = 0.2, position = position_dodge(width = dodge_width)) +  
geom_point(position = position_dodge(width = dodge_width)) +  # Points for ATE
 geom_point(aes(proj_id,guizar), color=guizar_colour, shape=3, size=1, alpha=0.6) + # Adds Guizar
xlab ("") +
ylab("Difference in forest loss (%"~yr^-1*')') +
  theme_bw() +  
  theme(panel.border = element_rect( fill=NA, colour = "black", size=1),
        axis.text.x = element_text(angle = 90, hjust = 1, face = "bold"),  # Rotate x-axis labels
        legend.title=element_blank(),
        axis.line = element_blank(), 
        legend.position = "bottom", 
        axis.title.x = element_text(margin = margin(t = 1)),
        axis.title.y = element_text(margin = margin(r = 1)),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "in"),
        legend.margin=margin(0.1,0.1,0.1,0.1),
        legend.box.spacing = unit(0.001, "in"),
        ) +  
  
  scale_color_manual(values=col_palette, name="Method") +
  
  # Define color for x-axis labels: Change for problematic sites
  theme(face = "bold")

# --- Middle panel ---
order_vcs <- comp_meths %>%
  select(proj_id, ate_ha) %>%
  group_by(proj_id) %>%  summarise(ate_ha = mean(ate_ha)) %>%
  arrange(ate_ha) %>% mutate(proj_id = as.character(proj_id)) %>% filter(!proj_id %in% projects_with_low_matched_prop) %>%  pull(proj_id) 

middle_panel <- ggplot(comp_meths_sub, aes(x = proj_id, y = ate_ha, color = method)) +
  geom_hline(lwd = 0.5, yintercept = 0) +  
  geom_errorbar(aes(ymin = ate_ha - 2 * ate_se_ha, ymax = ate_ha + 2 * ate_se_ha), 
                width = 0.2, position = position_dodge(width = dodge_width)) +  
geom_point(position = position_dodge(width = dodge_width)) +  # Points for ATE
geom_point(aes(proj_id,guizar_ha), color=guizar_colour, shape=3, size=1, alpha=0.6) + # Adds Guizar
xlab ("") +
ylab("Difference in forest loss (Ha)") +
  theme_bw() +  
  theme(panel.border = element_rect( fill=NA, colour = "black", size=1),
        axis.text.x = element_text(angle = 90, hjust = 1, face = "bold"),  # Rotate x-axis labels
        legend.title=element_blank(),
        axis.line = element_blank(), 
        legend.position = "bottom", 
        axis.title.x = element_text(margin = margin(t = 1)),
        axis.title.y = element_text(margin = margin(r = 1)),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "in"),
        legend.margin=margin(0.1,0.1,0.1,0.1),
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
  arrange(ate_yr) %>% mutate(proj_id = as.character(proj_id)) %>% filter(!proj_id %in% projects_with_low_matched_prop) %>%  pull(proj_id) 

bottom_left <- ggplot(comp_meths_sub %>% filter(method==methnames$lm_simple), aes(x = proj_id, y = ate_yr)) +
  geom_hline(lwd = 0.5, yintercept = 0) +  
  geom_point(position = position_dodge(width = dodge_width), color=grey_color) +  # Points for ATE
  geom_errorbar(aes(ymin = ate_yr - 2 * ate_yr_se, ymax = ate_yr + 2 * ate_yr_se), 
                width = 0.2, position = position_dodge(width = dodge_width), color=grey_color) +  
 geom_point(aes(proj_id,implied_ate), color=red_color, shape=19, size=1) + # Adds 4C
xlab ("") +
ylab("Difference in forest loss (%"~yr^-1*')') +
scale_y_break(c(-14, -95)) +
  
scale_x_discrete(limits = order_vcs) +

# Define color for x-axis labels: Change for problematic projects
theme_bw() + 
  theme(panel.border = element_rect( fill=NA, colour = "black", size=1),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "in"),
        legend.title=element_blank(),
        # axis.text.y = element_text(margin = margin(r = 1)),
        axis.line = element_blank(), 
        legend.position = "bottom", 
        axis.title.x = element_text(margin = margin(t = 1)),
        axis.title.y = element_text(margin = margin(r = 1)),
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
  pull(proj_id)

bottom_right <- ggplot(comp_meths_sub %>% filter(method==methnames$lm_simple), aes(x = proj_id, y = ate_ha)) +
  geom_hline(lwd = 0.5, yintercept = 0) +  
  geom_point(position = position_dodge(width = dodge_width), color=grey_color) +  # Points for ATE
  geom_errorbar(aes(ymin = ate_ha - 2 * ate_se_ha, ymax = ate_ha + 2 * ate_se_ha), 
                width = 0.2, position = position_dodge(width = dodge_width), color=grey_color) +  
 geom_point(aes(proj_id,avoided_verra), color=red_color, shape=19, size=1) + # Adds 4C
xlab ("") +
ylab("Difference in forest loss (Ha)") +

scale_x_discrete(limits = order_vcs) +

# Define color for x-axis labels: Change for problematic projects
theme_bw() + 
  theme(panel.border = element_rect( fill=NA, colour = "black", size=1),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "in"),
        legend.title=element_blank(),
        # axis.text.y = element_text(margin = margin(r = 1)),
        axis.line = element_blank(), 
        legend.position = "bottom", 
        axis.title.x = element_text(margin = margin(t = 1)),
        axis.title.y = element_text(margin = margin(r = 1)),
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
          legend.margin=margin(0.1,0.1,0.1,0.1),
          legend.box.spacing = unit(0.001, "in"),
          axis.text.x.top = element_blank(),  # Remove top x-axis text
          axis.ticks.x.top = element_blank()
  )

ggsave(file_main_plot, gg_out, width=10, height=10, units='in', dpi=300)


# ---------------------------------------------------
# Condense proj info data and generate Supp table 
# ---------------------------------------------------

# # flag projs with low matching samples (below threshold)
# vcs_in_low_prop <- str_extract(projects_with_low_matched_prop, "_.*")
# vcs_in_low_prop <- sub("_", "", vcs_in_low_prop)

# -- label projects according to whether they've been included in the analysis:
# -- projs: first all projects for which we have an fx size; then adjust label to flag those undersampled
# vcs_in <-comp_meths %>% select(vcs_id) %>% distinct() %>% .$vcs_id
# proj_info <- proj_info %>% mutate(inclusion_status = ifelse(vcs_id %in% vcs_in, "Inlcuded",inclusion_status))
# proj_info <- proj_info %>% mutate(inclusion_status = ifelse(vcs_id %in% vcs_in_low_prop, '<80% of plots matched',inclusion_status))

# VCS not examined
# vcs_out <- proj_info %>% filter(inclusion_status %in% inclusion_lab[1:2]) %>% .$vcs_id

# The reining project (1477) did not meet the the proportional matched requirement in the subclasses analysis (based on propensity scores)
# proj_info <- proj_info %>% mutate(inclusion_status = ifelse(
#     !vcs_id %in% vcs_in & !vcs_id %in% vcs_out, "Not\nmatched",inclusion_status))

# Relabel projs according to their inclusion into this analysis
# proj_info$inclusion_status = factor(proj_info$inclusion_status, levels=inclusion_lab)

# # Write updated file 
# write_csv(proj_info %>% filter(!is.na(inclusion_status)), file = file.path(pth, "data", "vcs-info.csv"))


#-----------------------------------------------------
# Covariate summaries of projects included/excluded
# ----------------------------------------------------

proj_covar_sums <- read_csv(file.path(pth, "data", "proj_covar_sums.csv"))

proj_covar_sums <- proj_covar_sums %>% mutate(type = 
case_when(proj_id %in% order_vcs ~ 'Included',
          proj_id %in% projects_with_low_matched_prop ~ under_80_label)) %>%
          filter(proj_id != 'IDN_1477')


proj_covar_sums <- proj_covar_sums %>% 
  gather(var, val, -c(type, proj_id)) %>%
  group_by(var) %>% nest() %>%
  mutate(
    mean_matched = mean(data[[1]]$val[data[[1]]$type == 'Included']),
    mean_unmatched = mean(data[[1]]$val[data[[1]]$type == under_80_label]),
    mean_diff = mean_matched - mean_unmatched,
    test = map(data, ~ wilcox.test(val ~ type, data = .x, paired = FALSE, conf.int = TRUE)))

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
cls = c('Covariate','Mean\nmatched (n=33)','Mean\nunmatched (n=10)','Mean\ndifference')

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
writeLines(md_table, file.path(pth, "tables", "Covariate_differences.md"))

# system(paste0("pandoc ", file.path(pth, "tables", "Covariate_differences.md"), " -o ", file.path(pth, "tables", "SuppTable_2.docx")))


# Render table to Latex
tex_table <- capture.output(kable(out_table, format = "latex", booktabs = TRUE, caption = ""))

writeLines(tex_table, file.path(pth, "tables", "Covariate_differences.tex"))

# ------------------
# Construct SI table 
# -----------------

# Select the design table for the chosen distance metric and alpha
design_tab <- ordl[[dist_use]][[as.character(alpha_use)]]

# add matched prop info
proj_info <- proj_info %>% left_join(design_tab)

# Load N samples
proj_samples <- read_csv(file.path(pth, "tables", "proj_treat_counts.csv"))
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

cls = c('Project ID','Project Name','Area (km^2^)','Effect size [95% CI] (%/yr^-1^), for projects where we could not estimate the effect size, reason for exclusion is give','Sample size','Percentage matched in treatment','Included in ​Guizar-Coutiño et al.,(2022)​')
out_table = out_table %>%
      mutate(
      estimate_char = paste0(round(ate_yr,2),' [',round(conf_low,2),' ', round(conf_high,2),']'),
      km = round(area_ha*0.01,2),
      id = paste0(vcs_id,' (',iso3,')'),
      perc_matched = eval(parse(text = paste0('prop_matched_alpha_', as.character(alpha_use)))) * 100,
      ) %>% 
      mutate(
        estimate_char = ifelse(!inclusion_status %in% c('Included',under_80_label),inclusion_status,estimate_char),
        guizar_2022 = ifelse(vcs_id %in% d_guizar$vcs_id,'X','')
      ) %>%
      left_join(proj_samples) %>%
      arrange(country) %>%
    select(id,project_name,km,estimate_char,samples_str,perc_matched,guizar_2022)
colnames(out_table) = cls

# Render table to MD
md_table <- capture.output(pandoc.table(out_table, style='multiline',split.table = Inf, digits=3))
writeLines(md_table, file.path(pth, "tables", "project_summaries.md"))

# Render table to Latex
# tex_table <- capture.output(kable(out_table, format = "latex", booktabs = TRUE, caption = ""))
# writeLines(tex_table, file.path(pth, "tables", "project_summaries.tex"))

# Render MD to Docx (requires Pandoc)
system(paste0("pandoc ", file.path(pth, "tables", "project_summaries.md"), " -o ", file.path(pth, "tables", "SuppTable_1.docx")))


# ---------------
#  Summaries for reporting
# ---------------

comp_meths %>% filter(method==methnames$lm_simple)  %>% filter(!is.na(avoided_verra)) %>% tally()
# n=26

comp_meths %>% 
  filter(method==methnames$lm_simple)  %>% 
  filter(!is.na(avoided_verra)) %>% 
  select(vcs_id) %>% distinct() %>%
  left_join(proj_info %>% select(vcs_id, inclusion_status)) %>% group_by(inclusion_status) %>% tally()

proj_info %>% group_by(inclusion_status) %>% tally()
# inclusion_status                         n
#   <fct>                                <int>
#  "Forest type excluded"                  22
#  "Insufficient temporal\n records"        5
#  "Not\nmatched"                           1
#  "<80% of plots matched"    10
#  "Included"                      33


# ---- Count of projects examined
proj_info %>% filter(inclusion_status %in% c("Included","<80% of plots matched","Not\nmatched")) %>% group_by(continent) %>% tally()
# # A tibble: 4 × 2
#   continent     n
#   <chr>     <int>
#  Africa        7
#  Americas     34
#  Asia          2
#  Oceania       1

# ---  Matched below optimal sample size
proj_info %>% filter(inclusion_status == "<80% of plots matched") %>% group_by(continent) %>% tally()
# # A tibble: 4 × 2
#   continent     n
#   <chr>     <int>
#  Africa        4
#  Americas      4
#  Asia          1
#  Oceania       1

# --- Included
proj_info %>% filter(inclusion_status == "Included") %>% group_by(continent) %>% tally()
# # A tibble: 2 × 2
#   continent     n
#   <chr>     <int>
#  Africa        3
#  Americas     30


# COMPARISON VS GUIZAR 2022 AND VERRA

# Corr with Guizar 2022
spearman_corr <- comp_meths_sub %>% filter(method==methnames$doubly_robust) %>%
  filter(!is.na(guizar)) %>%
  {cor.test(.$ate_yr, .$guizar, method = "spearman")}

# vera --
spearman_corr <- comp_meths_sub %>% filter(method==methnames$doubly_robust) %>%
  filter(!is.na(avoided_verra)) %>% 
  left_join(proj_info) %>%
  filter(inclusion_status == "Included") %>%
  {cor.test(.$ate_ha, .$avoided_verra, method = "spearman")}

comp_meths_sub %>% filter(method==methnames$doubly_robust) %>%
  filter(!is.na(avoided_verra)) %>% 
  left_join(proj_info) %>%
  filter(inclusion_status == "Included") %>% 
  summarise (ate_ha = sum(ate_ha), avoided_verra = sum(avoided_verra)) %>%
  kable(digits=2)
  
# |    ate_ha| avoided_verra| area_ha| perc_avoided| perc_avoided_yr|
# |---------:|-------------:|-------:|------------:|---------------:|
# | -19889.83|     -96580.93| 1863487|         1.07|            0.21|

# ---- Bootstrap estimates (similar to guizar-coutino et al 2022)
d_mean_estimates <- comp_meths_sub %>% filter(method==methnames$doubly_robust) %>%
  left_join(proj_info) %>%
  filter(inclusion_status == "Included") %>% 
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
# |          0.25|   0.16|   0.39|

# Total avoided deforestation
comp_meths_sub %>% filter(method==methnames$doubly_robust) %>%
  left_join(proj_info) %>%
  filter(inclusion_status == "Included") %>% 
  summarise (ate_ha = sum(ate_ha), area_ha = sum(area_ha)) %>% 
  mutate(perc_avoided = (abs(ate_ha)/area_ha)*100, perc_avoided_yr = perc_avoided/5) %>%
  kable(digits=2)

# |    ate_ha| area_ha| perc_avoided| perc_avoided_yr|
# |---------:|-------:|------------:|---------------:|
# | -24848.02| 2840961|         0.87|            0.17|


##---------------------------
## Map of proj locations 
# --------------------------
library(rnaturalearth)
library(rnaturalearthdata)
library(countrycode)
library(sf)

# --- PLOT STARTS HERE
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
          legend.margin=margin(0.1,0.1,0.1,0.1),
          legend.box.spacing = unit(0.001, "in"))

ggsave(filename=file.path(pth, "figures", "sites-map-moll.png"), plot=mp, width=10, dpi=300)