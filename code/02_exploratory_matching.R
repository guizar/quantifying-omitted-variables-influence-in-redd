# Load preamble settings and functions
source(file.path("code", "00_preamble.R"), echo = TRUE)

# Load necessary libraries
suppressPackageStartupMessages(library(tidyverse))
library(dplyr)
library(knitr)
library(magrittr)
library(doParallel)
library(ggplot2)
library(MatchIt)


multi_core <- TRUE
cl <- parallel::makePSOCKcluster(ifelse(multi_core, 25, 1))
doParallel::registerDoParallel(cl)

# j=742 | PL1218 | Mahalanobis
# j=1030 | 1400| Mahalanobis
# j=1024
outl <- foreach::foreach(
                         j = 1:nrow(do_tab_match_init),
                         .verbose = TRUE,
                         .packages = c("tidyverse", "dplyr", "magrittr", "qqplotr", "ggplot2"),
                         .errorhandling = "pass"
                         ) %dopar%  {

  # Try-catch block to handle errors gracefully
  try_output <- try({
    
    # Retrieve current project information and parameters from do_tab
    
    id <- do_tab_match_init$id[j]  # Iter ID
    vcs_id <- do_tab_match_init$vcs_id[j] # VCS ID
    proj_id <- do_tab_match_init$proj_id[j]  # Current project ID
    caliper_val <- do_tab_match_init$caliper_val[j]  # Caliper value for matching
    dist_use <- do_tab_match_init$dist_use[j]  # Distance measure for matching
    match_w_replacement <- do_tab_match_init$match_w_replacement[j] # repl
  
    file_name <- paste0("id_",id,"_design_proj_", proj_id, "_caliper_", caliper_val, "_dist_", dist_use, "_replace_", match_w_replacement, ".RDS")
    
    # Check if all output files exist,
    if (file.exists(
    file.path(d_matched_dir, file_name)) && 
    file.exists(file.path(match_out_init_dir, file_name)) && 
    file.exists(file.path(match_dd_dir, file_name))) {
    cat(sprintf("All files exist for project %s. Skipping.\n", proj_id))
    return(NULL)
    }

  # IF matching file not produced then run the analysis
  d_qc <- readRDS(file = file.path(dir_qc_data, paste0("qc_data_", proj_id, ".RDS")))
  # 'reinforce' treat as a binary variable
  d_qc$treat <- as.integer(d_qc$treat)
  match_covars = unlist(str_split(do_tab_match_init$covars[j],', '))
  f <- reformulate(response = "treat",termlabels = match_covars)

  #########################################
  # MATCHING
  #########################################

  cov_terms <- attr(x = terms(f), which = "term.labels")
  all_terms <- c("treat", cov_terms)
  calipers <- rep(caliper_val, length(cov_terms))
  names(calipers) <- cov_terms

  # --- Fine-tune Mahalanobis dist? no longer used 
  # if (dist_use == 'mahalanobis') 
  # {v.mahvars = reformulate(response = NULL ,termlabels = match_covars); v.dist_use = 'glm' } else
  # {v.mahvars = NULL; v.dist_use = dist_use}

  # do match HERE
  match_out <- MatchIt::matchit(formula = f,
                                    data = d_qc,
                                    method = "nearest", 
                                    replace = match_w_replacement,
                                    caliper = calipers,
                                    estimand = "ATT",
                                    # mahvars = v.mahvars,
                                    distance = dist_use)

  # print(summary(match_out))
  d_qc %<>%
    mutate(weight = match_out$weights) %>%
    mutate(rounded_weight = round(weight)) %>%
    mutate(matched = match_out$weights != 0)
  d_matched <- d_qc %>%
    filter(matched) %>%
    uncount(rounded_weight, .remove = FALSE)
  for (var_curr in cov_terms) {
    d_matched[[paste0(var_curr,'_scl')]] <- c(scale(d_matched[[var_curr]]))
  }

  # get matching summary objects
  sm = summary(match_out)
  nn= as_tibble(sm[['nn']])
  nn$variable= row.names(sm[['nn']])
  sum.all= as_tibble(sm[['sum.all']])
  sum.all$variable= row.names(sm[['sum.all']])
  sum.matched= as_tibble(sm[['sum.matched']])
  sum.matched$variable= row.names(sm[['sum.matched']])

  ####################################################
  # Get diagnostics: matched proportions & covar balance
  #####################################################

  # get ratio matched
  matched_counts = nn %>% select(-Control) %>% filter(variable %in% c('All','Matched')) %>% spread(variable,Treated)
  n_treated = matched_counts$All
  ratio_matched = matched_counts$Matched/n_treated

  # get covariate balance 
  mean_diffs = sum.matched %>% filter(variable %in% match_covars) %>% .$`Std. Mean Diff.`
  all_covars_matched = all(abs(mean_diffs) < 0.25)
  mean_diffs = str_flatten(mean_diffs,', ')

  ####################################################
  # Simple DD 
  #####################################################
  
  # Replace scaled values with raw
  d_matched <- d_matched %>% mutate(area_forest_t_1990 = area_forest_t_1990_unscl,
  area_forest_t_minus_6 = area_forest_t_minus_6_unscl,
  area_forest_t_zero = area_forest_t_zero_unscl,
  area_loss_t_minus_6 = area_loss_t_minus_6_unscl,
  area_loss_t_zero = area_loss_t_zero_unscl,
  area_loss_t_plus_5 = area_loss_t_plus_5_unscl)
  
  # summarise
  d_matched_sum = d_matched %>% 
  ungroup() %>% 
  select(treat,gid,area_forest_t_1990, area_forest_t_minus_6, area_forest_t_zero, area_loss_t_minus_6, area_loss_t_zero, area_loss_t_plus_5,weight) %>% gather(var,val,-c(weight,gid,treat)) %>% rowwise() %>%
  mutate(val_wgt = val*weight) %>%
  group_by(treat,var) %>% summarise(val=sum(val_wgt)) %>% 
  group_by(treat) %>% spread(var,val)

  # condense into a oneliner
  d_matched_sum = d_matched_sum %>% mutate(Arealoss_Before=area_loss_t_zero-area_loss_t_minus_6, Arealoss_After=area_loss_t_plus_5-area_loss_t_zero) %>% mutate(f.treat = ifelse(treat==0,'Control','Treatment')) %>% 
  ungroup() %>% select(-treat) %>% myspread(f.treat,-c(f.treat))

  # Now get proportional DDs
  d_matched_sum = d_matched_sum %>% mutate(
  Control_Arealoss_Before_prop=Control_Arealoss_Before/Control_area_forest_t_minus_6,
  Control_Arealoss_After_prop=Control_Arealoss_After/Control_area_forest_t_minus_6,
  Treatment_Arealoss_Before_prop=Treatment_Arealoss_Before/Treatment_area_forest_t_minus_6,
  Treatment_Arealoss_After_prop=Treatment_Arealoss_After/Treatment_area_forest_t_minus_6,
  DD_Arealoss = ((Treatment_Arealoss_After - Control_Arealoss_After) - (Treatment_Arealoss_Before - Control_Arealoss_Before)),
  DD_Arealoss_prop = ((Treatment_Arealoss_After_prop - Control_Arealoss_After_prop) - (Treatment_Arealoss_Before_prop - Control_Arealoss_Before_prop)),
  After_Only_Arealoss = Treatment_Arealoss_After - Control_Arealoss_After,
  After_Only_Arealoss_prop = Treatment_Arealoss_After_prop - Control_Arealoss_After_prop)

  # Add metadata and diagnostic scores
  d_matched_sum = d_matched_sum %>% 
  mutate(id = id, vcs_id = vcs_id, proj_id = proj_id, n_treated =  n_treated, ratio_matched =  ratio_matched, all_covars_matched =  all_covars_matched, mean_diffs =  mean_diffs)

  ##########################################
  # EXPORT OBJECTS
  ##########################################

  file_name <- paste0("id_",id,"_design_proj_", proj_id, "_caliper_", caliper_val, "_dist_", dist_use, "_replace_", match_w_replacement, ".RDS")

  # simple DD & matching diagnostics 
  d_matched_sum = d_matched_sum[,c(c('id', 'vcs_id', 'proj_id'), colnames(d_matched_sum)[!colnames(d_matched_sum) %in% c('id', 'vcs_id', 'proj_id')])]  

  saveRDS(match_out, file = file.path(match_out_init_dir, file_name))
  saveRDS(d_matched_sum, file = file.path(match_dd_dir, file_name))
  })
  
  return(try_output)  # Return the result of the try block
}

# Make a control table to list successful/pending runs
do_tab_match_init$file_name <- paste0("id_",do_tab_match_init$id,"_design_proj_",do_tab_match_init$proj_id, "_caliper_",do_tab_match_init$caliper_val, "_dist_",do_tab_match_init$dist_use, "_replace_",do_tab_match_init$match_w_replacement, ".RDS")

do_tab_match_init$match_out_init_dir =  file.exists(file.path(match_out_init_dir, do_tab_match_init$file_name))
do_tab_match_init$match_dd_dir =  file.exists(file.path(match_dd_dir, do_tab_match_init$file_name))

do_tab_match_init %>%  select(-file_name) %>% 
  write_csv(file.path("data", "02_exploratory_matching_tracker.csv"))

stopCluster(cl)

# -----------------------
# Post matching validation
# --------------------------

## READ DATA & CREATE RDSBs
dd_summaries_file <- file.path("data","output","matching_init_summaries_dd.csv")

if (!file.exists(dd_summaries_file)) {
  
# define target object
dd_summaries = tibble()

# read details
f_all <- list.files(match_dd_dir)
f_all <- f_all[grep("\\.RDS$", f_all)]

# iterate through file list
for (i in 1:length(f_all)) {
    f <- str_remove(f_all[i], "\\.RDS")
    # temporary-fix
    f <- str_replace_all(f, pattern = "([0-9])design_", replacement = "\\1_design_")
    splits <- strsplit(f_all[f], split = "_")
    r_sums = readRDS(file = file.path(match_dd_dir, f_all[i]))
    # combine
    dd_summaries = bind_rows(dd_summaries,r_sums)
}
# WRITE to main project folder
write_csv(dd_summaries, dd_summaries_file)
dd_summaries <- dd_summaries[match(unique(dd_summaries$id), dd_summaries$id),]
} else {
  dd_summaries <- read_csv(dd_summaries_file, col_type = cols(vcs_id = col_character()))
  dd_summaries = dd_summaries[match(unique(dd_summaries$id), dd_summaries$id),]
}

# load matching params
d_match_init <- read_csv(file.path('data','do_tab_match_init.csv'), col_type = cols(vcs_id = col_character()))
d = dd_summaries %>% left_join(d_match_init)

# Define factors
cov_labs = sort(unique(d$covars))
algo_labs = sort(unique(d$dist_use))
# algo_labs

names(cov_labs) = c('Base', 'Base + FA (1990)', 'Base + FA 1990 + FA (t-6)', 'Base + FA (t-6)') 
names(algo_labs) = c('PSM','MHN','RFM')

d$f.covariates = factor(d$covars,labels=names(cov_labs))
d$f.covariates = factor(d$f.covariates, levels=names(cov_labs)[c(1,2,4,3)])
d$f.algorithm = factor(d$dist_use,labels=names(algo_labs))
d$f.algorithm = factor(d$f.algorithm, levels=names(algo_labs)[c(2,1,3)])
d$f.caliper = factor(d$caliper_val)
d$f.with_repl = factor(d$match_w_replacement)

# SUBSET SUCCESSFULLY MATCHED MODELS
d %<>% mutate(is_valid = ratio_matched >= 0.8  & all_covars_matched==T)
dd = d %>% filter(is_valid==T)

library(rcartocolor)
library(monochromeR)

ftcs=c('f.algorithm', 'f.covariates', 'f.caliper', 'f.with_repl')
base_col=carto_pal(7, "Temps")[c(3,1,5,7)]
names(base_col) =  ftcs

d_fact = data.frame()

for(e in ftcs) {
  # get counts by factor in each param
  d_tmp = dd %>% select(!!ensym(e)) %>% rename('param' = !!ensym(e)) %>% group_by(param) %>% tally() %>% arrange(desc(n))
  d_tmp$param = factor(d_tmp$param, levels=d_tmp$param)
  d_tmp$factor = e

  # now extract colours for each factor
  tmp_pal = generate_palette(base_col[e], modification = "go_lighter", n_colours = nrow(d_tmp), view_palette = F)
  d_tmp$col = tmp_pal

  d_fact %<>% bind_rows(d_tmp)
}

d_fact$factor = factor(d_fact$factor)
d_fact$factor = fct_relevel(d_fact$factor,ftcs)
d_fact$factor = factor(d_fact$factor,label= c('Algorithm', 'Covariates' ,'Caliper', 'Replacement'))
# d_fact$param = paste0(d_fact$factor,': ', d_fact$param)

ggp =  ggplot(d_fact, aes(factor,n, fill=param)) +
  geom_bar(stat='identity', color='black', position=position_dodge2(width=0.9, preserve = "single"),width=0.9, linewidth=0.3) +
  scale_fill_manual(values=d_fact$col) +
   guides(fill = guide_legend(ncol = 1)) +
  # geom_text(aes(label = n), vjust = -0.5, color = "black", size = 3, position = position_dodge(width = 0.9)) +
  theme_classic() +
  theme(
  panel.background = element_rect(fill = '#FFFFFF', color = NA),
  panel.border = element_rect( fill=NA, colour = "black", size=1),
  legend.position = "bottom",
  legend.title=element_blank(),
  axis.line = element_blank()) + xlab("") + ylab("")
  #   axis.text.x  = element_text(angle=45,hjust=0.95,vjust=0.2)

ggsave(file.path('figures', 'barchart-matching-factors.png'), ggp, width=6, height=8, units='in')
