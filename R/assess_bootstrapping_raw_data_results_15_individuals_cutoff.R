# AN R SCRIPT TO explore the bootstrap results
# and look at the raw data to see what it is showing.

# packages
# packages
library(ggplot2)
library(patchwork)
library(dplyr)
library(purrr)
library(tidyr)
library(readr)

# read in eBird data
ebird_data <- readRDS("Data/ebird_data_raw_May.RDS") %>% 
  bind_rows(readRDS("Data/ebird_data_raw_Jun.RDS")) %>%
  bind_rows(readRDS("Data/ebird_data_raw_Jul.RDS")) %>%
  bind_rows(readRDS("Data/ebird_data_raw_Aug.RDS")) %>%
  left_join(., read_csv("Data/checklists_mod_scores/ebird_samples_mod_scores.csv") %>%
              dplyr::select(first, SAMPLIN) %>%
              rename(ghm=first) %>%
              rename(SAMPLING_EVENT_IDENTIFIER=SAMPLIN), by="SAMPLING_EVENT_IDENTIFIER") %>%
  left_join(., read_csv("Data/Clements-Checklist-v2019-August-2019.csv") %>%
              dplyr::filter(category=="species") %>%
              dplyr::select(category, `English name`, `scientific name`, order, family) %>%
              rename(COMMON_NAME=`English name`,
                     SCIENTIFIC_NAME=`scientific name`)) %>%
  dplyr::filter(!family %in% c("Strigidae (Owls)", "Tytonidae (Barn-Owls)",
                               "Stercorariidae (Skuas and Jaegers)", "Alcidae (Auks, Murres, and Puffins)",
                               "Sulidae (Boobies and Gannets)", "Procellariidae (Shearwaters and Petrels)",
                               "Hydrobatidae (Northern Storm-Petrels)", "Oceanitidae (Southern Storm-Petrels)")) %>%
  dplyr::filter(complete.cases(BCR_CODE))

length(unique(ebird_data$SAMPLING_EVENT_IDENTIFIER))
length(unique(ebird_data$COMMON_NAME))


# get a list of BCRs
bcr_list <- ebird_data %>%
  dplyr::select(BCR_CODE) %>%
  distinct() %>%
  dplyr::filter(BCR_CODE != 0) %>%
  .$BCR_CODE

# write a function to summarize the bootstrap results
# to get a 'mean' response, averaging across the bootstrapping results
# super hacky because not all files may read in
# as they may not exist
# but I think gets the job done at least
summarize_boot_results <- function(BCR_number, index_name){
  
  message(paste0("Summarizing data for BCR ", BCR_number))
  
  file_list <- c(paste0("Intermediate_results/bootstrapping_15_individuals_cutoff/grid_size_0.1/number_samples_10/BCR_", BCR_number, ".RDS"),
                 paste0("Intermediate_results/bootstrapping_15_individuals_cutoff/grid_size_0.2/number_samples_10/BCR_", BCR_number, ".RDS"),
                 paste0("Intermediate_results/bootstrapping_15_individuals_cutoff/grid_size_0.3/number_samples_10/BCR_", BCR_number, ".RDS"),
                 paste0("Intermediate_results/bootstrapping_15_individuals_cutoff/grid_size_0.4/number_samples_10/BCR_", BCR_number, ".RDS"),
                 paste0("Intermediate_results/bootstrapping_15_individuals_cutoff/grid_size_0.5/number_samples_10/BCR_", BCR_number, ".RDS"),
                 paste0("Intermediate_results/bootstrapping_15_individuals_cutoff/grid_size_0.6/number_samples_10/BCR_", BCR_number, ".RDS"),
                 paste0("Intermediate_results/bootstrapping_15_individuals_cutoff/grid_size_0.7/number_samples_10/BCR_", BCR_number, ".RDS"),
                 paste0("Intermediate_results/bootstrapping_15_individuals_cutoff/grid_size_0.8/number_samples_10/BCR_", BCR_number, ".RDS"),
                 paste0("Intermediate_results/bootstrapping_15_individuals_cutoff/grid_size_0.9/number_samples_10/BCR_", BCR_number, ".RDS"),
                 paste0("Intermediate_results/bootstrapping_15_individuals_cutoff/grid_size_1/number_samples_10/BCR_", BCR_number, ".RDS"))
  
  file.exists(file_list)
  
  read_data_0.1 <- function(x){
    
    dat <- readRDS(x) %>%
      mutate(grid_size=0.1)
  }
  
  read_data_0.2 <- function(x){
    
    dat <- readRDS(x) %>%
      mutate(grid_size=0.2)
  }
  
  read_data_0.3 <- function(x){
    
    dat <- readRDS(x) %>%
      mutate(grid_size=0.3)
  }
  
  read_data_0.4 <- function(x){
    
    dat <- readRDS(x) %>%
      mutate(grid_size=0.4)
  }
  
  read_data_0.5 <- function(x){
    
    dat <- readRDS(x) %>%
      mutate(grid_size=0.5)
  }
  
  read_data_0.6 <- function(x){
    
    dat <- readRDS(x) %>%
      mutate(grid_size=0.6)
  }
  
  read_data_0.7 <- function(x){
    
    dat <- readRDS(x) %>%
      mutate(grid_size=0.7)
  }
  
  read_data_0.8 <- function(x){
    
    dat <- readRDS(x) %>%
      mutate(grid_size=0.8)
  }
  
  read_data_0.9 <- function(x){
    
    dat <- readRDS(x) %>%
      mutate(grid_size=0.9)
  }
  
  read_data_1 <- function(x){
    
    dat <- readRDS(x) %>%
      mutate(grid_size=1)
  }
  
  dat_0.1 <- bind_rows(lapply(file_list[1] %>%
                                as.data.frame() %>%
                                mutate(exists=ifelse(file.exists(.)==TRUE, "yes", "no")) %>%
                                dplyr::filter(exists=="yes") %>%
                                .$., read_data_0.1))
  
  dat_0.2 <- bind_rows(lapply(file_list[2] %>%
                                as.data.frame() %>%
                                mutate(exists=ifelse(file.exists(.)==TRUE, "yes", "no")) %>%
                                dplyr::filter(exists=="yes") %>%
                                .$., read_data_0.2))
  
  dat_0.3 <- bind_rows(lapply(file_list[3] %>%
                                as.data.frame() %>%
                                mutate(exists=ifelse(file.exists(.)==TRUE, "yes", "no")) %>%
                                dplyr::filter(exists=="yes") %>%
                                .$., read_data_0.3))
  
  dat_0.4 <- bind_rows(lapply(file_list[4] %>%
                                as.data.frame() %>%
                                mutate(exists=ifelse(file.exists(.)==TRUE, "yes", "no")) %>%
                                dplyr::filter(exists=="yes") %>%
                                .$., read_data_0.4))
  
  dat_0.5 <- bind_rows(lapply(file_list[5] %>%
                                as.data.frame() %>%
                                mutate(exists=ifelse(file.exists(.)==TRUE, "yes", "no")) %>%
                                dplyr::filter(exists=="yes") %>%
                                .$., read_data_0.5))
  
  dat_0.6 <- bind_rows(lapply(file_list[6] %>%
                                as.data.frame() %>%
                                mutate(exists=ifelse(file.exists(.)==TRUE, "yes", "no")) %>%
                                dplyr::filter(exists=="yes") %>%
                                .$., read_data_0.6))
  
  dat_0.7 <- bind_rows(lapply(file_list[7] %>%
                                as.data.frame() %>%
                                mutate(exists=ifelse(file.exists(.)==TRUE, "yes", "no")) %>%
                                dplyr::filter(exists=="yes") %>%
                                .$., read_data_0.7))
  
  dat_0.8 <- bind_rows(lapply(file_list[8] %>%
                                as.data.frame() %>%
                                mutate(exists=ifelse(file.exists(.)==TRUE, "yes", "no")) %>%
                                dplyr::filter(exists=="yes") %>%
                                .$., read_data_0.8))
  
  dat_0.9 <- bind_rows(lapply(file_list[9] %>%
                                as.data.frame() %>%
                                mutate(exists=ifelse(file.exists(.)==TRUE, "yes", "no")) %>%
                                dplyr::filter(exists=="yes") %>%
                                .$., read_data_0.9))
  
  dat_1 <- bind_rows(lapply(file_list[10] %>%
                              as.data.frame() %>%
                              mutate(exists=ifelse(file.exists(.)==TRUE, "yes", "no")) %>%
                              dplyr::filter(exists=="yes") %>%
                              .$., read_data_1))
  dat <- bind_rows(dat_0.1,
                   dat_0.2,
                   dat_0.3,
                   dat_0.4,
                   dat_0.5,
                   dat_0.6,
                   dat_0.7,
                   dat_0.8,
                   dat_0.9,
                   dat_1)
  
  temp_dat <- dat %>%
    dplyr::filter(index==index_name) %>%
    dplyr::filter(scale %in% c("alpha", "gamma"))
  
  # get mean bootstrap responses
  # across 100 different draws
  # maybe a better way to do this?
  summary <- temp_dat %>%
    mutate(ghm=round(ghm, digits=2)) %>%
    group_by(scale, index, grid_size, checklists_per_grid, ghm) %>%
    summarize(mean_value=mean(value)) %>%
    mutate(BCR_CODE=BCR_number)
  
  return(summary)
  
}

# make a fault tolerant lapply
lapply_with_error <- function(X,FUN,...){    
  lapply(X, function(x, ...) tryCatch(FUN(x, ...),
                                      error=function(e) NULL))
}

# Now can use this function to get mean results 
# across the different bootstrapping analysis
# and different indices
all_raw_data_S <- bind_rows(lapply_with_error(bcr_list, function(x){summarize_boot_results(x, "S")}))
all_raw_data_S_n <- bind_rows(lapply_with_error(bcr_list, function(x){summarize_boot_results(x, "S_n")}))
all_raw_data_N <- bind_rows(lapply_with_error(bcr_list, function(x){summarize_boot_results(x, "N")}))
all_raw_data_S_PIE <- bind_rows(lapply_with_error(bcr_list, function(x){summarize_boot_results(x, "S_PIE")}))
all_raw_data_beta_S <- bind_rows(lapply_with_error(bcr_list, function(x){summarize_boot_results(x, "beta_S")}))
all_raw_data_beta_S_n <- bind_rows(lapply_with_error(bcr_list, function(x){summarize_boot_results(x, "beta_S_n")}))
all_raw_data_beta_S_PIE <- bind_rows(lapply_with_error(bcr_list, function(x){summarize_boot_results(x, "beta_S_PIE")}))


###################################################
###################################################
########### PLOT RESULTS FOR ALL BCRS at once #####
###################################################
# First for "S"
gam_S <- all_raw_data_S %>%
  bind_rows(all_raw_data_beta_S %>%
              dplyr::filter(index=="beta_S") %>%
              mutate(scale="beta")) %>%
  mutate(scale=factor(scale, levels=c("alpha", "gamma", "beta"))) %>%
  dplyr::filter(grid_size==0.5) %>%
  ggplot(., aes(x=ghm, y=mean_value, 
                color=as.factor(scale), group=as.factor(scale)))+
  geom_smooth(method="gam", se=TRUE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  #facet_wrap(~scale)+
  xlab("Global Human Modification")+
  ylab("S")+
  scale_color_brewer(palette = "Set1")+
  guides(color=guide_legend(title="Grain size"))+
  ggtitle("(A)")+
  theme(legend.position="none")

gam_S

# Now repeat that but for S_n
gam_S_n <- all_raw_data_S_n %>%
  bind_rows(all_raw_data_beta_S_n %>%
              dplyr::filter(index=="beta_S_n") %>%
              mutate(scale="beta")) %>%
  mutate(scale=factor(scale, levels=c("alpha", "gamma", "beta"))) %>%
  dplyr::filter(grid_size==0.5) %>%
  ggplot(., aes(x=ghm, y=mean_value, 
                color=as.factor(scale), group=as.factor(scale)))+
  geom_smooth(method="gam", se=TRUE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  #facet_wrap(~scale)+
  xlab("Global Human Modification")+
  ylab("S_n")+
  scale_color_brewer(palette = "Set1")+
  guides(color=guide_legend(title="Grain size"))+
  ggtitle("(B)")+
  theme(legend.position="none")

gam_S_n

# Now repeat that but for S_PIE
gam_S_PIE <- all_raw_data_S_PIE %>%
  bind_rows(all_raw_data_beta_S_PIE %>%
              dplyr::filter(index=="beta_S_PIE") %>%
              mutate(scale="beta")) %>%
  mutate(scale=factor(scale, levels=c("alpha", "gamma", "beta"))) %>%
  dplyr::filter(grid_size==0.5) %>%
  ggplot(., aes(x=ghm, y=mean_value, 
                color=as.factor(scale), group=as.factor(scale)))+
  geom_smooth(method="gam", se=TRUE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  #facet_wrap(~scale)+
  xlab("Global Human Modification")+
  ylab("S_PIE")+
  scale_color_brewer(palette = "Set1")+
  guides(color=guide_legend(title="Grain size"))+
  ggtitle("(C)")+
  theme(legend.position="bottom")

gam_S_PIE

# ONE MORE TIME, but for N
gam_N <- all_raw_data_N %>%
  dplyr::filter(grid_size==0.5) %>%
  ggplot(., aes(x=ghm, y=mean_value, 
                color=as.factor(scale), group=as.factor(scale)))+
  geom_smooth(method="gam", se=TRUE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  #facet_wrap(~scale)+
  xlab("Global Human Modification")+
  ylab("N")+
  scale_color_brewer(palette = "Set1")+
  guides(color=guide_legend(title="Grain size"))+
  ggtitle("(D)")+
  theme(legend.position="none")

gam_N

# Put plots together into one
gam_S + gam_S_n + gam_S_PIE + gam_N + plot_layout(ncol=2)

ggsave("Figures/0.5_degree_results_only.png", width=8.5, height=6.8, units="in")



# Fit a model for each grid size
# and loop through the three diversity variables possible
model_function <- function(size_of_grid, data){
  
  tmp <- data %>%
    ungroup() %>%
    dplyr::filter(grid_size==size_of_grid) %>%
    mutate(BCR_CODE=as.factor(as.integer(BCR_CODE)))
  
  metric_scale_function <- function(diversity_scale){
    
    tmp2 <- tmp %>%
      dplyr::filter(scale==diversity_scale)
    
    ggplot(tmp2, aes(x=mean_value))+
      geom_histogram()
    
    mod <- mgcv::gam(mean_value ~ s(ghm, bs="cs", k=10) + s(BCR_CODE, bs="re"), data=tmp2)
    
    summary(mod)
    
    newdat <- data.frame(ghm=seq(0, 1, by=0.01))
    
    predicted_values=data.frame(ghm=newdat$ghm,
                                predicted_value=predict(mod, newdata = newdat, type="response",
                                                        exclude="s(BCR_CODE)", newdata.guaranteed=TRUE),
                                se=predict(mod, newdata = newdat, type="response", exclude="s(BCR_CODE)", 
                                           newdata.guaranteed=TRUE, se.fit=TRUE)[2]$se.fit) %>%
      mutate(upr_95=predicted_value+(1.96*se)) %>%
      mutate(lwr_95=predicted_value-(1.96*se))
    
    ggplot(predicted_values, aes(x=ghm, y=predicted_value))+
      geom_point()
    
    difference <- (1-(predicted_values %>%
                        dplyr::filter(ghm==1) %>%
                        .$predicted_value)/(predicted_values %>%
                                              dplyr::filter(ghm==0) %>%
                                              .$predicted_value))*100
    
    max_difference <- (1-(predicted_values %>%
                            dplyr::filter(ghm==1) %>%
                            .$predicted_value)/(predicted_values %>%
                                                  .$predicted_value %>%
                                                  max(.)))*100
    
    max_ghm <- predicted_values %>%
      arrange(desc(predicted_value)) %>%
      slice(1) %>%
      .$ghm
    
    summary_df <- predicted_values %>%
      mutate(difference=difference) %>%
      mutate(max_difference=max_difference) %>%
      mutate(max_ghm=max_ghm) %>%
      mutate(grain_size=size_of_grid) %>%
      mutate(number_of_obs=summary(mod)$n) %>%
      mutate(p_value=summary(mod)$s.table[1,4]) %>%
      mutate(scale=diversity_scale)
    
    return(summary_df)
    
  }
  
  temp_results <- bind_rows(lapply(unique(tmp$scale), metric_scale_function))
  
}

modelled_data_S <- bind_rows(lapply(unique(all_raw_data_S$grid_size), function(x){model_function(x, all_raw_data_S)})) %>%
  mutate(response="S")
modelled_data_S_n <- bind_rows(lapply(unique(all_raw_data_S_n$grid_size), function(x){model_function(x, all_raw_data_S_n)})) %>%
  mutate(response="S_n")
modelled_data_N <- bind_rows(lapply(unique(all_raw_data_N$grid_size), function(x){model_function(x, all_raw_data_N)})) %>%
  mutate(response="N")
modelled_data_S_PIE <- bind_rows(lapply(unique(all_raw_data_S_PIE$grid_size), function(x){model_function(x, all_raw_data_S_PIE)})) %>%
  mutate(response="S_PIE")
modelled_data_beta_S <- bind_rows(lapply(unique(all_raw_data_S$grid_size), function(x){model_function(x, all_raw_data_beta_S)})) %>%
  mutate(response="S") %>%
  mutate(scale="beta")
modelled_data_beta_S_n <- bind_rows(lapply(unique(all_raw_data_S_n$grid_size), function(x){model_function(x, all_raw_data_beta_S_n)})) %>%
  mutate(response="S_n") %>%
  mutate(scale="beta")
modelled_data_beta_S_PIE <- bind_rows(lapply(unique(all_raw_data_S_PIE$grid_size), function(x){model_function(x, all_raw_data_beta_S_PIE)})) %>%
  mutate(response="S_PIE") %>%
  mutate(scale="beta")


modelled_data_results <- modelled_data_S %>%
  bind_rows(modelled_data_S_n) %>%
  bind_rows(modelled_data_N) %>%
  bind_rows(modelled_data_S_PIE) %>%
  bind_rows(modelled_data_beta_S) %>%
  bind_rows(modelled_data_beta_S_n) %>%
  bind_rows(modelled_data_beta_S_PIE)

##########################################
##########################################
######## NOW PLOT MODELLED RESULTS
##########################################
##########################################
gam_S <- modelled_data_results %>%
  dplyr::filter(response=="S") %>%
  dplyr::filter(scale != "beta") %>%
  mutate(scale=factor(scale, levels=c("alpha", "gamma"))) %>%
  dplyr::filter(grain_size==0.5) %>%
  ggplot(.)+
  geom_ribbon(aes(x=ghm, y=predicted_value, ymax=upr_95, ymin=lwr_95, fill=as.factor(scale)), alpha=0.6)+
  geom_line(aes(x=ghm, y=predicted_value, 
                color=as.factor(scale)), size=1.2)+
  #geom_smooth(method="gam", se=TRUE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  #facet_wrap(~scale)+
  xlab("")+
  ylab("S")+
  scale_color_brewer(palette = "Set1")+
  scale_fill_brewer(palette = "Set1")+
  guides(color=guide_legend(title="Grain size"))+
  ggtitle("(B)")+
  theme(legend.position="none")

gam_S

gam_S_n <- modelled_data_results %>%
  dplyr::filter(response=="S_n") %>%
  dplyr::filter(scale != "beta") %>%
  mutate(scale=factor(scale, levels=c("alpha", "gamma"))) %>%
  dplyr::filter(grain_size==0.5) %>%
  ggplot(.)+
  geom_ribbon(aes(x=ghm, y=predicted_value, ymax=upr_95, ymin=lwr_95, fill=as.factor(scale)), alpha=0.6)+
  geom_line(aes(x=ghm, y=predicted_value, 
                color=as.factor(scale)), size=1.2)+
  #geom_smooth(method="gam", se=TRUE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  #facet_wrap(~scale)+
  xlab("Global Human Modification")+
  ylab("S_n")+
  scale_color_brewer(palette = "Set1")+
  scale_fill_brewer(palette = "Set1")+
  guides(color=guide_legend(title="Grain size"))+
  ggtitle("(C)")+
  theme(legend.position="none")

gam_S_n

gam_S_PIE <- modelled_data_results %>%
  dplyr::filter(response=="S_PIE") %>%
  dplyr::filter(scale != "beta") %>%
  mutate(scale=factor(scale, levels=c("alpha", "gamma"))) %>%
  dplyr::filter(grain_size==0.5) %>%
  ggplot(.)+
  geom_ribbon(aes(x=ghm, y=predicted_value, ymax=upr_95, ymin=lwr_95, fill=as.factor(scale)), alpha=0.6)+
  geom_line(aes(x=ghm, y=predicted_value, 
                color=as.factor(scale)), size=1.2)+
  #geom_smooth(method="gam", se=TRUE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  #facet_wrap(~scale)+
  xlab("Global Human Modification")+
  ylab("S_PIE")+
  scale_color_brewer(palette = "Set1")+
  scale_fill_brewer(palette = "Set1")+
  guides(color=guide_legend(title="Grain size"))+
  guides(fill=FALSE)+
  ggtitle("(D)")+
  theme(legend.position="none")

gam_S_PIE

gam_N <- modelled_data_results %>%
  dplyr::filter(response=="N") %>%
  mutate(scale=factor(scale, levels=c("alpha", "gamma"))) %>%
  dplyr::filter(grain_size==0.5) %>%
  ggplot(.)+
  geom_ribbon(aes(x=ghm, y=predicted_value, ymax=upr_95, ymin=lwr_95, fill=as.factor(scale)), alpha=0.6)+
  geom_line(aes(x=ghm, y=predicted_value, 
                color=as.factor(scale)), size=1.2)+
  #geom_smooth(method="gam", se=TRUE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  #facet_wrap(~scale)+
  xlab("")+
  ylab("N")+
  scale_color_brewer(palette = "Set1")+
  scale_fill_brewer(palette = "Set1")+
  guides(color=guide_legend(title=""))+
  guides(fill=FALSE)+
  ggtitle("(A)")+
  theme(legend.position="none")

gam_N

# make a beta figure
beta_fig <- modelled_data_results %>%
  dplyr::filter(response %in% c("S", "S_n", "S_PIE")) %>%
  dplyr::filter(scale=="beta") %>%
  dplyr::filter(grain_size==0.5) %>%
  ggplot(.)+
  geom_ribbon(aes(x=ghm, y=predicted_value, ymax=upr_95, ymin=lwr_95, group=response), fill="gray60")+
  geom_line(aes(x=ghm, y=predicted_value, 
                linetype=as.factor(response), group=as.factor(response)), size=1.2)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("Global Human Modification")+
  ylab("Beta diversity value")+
  guides(linetype=guide_legend(title="Biodiversity metric:"))+
  ggtitle("(E)")+
  theme(legend.position="bottom")+
  theme(legend.key.size = unit(4,"line"))

beta_fig


# Put plots together into one
(gam_N | gam_S) / (gam_S_n | gam_S_PIE) / beta_fig + plot_layout(nrow=3)

ggsave("Figures/0.5_degree_results_only_15_individuals.png", width=7.5, height=7.2, units="in")




# Make a visualization showing the spatial scale effect and robustness of the results
# to spatial grain
gam_S_all_grids <- modelled_data_results %>%
  dplyr::filter(response=="S") %>%
  mutate(scale=factor(scale, levels=c("alpha", "gamma", "beta"))) %>%
  ggplot(., aes(x=ghm, y=predicted_value, 
                color=as.factor(grain_size), group=as.factor(grain_size)))+
  geom_line(size=1.2)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~scale, scales="free", ncol=1)+
  xlab("Global Human Modification")+
  ylab("S")+
  scale_color_brewer(palette = "Spectral")+
  guides(color=guide_legend(title="Grain size"))+
  ggtitle("(A)")+
  theme(legend.position="bottom")+
  theme(legend.text=element_text(size=6))

gam_S_all_grids

# plot the 'difference' as a function of grain size
max_difference_S <- modelled_data_results %>%
  dplyr::filter(response=="S") %>%
  mutate(scale=factor(scale, levels=c("alpha", "gamma", "beta"))) %>%
  dplyr::select(scale, grain_size, max_difference) %>%
  distinct() %>%
  ggplot(., aes(x=grain_size, y=max_difference, color=as.factor(grain_size)))+
  geom_point()+
  geom_smooth(method="loess", se=FALSE, color="black", linetype="dashed")+
  facet_wrap(~scale, scales="free", ncol=1)+
  xlab("Grain size")+
  ylab("Maximum difference (%) in S")+
  scale_color_brewer(palette = "Spectral")+
  theme_bw()+
  ggtitle("(B)")+
  theme(axis.text=element_text(color="black"))+
  guides(color=FALSE)

max_difference_S

gam_S_all_grids + max_difference_S + plot_layout(ncol=2)

ggsave("Figures/S_grain_size_results.png", width=7.4, height=8.6, units="in")

# Make a visualization showing the spatial scale effect and robustness of the results
# to spatial scale
gam_S_n_all_grids <- modelled_data_results %>%
  dplyr::filter(response=="S_n") %>%
  mutate(scale=factor(scale, levels=c("alpha", "gamma", "beta"))) %>%
  ggplot(., aes(x=ghm, y=predicted_value, 
                color=as.factor(grain_size), group=as.factor(grain_size)))+
  geom_line(size=1.2)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~scale, scales="free", ncol=1)+
  xlab("Global Human Modification")+
  ylab("S_n")+
  scale_color_brewer(palette = "Spectral")+
  guides(color=guide_legend(title="Grain size"))+
  ggtitle("(A)")+
  theme(legend.position="bottom")+
  theme(legend.text=element_text(size=6))

gam_S_n_all_grids

# plot the 'difference' as a function of grain size
max_difference_S_n <- modelled_data_results %>%
  dplyr::filter(response=="S_n") %>%
  mutate(scale=factor(scale, levels=c("alpha", "gamma", "beta"))) %>%
  dplyr::select(scale, grain_size, max_difference) %>%
  distinct() %>%
  ggplot(., aes(x=grain_size, y=max_difference, color=as.factor(grain_size)))+
  geom_point()+
  geom_smooth(method="loess", se=FALSE, color="black", linetype="dashed")+
  facet_wrap(~scale, scales="free", ncol=1)+
  xlab("Grain size")+
  ylab("Maximum difference (%) in S_n")+
  scale_color_brewer(palette = "Spectral")+
  theme_bw()+
  ggtitle("(B)")+
  theme(axis.text=element_text(color="black"))+
  guides(color=FALSE)

max_difference_S_n

gam_S_n_all_grids + max_difference_S_n + plot_layout(ncol=2)

ggsave("Figures/S_n_grain_size_results.png", width=6.8, height=8.6, units="in")


# Make a visualization showing the spatial scale effect and robustness of the results
# to spatial scale
gam_S_PIE_all_grids <- modelled_data_results %>%
  dplyr::filter(response=="S_PIE") %>%
  mutate(scale=factor(scale, levels=c("alpha", "gamma", "beta"))) %>%
  ggplot(., aes(x=ghm, y=predicted_value, 
                color=as.factor(grain_size), group=as.factor(grain_size)))+
  geom_line(size=1.2)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~scale, scales="free", ncol=1)+
  xlab("Global Human Modification")+
  ylab("S_PIE")+
  scale_color_brewer(palette = "Spectral")+
  guides(color=guide_legend(title="Grain size"))+
  ggtitle("(A)")+
  theme(legend.position="bottom")+
  theme(legend.text=element_text(size=6))

gam_S_PIE_all_grids

# plot the 'difference' as a function of grain size
max_difference_S_PIE <- modelled_data_results %>%
  dplyr::filter(response=="S_PIE") %>%
  mutate(scale=factor(scale, levels=c("alpha", "gamma", "beta"))) %>%
  dplyr::select(scale, grain_size, max_difference) %>%
  distinct() %>%
  ggplot(., aes(x=grain_size, y=max_difference, color=as.factor(grain_size)))+
  geom_point()+
  geom_smooth(method="loess", se=FALSE, color="black", linetype="dashed")+
  facet_wrap(~scale, scales="free", ncol=1)+
  xlab("Grain size")+
  ylab("Maximum difference (%) in S_PIE")+
  scale_color_brewer(palette = "Spectral")+
  theme_bw()+
  ggtitle("(B)")+
  theme(axis.text=element_text(color="black"))+
  guides(color=FALSE)

max_difference_S_PIE

gam_S_PIE_all_grids + max_difference_S_PIE + plot_layout(ncol=2)

ggsave("Figures/S_PIE_grain_size_results.png", width=6.8, height=8.6, units="in")

# Make a visualization showing the spatial scale effect and robustness of the results
# to spatial scale
gam_N_all_grids <- modelled_data_results %>%
  dplyr::filter(response=="N") %>%
  mutate(scale=factor(scale, levels=c("alpha", "gamma"))) %>%
  ggplot(., aes(x=ghm, y=predicted_value, 
                color=as.factor(grain_size), group=as.factor(grain_size)))+
  geom_line(size=1.2)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~scale, scales="free", ncol=1)+
  xlab("Global Human Modification")+
  ylab("N")+
  scale_color_brewer(palette = "Spectral")+
  guides(color=guide_legend(title="Grain size"))+
  ggtitle("(A)")+
  theme(legend.position="bottom")+
  theme(legend.text=element_text(size=6))

gam_N_all_grids

# plot the 'difference' as a function of grain size
max_difference_N <- modelled_data_results %>%
  dplyr::filter(response=="N") %>%
  mutate(scale=factor(scale, levels=c("alpha", "gamma"))) %>%
  dplyr::select(scale, grain_size, max_difference) %>%
  distinct() %>%
  ggplot(., aes(x=grain_size, y=max_difference, color=as.factor(grain_size)))+
  geom_point()+
  geom_smooth(method="loess", se=FALSE, color="black", linetype="dashed")+
  facet_wrap(~scale, scales="free", ncol=1)+
  xlab("Grain size")+
  ylab("Maximum difference (%) in N")+
  scale_color_brewer(palette = "Spectral")+
  theme_bw()+
  ggtitle("(B)")+
  theme(axis.text=element_text(color="black"))+
  guides(color=FALSE)

max_difference_N

gam_N_all_grids + max_difference_N + plot_layout(ncol=2)

ggsave("Figures/N_grain_size_results.png", width=6.8, height=8.6, units="in")


# SUMMARIZE SOME STUFF FOR TEXTUAL DESCRIPTION OF EMPIRICAL PATTERNS...
modelled_data_results %>%
  dplyr::select(6:13) %>%
  distinct() %>%
  group_by(response, scale) %>%
  summarize(mean_max_ghm=mean(max_ghm),
            sd_max_ghm=sd(max_ghm))

modelled_data_results %>%
  dplyr::select(6:13) %>%
  distinct() %>%
  group_by(response, scale) %>%
  summarize(mean_percent_change=mean(max_difference),
            sd_percent_change=sd(max_difference))

modelled_data_results %>%
  dplyr::select(6:13) %>%
  dplyr::filter(grain_size==0.5) %>%
  distinct()

modelled_data_results %>%
  dplyr::select(6:13) %>%
  distinct() %>%
  ggplot(., aes(x=response, y=max_ghm, fill=scale))+
  geom_boxplot(position=position_dodge())+
  theme_bw()+
  coord_flip()

modelled_data_results %>%
  dplyr::select(6:13) %>%
  dplyr::filter(scale %in% c("alpha", "gamma")) %>%
  distinct() %>%
  ggplot(., aes(x=response, y=max_difference, fill=scale))+
  geom_boxplot(position=position_dodge())+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  coord_flip()+
  scale_fill_brewer(palette = "Set1")+
  xlab("Biodiversity component")+
  ylab("Maximum difference (%) in diversity change along the ghm gradient")

ggsave("Figures/maximum_difference_figure.png", width=6.8, height=5.6, units="in")




















###############################################
############ ALTERNATIVE VISUALIATION THAT ISN"T AS GOOD
########################################################




# Now for beta
lm_S_PIE_beta <- all_raw_data_beta_S_PIE %>%
  ggplot(., aes(x=ghm, y=mean_value, 
                color=as.factor(grid_size), group=as.factor(grid_size)))+
  geom_smooth(method="lm", se=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~scale, scales="free")+
  xlab("Global Human Modification")+
  ylab("beta_S_PIE")+
  scale_color_brewer(palette = "Spectral")+
  guides(color=guide_legend(title="Grain size"))+
  ggtitle("C")+
  theme(legend.position="none")

lm_S_PIE_beta

gam_S_PIE <- all_raw_data_S_PIE %>%
  ggplot(., aes(x=ghm, y=mean_value, 
                color=as.factor(grid_size), group=as.factor(grid_size)))+
  geom_smooth(method="gam", se=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~scale, scales="free")+
  xlab("Global Human Modification")+
  ylab("S_PIE")+
  scale_color_brewer(palette = "Spectral")+
  guides(color=guide_legend(title="Grain size"))+
  ggtitle("C")+
  theme(legend.position="none")

gam_S_PIE





# MAke a potentially figure 2
# only look at checklists=10
# and plot for alpha, gamma, and beta
# for S, S_n, S_PIE
# First let's do alpha scale
lm_all_alpha <- all_raw_data_N %>%
  dplyr::filter(scale=="alpha") %>%
  dplyr::filter(checklists_per_grid==10) %>%
  bind_rows(all_raw_data_S %>%
              dplyr::filter(scale=="alpha") %>%
              dplyr::filter(checklists_per_grid==10)) %>%
  bind_rows(all_raw_data_S_PIE %>%
              dplyr::filter(scale=="alpha") %>%
              dplyr::filter(checklists_per_grid==10)) %>%
  bind_rows(all_raw_data_S_n %>%
              dplyr::filter(scale=="alpha") %>%
              dplyr::filter(checklists_per_grid==10)) %>%
  mutate(index=factor(index, levels=c("S", "S_n", "S_PIE", "N"))) %>%
  ggplot(., aes(x=ghm, y=mean_value, color=as.factor(grid_size)))+
  geom_smooth(method="lm", se=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~index, scales="free")+
  xlab("Global Human Modification")+
  ylab("MoB variable")+
  ggtitle(paste0("A) alpha scale across all BCRs"))+
  scale_color_brewer(palette = "Spectral")+
  guides(color=guide_legend(title="Grain size"))

lm_all_alpha

gam_all_alpha <- all_raw_data_N %>%
  dplyr::filter(scale=="alpha") %>%
  dplyr::filter(checklists_per_grid==10) %>%
  bind_rows(all_raw_data_S %>%
              dplyr::filter(scale=="alpha") %>%
              dplyr::filter(checklists_per_grid==10)) %>%
  bind_rows(all_raw_data_S_PIE %>%
              dplyr::filter(scale=="alpha") %>%
              dplyr::filter(checklists_per_grid==10)) %>%
  bind_rows(all_raw_data_S_n %>%
              dplyr::filter(scale=="alpha") %>%
              dplyr::filter(checklists_per_grid==10)) %>%
  mutate(index=factor(index, levels=c("S", "S_n", "S_PIE", "N"))) %>%
  ggplot(., aes(x=ghm, y=mean_value, color=as.factor(grid_size)))+
  geom_smooth(method="gam", se=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~index, scales="free")+
  xlab("Global Human Modification")+
  ylab("MoB variable")+
  ggtitle(paste0("A) alpha scale across all BCRs"))+
  scale_color_brewer(palette = "Spectral")+
  guides(color=guide_legend(title="Grain size"))

gam_all_alpha

lm_all_gamma <- all_raw_data_N %>%
  dplyr::filter(scale=="gamma") %>%
  dplyr::filter(checklists_per_grid==10) %>%
  bind_rows(all_raw_data_S %>%
              dplyr::filter(scale=="gamma") %>%
              dplyr::filter(checklists_per_grid==10)) %>%
  bind_rows(all_raw_data_S_PIE %>%
              dplyr::filter(scale=="gamma") %>%
              dplyr::filter(checklists_per_grid==10)) %>%
  bind_rows(all_raw_data_S_n %>%
              dplyr::filter(scale=="gamma") %>%
              dplyr::filter(checklists_per_grid==10)) %>%
  mutate(index=factor(index, levels=c("S", "S_n", "S_PIE", "N"))) %>%
  ggplot(., aes(x=ghm, y=mean_value, color=as.factor(grid_size)))+
  geom_smooth(method="lm", se=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~index, scales="free")+
  xlab("Global Human Modification")+
  ylab("MoB variable")+
  ggtitle(paste0("B) gamma scale across all BCRs"))+
  scale_color_brewer(palette = "Spectral")+
  guides(color=guide_legend(title="Grain size"))

lm_all_gamma

gam_all_gamma <- all_raw_data_N %>%
  dplyr::filter(scale=="gamma") %>%
  dplyr::filter(checklists_per_grid==10) %>%
  bind_rows(all_raw_data_S %>%
              dplyr::filter(scale=="gamma") %>%
              dplyr::filter(checklists_per_grid==10)) %>%
  bind_rows(all_raw_data_S_PIE %>%
              dplyr::filter(scale=="gamma") %>%
              dplyr::filter(checklists_per_grid==10)) %>%
  bind_rows(all_raw_data_S_n %>%
              dplyr::filter(scale=="gamma") %>%
              dplyr::filter(checklists_per_grid==10)) %>%
  mutate(index=factor(index, levels=c("S", "S_n", "S_PIE", "N"))) %>%
  ggplot(., aes(x=ghm, y=mean_value, color=as.factor(grid_size)))+
  geom_smooth(method="gam", se=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~index, scales="free")+
  xlab("Global Human Modification")+
  ylab("MoB variable")+
  ggtitle(paste0("B) gamma scale across all BCRs"))+
  scale_color_brewer(palette = "Spectral")+
  guides(color=guide_legend(title="Grain size"))

gam_all_gamma

lm_all_beta <- all_raw_data_beta_S %>%
  dplyr::filter(scale=="alpha") %>%
  dplyr::filter(checklists_per_grid==10) %>%
  bind_rows(all_raw_data_beta_S_PIE %>%
              dplyr::filter(scale=="alpha") %>%
              dplyr::filter(checklists_per_grid==10)) %>%
  bind_rows(all_raw_data_beta_S_n %>%
              dplyr::filter(scale=="alpha") %>%
              dplyr::filter(checklists_per_grid==10)) %>%
  mutate(scale="beta") %>%
  mutate(index=gsub("beta_", "", index)) %>%
  ggplot(., aes(x=ghm, y=mean_value, color=as.factor(grid_size)))+
  geom_smooth(method="lm", se=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~index, ncol=2, scales="free")+
  xlab("Global Human Modification")+
  ylab("MoB variable")+
  ggtitle(paste0("C) beta scale across all BCRs"))+
  scale_color_brewer(palette = "Spectral")+
  guides(color=guide_legend(title="Grain size"))

lm_all_beta

gam_all_beta <- all_raw_data_beta_S %>%
  dplyr::filter(scale=="alpha") %>%
  dplyr::filter(checklists_per_grid==10) %>%
  bind_rows(all_raw_data_beta_S_PIE %>%
              dplyr::filter(scale=="alpha") %>%
              dplyr::filter(checklists_per_grid==10)) %>%
  bind_rows(all_raw_data_beta_S_n %>%
              dplyr::filter(scale=="alpha") %>%
              dplyr::filter(checklists_per_grid==10)) %>%
  mutate(scale="beta") %>%
  mutate(index=gsub("beta_", "", index)) %>%
  ggplot(., aes(x=ghm, y=mean_value, color=as.factor(grid_size)))+
  geom_smooth(method="gam", se=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~index, ncol=2, scales="free")+
  xlab("Global Human Modification")+
  ylab("MoB variable")+
  ggtitle(paste0("C) beta scale across all BCRs"))+
  scale_color_brewer(palette = "Spectral")+
  guides(color=guide_legend(title="Grain size"))

gam_all_beta


lm_all_alpha + lm_all_gamma + lm_all_beta + plot_layout(ncol=1)

ggsave("Figures/lm_alpha_gamma_beta.png", height=8.7, width=6.7, units="in")

gam_all_alpha + gam_all_gamma + gam_all_beta + plot_layout(ncol=1)

ggsave("Figures/gam_alpha_gamma_beta.png", height=8.7, width=6.7, units="in")











#######################################################################
#######################################################################
########### VISUALIZATION WITH LM instead of GAM ######################
#######################################################################
# OLD STUFF NOT USED, BUT SAVING FOR NOW
# First for "S"
lm_S <- all_raw_data_S %>%
  bind_rows(all_raw_data_beta_S %>%
              dplyr::filter(index=="beta_S") %>%
              mutate(scale="beta")) %>%
  mutate(scale=factor(scale, levels=c("alpha", "gamma", "beta"))) %>%
  ggplot(., aes(x=ghm, y=mean_value, 
                color=as.factor(grid_size), group=as.factor(grid_size)))+
  geom_smooth(method="lm", se=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~scale, scales="free")+
  xlab("Global Human Modification")+
  ylab("S")+
  scale_color_brewer(palette = "Spectral")+
  guides(color=guide_legend(title="Grain size"))+
  ggtitle("A")+
  theme(legend.position="none")

lm_S

# Now repeat that but for S_n
lm_S_n <- all_raw_data_S_n %>%
  bind_rows(all_raw_data_beta_S_n %>%
              dplyr::filter(index=="beta_S_n") %>%
              mutate(scale="beta")) %>%
  mutate(scale=factor(scale, levels=c("alpha", "gamma", "beta"))) %>%
  ggplot(., aes(x=ghm, y=mean_value, 
                color=as.factor(grid_size), group=as.factor(grid_size)))+
  geom_smooth(method="lm", se=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~scale, scales="free")+
  xlab("Global Human Modification")+
  ylab("S_n")+
  scale_color_brewer(palette = "Spectral")+
  guides(color=guide_legend(title="Grain size"))+
  ggtitle("B")+
  theme(legend.position="none")

lm_S_n

# Now repeat that but for S_PIE
lm_S_PIE <- all_raw_data_S_PIE %>%
  bind_rows(all_raw_data_beta_S_PIE %>%
              dplyr::filter(index=="beta_S_PIE") %>%
              mutate(scale="beta")) %>%
  mutate(scale=factor(scale, levels=c("alpha", "gamma", "beta"))) %>%
  ggplot(., aes(x=ghm, y=mean_value, 
                color=as.factor(grid_size), group=as.factor(grid_size)))+
  geom_smooth(method="lm", se=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~scale, scales="free")+
  xlab("Global Human Modification")+
  ylab("S_PIE")+
  scale_color_brewer(palette = "Spectral")+
  guides(color=guide_legend(title="Grain size"))+
  ggtitle("C")+
  theme(legend.position="none")

lm_S_PIE

# ONE MORE TIME, but for N
lm_N <- all_raw_data_N %>%
  ggplot(., aes(x=ghm, y=mean_value, 
                color=as.factor(grid_size), group=as.factor(grid_size)))+
  geom_smooth(method="lm", se=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~scale, scales="free")+
  xlab("Global Human Modification")+
  ylab("N")+
  scale_color_brewer(palette = "Spectral")+
  guides(color=guide_legend(title="Grain size"))+
  ggtitle("D")+
  theme(legend.position="bottom")

lm_N


# Put plots together into one
lm_S + lm_S_n + lm_S_PIE + lm_N + plot_layout(ncol=2)

ggsave("Figures/lm_alpha_gamma_v1.png", width=8.5, height=6.8, units="in")



