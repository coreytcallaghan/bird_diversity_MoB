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
  
  file_list <- c(paste0("Intermediate_results/bootstrapping/grid_size_0.1/number_samples_10/BCR_", BCR_number, ".RDS"),
                 paste0("Intermediate_results/bootstrapping/grid_size_0.2/number_samples_10/BCR_", BCR_number, ".RDS"),
                 paste0("Intermediate_results/bootstrapping/grid_size_0.3/number_samples_10/BCR_", BCR_number, ".RDS"),
                 paste0("Intermediate_results/bootstrapping/grid_size_0.4/number_samples_10/BCR_", BCR_number, ".RDS"),
                 paste0("Intermediate_results/bootstrapping/grid_size_0.5/number_samples_10/BCR_", BCR_number, ".RDS"),
                 paste0("Intermediate_results/bootstrapping/grid_size_0.6/number_samples_10/BCR_", BCR_number, ".RDS"),
                 paste0("Intermediate_results/bootstrapping/grid_size_0.7/number_samples_10/BCR_", BCR_number, ".RDS"),
                 paste0("Intermediate_results/bootstrapping/grid_size_0.8/number_samples_10/BCR_", BCR_number, ".RDS"),
                 paste0("Intermediate_results/bootstrapping/grid_size_0.9/number_samples_10/BCR_", BCR_number, ".RDS"),
                 paste0("Intermediate_results/bootstrapping/grid_size_1/number_samples_10/BCR_", BCR_number, ".RDS"))
  
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

gam_S <- all_raw_data_S %>%
  bind_rows(all_raw_data_beta_S %>%
              dplyr::filter(index=="beta_S") %>%
              mutate(scale="beta")) %>%
  mutate(scale=factor(scale, levels=c("alpha", "gamma", "beta"))) %>%
  ggplot(., aes(x=ghm, y=mean_value, 
                color=as.factor(grid_size), group=as.factor(grid_size)))+
  geom_smooth(method="gam", se=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~scale, scales="free")+
  xlab("Global Human Modification")+
  ylab("S")+
  scale_color_brewer(palette = "Spectral")+
  guides(color=guide_legend(title="Grain size"))+
  ggtitle("A")+
  theme(legend.position="none")

gam_S

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

gam_S_n <- all_raw_data_S_n %>%
  bind_rows(all_raw_data_beta_S_n %>%
              dplyr::filter(index=="beta_S_n") %>%
              mutate(scale="beta")) %>%
  mutate(scale=factor(scale, levels=c("alpha", "gamma", "beta"))) %>%
  ggplot(., aes(x=ghm, y=mean_value, 
                color=as.factor(grid_size), group=as.factor(grid_size)))+
  geom_smooth(method="gam", se=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~scale, scales="free")+
  xlab("Global Human Modification")+
  ylab("S_n")+
  scale_color_brewer(palette = "Spectral")+
  guides(color=guide_legend(title="Grain size"))+
  ggtitle("B")+
  theme(legend.position="none")

gam_S_n

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

gam_S_PIE <- all_raw_data_S_PIE %>%
  bind_rows(all_raw_data_beta_S_PIE %>%
              dplyr::filter(index=="beta_S_PIE") %>%
              mutate(scale="beta")) %>%
  mutate(scale=factor(scale, levels=c("alpha", "gamma", "beta"))) %>%
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

gam_N <- all_raw_data_N %>%
  ggplot(., aes(x=ghm, y=mean_value, 
                color=as.factor(grid_size), group=as.factor(grid_size)))+
  geom_smooth(method="gam", se=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~scale, scales="free")+
  xlab("Global Human Modification")+
  ylab("N")+
  scale_color_brewer(palette = "Spectral")+
  guides(color=guide_legend(title="Grain size"))+
  ggtitle("D")+
  theme(legend.position="bottom")

gam_N

# Put plots together into one
lm_S + lm_S_n + lm_S_PIE + lm_N + plot_layout(ncol=2)

ggsave("Figures/lm_alpha_gamma_v1.png", width=8.5, height=6.8, units="in")

# Put plots together into one
gam_S + gam_S_n + gam_S_PIE + gam_N + plot_layout(ncol=2)

ggsave("Figures/gam_alpha_gamma_v1.png", width=8.5, height=6.8, units="in")

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






