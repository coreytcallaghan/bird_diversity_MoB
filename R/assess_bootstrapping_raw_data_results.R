# AN R SCRIPT TO explore the bootstrap results
# and look at the raw data to see what it is showing.

# packages
# packages
library(ggplot2)
library(dplyr)
library(purrr)
library(tidyr)
library(readr)

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

# write a function to visualize results for a given index
# and BCR name
# at the alpha and gamma scale
summarize_boot_results <- function(BCR_number, index_name){
  
  message(paste0("Summarizing data for BCR ", BCR_number))
  
  file_list <- c(paste0("Intermediate_results/bootstrapping/grid_size_0.1/number_samples_10/BCR_", BCR_number, ".RDS"),
                 paste0("Intermediate_results/bootstrapping/grid_size_0.1/number_samples_30/BCR_", BCR_number, ".RDS"),
                 paste0("Intermediate_results/bootstrapping/grid_size_0.1/number_samples_50/BCR_", BCR_number, ".RDS"),
                 paste0("Intermediate_results/bootstrapping/grid_size_0.5/number_samples_10/BCR_", BCR_number, ".RDS"),
                 paste0("Intermediate_results/bootstrapping/grid_size_0.5/number_samples_30/BCR_", BCR_number, ".RDS"),
                 paste0("Intermediate_results/bootstrapping/grid_size_0.5/number_samples_50/BCR_", BCR_number, ".RDS"),
                 paste0("Intermediate_results/bootstrapping/grid_size_1/number_samples_10/BCR_", BCR_number, ".RDS"),
                 paste0("Intermediate_results/bootstrapping/grid_size_1/number_samples_30/BCR_", BCR_number, ".RDS"),
                 paste0("Intermediate_results/bootstrapping/grid_size_1/number_samples_50/BCR_", BCR_number, ".RDS"))
  
  file.exists(file_list)
  
  read_data_0.1 <- function(x){
    
    dat <- readRDS(x) %>%
      mutate(grid_size=0.1)
  }
  
  read_data_0.5 <- function(x){
    
    dat <- readRDS(x) %>%
      mutate(grid_size=0.5)
  }
  
  read_data_1 <- function(x){
    
    dat <- readRDS(x) %>%
      mutate(grid_size=1)
  }
  
  dat_0.1 <- bind_rows(lapply(file_list[1:3] %>%
                                as.data.frame() %>%
                                mutate(exists=ifelse(file.exists(.)==TRUE, "yes", "no")) %>%
                                dplyr::filter(exists=="yes") %>%
                                .$., read_data_0.1))
  
  dat_0.5 <- bind_rows(lapply(file_list[4:6] %>%
                                as.data.frame() %>%
                                mutate(exists=ifelse(file.exists(.)==TRUE, "yes", "no")) %>%
                                dplyr::filter(exists=="yes") %>%
                                .$., read_data_0.5))
  
  dat_1 <- bind_rows(lapply(file_list[7:9] %>%
                                as.data.frame() %>%
                                mutate(exists=ifelse(file.exists(.)==TRUE, "yes", "no")) %>%
                                dplyr::filter(exists=="yes") %>%
                                .$., read_data_1))
  dat <- bind_rows(dat_0.1,
                   dat_0.5,
                   dat_1)
  
  # dat <- readRDS(paste0("Intermediate_results/bootstrapping/grid_size_0.1/number_samples_10/BCR_", BCR_number, ".RDS")) %>%
  #   mutate(grid_size=0.1) %>%
  #   bind_rows(readRDS(paste0("Intermediate_results/bootstrapping/grid_size_0.1/number_samples_30/BCR_", BCR_number, ".RDS")) %>%
  #               mutate(grid_size=0.1)) %>%
  #   bind_rows(readRDS(paste0("Intermediate_results/bootstrapping/grid_size_0.1/number_samples_50/BCR_", BCR_number, ".RDS")) %>%
  #               mutate(grid_size=0.1)) %>%
  #   bind_rows(readRDS(paste0("Intermediate_results/bootstrapping/grid_size_0.5/number_samples_10/BCR_", BCR_number, ".RDS")) %>%
  #               mutate(grid_size=0.5)) %>%
  #   bind_rows(readRDS(paste0("Intermediate_results/bootstrapping/grid_size_0.5/number_samples_30/BCR_", BCR_number, ".RDS")) %>%
  #               mutate(grid_size=0.5)) %>%
  #   bind_rows(readRDS(paste0("Intermediate_results/bootstrapping/grid_size_0.5/number_samples_50/BCR_", BCR_number, ".RDS")) %>%
  #               mutate(grid_size=0.5)) %>%
  #   bind_rows(readRDS(paste0("Intermediate_results/bootstrapping/grid_size_1/number_samples_10/BCR_", BCR_number, ".RDS")) %>%
  #               mutate(grid_size=1.0)) %>%
  #   bind_rows(readRDS(paste0("Intermediate_results/bootstrapping/grid_size_1/number_samples_30/BCR_", BCR_number, ".RDS")) %>%
  #               mutate(grid_size=1.0)) %>%
  #   bind_rows(readRDS(paste0("Intermediate_results/bootstrapping/grid_size_1/number_samples_50/BCR_", BCR_number, ".RDS")) %>%
  #               mutate(grid_size=1.0))
  
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

# Now can use this function to visualize raw results for different BCRs
# and different indices
# haven't bothered looping through everything yet
all_raw_data_S <- bind_rows(lapply_with_error(bcr_list, function(x){summarize_boot_results(x, "S")}))
all_raw_data_S_n <- bind_rows(lapply_with_error(bcr_list, function(x){summarize_boot_results(x, "S_n")}))
all_raw_data_N <- bind_rows(lapply_with_error(bcr_list, function(x){summarize_boot_results(x, "N")}))
all_raw_data_S_PIE <- bind_rows(lapply_with_error(bcr_list, function(x){summarize_boot_results(x, "S_PIE")}))
all_raw_data_beta_S <- bind_rows(lapply_with_error(bcr_list, function(x){summarize_boot_results(x, "beta_S")}))
all_raw_data_beta_S_n <- bind_rows(lapply_with_error(bcr_list, function(x){summarize_boot_results(x, "beta_S_n")}))
all_raw_data_beta_S_PIE <- bind_rows(lapply_with_error(bcr_list, function(x){summarize_boot_results(x, "beta_S_PIE")}))

# plot results for one BCR to start out with
# First for "S"
all_raw_data_S %>%
  dplyr::filter(BCR_CODE==30) %>%
  ggplot(., aes(x=ghm, y=mean_value, 
                    color=as.factor(grid_size), group=as.factor(grid_size)))+
  geom_smooth(method="lm", se=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(scale ~ checklists_per_grid, scales="free")+
  xlab("Global Human Modification")+
  ylab("S")+
  ggtitle(paste0("BCR 30"))+
  scale_color_brewer(palette = "Set1")+
  guides(color=guide_legend(title="Grid size"))

all_raw_data_S %>%
  dplyr::filter(BCR_CODE==30) %>%
  ggplot(., aes(x=ghm, y=mean_value, 
                color=as.factor(grid_size), group=as.factor(grid_size)))+
  geom_smooth(method="gam", se=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(scale ~ checklists_per_grid, scales="free")+
  xlab("Global Human Modification")+
  ylab("S")+
  ggtitle(paste0("BCR 30"))+
  scale_color_brewer(palette = "Set1")+
  guides(color=guide_legend(title="Grid size"))

all_raw_data_S %>%
  dplyr::filter(BCR_CODE==30) %>%
  ggplot(., aes(x=ghm, y=mean_value, 
                color=as.factor(checklists_per_grid), group=as.factor(checklists_per_grid)))+
  geom_smooth(method="lm", se=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(scale ~ grid_size, scales="free")+
  xlab("Global Human Modification")+
  ylab("S")+
  ggtitle(paste0("BCR 30"))+
  scale_color_brewer(palette = "Set1")+
  guides(color=guide_legend(title="Number of checklists"))

all_raw_data_S %>%
  dplyr::filter(BCR_CODE==30) %>%
  ggplot(., aes(x=ghm, y=mean_value, 
                color=as.factor(checklists_per_grid), linetype=as.factor(grid_size)))+
  geom_smooth(method="lm", se=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~scale, scales="free")+
  xlab("Global Human Modification")+
  ylab("S")+
  ggtitle(paste0("BCR 30"))+
  scale_color_brewer(palette = "Set1")+
  guides(color=guide_legend(title="Number of checklists"))+
  guides(linetype=guide_legend(title="Grid size"))



# Now repeat that but for S_n
all_raw_data_S_n %>%
  dplyr::filter(BCR_CODE==30) %>%
  ggplot(., aes(x=ghm, y=mean_value, 
                color=as.factor(grid_size), group=as.factor(grid_size)))+
  geom_smooth(method="lm", se=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(scale ~ checklists_per_grid, scales="free")+
  xlab("Global Human Modification")+
  ylab("S_n")+
  ggtitle(paste0("BCR 30"))+
  scale_color_brewer(palette = "Set1")+
  guides(color=guide_legend(title="Grid size"))

all_raw_data_S_n %>%
  dplyr::filter(BCR_CODE==30) %>%
  ggplot(., aes(x=ghm, y=mean_value, 
                color=as.factor(grid_size), group=as.factor(grid_size)))+
  geom_smooth(method="gam", se=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(scale ~ checklists_per_grid, scales="free")+
  xlab("Global Human Modification")+
  ylab("S_n")+
  ggtitle(paste0("BCR 30"))+
  scale_color_brewer(palette = "Set1")+
  guides(color=guide_legend(title="Grid size"))

all_raw_data_S_n %>%
  dplyr::filter(BCR_CODE==30) %>%
  ggplot(., aes(x=ghm, y=mean_value, 
                color=as.factor(checklists_per_grid), group=as.factor(checklists_per_grid)))+
  geom_smooth(method="lm", se=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(scale ~ grid_size, scales="free")+
  xlab("Global Human Modification")+
  ylab("S_n")+
  ggtitle(paste0("BCR 30"))+
  scale_color_brewer(palette = "Set1")+
  guides(color=guide_legend(title="Number of checklists"))

all_raw_data_S_n %>%
  dplyr::filter(BCR_CODE==30) %>%
  ggplot(., aes(x=ghm, y=mean_value, 
                color=as.factor(checklists_per_grid), linetype=as.factor(grid_size)))+
  geom_smooth(method="lm", se=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~scale, scales="free")+
  xlab("Global Human Modification")+
  ylab("S_n")+
  ggtitle(paste0("BCR 30"))+
  scale_color_brewer(palette = "Set1")+
  guides(color=guide_legend(title="Number of checklists"))+
  guides(linetype=guide_legend(title="Grid size"))


# Now repeat that but for S_PIE
all_raw_data_S_PIE %>%
  dplyr::filter(BCR_CODE==30) %>%
  ggplot(., aes(x=ghm, y=mean_value, 
                color=as.factor(grid_size), group=as.factor(grid_size)))+
  geom_smooth(method="lm", se=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(scale ~ checklists_per_grid, scales="free")+
  xlab("Global Human Modification")+
  ylab("S_PIE")+
  ggtitle(paste0("BCR 30"))+
  scale_color_brewer(palette = "Set1")+
  guides(color=guide_legend(title="Grid size"))

all_raw_data_S_PIE %>%
  dplyr::filter(BCR_CODE==30) %>%
  ggplot(., aes(x=ghm, y=mean_value, 
                color=as.factor(grid_size), group=as.factor(grid_size)))+
  geom_smooth(method="gam", se=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(scale ~ checklists_per_grid, scales="free")+
  xlab("Global Human Modification")+
  ylab("S_PIE")+
  ggtitle(paste0("BCR 30"))+
  scale_color_brewer(palette = "Set1")+
  guides(color=guide_legend(title="Grid size"))

all_raw_data_S_PIE %>%
  dplyr::filter(BCR_CODE==30) %>%
  ggplot(., aes(x=ghm, y=mean_value, 
                color=as.factor(checklists_per_grid), group=as.factor(checklists_per_grid)))+
  geom_smooth(method="lm", se=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(scale ~ grid_size, scales="free")+
  xlab("Global Human Modification")+
  ylab("S_PIE")+
  ggtitle(paste0("BCR 30"))+
  scale_color_brewer(palette = "Set1")+
  guides(color=guide_legend(title="Number of checklists"))

all_raw_data_S_PIE %>%
  dplyr::filter(BCR_CODE==30) %>%
  ggplot(., aes(x=ghm, y=mean_value, 
                color=as.factor(checklists_per_grid), linetype=as.factor(grid_size)))+
  geom_smooth(method="lm", se=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~scale, scales="free")+
  xlab("Global Human Modification")+
  ylab("S_PIE")+
  ggtitle(paste0("BCR 30"))+
  scale_color_brewer(palette = "Set1")+
  guides(color=guide_legend(title="Number of checklists"))+
  guides(linetype=guide_legend(title="Grid size"))

# ONE MORE TIME, but for N
# Now repeat that but for S_PIE
all_raw_data_N %>%
  dplyr::filter(BCR_CODE==30) %>%
  ggplot(., aes(x=ghm, y=mean_value, 
                color=as.factor(grid_size), group=as.factor(grid_size)))+
  geom_smooth(method="lm", se=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(scale ~ checklists_per_grid, scales="free")+
  xlab("Global Human Modification")+
  ylab("N")+
  ggtitle(paste0("BCR 30"))+
  scale_color_brewer(palette = "Set1")+
  guides(color=guide_legend(title="Grid size"))

all_raw_data_N %>%
  dplyr::filter(BCR_CODE==30) %>%
  ggplot(., aes(x=ghm, y=mean_value, 
                color=as.factor(grid_size), group=as.factor(grid_size)))+
  geom_smooth(method="gam", se=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(scale ~ checklists_per_grid, scales="free")+
  xlab("Global Human Modification")+
  ylab("N")+
  ggtitle(paste0("BCR 30"))+
  scale_color_brewer(palette = "Set1")+
  guides(color=guide_legend(title="Grid size"))

all_raw_data_N %>%
  dplyr::filter(BCR_CODE==30) %>%
  ggplot(., aes(x=ghm, y=mean_value, 
                color=as.factor(checklists_per_grid), group=as.factor(checklists_per_grid)))+
  geom_smooth(method="lm", se=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(scale ~ grid_size, scales="free")+
  xlab("Global Human Modification")+
  ylab("N")+
  ggtitle(paste0("BCR 30"))+
  scale_color_brewer(palette = "Set1")+
  guides(color=guide_legend(title="Number of checklists"))

all_raw_data_N %>%
  dplyr::filter(BCR_CODE==30) %>%
  ggplot(., aes(x=ghm, y=mean_value, 
                color=as.factor(checklists_per_grid), linetype=as.factor(grid_size)))+
  geom_smooth(method="lm", se=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~scale, scales="free")+
  xlab("Global Human Modification")+
  ylab("N")+
  ggtitle(paste0("BCR 30"))+
  scale_color_brewer(palette = "Set1")+
  guides(color=guide_legend(title="Number of checklists"))+
  guides(linetype=guide_legend(title="Grid size"))



###################################################
###################################################
########### PLOT RESULTS FOR ALL BCRS at once #####
###################################################
# plot results for one BCR to start out with
# First for "S"
all_raw_data_S %>%
  ggplot(., aes(x=ghm, y=mean_value, 
                color=as.factor(grid_size), group=as.factor(grid_size)))+
  geom_smooth(method="lm", se=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(scale ~ checklists_per_grid, scales="free")+
  xlab("Global Human Modification")+
  ylab("S")+
  ggtitle(paste0("Across all BCRs"))+
  scale_color_brewer(palette = "Set1")+
  guides(color=guide_legend(title="Grid size"))

all_raw_data_S %>%
  ggplot(., aes(x=ghm, y=mean_value, 
                color=as.factor(grid_size), group=as.factor(grid_size)))+
  geom_smooth(method="gam", se=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(scale ~ checklists_per_grid, scales="free")+
  xlab("Global Human Modification")+
  ylab("S")+
  ggtitle(paste0("Across all BCRs"))+
  scale_color_brewer(palette = "Set1")+
  guides(color=guide_legend(title="Grid size"))

all_raw_data_S %>%
  ggplot(., aes(x=ghm, y=mean_value, 
                color=as.factor(checklists_per_grid), group=as.factor(checklists_per_grid)))+
  geom_smooth(method="lm", se=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(scale ~ grid_size, scales="free")+
  xlab("Global Human Modification")+
  ylab("S")+
  ggtitle(paste0("Across all BCRs"))+
  scale_color_brewer(palette = "Set1")+
  guides(color=guide_legend(title="Number of checklists"))

all_raw_data_S %>%
  ggplot(., aes(x=ghm, y=mean_value, 
                color=as.factor(checklists_per_grid), linetype=as.factor(grid_size)))+
  geom_smooth(method="lm", se=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~scale, scales="free")+
  xlab("Global Human Modification")+
  ylab("S")+
  ggtitle(paste0("Across all BCRs"))+
  scale_color_brewer(palette = "Set1")+
  guides(color=guide_legend(title="Number of checklists"))+
  guides(linetype=guide_legend(title="Grid size"))



# Now repeat that but for S_n
all_raw_data_S_n %>%
  ggplot(., aes(x=ghm, y=mean_value, 
                color=as.factor(grid_size), group=as.factor(grid_size)))+
  geom_smooth(method="lm", se=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(scale ~ checklists_per_grid, scales="free")+
  xlab("Global Human Modification")+
  ylab("S_n")+
  ggtitle(paste0("Across all BCRs"))+
  scale_color_brewer(palette = "Set1")+
  guides(color=guide_legend(title="Grid size"))

all_raw_data_S_n %>%
  ggplot(., aes(x=ghm, y=mean_value, 
                color=as.factor(grid_size), group=as.factor(grid_size)))+
  geom_smooth(method="gam", se=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(scale ~ checklists_per_grid, scales="free")+
  xlab("Global Human Modification")+
  ylab("S_n")+
  ggtitle(paste0("Across all BCRs"))+
  scale_color_brewer(palette = "Set1")+
  guides(color=guide_legend(title="Grid size"))

all_raw_data_S_n %>%
  ggplot(., aes(x=ghm, y=mean_value, 
                color=as.factor(checklists_per_grid), group=as.factor(checklists_per_grid)))+
  geom_smooth(method="lm", se=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(scale ~ grid_size, scales="free")+
  xlab("Global Human Modification")+
  ylab("S_n")+
  ggtitle(paste0("Across all BCRs"))+
  scale_color_brewer(palette = "Set1")+
  guides(color=guide_legend(title="Number of checklists"))

all_raw_data_S_n %>%
  ggplot(., aes(x=ghm, y=mean_value, 
                color=as.factor(checklists_per_grid), linetype=as.factor(grid_size)))+
  geom_smooth(method="lm", se=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~scale, scales="free")+
  xlab("Global Human Modification")+
  ylab("S_n")+
  ggtitle(paste0("Across all BCRs"))+
  scale_color_brewer(palette = "Set1")+
  guides(color=guide_legend(title="Number of checklists"))+
  guides(linetype=guide_legend(title="Grid size"))


# Now repeat that but for S_PIE
all_raw_data_S_PIE %>%
  ggplot(., aes(x=ghm, y=mean_value, 
                color=as.factor(grid_size), group=as.factor(grid_size)))+
  geom_smooth(method="lm", se=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(scale ~ checklists_per_grid, scales="free")+
  xlab("Global Human Modification")+
  ylab("S_PIE")+
  ggtitle(paste0("Across all BCRs"))+
  scale_color_brewer(palette = "Set1")+
  guides(color=guide_legend(title="Grid size"))

all_raw_data_S_PIE %>%
  ggplot(., aes(x=ghm, y=mean_value, 
                color=as.factor(grid_size), group=as.factor(grid_size)))+
  geom_smooth(method="gam", se=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(scale ~ checklists_per_grid, scales="free")+
  xlab("Global Human Modification")+
  ylab("S_PIE")+
  ggtitle(paste0("Across all BCRs"))+
  scale_color_brewer(palette = "Set1")+
  guides(color=guide_legend(title="Grid size"))

all_raw_data_S_PIE %>%
  ggplot(., aes(x=ghm, y=mean_value, 
                color=as.factor(checklists_per_grid), group=as.factor(checklists_per_grid)))+
  geom_smooth(method="lm", se=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(scale ~ grid_size, scales="free")+
  xlab("Global Human Modification")+
  ylab("S_PIE")+
  ggtitle(paste0("Across all BCRs"))+
  scale_color_brewer(palette = "Set1")+
  guides(color=guide_legend(title="Number of checklists"))

all_raw_data_S_PIE %>%
  ggplot(., aes(x=ghm, y=mean_value, 
                color=as.factor(checklists_per_grid), linetype=as.factor(grid_size)))+
  geom_smooth(method="lm", se=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~scale, scales="free")+
  xlab("Global Human Modification")+
  ylab("S_PIE")+
  ggtitle(paste0("Across all BCRs"))+
  scale_color_brewer(palette = "Set1")+
  guides(color=guide_legend(title="Number of checklists"))+
  guides(linetype=guide_legend(title="Grid size"))

# ONE MORE TIME, but for N
all_raw_data_N %>%
  ggplot(., aes(x=ghm, y=mean_value, 
                color=as.factor(grid_size), group=as.factor(grid_size)))+
  geom_smooth(method="lm", se=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(scale ~ checklists_per_grid, scales="free")+
  xlab("Global Human Modification")+
  ylab("N")+
  ggtitle(paste0("Across all BCRs"))+
  scale_color_brewer(palette = "Set1")+
  guides(color=guide_legend(title="Grid size"))

all_raw_data_N %>%
  ggplot(., aes(x=ghm, y=mean_value, 
                color=as.factor(grid_size), group=as.factor(grid_size)))+
  geom_smooth(method="gam", se=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(scale ~ checklists_per_grid, scales="free")+
  xlab("Global Human Modification")+
  ylab("N")+
  ggtitle(paste0("Across all BCRs"))+
  scale_color_brewer(palette = "Set1")+
  guides(color=guide_legend(title="Grid size"))

all_raw_data_N %>%
  ggplot(., aes(x=ghm, y=mean_value, 
                color=as.factor(checklists_per_grid), group=as.factor(checklists_per_grid)))+
  geom_smooth(method="lm", se=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(scale ~ grid_size, scales="free")+
  xlab("Global Human Modification")+
  ylab("N")+
  ggtitle(paste0("Across all BCRs"))+
  scale_color_brewer(palette = "Set1")+
  guides(color=guide_legend(title="Number of checklists"))

all_raw_data_N %>%
  ggplot(., aes(x=ghm, y=mean_value, 
                color=as.factor(checklists_per_grid), linetype=as.factor(grid_size)))+
  geom_smooth(method="lm", se=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(~scale, scales="free")+
  xlab("Global Human Modification")+
  ylab("N")+
  ggtitle(paste0("Across all BCRs"))+
  scale_color_brewer(palette = "Set1")+
  guides(color=guide_legend(title="Number of checklists"))+
  guides(linetype=guide_legend(title="Grid size"))








visualize_results(23, "S")
visualize_results(25, "S")
visualize_results(30, "S")
visualize_results(5, "S")
visualize_results(23, "S_n")
visualize_results(25, "S_n")
visualize_results(30, "S_n")
visualize_results(5, "S_n")



# plot results
plot <- ggplot(summary, aes(x=ghm, y=mean_value, 
                            color=as.factor(grid_size), group=as.factor(grid_size)))+
  geom_smooth(method="lm", se=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(scale ~ checklists_per_grid, scales="free")+
  xlab("Global Human Modification")+
  ylab(paste0(index_name))+
  ggtitle(paste0("BCR ", BCR_number))+
  scale_color_brewer(palette = "Set1")+
  guides(color=guide_legend(title="Grid size"))

return(plot)


###################
############## OLD
#################




# pick a BCR
BCR=23

# collate data across the factorial design
# for that BCR
dat <- readRDS(paste0("Intermediate_results/bootstrapping/grid_size_0.1/number_samples_10/BCR_", BCR, ".RDS")) %>%
  mutate(grid_size=0.1) %>%
  bind_rows(readRDS(paste0("Intermediate_results/bootstrapping/grid_size_0.1/number_samples_30/BCR_", BCR, ".RDS")) %>%
              mutate(grid_size=0.1)) %>%
  bind_rows(readRDS(paste0("Intermediate_results/bootstrapping/grid_size_0.1/number_samples_50/BCR_", BCR, ".RDS")) %>%
              mutate(grid_size=0.1)) %>%
  bind_rows(readRDS(paste0("Intermediate_results/bootstrapping/grid_size_0.5/number_samples_10/BCR_", BCR, ".RDS")) %>%
              mutate(grid_size=0.5)) %>%
  bind_rows(readRDS(paste0("Intermediate_results/bootstrapping/grid_size_0.5/number_samples_30/BCR_", BCR, ".RDS")) %>%
              mutate(grid_size=0.5)) %>%
  bind_rows(readRDS(paste0("Intermediate_results/bootstrapping/grid_size_0.5/number_samples_50/BCR_", BCR, ".RDS")) %>%
              mutate(grid_size=0.5)) %>%
  bind_rows(readRDS(paste0("Intermediate_results/bootstrapping/grid_size_1/number_samples_10/BCR_", BCR, ".RDS")) %>%
              mutate(grid_size=1.0)) %>%
  bind_rows(readRDS(paste0("Intermediate_results/bootstrapping/grid_size_1/number_samples_30/BCR_", BCR, ".RDS")) %>%
              mutate(grid_size=1.0)) %>%
  bind_rows(readRDS(paste0("Intermediate_results/bootstrapping/grid_size_1/number_samples_50/BCR_", BCR, ".RDS")) %>%
              mutate(grid_size=1.0))

# let's look only at S for now
# and only at alpha and gamma scale
temp_dat <- dat %>%
  dplyr::filter(index=="S") %>%
  dplyr::filter(scale %in% c("alpha", "gamma"))

# get mean bootstrap responses
summary <- temp_dat %>%
  group_by(scale, index, grid_size, checklists_per_grid, ghm) %>%
  summarize(mean_response=mean(value))

# plot results
ggplot(summary, aes(x=ghm, y=mean_response, 
                    color=as.factor(grid_size), group=as.factor(grid_size)))+
  geom_smooth(method="lm", se=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  facet_wrap(scale ~ checklists_per_grid, scales="free")+
  ggtitle("BCR=BCR23")



# packages
library(ggplot2)
library(dplyr)
library(purrr)
library(tidyr)

# pick a BCR
BCR=23

dat <- readRDS(paste0("Intermediate_results/bootstrapping/grid_size_0.1/number_samples_10/BCR_", BCR, ".RDS")) %>%
  mutate(grid_size=0.1)

S_test <- dat %>%
  dplyr::filter(index=="S") %>%
  dplyr::filter(scale=="alpha")

ggplot(S_test, aes(x=ghm, y=value, group=draw))+
  geom_smooth(method="lm", se=FALSE)

mod <- lm(value ~ ghm, data=S_test)

newdat <- tibble(ghm = seq(0, 1, length.out=100)) 

prediction <- tibble(ghm = seq(0, 1, length.out=100),
                     value=predict(mod, newdata=newdat),
                     value_se=predict(mod, newdata=newdat, se.fit=TRUE)$se.fit)

ggplot(prediction, aes(x=ghm, y=value))+
  geom_line()+
  geom_line(aes(x=ghm, y=value-value_se), linetype="dashed")+
  geom_line(aes(x=ghm, y=value+value_se), linetype="dashed")

S_test2 <- dat %>%
  dplyr::filter(index=="S") %>%
  dplyr::filter(scale=="gamma")

ggplot(S_test2, aes(x=ghm, y=value, group=draw))+
  geom_smooth(method="lm", se=FALSE)

mod <- lm(value ~ ghm, data=S_test2)

newdat <- tibble(ghm = seq(0, 1, length.out=100)) 

prediction2 <- tibble(ghm = seq(0, 1, length.out=100),
                     value=predict(mod, newdata=newdat),
                     value_se=predict(mod, newdata=newdat, se.fit=TRUE)$se.fit)

ggplot(prediction2, aes(x=ghm, y=value))+
  geom_line()+
  geom_line(aes(x=ghm, y=value-value_se), linetype="dashed")+
  geom_line(aes(x=ghm, y=value+value_se), linetype="dashed")

# now collapse by each sample_level
# across the bootstrapped results
get_mean_relationship <- function(sample_level){
  
  temp <- dat %>%
    dplyr::filter(checklists_per_grid==sample_level)
  
  # apply for each grid size
  across_grids <- function(grid_size){
    
    temp2 <- temp %>%
      ungroup() %>%
      dplyr::filter(grid_size==grid_level)
    
    S_test <- temp2 %>%
      dplyr::filter(index=="S") %>%
      dplyr::filter(scale=="alpha")
    
    ggplot(S_test, aes(x=ghm, y=value, group=draw))+
      geom_smooth(method="lm", se=FALSE)
    
    mod <- lm(value ~ ghm, data=S_test)
    
    newdat <- tibble(ghm = seq(0, 1, length.out=100)) 
    
    prediction <- tibble(ghm = seq(0, 1, length.out=100),
                         value=predict(mod, newdata=newdat),
                         valeu_se=predict(mod, newdata=newdat, se.fit=TRUE)$se.fit)
    
    
      mutate(predict(mod, newdata=ghm))
    
      tidybayes::add_fitted_draws(mod,
                                  re_formula = NA,
                                  scale = "response", 
                                  n = 1000) %>%
      dplyr::mutate(index=index_name) %>%
      dplyr::mutate(scale=scale_name) %>%
      dplyr::mutate(level="fixed_effects")
  }
  
  
  
}