#library(data.table)
#library(tidyverse)

#path <- getwd()
#file <- list.files(pattern = "res.dat")

### Read_res_ORYZA for experimental data.   sometimes the experiment don't have all varaibles...LAI_OBS for example
## fread +C
read_res_exp <- function (file) {
    res_file <- read_lines(file) #%>%
    #as_tibble() %>% filter(nchar(value)>100)
    
    #res_file %>% 
    #   .[str_detect(., pattern = "[\n /w ]+$")] %>%
    #  str_detect(., pattern = "")
    rescolnames <- res_file %>%
        str_subset("TIME") %>%
        str_split("\t")
    
    #res_file %>% read_table(skip = 24, n_max = 100)
    
    find_tbi <- res_file %>%
        str_detect(pattern ="TIME") %>%
        which()
    
    nlines <- c(find_tbi[-1], length(res_file))-find_tbi
    
    
    # Argument list 
    arg_list <- list(file= file,
                     skip = find_tbi, 
                     nrows = nlines,
                     na.string="-", 
                     col.names= rescolnames)
    
    # Read_pmap
    dat <- suppressWarnings(pmap(arg_list, fread)) %>%
        map(., ~mutate(.,date = lubridate::make_date(YEAR)+DOY-1))
    
    return(dat)
    
}

# return a list 
#dat <- read_res_exp("ir64_res.dat")

# sim_data extract the main crop variables to evaluate.
#sim_data <- dat %>%
#    map(., ~select(., date, DVS, WRR14, WAGT, LAI, WLVG, WST, WSO, WLVD)) %>% set_names(str_sub(cal_set, 1, -5)) %>%
#    bind_rows(.id = "exp_file") 
#
#sim_data %>% na.omit() %>%
#    ggplot(aes(date, WLVG)) +
#    geom_line() + 
#    scale_x_date(date_labels = "%Y-%m") +
#    facet_wrap(. ~id, scales="free_x") +
#    theme_bw()
#
#
##################################################################################
#sim_data %>% select(-c("LAI", "WRR14")) %>%
#    gather(key = "sim_var", value = "dry_matter", -(1:3)) %>%
#    mutate(LOC_ID = str_sub(exp_file, 1, 4)) %>%
#    ggplot(aes(date, dry_matter)) +
#    geom_line(aes(color=sim_var)) +
#    geom_point(data= test_point, aes(date, dry_matter, shape=obs_var, fill=obs_var)) + 
#    geom_errorbar(data = test_point, aes(ymin = dry_matter - dry_matter_sd, 
#                                         ymax = dry_matter + dry_matter_sd), 
#                                         width=.2,
#                                         position=position_dodge(0.05)) + 
#    facet_wrap(exp_file~., scales = "free_x") +
#    theme_bw()
################################################    
#sim_data %>% select(-c("LAI", "WRR14")) %>%
#    gather(key = "sim_var", value = "sim_value", -(1:3)) %>%
#    mutate(LOC_ID = str_sub(id, 1, 4)) %>%
#    ggplot(aes(DVS, sim_value)) +
#    geom_point(aes(shape=LOC_ID)) + 
#    facet_wrap(sim_var ~.) +
#    theme_bw()
#
#
#test <- dat %>%
#    mutate(
#        gphase = case_when(
#            DVS >= 0 & DVS < 0.65 ~ "Vegetative",
#            DVS >= 0.65 & DVS <= 1 ~ "Reproductive",
#            DVS > 1  ~ "Ripening",
#            TRUE ~ "NA")) 
#
#
#summary_res <- test%>%
#    group_by(id, gphase) %>%
#    summarise_at(vars(RDD, TMAX, TMIN, RAIN, ETD, TRW, TRC, EVSC, EVSW, PCEW), 
#                 funs(mean, median, max, min, sum, sd), na.rm=T)
#
#summary_res %>% na.omit() %>% 
#    select(1:2, contains("TMAX"), -contains("sd")) %>%
#    gather("TMAX_var", "value", -c(1, 2)) %>%
#    ggplot(aes(value)) +
#        geom_histogram() +
#        facet_grid(gphase ~ TMAX_var, scales="free")

 
##########################################################################
##########################################################################
### TEST READ BIG res by tydyverse

read_res_big <- function(path, res_filename){
    stopifnot(require(tidyverse))
    
    res_file <- read_lines(paste0(path, "/",  res_filename))
    
    res_colnames <- res_file %>%
        str_subset("TIME") %>%
        str_split("\t") %>%
        .[[1]]
    
    res_file %>%
        tibble() %>%
        filter(!str_detect(.,pattern = "^[ \t]*$|^[*]"), 
               !str_detect(.,pattern = "WARNING|Please check"))%>%
        mutate(id=cumsum(str_detect(.,"TIME"))) %>%
        filter(!str_detect(.,"TIME")) %>%
        separate(data=., col = ., into =  res_colnames, sep = '[\\t]') %>%
        mutate_if(is.character, as.numeric)
    
} 

#dat <- read_res_big(path, "res.dat")

#system.time({read_res_oryza(path, "res.dat")})


#dat %>%
#    map(., ~select(., date, DVS, contains("LAI"))) %>% 
#    bind_rows(.id = "id") %>%
#    mutate(id = factor(id, labels  = str_sub(cal_set, 1, -5))) %>%
#    ggplot(aes(date, LAI)) +
#    geom_line() + 
#    geom_point(aes(date, LAI_OBS)) +
#    scale_x_date(date_labels = "%Y-%m") +
#    facet_wrap(. ~id, scales="free_x") +
#    theme_bw()
#
#dat %>%
#    map(., ~select(., date, DVS, contains("WLVG"))) %>% 
#    bind_rows(.id = "id") %>%
#    mutate(id = factor(id, labels  = str_sub(cal_set, 1, -5))) %>%
#    ggplot(aes(date, WLVG)) +
#    geom_line() + 
#    geom_point(aes(date, WLVG_OBS)) +
#    scale_x_date(date_labels = "%Y-%m") +
#    facet_wrap(. ~id, scales="free_x") +
#    theme_bw()
#
#dat %>%
#    map(., ~select(., date, DVS, contains("WST"))) %>% 
#    bind_rows(.id = "id") %>%
#    mutate(id = factor(id, labels  = str_sub(cal_set, 1, -5))) %>%
#    ggplot(aes(date, WST)) +
#    geom_line() + 
#    geom_point(aes(date, WST_OBS)) +
#    scale_x_date(date_labels = "%Y-%m") +
#    facet_wrap(. ~id, scales="free_x") +
#    theme_bw()
#
#dat %>%
#    map(., ~select(., date, DVS, contains("WSO"))) %>% 
#    bind_rows(.id = "id") %>%
#    mutate(id = factor(id, labels  = str_sub(cal_set, 1, -5))) %>%
#    ggplot(aes(date, WSO)) +
#    geom_line() + 
#    geom_point(aes(date, WSO_OBS)) +
#    scale_x_date(date_labels = "%Y-%m") +
#    facet_wrap(. ~id, scales="free_x") +
#    theme_bw()
#
#dat %>%
#    map(., ~select(., date, DVS, contains("WAGT"))) %>% 
#    bind_rows(.id = "id") %>%
#    mutate(id = factor(id, labels  = str_sub(cal_set, 1, -5))) %>%
#    ggplot(aes(date, WAGT)) +
#    geom_line() + 
#    geom_point(aes(date, WAGT_OBS)) +
#    scale_x_date(date_labels = "%Y-%m") +
#    facet_wrap(. ~id, scales="free_x") +
#    theme_bw()


