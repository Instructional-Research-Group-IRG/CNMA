# This script performs a component network meta-analysis (cNMA) of mathematics education interventions using outcomes in the whole and ration numbers domains. 
# Sample: all nodes
# Variable for defining outcome domain: intervention_content
# Disaggregated by domain: Yes

# Load required packages

  ## Install 'devel' version of metafor package
  #install.packages("remotes") 
  #remotes::install_github("wviechtb/metafor") 
  
  ## Install and load other required packages
  #install.packages("pacman") 
  pacman::p_load(metafor, googlesheets4, dplyr, tidyr, skimr, testit, assertable, meta, netmeta, stringr, janitor, naniar, igraph, multcomp, broom, gridExtra, ggplot2, writexl, readr, grid, gridExtra, cowplot, extrafont)
  
# Load (read) data (i.e., copy data to 'dat')
CNMA_Data <- read_sheet("https://docs.google.com/spreadsheets/d/1oCcRHU6OSc64OWVNLx1uksOu4QQlf2Xo7p4SZahPMio/edit?gid=931222966#gid=931222966&fvid=1356828720", sheet="Master Database") # <<CNMA master database>>
  
  ## Explore data  
  CNMA_Data %>% count() 
  head(CNMA_Data)
  skim(CNMA_Data)
  CNMA_Data$contrast_id <- as.character(CNMA_Data$contrast_id)
  CNMA_Data %>% count(study_id, contrast_id) %>% print(n= Inf)
  CNMA_Data %>% count(domain, measure_name) %>% print(n= Inf)
  
  ## Check ratings
  CNMA_Data %>% group_by(wwc_rating) %>% count() %>% ungroup()
  CNMA_Data %>% group_by(domain, wwc_rating) %>% count() %>% ungroup() %>% print(n= Inf)
  
  ## Check for full duplicates
  dups <- anyDuplicated(CNMA_Data)
  assert("assert no duplicate entries", dups == 0) #No full duplicates.
  
  ## Check key columns/variables
  
    inspect_categorical <- function(data, column) {
      col <- dplyr::pull(data, {{ column }})
      print(class(col))
      janitor::tabyl(col)
    }
    
    inspect_continuous <- function(data, column) {
      data %>%
        summarise(
          n_missing = sum(is.na({{ column }})),
          n_nonmissing = sum(!is.na({{ column }})),
          min  = min({{ column }}, na.rm = TRUE),
          max  = max({{ column }}, na.rm = TRUE),
          mean = mean({{ column }}, na.rm = TRUE)
        )
    }
      
    ### Domain
    inspect_categorical(CNMA_Data, intervention_content)

    ### Sample sizes
    CNMA_Data %>% count(intervention_n, comparison_n, full_sample_size) %>% print(n = Inf)
    
    ### Statistics
    inspect_continuous(CNMA_Data, effect_size)
    inspect_continuous(CNMA_Data, standard_error)
    
    ### Components
    
      #### Number Line- Primary or one of many
      inspect_categorical(CNMA_Data, NL_TX) # Column AB
      inspect_categorical(CNMA_Data, N_TX) # Column AC
      
      inspect_categorical(CNMA_Data, NL_COMP) # Column BY
      inspect_categorical(CNMA_Data, N_COMP) # Column BZ
      
      CNMA_Data %>% count(NL_TX, N_TX, NL_COMP, N_COMP) %>% print(n = Inf)
      
      #### Representations- Students use or view
      inspect_categorical(CNMA_Data, R_TX) # Column AE
      inspect_categorical(CNMA_Data, RV_TX) # Column AF
      
      inspect_categorical(CNMA_Data, R_COMP) # Column CB
      inspect_categorical(CNMA_Data, RV_COMP) # Column CC
      
      CNMA_Data %>% count(R_TX, RV_TX, R_COMP, RV_COMP) %>% print(n = Inf)      
      
      #### Student Explanations- Taught or given opportunities
      inspect_categorical(CNMA_Data, ME_TX) # Column AH
      inspect_categorical(CNMA_Data, VT_TX...35) # Column AI
      
      inspect_categorical(CNMA_Data, ME_COMP) # Column CE
      inspect_categorical(CNMA_Data, VT_COMP...84) # Column CF
      
      CNMA_Data %>% count(ME_TX, VT_TX...35, ME_COMP, VT_COMP...84) %>% print(n = Inf)   
      
      #### Vocabulary
      inspect_categorical(CNMA_Data, WTS_TX) # Column AK
      #inspect_categorical(CNMA_Data, VT_TX...38) # Column AL   
      inspect_categorical(CNMA_Data, SV_TX) # Column AM   
      
      inspect_categorical(CNMA_Data, WTS_COMP) # Column CH
      #inspect_categorical(CNMA_Data, VT_COMP...87) # Column CI   
      CNMA_Data$SV_COMP <- as.numeric(CNMA_Data$SV_COMP) #Change from class "logical" to "numeric". Imports as logical because all vlaues "N/A".
      inspect_categorical(CNMA_Data, SV_COMP) # Column CJ
      
      CNMA_Data %>% count(WTS_TX, SV_TX, WTS_COMP, SV_COMP) %>% print(n = Inf)
      
      #### Fluency- Feedback, goals, content, count of activities
      inspect_categorical(CNMA_Data, FF_TX) # Column AO
      inspect_categorical(CNMA_Data, FO_TX) # Column AP   

      inspect_categorical(CNMA_Data, FF_COMP) # Column CL
      inspect_categorical(CNMA_Data, FO_COMP) # Column CM  
      
      CNMA_Data %>% count(FF_TX, FO_TX, FF_COMP, FO_COMP) %>% print(n = Inf)
      
      #### Positive Reinforcement- Math or Behavior (Bx)
      inspect_categorical(CNMA_Data, BR_TX) # Column BJ
      inspect_categorical(CNMA_Data, MR_TX) # Column BK   
      inspect_categorical(CNMA_Data, PREXTRA_TX) # Column BL
      inspect_categorical(CNMA_Data, BX_TX) # Column BM   
      
      inspect_categorical(CNMA_Data, BR_COMP) # Column DG
      inspect_categorical(CNMA_Data, MR_COMP) # Column DH  
      inspect_categorical(CNMA_Data, PREXTRA_COMP) # Column DI
      inspect_categorical(CNMA_Data, BX_COMP) # Column DJ 
      
      CNMA_Data %>% count(BR_TX, MR_TX, PREXTRA_TX, BX_TX, BR_COMP, MR_COMP, PREXTRA_COMP, BX_COMP) %>% print(n = Inf)
      
      #### Worked Examples
      inspect_categorical(CNMA_Data, WXA_TX) # Column BO
      inspect_categorical(CNMA_Data, WXP_TX) # Column BP   
      
      inspect_categorical(CNMA_Data, WXA_COMP) # Column DL
      inspect_categorical(CNMA_Data, WXP_COMP) # Column DM  
      
      CNMA_Data %>% count(WXA_TX, WXP_TX, WXA_COMP, WXP_COMP) %>% print(n = Inf)      
      
      #### Strategy Instruction-  multi step strategy or basic fact strategy taught
      CNMA_Data <- CNMA_Data %>% rename(WP2_TX = `2_WPS_word problem specific [strategy]`)
      
      inspect_categorical(CNMA_Data, MS2_TX) # Column BR
      inspect_categorical(CNMA_Data, WPS_TX) # Column BS   
      inspect_categorical(CNMA_Data, WP2_TX) # Column BT
      inspect_categorical(CNMA_Data, MS_TX) # Column BU
      inspect_categorical(CNMA_Data, BFS_TX) # Column BV   
      
      inspect_categorical(CNMA_Data, MS2_COMP) # Column DO
      inspect_categorical(CNMA_Data, WPS_COMP) # Column DP  
      inspect_categorical(CNMA_Data, WP2_COMP) # Column DQ
      inspect_categorical(CNMA_Data, MS_COMP) # Column DR
      inspect_categorical(CNMA_Data, BFS_COMP) # Column DS 
      
      CNMA_Data %>% count(MS2_TX, WPS_TX, WP2_TX, MS_TX, BFS_TX, MS2_COMP, WPS_COMP, WP2_COMP, MS_COMP, BFS_COMP) %>% print(n = Inf) 
      
# Additional modifications to NMA subset analysis data for running NMA with metafor  
  
  ## Replace n/A with zeros in components   
  
  replace_na_specific <- function(df, cols) {
    df %>%
      mutate(across(
        all_of(cols),
        ~ replace(.x, is.na(.x), 0)
      ))
  } 
  
  CNMA_Data <- replace_na_specific(CNMA_Data, c("NL_TX", "N_TX", "NL_COMP", "N_COMP"))
  CNMA_Data <- replace_na_specific(CNMA_Data, c("R_TX", "RV_TX", "R_COMP", "RV_COMP"))
  CNMA_Data <- replace_na_specific(CNMA_Data, c("ME_TX", "VT_TX...35", "ME_COMP", "VT_COMP...84"))
  CNMA_Data <- replace_na_specific(CNMA_Data, c("WTS_TX", "SV_TX", "WTS_COMP", "SV_COMP"))
  CNMA_Data <- replace_na_specific(CNMA_Data, c("FF_TX", "FO_TX", "FF_COMP", "FO_COMP"))
  CNMA_Data <- replace_na_specific(CNMA_Data, c("BR_TX", "MR_TX", "PREXTRA_TX", "BX_TX", "BR_COMP", "MR_COMP", "PREXTRA_COMP", "BX_COMP"))
  CNMA_Data <- replace_na_specific(CNMA_Data, c("WXA_TX", "WXP_TX", "WXA_COMP", "WXP_COMP"))
  CNMA_Data <- replace_na_specific(CNMA_Data, c("MS2_TX", "WPS_TX", "WP2_TX", "MS_TX", "BFS_TX", "MS2_COMP", "WPS_COMP", "WP2_COMP", "MS_COMP", "BFS_COMP"))
  
  CNMA_Data %>% count(NL_TX, N_TX, NL_COMP, N_COMP) %>% print(n = Inf)
  CNMA_Data %>% count(R_TX, RV_TX, R_COMP, RV_COMP) %>% print(n = Inf)  
  CNMA_Data %>% count(ME_TX, VT_TX...35, ME_COMP, VT_COMP...84) %>% print(n = Inf) 
  CNMA_Data %>% count(WTS_TX, SV_TX, WTS_COMP, SV_COMP) %>% print(n = Inf)
  CNMA_Data %>% count(FF_TX, FO_TX, FF_COMP, FO_COMP) %>% print(n = Inf)
  CNMA_Data %>% count(BR_TX, MR_TX, PREXTRA_TX, BX_TX, BR_COMP, MR_COMP, PREXTRA_COMP, BX_COMP) %>% print(n = Inf)
  CNMA_Data %>% count(WXA_TX, WXP_TX, WXA_COMP, WXP_COMP) %>% print(n = Inf)   
  CNMA_Data %>% count(MS2_TX, WPS_TX, WP2_TX, MS_TX, BFS_TX, MS2_COMP, WPS_COMP, WP2_COMP, MS_COMP, BFS_COMP) %>% print(n = Inf) 
      
  ## Create intervention and comparison bundles  
  intervention_component_bundle
  
  comparison_component_bundle
      
  ## Create contrast codes    
  
  ## Convert variables to their intended types 
  convert_to_character <- function(x) {
    as.character(x)
  }
  
  convert_to_factor <- function(x) {
    as.factor(x)
  }  
 
  ## Drop intervention versus comparison contrasts that have the same bundles

  ## Correct variable names
  
 
# Execute additive component network meta-analysis using a contrast-based random-effects model using BAU as the reference condition: intervention_content == "Whole Numbers (W)"
      
  ## Subset analysis data frame further to just the Whole Numbers (W) intervention content (icW)
  tabyl(NMA_data_analysis_subset_grpID$intervention_content)
  NMA_data_analysis_subset_grpID_icW <- NMA_data_analysis_subset_grpID %>% filter(intervention_content == "W")
  tabyl(NMA_data_analysis_subset_grpID_icW$intervention_content)
  NMA_data_analysis_subset_grpID_icW_c <- NMA_data_analysis_subset_grpID_icW %>% distinct(contrast_id, .keep_all = TRUE)
  NMA_data_analysis_subset_grpID_icW_c %>% count()
  
  ## Add contrast matrix to dataset
  NMA_data_analysis_subset_grpID_icW <- contrmat(NMA_data_analysis_subset_grpID_icW, grp1="intervention_prelim", grp2="comparison_prelim")
  str(NMA_data_analysis_subset_grpID_icW)
  
  ## Calculate the variance-covariance matrix for multi-treatment studies
  V_list <- vcalc(variance, cluster= record_id, obs= measure_name, type= domain, rho=c(0.6, 0.6), grp1=group1_id, grp2=group2_id, w1=intervention_n, w2=comparison_n, data=NMA_data_analysis_subset_grpID_icW)
  V_list    
  V_list_icW <- data.frame(V_list)
  #write_csv(V_list_icW, 'V_list_icW.csv')
        
  ## Run additive cNMA with the unique intervention components as moderators  
  tabyl(NMA_data_analysis_subset_grpID_icW$intervention_prelim)
  tabyl(NMA_data_analysis_subset_grpID_icW$comparison_prelim)
  check_icW <- NMA_data_analysis_subset_grpID_icW %>% dplyr::select(record_id, contrast_id, intervention_prelim, comparison_prelim)
  print(check_icW)
  
    ### Prepare component binaries for cNMA
    cNMA_data_analysis_subset_grpID_icW <- NMA_data_analysis_subset_grpID_icW
    tabyl(cNMA_data_analysis_subset_grpID_icW$intervention_prelim)
    tabyl(cNMA_data_analysis_subset_grpID_icW$comparison_prelim)
    
    cNMA_data_analysis_subset_grpID_icW$FF <- 0
    cNMA_data_analysis_subset_grpID_icW$RS <- 0
    cNMA_data_analysis_subset_grpID_icW$NL <- 0
    cNMA_data_analysis_subset_grpID_icW$SE <- 0
    cNMA_data_analysis_subset_grpID_icW$VF <- 0
    cNMA_data_analysis_subset_grpID_icW$BAU <- 0
    
    tabyl(cNMA_data_analysis_subset_grpID_icW$FF)
    tabyl(cNMA_data_analysis_subset_grpID_icW$RS)
    tabyl(cNMA_data_analysis_subset_grpID_icW$NL)
    tabyl(cNMA_data_analysis_subset_grpID_icW$SE)  
    tabyl(cNMA_data_analysis_subset_grpID_icW$VF)
    tabyl(cNMA_data_analysis_subset_grpID_icW$BAU)
    
    ### FF
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(FF = ifelse(intervention_prelim=="FF",1, FF))
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(FF = ifelse(intervention_prelim=="FF+RS",1, FF))
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(FF = ifelse(intervention_prelim=="NL+FF+RS",1, FF))
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(FF = ifelse(intervention_prelim=="NL+SE+FF+RS",1, FF))
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(FF = ifelse(intervention_prelim=="VF+FF+RS",1, FF))
    
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(FF = ifelse(comparison_prelim=="FF" & FF==0,-1, ifelse(comparison_prelim=="FF" & FF==1, 0, FF)))
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(FF = ifelse(comparison_prelim=="FF+RS" & FF==0,-1, ifelse(comparison_prelim=="FF+RS" & FF==1, 0, FF)))
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(FF = ifelse(comparison_prelim=="NL+FF+RS" & FF==0,-1, ifelse(comparison_prelim=="NL+FF+RS" & FF==1, 0, FF)))
    
    cNMA_data_analysis_subset_grpID_icW_FF <- cNMA_data_analysis_subset_grpID_icW %>% dplyr::select(intervention_prelim, comparison_prelim, FF)
    print(cNMA_data_analysis_subset_grpID_icW_FF)
    tabyl(cNMA_data_analysis_subset_grpID_icW$FF)
    
    ### RS
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(RS = ifelse(intervention_prelim=="FF+RS",1, RS))
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(RS = ifelse(intervention_prelim=="NL+FF+RS",1, RS))
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(RS = ifelse(intervention_prelim=="NL+RS",1, RS))    
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(RS = ifelse(intervention_prelim=="NL+SE+FF+RS",1, RS))
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(RS = ifelse(intervention_prelim=="NL+SE+RS",1, RS))   
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(RS = ifelse(intervention_prelim=="NL+SE+VF+RS",1, RS))     
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(RS = ifelse(intervention_prelim=="RS",1, RS))    
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(RS = ifelse(intervention_prelim=="SE+RS",1, RS))     
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(RS = ifelse(intervention_prelim=="VF+FF+RS",1, RS))
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(RS = ifelse(intervention_prelim=="VF+RS",1, RS))    
    
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(RS = ifelse(comparison_prelim=="FF+RS" & RS==0,-1, ifelse(comparison_prelim=="FF+RS" & RS==1, 0, RS)))
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(RS = ifelse(comparison_prelim=="NL+FF+RS" & RS==0,-1, ifelse(comparison_prelim=="NL+FF+RS" & RS==1, 0, RS)))
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(RS = ifelse(comparison_prelim=="NL+SE+RS" & RS==0,-1, ifelse(comparison_prelim=="NL+SE+RS" & RS==1, 0, RS)))
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(RS = ifelse(comparison_prelim=="RS" & RS==0,-1, ifelse(comparison_prelim=="RS" & RS==1, 0, RS)))
    
    cNMA_data_analysis_subset_grpID_icW_RS <- cNMA_data_analysis_subset_grpID_icW %>% dplyr::select(intervention_prelim, comparison_prelim, RS)
    print(cNMA_data_analysis_subset_grpID_icW_RS)
    tabyl(cNMA_data_analysis_subset_grpID_icW$RS)    

    ### NL
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(NL = ifelse(intervention_prelim=="NL+FF+RS",1, NL))
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(NL = ifelse(intervention_prelim=="NL+RS",1, NL))    
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(NL = ifelse(intervention_prelim=="NL+SE+FF+RS",1, NL))
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(NL = ifelse(intervention_prelim=="NL+SE+RS",1, NL))   
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(NL = ifelse(intervention_prelim=="NL+SE+VF+RS",1, NL))     

    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(NL = ifelse(comparison_prelim=="NL+FF+RS" & NL==0,-1, ifelse(comparison_prelim=="NL+FF+RS" & NL==1, 0, NL)))
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(NL = ifelse(comparison_prelim=="NL+SE+RS" & NL==0,-1, ifelse(comparison_prelim=="NL+SE+RS" & NL==1, 0, NL)))

    cNMA_data_analysis_subset_grpID_icW_NL <- cNMA_data_analysis_subset_grpID_icW %>% dplyr::select(intervention_prelim, comparison_prelim, NL)
    print(cNMA_data_analysis_subset_grpID_icW_NL)
    tabyl(cNMA_data_analysis_subset_grpID_icW$NL)
    
    ### SE
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(SE = ifelse(intervention_prelim=="NL+SE+FF+RS",1, SE))
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(SE = ifelse(intervention_prelim=="NL+SE+RS",1, SE))
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(SE = ifelse(intervention_prelim=="NL+SE+VF+RS",1, SE))  
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(SE = ifelse(intervention_prelim=="SE+RS",1, SE))  
    
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(SE = ifelse(comparison_prelim=="NL+SE+RS" & SE==0,-1, ifelse(comparison_prelim=="NL+SE+RS" & SE==1, 0, SE)))
    
    cNMA_data_analysis_subset_grpID_icW_SE <- cNMA_data_analysis_subset_grpID_icW %>% dplyr::select(intervention_prelim, comparison_prelim, SE)
    print(cNMA_data_analysis_subset_grpID_icW_SE)
    tabyl(cNMA_data_analysis_subset_grpID_icW$SE)
    
    ### VF
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(VF = ifelse(intervention_prelim=="NL+SE+VF+RS",1, VF))
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(VF = ifelse(intervention_prelim=="VF+FF+RS",1, VF))   
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(VF = ifelse(intervention_prelim=="VF+RS",1, VF))     

    cNMA_data_analysis_subset_grpID_icW_VF <- cNMA_data_analysis_subset_grpID_icW %>% dplyr::select(intervention_prelim, comparison_prelim, VF)
    print(cNMA_data_analysis_subset_grpID_icW_VF)
    tabyl(cNMA_data_analysis_subset_grpID_icW$VF)    
    
    ### Fit NMA model assuming consistency (tau^2_omega=0)
    res_mod_icW_cnma <- rma.mv(effect_size, V_list, 
                            mods = ~ FF + RS + NL + SE + VF - 1, # BAU is excluded to serve as the reference level for the comparisons.
                            random = ~ 1 | record_id/es_id, 
                            rho=0.60, 
                            data=cNMA_data_analysis_subset_grpID_icW)
    summary(res_mod_icW_cnma) 
    
    ### Estimate all pairwise differences between treatments
    contr <- data.frame(t(combn(names(coef(res_mod_icW_cnma)), 2)))
    contr <- contrmat(contr, "X1", "X2")
    rownames(contr) <- paste(contr$X1, "-", contr$X2)
    contr <- as.matrix(contr[-c(1:2)])
    sav <- predict(res_mod_icW_cnma, newmods=contr)
    sav[["slab"]] <- rownames(contr)
    sav
        
    ### Create league table (create diagonal matrix from output sav)
    lt_info_df <- as.data.frame(sav, optional = TRUE)
    lt_info_df <- cbind(Comparison = rownames(lt_info_df), lt_info_df)
    lt_info_df2 <- lt_info_df %>% separate_wider_delim(Comparison, delim = ' - ', names = c('comp1', 'comp2'))
    round_digits <- function(x) {
      round(x, digits = 2)
    }
    convert_to_character <- function(x) {
      as.character(x)
    }
    lt_info_df2[c("pred","ci.lb","ci.ub")] <- lapply(lt_info_df2[c("pred","ci.lb","ci.ub")], round_digits)
    lt_info_df2[c("pred","ci.lb","ci.ub")] <- lapply(lt_info_df2[c("pred","ci.lb","ci.ub")], as.character)
    lt_info_df2$ci.lb <- paste("(", lt_info_df2$ci.lb, " ,", sep= "")
    lt_info_df2$ci.ub <- paste(lt_info_df2$ci.ub, ")", sep= "")
    lt_info_df2 <- lt_info_df2 %>% unite(pred_cis, pred, ci.lb, ci.ub, sep= " ", remove = FALSE )
    print(lt_info_df2)
    lt_info_df3 <- lt_info_df2 %>% pivot_wider(id_cols= "comp1", names_from= "comp2", values_from = "pred_cis") #This creates the league table formatted as "left vs top".
    lt_info_df3 <- rename(lt_info_df3, Intervention = comp1)
    print(lt_info_df3)
    write_csv(lt_info_df3, file = "cnma_league_table_icW_allnodes.csv")
    #write_xlsx(lt_info_df3, 'cnma_league_table_icW_allnodes.xlsx')
    
    ### Compute p-values
    contr <- data.frame(t(combn(c(names(coef(res_mod_icW_cnma)),"BAU"), 2))) # add "BAU" to contrast matrix / Likely to remove this from output/forest plot
    contr <- contrmat(contr, "X1", "X2", last="BAU", append=FALSE)
    b <- c(coef(res_mod_icW_cnma),0) # add 0 for 'BAU' (the "reference treatment" excluded from the mods argument of the rma.mv function executing the NMA above)
    vb <- bldiag(vcov(res_mod_icW_cnma),0) # add 0 row/column for 'BAU' (the "reference treatment" excluded from the mods argument of the rma.mv function executing the NMA above)
    pvals <- apply(contr, 1, function(x) pnorm((x%*%b) / sqrt(t(x)%*%vb%*%x)))
    pvals
        
    ### Create table of p-values
    tab <- vec2mat(pvals, corr=FALSE)
    tab[lower.tri(tab)] <- t((1 - tab)[lower.tri(tab)])
    rownames(tab) <- colnames(tab) <- colnames(contr)
    round(tab, 2) # Like Table 2 in the following: https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/s12874-015-0060-8/tables/2
        
    ### Compute the P-scores
    pscores <- cbind(round(sort(apply(tab, 1, mean, na.rm=TRUE), decreasing=TRUE), 3))
    pscores
        
    ### Add P-scores to model output object
    res_mod_icW_cnma_df <- tidy(res_mod_icW_cnma, conf.int = TRUE)
    pscores_df <- cbind(term = rownames(pscores), as.data.frame(pscores))
    res_mod_icW_cnma_pscore <- res_mod_icW_cnma_df %>% left_join(pscores_df, by = c("term"))
    res_mod_icW_cnma_pscore <- res_mod_icW_cnma_pscore %>% rename(intervention = term, se = std.error, zval = statistic, pval = p.value, ci.lb = conf.low, ci.ub = conf.high,  Pscore = V1)
    res_mod_icW_cnma_pscore
    

# Execute network meta-analysis using a contrast-based random-effects model using BAU as the reference condition: intervention_content == "Rational Numbers (R)"
    
    ## Subset analysis data frame further to just the Rhole Numbers (R) intervention content (icR)
    tabyl(NMA_data_analysis_subset_grpID$intervention_content)
    NMA_data_analysis_subset_grpID_icR <- NMA_data_analysis_subset_grpID %>% filter(intervention_content == "R")
    tabyl(NMA_data_analysis_subset_grpID_icR$intervention_content)
    NMA_data_analysis_subset_grpID_icR_c <- NMA_data_analysis_subset_grpID_icR %>% distinct(contrast_id, .keep_all = TRUE)
    NMA_data_analysis_subset_grpID_icR_c %>% count()
    
    ## Add contrast matrix to dataset
    NMA_data_analysis_subset_grpID_icR <- contrmat(NMA_data_analysis_subset_grpID_icR, grp1="intervention_prelim", grp2="comparison_prelim")
    str(NMA_data_analysis_subset_grpID_icR)
    
    ## Calculate the variance-covariance matrix for multi-treatment studies
    V_list <- vcalc(variance, cluster= record_id, obs= measure_name, type= domain, rho=c(0.6, 0.6), grp1=group1_id, grp2=group2_id, w1=intervention_n, w2=comparison_n, data=NMA_data_analysis_subset_grpID_icR)
    V_list    
    V_list_icR <- data.frame(V_list)
    #write_csv(V_list_icR, 'V_list_icR.csv')
    
    ## Run additive cNMA with the unique intervention components as moderators  
    tabyl(NMA_data_analysis_subset_grpID_icR$intervention_prelim)
    tabyl(NMA_data_analysis_subset_grpID_icR$comparison_prelim)
    check_icR <- NMA_data_analysis_subset_grpID_icR %>% dplyr::select(record_id, contrast_id, intervention_prelim, comparison_prelim)
    print(check_icR)
    
      ### Prepare component binaries for cNMA
      cNMA_data_analysis_subset_grpID_icR <- NMA_data_analysis_subset_grpID_icR
      tabyl(cNMA_data_analysis_subset_grpID_icR$intervention_prelim)
      tabyl(cNMA_data_analysis_subset_grpID_icR$comparison_prelim)
      
      cNMA_data_analysis_subset_grpID_icR$FF <- 0
      cNMA_data_analysis_subset_grpID_icR$RS <- 0
      cNMA_data_analysis_subset_grpID_icR$NL <- 0
      cNMA_data_analysis_subset_grpID_icR$SE <- 0
      cNMA_data_analysis_subset_grpID_icR$VF <- 0
      cNMA_data_analysis_subset_grpID_icR$BAU <- 0
      
      tabyl(cNMA_data_analysis_subset_grpID_icR$FF)
      tabyl(cNMA_data_analysis_subset_grpID_icR$RS)
      tabyl(cNMA_data_analysis_subset_grpID_icR$NL)
      tabyl(cNMA_data_analysis_subset_grpID_icR$SE)  
      tabyl(cNMA_data_analysis_subset_grpID_icR$VF)
      tabyl(cNMA_data_analysis_subset_grpID_icR$BAU)
      
      ### FF
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(FF = ifelse(intervention_prelim=="FF",1, FF))
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(FF = ifelse(intervention_prelim=="FF+RS",1, FF))
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(FF = ifelse(intervention_prelim=="NL+FF+RS",1, FF))
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(FF = ifelse(intervention_prelim=="NL+SE+FF+RS",1, FF))
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(FF = ifelse(intervention_prelim=="VF+FF+RS",1, FF))
      
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(FF = ifelse(comparison_prelim=="FF" & FF==0,-1, ifelse(comparison_prelim=="FF" & FF==1, 0, FF)))
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(FF = ifelse(comparison_prelim=="FF+RS" & FF==0,-1, ifelse(comparison_prelim=="FF+RS" & FF==1, 0, FF)))
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(FF = ifelse(comparison_prelim=="NL+FF+RS" & FF==0,-1, ifelse(comparison_prelim=="NL+FF+RS" & FF==1, 0, FF)))
      
      cNMA_data_analysis_subset_grpID_icR_FF <- cNMA_data_analysis_subset_grpID_icR %>% dplyr::select(intervention_prelim, comparison_prelim, FF)
      print(cNMA_data_analysis_subset_grpID_icR_FF)
      tabyl(cNMA_data_analysis_subset_grpID_icR$FF)
      
      ### RS
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(RS = ifelse(intervention_prelim=="FF+RS",1, RS))
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(RS = ifelse(intervention_prelim=="NL+FF+RS",1, RS))
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(RS = ifelse(intervention_prelim=="NL+RS",1, RS))    
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(RS = ifelse(intervention_prelim=="NL+SE+FF+RS",1, RS))
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(RS = ifelse(intervention_prelim=="NL+SE+RS",1, RS))   
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(RS = ifelse(intervention_prelim=="NL+SE+VF+RS",1, RS))     
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(RS = ifelse(intervention_prelim=="RS",1, RS))    
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(RS = ifelse(intervention_prelim=="SE+RS",1, RS))     
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(RS = ifelse(intervention_prelim=="VF+FF+RS",1, RS))
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(RS = ifelse(intervention_prelim=="VF+RS",1, RS))    
      
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(RS = ifelse(comparison_prelim=="FF+RS" & RS==0,-1, ifelse(comparison_prelim=="FF+RS" & RS==1, 0, RS)))
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(RS = ifelse(comparison_prelim=="NL+FF+RS" & RS==0,-1, ifelse(comparison_prelim=="NL+FF+RS" & RS==1, 0, RS)))
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(RS = ifelse(comparison_prelim=="NL+SE+RS" & RS==0,-1, ifelse(comparison_prelim=="NL+SE+RS" & RS==1, 0, RS)))
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(RS = ifelse(comparison_prelim=="RS" & RS==0,-1, ifelse(comparison_prelim=="RS" & RS==1, 0, RS)))
      
      cNMA_data_analysis_subset_grpID_icR_RS <- cNMA_data_analysis_subset_grpID_icR %>% dplyr::select(intervention_prelim, comparison_prelim, RS)
      print(cNMA_data_analysis_subset_grpID_icR_RS)
      tabyl(cNMA_data_analysis_subset_grpID_icR$RS)    
      
      ### NL
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(NL = ifelse(intervention_prelim=="NL+FF+RS",1, NL))
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(NL = ifelse(intervention_prelim=="NL+RS",1, NL))    
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(NL = ifelse(intervention_prelim=="NL+SE+FF+RS",1, NL))
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(NL = ifelse(intervention_prelim=="NL+SE+RS",1, NL))   
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(NL = ifelse(intervention_prelim=="NL+SE+VF+RS",1, NL))     
      
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(NL = ifelse(comparison_prelim=="NL+FF+RS" & NL==0,-1, ifelse(comparison_prelim=="NL+FF+RS" & NL==1, 0, NL)))
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(NL = ifelse(comparison_prelim=="NL+SE+RS" & NL==0,-1, ifelse(comparison_prelim=="NL+SE+RS" & NL==1, 0, NL)))
      
      cNMA_data_analysis_subset_grpID_icR_NL <- cNMA_data_analysis_subset_grpID_icR %>% dplyr::select(intervention_prelim, comparison_prelim, NL)
      print(cNMA_data_analysis_subset_grpID_icR_NL)
      tabyl(cNMA_data_analysis_subset_grpID_icR$NL)
      
      ### SE
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(SE = ifelse(intervention_prelim=="NL+SE+FF+RS",1, SE))
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(SE = ifelse(intervention_prelim=="NL+SE+RS",1, SE))      
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(SE = ifelse(intervention_prelim=="NL+SE+VF+RS",1, SE))  
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(SE = ifelse(intervention_prelim=="SE+RS",1, SE))  
      
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(SE = ifelse(comparison_prelim=="NL+SE+RS" & SE==0,-1, ifelse(comparison_prelim=="NL+SE+RS" & SE==1, 0, SE)))
      
      cNMA_data_analysis_subset_grpID_icR_SE <- cNMA_data_analysis_subset_grpID_icR %>% dplyr::select(intervention_prelim, comparison_prelim, SE)
      print(cNMA_data_analysis_subset_grpID_icR_SE)
      tabyl(cNMA_data_analysis_subset_grpID_icR$SE)
      
      ### VF
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(VF = ifelse(intervention_prelim=="NL+SE+VF+RS",1, VF))
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(VF = ifelse(intervention_prelim=="VF+FF+RS",1, VF))   
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(VF = ifelse(intervention_prelim=="VF+RS",1, VF))     
      
      cNMA_data_analysis_subset_grpID_icR_VF <- cNMA_data_analysis_subset_grpID_icR %>% dplyr::select(intervention_prelim, comparison_prelim, VF)
      print(cNMA_data_analysis_subset_grpID_icR_VF)
      tabyl(cNMA_data_analysis_subset_grpID_icR$VF)    
      
      ### Fit NMA model assuming consistency (tau^2_omega=0)
      res_mod_icR_cnma <- rma.mv(effect_size, V_list, 
                                 mods = ~ FF + RS + NL + SE + VF - 1, # BAU is excluded to serve as the reference level for the comparisons.
                                 random = ~ 1 | record_id/es_id, 
                                 rho=0.60, 
                                 data=cNMA_data_analysis_subset_grpID_icR)
      summary(res_mod_icR_cnma) 
      
      ### Estimate all pairwise differences between treatments
      contr <- data.frame(t(combn(names(coef(res_mod_icR_cnma)), 2)))
      contr <- contrmat(contr, "X1", "X2")
      rownames(contr) <- paste(contr$X1, "-", contr$X2)
      contr <- as.matrix(contr[-c(1:2)])
      sav <- predict(res_mod_icR_cnma, newmods=contr)
      sav[["slab"]] <- rownames(contr)
      sav
      
      ### Create league table (create diagonal matrix from output sav)
      lt_info_df <- as.data.frame(sav, optional = TRUE)
      lt_info_df <- cbind(Comparison = rownames(lt_info_df), lt_info_df)
      lt_info_df2 <- lt_info_df %>% separate_wider_delim(Comparison, delim = ' - ', names = c('comp1', 'comp2'))
      round_digits <- function(x) {
        round(x, digits = 2)
      }
      convert_to_character <- function(x) {
        as.character(x)
      }
      lt_info_df2[c("pred","ci.lb","ci.ub")] <- lapply(lt_info_df2[c("pred","ci.lb","ci.ub")], round_digits)
      lt_info_df2[c("pred","ci.lb","ci.ub")] <- lapply(lt_info_df2[c("pred","ci.lb","ci.ub")], as.character)
      lt_info_df2$ci.lb <- paste("(", lt_info_df2$ci.lb, " ,", sep= "")
      lt_info_df2$ci.ub <- paste(lt_info_df2$ci.ub, ")", sep= "")
      lt_info_df2 <- lt_info_df2 %>% unite(pred_cis, pred, ci.lb, ci.ub, sep= " ", remove = FALSE )
      print(lt_info_df2)
      lt_info_df3 <- lt_info_df2 %>% pivot_wider(id_cols= "comp1", names_from= "comp2", values_from = "pred_cis") #This creates the league table formatted as "left vs top".
      lt_info_df3 <- rename(lt_info_df3, Intervention = comp1)
      print(lt_info_df3)
      write_csv(lt_info_df3, file = "cnma_league_table_icR_allnodes.csv")
      #write_xlsx(lt_info_df3, 'cnma_league_table_icR_allnodes.xlsx')
      
      ### Compute p-values
      contr <- data.frame(t(combn(c(names(coef(res_mod_icR_cnma)),"BAU"), 2))) # add "BAU" to contrast matrix / Likely to remove this from output/forest plot
      contr <- contrmat(contr, "X1", "X2", last="BAU", append=FALSE)
      b <- c(coef(res_mod_icR_cnma),0) # add 0 for 'BAU' (the "reference treatment" excluded from the mods argument of the rma.mv function executing the NMA above)
      vb <- bldiag(vcov(res_mod_icR_cnma),0) # add 0 row/column for 'BAU' (the "reference treatment" excluded from the mods argument of the rma.mv function executing the NMA above)
      pvals <- apply(contr, 1, function(x) pnorm((x%*%b) / sqrt(t(x)%*%vb%*%x)))
      pvals
      
      ### Create table of p-values
      tab <- vec2mat(pvals, corr=FALSE)
      tab[lower.tri(tab)] <- t((1 - tab)[lower.tri(tab)])
      rownames(tab) <- colnames(tab) <- colnames(contr)
      round(tab, 2) # Like Table 2 in the following: https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/s12874-015-0060-8/tables/2
      
      ### Compute the P-scores
      pscores <- cbind(round(sort(apply(tab, 1, mean, na.rm=TRUE), decreasing=TRUE), 3))
      pscores
      
      ### Add P-scores to model output object
      res_mod_icR_cnma_df <- tidy(res_mod_icR_cnma, conf.int = TRUE)
      pscores_df <- cbind(term = rownames(pscores), as.data.frame(pscores))
      res_mod_icR_cnma_pscore <- res_mod_icR_cnma_df %>% left_join(pscores_df, by = c("term"))
      res_mod_icR_cnma_pscore <- res_mod_icR_cnma_pscore %>% rename(intervention = term, se = std.error, zval = statistic, pval = p.value, ci.lb = conf.low, ci.ub = conf.high,  Pscore = V1)
      res_mod_icR_cnma_pscore        