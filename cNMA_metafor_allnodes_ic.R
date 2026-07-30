# This script performs a component network meta-analysis (cNMA) of mathematics education interventions using outcomes in the whole and ration numbers domains. 

# Variable for defining outcome domain: intervention_content
# Disaggregated by domain: Yes

# Load required packages

  ## Install 'devel' version of metafor package
  #install.packages("remotes") 
  #remotes::install_github("wviechtb/metafor") 
  
  ## Install and load other required packages
  #install.packages("pacman") 
  pacman::p_load(metafor, googlesheets4, dplyr, tidyr, skimr, testit, assertable, meta, netmeta, stringr, janitor, naniar, igraph, multcomp, broom, gridExtra, ggplot2, writexl, readr, grid, gridExtra, cowplot, extrafont, purrr)
  
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
  
  ## Correct duplicate column names
  CNMA_Data <- CNMA_Data %>% rename(VT_TX = VT_TX...35)
  CNMA_Data <- CNMA_Data %>% rename(VT_COMP = VT_COMP...84)
  
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
  CNMA_Data <- replace_na_specific(CNMA_Data, c("ME_TX", "VT_TX", "ME_COMP", "VT_COMP"))
  CNMA_Data <- replace_na_specific(CNMA_Data, c("WTS_TX", "SV_TX", "WTS_COMP", "SV_COMP"))
  CNMA_Data <- replace_na_specific(CNMA_Data, c("FF_TX", "FO_TX", "FF_COMP", "FO_COMP"))
  CNMA_Data <- replace_na_specific(CNMA_Data, c("BR_TX", "MR_TX", "PREXTRA_TX", "BX_TX", "BR_COMP", "MR_COMP", "PREXTRA_COMP", "BX_COMP"))
  CNMA_Data <- replace_na_specific(CNMA_Data, c("WXA_TX", "WXP_TX", "WXA_COMP", "WXP_COMP"))
  CNMA_Data <- replace_na_specific(CNMA_Data, c("MS2_TX", "WPS_TX", "WP2_TX", "MS_TX", "BFS_TX", "MS2_COMP", "WPS_COMP", "WP2_COMP", "MS_COMP", "BFS_COMP"))
  
  CNMA_Data %>% count(NL_TX, N_TX, NL_COMP, N_COMP) %>% print(n = Inf)
  CNMA_Data %>% count(R_TX, RV_TX, R_COMP, RV_COMP) %>% print(n = Inf)  
  CNMA_Data %>% count(ME_TX, VT_TX, ME_COMP, VT_COMP) %>% print(n = Inf) 
  CNMA_Data %>% count(WTS_TX, SV_TX, WTS_COMP, SV_COMP) %>% print(n = Inf)
  CNMA_Data %>% count(FF_TX, FO_TX, FF_COMP, FO_COMP) %>% print(n = Inf)
  CNMA_Data %>% count(BR_TX, MR_TX, PREXTRA_TX, BX_TX, BR_COMP, MR_COMP, PREXTRA_COMP, BX_COMP) %>% print(n = Inf)
  CNMA_Data %>% count(WXA_TX, WXP_TX, WXA_COMP, WXP_COMP) %>% print(n = Inf)   
  CNMA_Data %>% count(MS2_TX, WPS_TX, WP2_TX, MS_TX, BFS_TX, MS2_COMP, WPS_COMP, WP2_COMP, MS_COMP, BFS_COMP) %>% print(n = Inf) 
      
  ## Create intervention and comparison bundles  

    ### intervention_component_bundle
    make_indicator_labels_plus <- function(df, cols, new_col = "intervention_component_bundle") {
      df %>%
        rowwise() %>%
        mutate(
          {{ new_col }} := cols %>%
            keep(~ get(.x) == 1) %>%     # keep only column names where value == 1
            str_c(collapse = " + ")      # join with "+"
        ) %>%
        ungroup()
    }
    
    CNMA_Data <- make_indicator_labels_plus(CNMA_Data, c("NL_TX", "N_TX", "R_TX", "RV_TX", "ME_TX", "VT_TX", "WTS_TX", "SV_TX", "FF_TX", "FO_TX", "BR_TX", "MR_TX", "PREXTRA_TX", "BX_TX", "WXA_TX", "WXP_TX", "MS2_TX", "WPS_TX", "WP2_TX", "MS_TX", "BFS_TX"))
    tabyl(CNMA_Data$intervention_component_bundle)
    class(CNMA_Data$intervention_component_bundle)
    #CNMA_Data$intervention_component_bundle <- as.factor(CNMA_Data$intervention_component_bundle)
    class(CNMA_Data$intervention_component_bundle)
    tabyl(CNMA_Data$intervention_component_bundle)
    CNMA_Data %>% count(intervention_component_bundle, NL_TX, N_TX, R_TX, RV_TX, ME_TX, VT_TX, WTS_TX, SV_TX, FF_TX, FO_TX, BR_TX, MR_TX, PREXTRA_TX, BX_TX, WXA_TX, WXP_TX, MS2_TX, WPS_TX, WP2_TX, MS_TX, BFS_TX) %>% print(n = Inf)

    ### comparison_component_bundle
    make_indicator_labels_plus <- function(df, cols, new_col = "comparison_component_bundle") {
      df %>%
        rowwise() %>%
        mutate(
          {{ new_col }} := cols %>%
            keep(~ get(.x) == 1) %>%     # keep only column names where value == 1
            str_c(collapse = " + ")      # join with "+"
        ) %>%
        ungroup()
    }
    
    CNMA_Data <- make_indicator_labels_plus(CNMA_Data, c("NL_COMP", "N_COMP", "R_COMP", "RV_COMP", "ME_COMP", "VT_COMP", "WTS_COMP", "SV_COMP", "FF_COMP", "FO_COMP", "BR_COMP", "MR_COMP", "PREXTRA_COMP", "BX_COMP", "WXA_COMP", "WXP_COMP", "MS2_COMP", "WPS_COMP", "WP2_COMP", "MS_COMP", "BFS_COMP"))
    CNMA_Data <- CNMA_Data %>% mutate(comparison_component_bundle = if_else(comparison_component_bundle == "", "BAU", comparison_component_bundle))
    tabyl(CNMA_Data$comparison_component_bundle)
    class(CNMA_Data$comparison_component_bundle)
    #CNMA_Data$comparison_component_bundle <- as.factor(CNMA_Data$comparison_component_bundle)
    class(CNMA_Data$comparison_component_bundle)
    tabyl(CNMA_Data$comparison_component_bundle)
    CNMA_Data %>% count(comparison_component_bundle, NL_COMP, N_COMP, R_COMP, RV_COMP, ME_COMP, VT_COMP, WTS_COMP, SV_COMP, FF_COMP, FO_COMP, BR_COMP, MR_COMP, PREXTRA_COMP, BX_COMP, WXA_COMP, WXP_COMP, MS2_COMP, WPS_COMP, WP2_COMP, MS_COMP, BFS_COMP) %>% print(n = Inf)
    
  ## Create contrast codes   
  make_contrast_column <- function(df, col1, col2, new_col) {
    df %>%
      mutate(
        {{ new_col }} := case_when(
          .data[[col1]] == 1 & .data[[col2]] == 0 ~ 1,
          .data[[col1]] == 0 & .data[[col2]] == 1 ~ -1,
          .data[[col1]] == 1 & .data[[col2]] == 1 ~ 0,
          .data[[col1]] == 0 & .data[[col2]] == 0 ~ 0,
          TRUE ~ NA_real_
        )
      )
  }  
  
  CNMA_Data <- make_contrast_column(CNMA_Data, "NL_TX", "NL_COMP", NL)
  CNMA_Data %>% count(NL_TX, NL_COMP, NL) %>% print(n = Inf)
  
  CNMA_Data <- make_contrast_column(CNMA_Data, "N_TX", "N_COMP", N)
  CNMA_Data %>% count(N_TX, N_COMP, N) %>% print(n = Inf)
  
  CNMA_Data <- make_contrast_column(CNMA_Data, "R_TX", "R_COMP", R)
  CNMA_Data %>% count(R_TX, R_COMP, R) %>% print(n = Inf)
  
  CNMA_Data <- make_contrast_column(CNMA_Data, "RV_TX", "RV_COMP", RV)
  CNMA_Data %>% count(RV_TX, RV_COMP, RV) %>% print(n = Inf)
  
  CNMA_Data <- make_contrast_column(CNMA_Data, "ME_TX", "ME_COMP", ME)
  CNMA_Data %>% count(ME_TX, ME_COMP, ME) %>% print(n = Inf)
  
  CNMA_Data <- make_contrast_column(CNMA_Data, "VT_TX", "VT_COMP", VT)
  CNMA_Data %>% count(VT_TX, VT_COMP, VT) %>% print(n = Inf)
  
  CNMA_Data <- make_contrast_column(CNMA_Data, "WTS_TX", "WTS_COMP", WTS)
  CNMA_Data %>% count(WTS_TX, WTS_COMP, WTS) %>% print(n = Inf)
  
  CNMA_Data <- make_contrast_column(CNMA_Data, "SV_TX", "SV_COMP", SV)
  CNMA_Data %>% count(SV_TX, SV_COMP, SV) %>% print(n = Inf)
  
  CNMA_Data <- make_contrast_column(CNMA_Data, "FF_TX", "FF_COMP", FF)
  CNMA_Data %>% count(FF_TX, FF_COMP, FF) %>% print(n = Inf)
  
  CNMA_Data <- make_contrast_column(CNMA_Data, "FO_TX", "FO_COMP", FO)
  CNMA_Data %>% count(FO_TX, FO_COMP, FO) %>% print(n = Inf)  
  
  CNMA_Data <- make_contrast_column(CNMA_Data, "BR_TX", "BR_COMP", BR)
  CNMA_Data %>% count(BR_TX, BR_COMP, BR) %>% print(n = Inf)
  
  CNMA_Data <- make_contrast_column(CNMA_Data, "MR_TX", "MR_COMP", MR)
  CNMA_Data %>% count(MR_TX, MR_COMP, MR) %>% print(n = Inf)
  
  CNMA_Data <- make_contrast_column(CNMA_Data, "PREXTRA_TX", "PREXTRA_COMP", PREXTRA)
  CNMA_Data %>% count(PREXTRA_TX, PREXTRA_COMP, PREXTRA) %>% print(n = Inf)
  
  CNMA_Data <- make_contrast_column(CNMA_Data, "BX_TX", "BX_COMP", BX)
  CNMA_Data %>% count(BX_TX, BX_COMP, BX) %>% print(n = Inf)  
  
  CNMA_Data <- make_contrast_column(CNMA_Data, "WXA_TX", "WXA_COMP", WXA)
  CNMA_Data %>% count(WXA_TX, WXA_COMP, WXA) %>% print(n = Inf)
  
  CNMA_Data <- make_contrast_column(CNMA_Data, "WXP_TX", "WXP_COMP", WXP)
  CNMA_Data %>% count(WXP_TX, WXP_COMP, WXP) %>% print(n = Inf)  

  CNMA_Data <- make_contrast_column(CNMA_Data, "MS2_TX", "MS2_COMP", MS2)
  CNMA_Data %>% count(MS2_TX, MS2_COMP, MS2) %>% print(n = Inf)
  
  CNMA_Data <- make_contrast_column(CNMA_Data, "WPS_TX", "WPS_COMP", WPS)
  CNMA_Data %>% count(WPS_TX, WPS_COMP, WPS) %>% print(n = Inf)
  
  CNMA_Data <- make_contrast_column(CNMA_Data, "WP2_TX", "WP2_COMP", WP2)
  CNMA_Data %>% count(WP2_TX, WP2_COMP, WP2) %>% print(n = Inf)  
  
  CNMA_Data <- make_contrast_column(CNMA_Data, "MS_TX", "MS_COMP", MS)
  CNMA_Data %>% count(MS_TX, MS_COMP, MS) %>% print(n = Inf)
  
  CNMA_Data <- make_contrast_column(CNMA_Data, "BFS_TX", "BFS_COMP", BFS)
  CNMA_Data %>% count(BFS_TX, BFS_COMP, BFS) %>% print(n = Inf)  
  
  ## Drop intervention versus comparison contrasts that have the same bundles
  CNMA_Data <- CNMA_Data %>% mutate(nonzero_count = rowSums(across(c(NL, N, R, RV, ME, VT, WTS, SV, FF, FO, BR, MR, PREXTRA, BX, WXA, WXP, MS2, WPS, WP2, BFS), ~ .x != 0)))
  CNMA_Data2_dropcheck <- CNMA_Data %>% dplyr::select(study_id, contrast_id, es_id, intervention_component_bundle, comparison_component_bundle, NL, N, R, RV, ME, VT, WTS, SV, FF, FO, BR, MR, PREXTRA, BX, WXA, WXP, MS2, WPS, WP2, BFS, nonzero_count)
  print(CNMA_Data2_dropcheck, n=Inf)
  write_csv(CNMA_Data2_dropcheck, file = "CNMA_Data2_dropcheck.csv")
  #write_xlsx(CNMA_Data2_dropcheck, 'CNMA_Data2_dropcheck.xlsx')
  CNMA_Data %>% count()
  CNMA_Data_nomirrors <- CNMA_Data %>% filter(!(nonzero_count == 0)) #Those rows with all zero values for the contrast-coded components have the exact same intervention/comparison component bundles and thus are "mirrors" which we are dropping from the analysis.
  CNMA_Data_nomirrors %>% count()
  
  ## Check for lone components
  #TBD
  
# Execute additive component network meta-analysis using a contrast-based random-effects model using BAU as the reference condition: intervention_content == "Whole Numbers (W)"
      
  ## Subset analysis data frame further to just the Whole Numbers (W) intervention content (icW)
  tabyl(CNMA_Data_nomirrors$intervention_content)
  CNMA_Data_subset_icW <- CNMA_Data_nomirrors %>% filter(intervention_content == "W")
  tabyl(CNMA_Data_subset_icW$intervention_content)
  CNMA_Data_subset_icW_c <- CNMA_Data_subset_icW %>% distinct(contrast_id, .keep_all = TRUE)
  CNMA_Data_subset_icW_c %>% count()
  
  ## Add contrast matrix to dataset
  CNMA_Data_subset_icW <- contrmat(CNMA_Data_subset_icW, grp1="intervention_component_bundle", grp2="comparison_component_bundle")
  
  ## Calculate the variance-covariance matrix for multi-treatment studies
  V_list <- vcalc(variance, cluster= study_id, obs= measure_name, type= domain, rho=c(0.6, 0.6), grp1=intervention_component_bundle, grp2=comparison_component_bundle, w1=intervention_n, w2=comparison_n, data=CNMA_Data_subset_icW)
  V_list    
  V_list_icW <- data.frame(V_list)
  #write_csv(V_list_icW, 'V_list_icW.csv')
        
  ## Run additive cNMA with the unique intervention components as moderators  

    ### Fit additive CNMA model assuming consistency (tau^2_omega=0)
    res_mod_icW_cnma <- rma.mv(effect_size, V_list, 
                            # mods = ~ NL + N + R + RV + ME + VT + WTS + SV + FF + FO + BR + MR + PREXTRA + BX + WXA + WXP + MS2 + WPS + WP2 + BFS - 1, # Full list of available contrast-coded components for reference.
                            mods = ~ NL + R + ME + VT + WTS + FF + BR + MR + WXA + MS2 + BFS - 1, # BAU is excluded to serve as the reference level for the comparisons.
                            random = ~ 1 | study_id/es_id, # Not necessary to show that we do not need to include other dependencies and can reference previous investigations into this and related assumptions taken under NMA work
                            rho=0.60, 
                            data=CNMA_Data_subset_icW)
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
    write_csv(lt_info_df3, file = "cnma_league_table_icW.csv")
    #write_xlsx(lt_info_df3, 'cnma_league_table_icW.xlsx')
    
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
    
    ## Subset analysis data frame further to just the Rational Numbers (R) intervention content (icR)
    tabyl(CNMA_Data_nomirrors$intervention_content)
    CNMA_Data_subset_icR <- CNMA_Data_nomirrors %>% filter(intervention_content == "R")
    tabyl(CNMA_Data_subset_icR$intervention_content)
    CNMA_Data_subset_icR_c <- CNMA_Data_subset_icR %>% distinct(contrast_id, .keep_all = TRUE)
    CNMA_Data_subset_icR_c %>% count()
    
    ## Add contrast matrix to dataset
    CNMA_Data_subset_icR <- contrmat(CNMA_Data_subset_icR, grp1="intervention_component_bundle", grp2="comparison_component_bundle")
    
    ## Calculate the variance-covariance matrix for multi-treatment studies
    V_list <- vcalc(variance, cluster= study_id, obs= measure_name, type= domain, rho=c(0.6, 0.6), grp1=intervention_component_bundle, grp2=comparison_component_bundle, w1=intervention_n, w2=comparison_n, data=CNMA_Data_subset_icR)
    V_list    
    V_list_icR <- data.frame(V_list)
    #write_csv(V_list_icR, 'V_list_icR.csv')
    
    ## Run additive cNMA with the unique intervention components as moderators  
    
    ### Fit additive CNMA model assuming consistency (tau^2_omega=0)
    res_mod_icR_cnma <- rma.mv(effect_size, V_list, 
                               # mods = ~ NL + N + R + RV + ME + VT + WTS + SV + FF + FO + BR + MR + PREXTRA + BX + WXA + WXP + MS2 + WPS + WP2 + BFS - 1, # Full list of available contrast-coded components for reference.
                               mods = ~ NL + R + ME + VT + WTS + FF + BR + MR + WXA + MS2 + BFS - 1, # BAU is excluded to serve as the reference level for the comparisons.
                               random = ~ 1 | study_id/es_id, # Not necessary to show that we do not need to include other dependencies and can reference previous investigations into this and related assumptions taken under NMA work
                               rho=0.60, 
                               data=CNMA_Data_subset_icR)
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
    write_csv(lt_info_df3, file = "cnma_league_table_icR.csv")
    #write_xlsx(lt_info_df3, 'cnma_league_table_icR.xlsx')
    
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