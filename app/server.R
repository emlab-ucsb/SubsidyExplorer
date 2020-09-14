### -------------------------------------------------------------------
### SubsidyExplorer
### An interactive toolkit to explore the tradeoffs across proposed fisheries subsidies disciplines
### 
### Creator(s): Kat Millage and Vienna Saccomanno
### Release date (v1): July 2019
### Release date (v2): 
### 
### This script contains the the server logic of the app
### --------------------------------------------------------------------

shinyServer(function(input, output, session) {
  
  ### --------------------------
  ### Containers and general ---
  ### --------------------------
  
  # Reactive object that keeps track of all policies run -----------
  rv_results <- reactiveValues(
    
    run = tibble(id = character(),
                 
                 # Display name
                 name = character(),
                    
                 # # Policy triggers
                 iuu = list(),
                 oa = list(),
                 overcap = list(),
                 cap_tier = list(),
                 
                 # Policy description (words)
                 policy_description = list(),
                 
                 # Fleet summary
                 fleet_summary = list(),
                 
                 # Timeseries results
                 results_timeseries = list(),
                 
                 # Ending results
                 results_last = list())
    
  )
  
  # Reactive object that keeps track of what policy we're on ----
  rv_policy_id <- reactiveValues(id = "A")
  
  
  # Add most ambitious results to our reactive results data frame
  observeEvent(input$ab_introduction_to_selected_results, {
    
    # Ambitious results
    best_result <- tibble(id = "A",
                          name = "Most ambitious scenario",
                          iuu = list("NA"),
                          oa = list("NA"),
                          overcap = list("NA"),
                          cap_tier = list("NA"),
                          policy_description = list(
                            paste0(
                              "<b>", "Name: ", "</b>", "Most ambitious scenario", "</br>",
                              "<b>", "Summary: ", "</b>", "Complete removal of capacity-enhancing subsidies", "</br>")
                          ),
                          fleet_summary = list(remove_all_bad_fleet_summary),
                          results_timeseries = list(remove_all_bad_results_full),
                          results_last = list(remove_all_bad_results_last))
    
    # Add to results reactive object
    isolate(rv_results$run <- rbind(rv_results$run, best_result))
    
    # Start our policy id tracker
    rv_policy_id$id <- best_result$id
    
  })
  
  ### Reactive object that keeps track of user custom input policy selections -----
  rv_custom_policy <- reactiveValues(
    
    name = character(),
    iuu = list(),
    oa = list(),
    overcap = list(),
    cap_tier = list()
    
  )
  
  # Reactive object that keeps track of the currently selected policy -------
  rv_selected_result <- reactiveValues(id = "A")

  
  # observeEvent(c(event_data("plotly_selected", source = "kobe_plot"),
  #                event_data("plotly_deselect", source = "kobe_plot"),
  #                event_data("plotly_click", source = "kobe_plot")), {
  #                  
  #                  if(length(event_data("plotly_selected", source = "kobe_plot")) > 0){
  #                    
  #                    if(event_data("plotly_selected", source = "kobe_plot")$key[[1]] != rdf_selected_run$id){
  #                      
  #                      isolate(rdf_selected_run$id <- event_data("plotly_selected", source = "kobe_plot")$key[[1]])
  #                      
  #                    }
  #                    
  #                  }else if(length(event_data("plotly_selected", source = "kobe_plot")) == 0){
  #                    
  #                    isolate(rdf_selected_run$id <- "0")
  #                    
  #                  }
  #                  
  #                })
  
  
  ### ----------------------------
  ### 01. Introduction/Sidebar ---
  ### ----------------------------
  
  ### Navigation buttons on the intro page ----
  
  # Navigation button from introduction to selected-results
  observeEvent(input$ab_introduction_to_explore_results, {
    updateTabItems(session, "menu_items", "explore-results")
  })
  
  # Navigation button from introduction to methods-process
  observeEvent(input$ab_introduction_to_methods_process, {
    updateTabItems(session, "menu_items", "methods-process")
  })
  
  # Navigation button from introduction to global-subsidies
  observeEvent(input$ab_introduction_to_global_subsidies, {
    updateTabItems(session, "menu_items", "global-subsidies")
    updateTabItems(session, "subsidy-tabs", "global-subsidies-tab")
  })
  
  ### Navigation links in the left sidebar that we must specify manually for this to work ---
  
  # Navigation link to global-subsidies tab
  observeEvent(input$al_global_subsidies, {
    updateTabItems(session, "menu_items", "global-subsidies")
    updateTabItems(session, "subsidy-tabs", "global-subsidies-tab")
  })
  
  # Navigation link to country-fishery-stats-tab
  observeEvent(input$al_country_fishery_stats, {
    updateTabItems(session, "menu_items", "global-subsidies")
    updateTabItems(session, "subsidy-tabs", "country-fishery-stats-tab")
  })
  
  # Navigation link to compare-fishery-stats tab
  observeEvent(input$al_compare_fishery_stats, {
    updateTabItems(session, "menu_items", "global-subsidies")
    updateTabItems(session, "subsidy-tabs", "compare-fishery-stats-tab")
  })
  
  # Navigation link to global-fishing-footprint tab
  observeEvent(input$al_global_fishing_footprint, {
    updateTabItems(session, "menu_items", "global-subsidies")
    updateTabItems(session, "subsidy-tabs", "global-fishing-footprint-tab")
  })
  
  ### -------------------------
  ### 02a. selected-results ---
  ### -------------------------
  
  ### Navigation buttons ---------------------
  
  # Navigation button from selected-results to edit-policies
  observeEvent(input$ab_selected_results_to_edit_policies, {
    updateTabItems(session, "menu_items", "edit-policies")
  })
  
  # Navigation button from selected-results to introduction
  observeEvent(input$ab_selected_results_to_introduction, {
    updateTabItems(session, "menu_items", "introduction")
  })
  
  
  ### Background happenings: Get results for selected proposal -----------------------
  
  observeEvent(input$ab_run_model_proposal, {
    
    # Require policy selection
    req(input$w_selected_results_proposal_selection != "Default")
    
    # Get data for selected proposal
    selected_proposal <- proposal_settings %>%
      dplyr::filter(proposal == input$w_selected_results_proposal_selection)
    
    # Create run name
    run_name <- selected_proposal$proposal
    
    if(run_name %in% rv_results$run$name){
      
      # Find row cooresponding to that run
      run_id <- rv_results$run$id[rv_results$run$name == run_name]
      
      # Update selection tracker so new run is selected
      isolate(rv_selected_result$id <- run_id)
      
    }else{
      
      # Create run id 
      last_run_id <- which(LETTERS == rv_policy_id$id)
      new_run_id <- LETTERS[last_run_id + 1]
      
      # Update selection tracker so new run is selected
      isolate(rv_selected_result$id <- new_run_id)
      
    # Progress bar
    withProgress(message = 'Processing selection - please wait', value = 0.01, {
      
      ### Step 1: Policy selections ---
    
      # IUU
      iuu <- 
        list("definitions" = unlist(str_split(selected_proposal$iuu_definitions, ", ")),
             "assumption" = selected_proposal$iuu_assumption,
             "percent" = selected_proposal$iuu_percent,
             "scope" = selected_proposal$iuu_scope,
             "scope_select" = unlist(str_split(selected_proposal$iuu_scope_select, ", ")),
             "scope_manual" = unlist(str_split(selected_proposal$iuu_scope_manual, ", ")),
             "allow_sdt" = selected_proposal$iuu_allow_sdt,
             "sdt_ldc" = selected_proposal$iuu_sdt_ldc,
             "sdt_what_ldc" = unlist(str_split(selected_proposal$iuu_sdt_what_ldc, ", ")),
             "sdt_time_delay_ldc" = selected_proposal$iuu_sdt_time_delay_ldc,
             "sdt_developing" = selected_proposal$iuu_sdt_developing,
             "sdt_what_developing" = unlist(str_split(selected_proposal$iuu_sdt_what_developing, ", ")),
             "sdt_time_delay_developing" = selected_proposal$iuu_sdt_time_delay_developing,
             "sdt_sve" = selected_proposal$iuu_sdt_sve,
             "sdt_what_sve" = unlist(str_split(selected_proposal$iuu_sdt_what_sve, ", ")),
             "sdt_time_delay_sve" = selected_proposal$iuu_sdt_time_delay_sve)
    
      # OA
      oa <- 
        list("definitions" = unlist(str_split(selected_proposal$oa_definitions, ", ")),
             "scope" = selected_proposal$oa_scope,
             "scope_select" = unlist(str_split(selected_proposal$oa_scope_select, ", ")),
             "scope_manual" = unlist(str_split(selected_proposal$oa_scope_manual, ", ")),
             "hs_cutoff" = selected_proposal$oa_hs_cutoff,
             "length_cutoff" = selected_proposal$oa_length_cutoff,
             "tonnage_cutoff" = selected_proposal$oa_tonnage_cutoff,
             "engine_cutoff" = selected_proposal$oa_engine_cutoff,
             "allow_sdt" = selected_proposal$oa_allow_sdt,
             "sdt_ldc" = selected_proposal$oa_sdt_ldc,
             "sdt_what_ldc" = unlist(str_split(selected_proposal$oa_sdt_what_ldc, ", ")),
             "sdt_hs_cutoff_ldc" = selected_proposal$oa_sdt_hs_cutoff_ldc,
             "sdt_time_delay_ldc" = selected_proposal$oa_sdt_time_delay_ldc,
             "sdt_developing" = selected_proposal$oa_sdt_developing,
             "sdt_what_developing" = unlist(str_split(selected_proposal$oa_sdt_what_developing, ", ")),
             "sdt_hs_cutoff_developing" = selected_proposal$oa_sdt_hs_cutoff_developing,
             "sdt_time_delay_developing" = selected_proposal$oa_sdt_time_delay_developing,
             "sdt_sve" = selected_proposal$oa_sdt_sve,
             "sdt_what_sve" = unlist(str_split(selected_proposal$oa_sdt_what_sve, ", ")),
             "sdt_hs_cutoff_sve" = selected_proposal$oa_sdt_hs_cutoff_sve,
             "sdt_time_delay_sve" = selected_proposal$oa_sdt_time_delay_sve)
    
      # Overcap
      overcap <- 
        list("definitions" = unlist(str_split(selected_proposal$overcap_definitions, ", ")),
             "scope" = selected_proposal$overcap_scope,
             "scope_select" = unlist(str_split(selected_proposal$overcap_scope_select, ", ")),
             "scope_manual" = unlist(str_split(selected_proposal$overcap_scope_manual, ", ")),
             "hs_cutoff" = selected_proposal$overcap_hs_cutoff,
             "length_cutoff" = selected_proposal$overcap_length_cutoff,
             "tonnage_cutoff" = selected_proposal$overcap_tonnage_cutoff,
             "engine_cutoff" = selected_proposal$overcap_engine_cutoff,
             "allow_sdt" = selected_proposal$overcap_allow_sdt,
             "sdt_ldc" = selected_proposal$overcap_sdt_ldc,
             "sdt_what_ldc" = unlist(str_split(selected_proposal$overcap_sdt_what_ldc, ", ")),
             "sdt_hs_cutoff_ldc" = selected_proposal$overcap_sdt_hs_cutoff_ldc,
             "sdt_time_delay_ldc" = selected_proposal$overcap_sdt_time_delay_ldc,
             "sdt_developing" = selected_proposal$overcap_sdt_developing,
             "sdt_what_developing" = unlist(str_split(selected_proposal$overcap_sdt_what_developing, ", ")),
             "sdt_hs_cutoff_developing" = selected_proposal$overcap_sdt_hs_cutoff_developing,
             "sdt_time_delay_developing" = selected_proposal$overcap_sdt_time_delay_developing,
             "sdt_sve" = selected_proposal$overcap_sdt_sve,
             "sdt_what_sve" = unlist(str_split(selected_proposal$overcap_sdt_what_sve, ", ")),
             "sdt_hs_cutoff_sve" = selected_proposal$overcap_sdt_hs_cutoff_sve,
             "sdt_time_delay_sve" = selected_proposal$overcap_sdt_time_delay_sve)
    
      # Cap/Tier
      cap_tier = 
        list("on_off" = selected_proposal$cap_on_off,
           "subsidy_types" = unlist(str_split(selected_proposal$cap_subsidy_types, ", ")),
           "tier_number" = selected_proposal$cap_tier_number,
           "tier_system" = selected_proposal$tier_system,
           "two_tier_cutoff" = selected_proposal$two_tier_cutoff,
           "three_tier_cutoff" = as.numeric(unlist(str_split(selected_proposal$three_tier_cutoff, ", "))),
           "tier1_cap_rule" = selected_proposal$tier1_cap_rule,
           "tier2_cap_rule" = selected_proposal$tier2_cap_rule,
           "tier3_cap_rule" = selected_proposal$tier3_cap_rule,
           "tier1_cap_value" = selected_proposal$tier1_cap_value,
           "tier1_cap_fishers" = selected_proposal$tier1_cap_fishers,
           "tier1_cap_percent" = selected_proposal$tier1_cap_percent,
           "tier2_cap_value" = selected_proposal$tier2_cap_value,
           "tier2_cap_fishers" = selected_proposal$tier1_cap_fishers,
           "tier2_cap_percent" = selected_proposal$tier2_cap_percent,
           "tier3_cap_value" = selected_proposal$tier3_cap_value,
           "tier3_cap_fishers" = selected_proposal$tier3_cap_fishers,
           "tier3_cap_percent" = selected_proposal$tier3_cap_percent)
      
      # Policy summary
      policy_summary <- paste0(
        "<b>", "Name: ", "</b>", selected_proposal$title, "</br>",
        "<b>", "Summary: ", "</b>", selected_proposal$summary, "</br>")
      
      # Advance progress tracker
      incProgress(0.25)
    
      ### Find fleets ---
      fleet <-  CreateFleets(
        vessel_list = vessel_dat,
        iuu = iuu,
        oa = oa,
        overcap = overcap,
        cap_tier = cap_tier,
        managed_threshold = managed_cutoff,
        subsidy_types_all = subsidy_types_sorted_sumaila,
        cap_tier_lookup = cap_tier_lookup_table,
        country_lookup = country_lookup)
    
      # Create list by region
      fleet_list <- fleet$summary %>%
        group_by(region) %>%
        group_split()
      names(fleet_list) <- colnames(bio_dat)[-c(1:2)]
    
      # Advance progress tracker
      incProgress(0.75)
    
      ### Run Model ---
      out <- pmap_df(list(fleet = fleet_list, 
                          region = names(fleet_list),
                          bio_param = bio_dat_list),
                     BioEconModel,
                     end_year = 2100,
                     return = "all")
    
      # Store time series results both globally and regionally
      out_all <- out %>%
        dplyr::filter(Year > 2018) %>%
        dplyr::filter(Variable %in% c("biomass", "catches_total", "revenue_total")) %>%
        mutate(Diff = case_when(BAU != 0 ~ (Reform - BAU)/abs(BAU),
                                TRUE ~ 0)) %>%
        group_by(Year, Variable, Fleet) %>%
        mutate(BAU_global = sum(BAU, na.rm = T),
               Reform_global = sum(Reform, na.rm = T),
               Diff_global = case_when(BAU_global != 0 ~ (Reform_global - BAU_global)/abs(BAU_global),
                                       TRUE ~ 0)) %>%
        ungroup() %>%
        mutate(Id = new_run_id,
               Name = run_name,
               Type = "Proposal",
               Description = selected_proposal$title_tool)
    
    # Extract global difference in the last time step
    out_last <- out_all %>%
      dplyr::filter(Year == 2060) %>%
      group_by(Year, Variable, Id, Name, Type, Description) %>%
      summarize(Value = unique(Diff_global)*100) %>%
      ungroup() %>%
      spread(Variable, Value) %>%
      rename(Biomass = biomass,
             Catches = catches_total,
             Revenue = revenue_total)
    
    # Fill in new tibble row 
    new_result <- tibble(id = new_run_id,
                         name = run_name,
                         iuu = list(iuu),
                         oa = list(oa),
                         overcap = list(overcap),
                         cap_tier = list(cap_tier),
                         policy_description = list(policy_summary),
                         fleet_summary = list(remove_all_bad_fleet_summary),
                         results_timeseries = list(out_all),
                         results_last = list(out_last))
    
    # Add to results reactive object
    isolate(rv_results$run <- rbind(rv_results$run, new_result))
    
    # Update reactive policy id tracker
    rv_policy_id$id <- new_run_id
    
    # Update progress tracker
    incProgress(0.95)
    
    }) # close progress
    
    } # close result
    
  })
  
  ### Reactive text: Get description for selected proposal --------
  output$selected_results_selected_policy_description <- renderUI({
    
    # Make sure something is selected
    req(rv_selected_result$id != "0")
      
    # Filter for selected proposal
    selected_results <- rv_results$run %>%
      dplyr::filter(id == rv_selected_result$id)
    
    # Turn into HTML
    unlist(selected_results$policy_description) %>%
      lapply(htmltools::HTML)
    
  })
  
  
  ### Plotly figure: Model results over time ---------------------
  
  output$model_results_timeseries_plot <- renderPlotly({
    
    req(nrow(bind_rows(rv_results$run$results_timeseries)) > 0)
    req(input$w_selected_results_timeseries_plot_resolution)
    req(input$w_selected_results_timeseries_plot_variable)
    
    # Collect data 
    dat <- bind_rows(rv_results$run$results_timeseries)
    
    # Global 
    if(input$w_selected_results_timeseries_plot_resolution == "global"){
      
      plot_dat <- dat %>%
        group_by(Id, Name, Type, Description, Year, Variable, Fleet) %>%
        summarize(BAU = unique(BAU_global),
                  Reform = unique(Reform_global),
                  Diff = unique(Diff_global)) %>%
        ungroup() %>%
        mutate(Region = "Global")
      
    # Regional
    }else if(input$w_selected_results_timeseries_plot_resolution == "regional"){
      
      plot_dat <- dat %>%
        dplyr::select(Id, Name, Type, Description, Year, Variable, Fleet, Region, BAU, Reform, Diff) %>%
        mutate(Region = case_when(Region == "atlantic" ~ "Atlantic Ocean",
                                  Region == "indian" ~ "Indian Ocean",
                                  Region == "pacific" ~ "Pacific Ocean"))
      
    }
    
    # Determine variable for plotting
    plot_variable <- switch(input$w_selected_results_timeseries_plot_variable,
                            "biomass" = list("biomass", "Change in biomass (%)"),
                            
                            "catches_total" = list("catches_total", "Change in catch (%)"),
                            
                            "revenue_total" = list("revenue_total", "Change in revenue (%)"))
    
    # Make biomass plot
    out_plot_dat <- plot_dat %>%
      dplyr::filter(Variable == plot_variable[[1]])
    
    if(rv_policy_id$id == "A"){

      plot <-  ggplot()+
        aes(x = Year, y = Diff*100)+
        geom_line(data = out_plot_dat, size = 1, color = "grey",
                  aes(key = Id,
                      group = Id,
                      text = paste0("<b>","Year: ","</b>", Year,
                                    "<br>",
                                    "<b>","Policy Name: ","</b>", Name,
                                    "<br>",
                                    "<b>","Description: ","</b>", Description,
                                    "<br>",
                                    "<b>","Policy Type: ","</b>", Type,
                                    "<br>",
                                    "<b>","Region: ", "</b>", Region,
                                    "<br>",
                                    "<b>", plot_variable[[2]], ": ","</b>", round(Diff*100, 2))))+
        theme_bw()+
        geom_hline(yintercept = 0)+
        #scale_color_manual(values = customDiscrete)+
        scale_x_continuous(expand = c(0,0))+
        labs(x = "Year", y = plot_variable[[2]])+
        theme(legend.position = "none")+
        facet_wrap(~Region)

    }else {
      
      plot <-  ggplot()+
        aes(x = Year, y = Diff*100, group = Id)+
        geom_line(data = out_plot_dat, size = 1, color = "grey",
                  aes(key = Id,
                      text = paste0("<b>","Year: ","</b>", Year,
                                    "<br>",
                                    "<b>","Policy Name: ","</b>", Name,
                                    "<br>",
                                    "<b>","Description: ","</b>", Description,
                                    "<br>",
                                    "<b>","Policy Type: ","</b>", Type,
                                    "<br>",
                                    "<b>","Region: ", "</b>", Region,
                                    "<br>",
                                    "<b>", plot_variable[[2]], ": ","</b>", round(Diff*100, 2))))+
        theme_bw()+
        geom_line(data = out_plot_dat %>% dplyr::filter(Id == rv_selected_result$id), size = 2, color = "#3c8dbc",
                  aes(text = paste0("<b>","Year: ","</b>", Year,
                                    "<br>",
                                    "<b>","Policy Name: ","</b>", Name,
                                    "<br>",
                                    "<b>","Description: ","</b>", Description,
                                    "<br>",
                                    "<b>","Policy Type: ","</b>", Type,
                                    "<br>",
                                    "<b>","Region: ", "</b>", Region,
                                    "<br>",
                                    "<b>", plot_variable[[2]], ": ","</b>", round(Diff*100, 2))))+
        geom_hline(yintercept = 0)+
        #scale_color_manual(values = customDiscrete)+
        scale_x_continuous(expand = c(0,0))+
        labs(x = "Year", y = plot_variable[[2]])+
        theme(legend.position = "none")+
        facet_wrap(~Region)
      
    }
    
    # Convert to plotly
    gg2 <- ggplotly(plot, tooltip = "text") %>%
      hide_legend() 
    
    
    # Return
    gg2
    
  })
  
  
  ### Update proposal selection widget ---------------------
  observe({
    
    # Only allow proposals from the selected category to be chosen
    allowable_policies <- included_proposals %>%
      dplyr::filter(category %in% input$w_selected_results_proposal_category | proposal == "Default")
    
    updated_proposal_choices <- allowable_policies$proposal
    names(updated_proposal_choices) <- allowable_policies$display_name
    
    # Update input
    updateSelectizeInput(session, 
                         "w_selected_results_proposal_selection",
                         choices = updated_proposal_choices,
                         selected = "Default")
  })
  
  
  ### Reactive text: Get description for selected proposal --------
  observe({

    # Want to observe proposal selection
    req(input$w_selected_results_proposal_selection)

    # Get selected entry
    selected_policy <- proposal_settings %>%
      dplyr::filter(proposal == input$w_selected_results_proposal_selection)

    # Create reactive text
    output$selected_results_proposal_selection_text <- renderUI({

      req(input$w_selected_results_proposal_selection != "Default")

      paste0("<b>", "Formal Title: ", "</b>", selected_policy$title, "</br>",
             "<b>", "Summary: ", "</b>", selected_policy$summary, "</br>",
             "<b>", "Modeling assumptions: ", "</b>", selected_policy$model_details_assumptions) %>%
        lapply(htmltools::HTML)

    })
    
  })
  
  ### ----------------------
  ### 02b. edit-policies ---
  ### ----------------------
  
  ### Navigation buttons ---------------------
  
  ### Tabs
  
  # Navigation button from tab 1 to tab 0
  observeEvent(input$ab_edit_policies_tabs_iuu_to_instructions, {
    updateTabsetPanel(session, "policy_tabs", "instructions") 
  })
  
  # Navigation button from tab 1 to tab 2
  observeEvent(input$ab_edit_policies_tabs_iuu_to_oa, {
    updateTabsetPanel(session, "policy_tabs", "oa") 
  })
  
  # Navigation button from tab 2 to tab 1
  observeEvent(input$ab_edit_policies_tabs_oa_to_iuu, {
    updateTabsetPanel(session, "policy_tabs", "iuu") 
  })
  
  # Navigation button from tab 2 to tab 3
  observeEvent(input$ab_edit_policies_tabs_oa_to_overcap, {
    updateTabsetPanel(session, "policy_tabs", "overcap") 
  })
  
  # Navigation button from tab 3 to tab 2
  observeEvent(input$ab_edit_policies_tabs_overcap_to_oa, {
    updateTabsetPanel(session, "policy_tabs", "overcap") 
  })
  
  # Navigation button from tab 3 to tab 4
  observeEvent(input$ab_edit_policies_tabs_overcap_to_other, {
    updateTabsetPanel(session, "policy_tabs", "other") 
  })
  
  ### Text Output: IUU data warning -------------------
  output$iuu_warning <- renderText({
    
    if("IUU2" %in% input$w_iuu_definitions | "IUU3" %in% input$w_iuu_definitions | "IUU4" %in% input$w_iuu_definitions | "IUU5" %in% input$w_iuu_definitions | "IUU6" %in% input$w_iuu_definitions){
      "Note: At present, no data exists on a global scale to identify vessels listed as having engaged in IUU fishing activities by coastal, flag, subsidizing Member, port, or market states."
    }else{
      ""
    }
  })
  
  ### Update when any custom widget changes ----------
  
  observe({
    
    rv_custom_policy$name <- input$w_run_name
    
    rv_custom_policy$iuu <- list("definitions" = input$w_iuu_definitions,
                                 "assumption" = input$w_iuu_assumption,
                                 "percent" = input$w_iuu_percent,
                                 "scope" = input$w_iuu_scope,
                                 "scope_select" = input$w_iuu_scope_select,
                                 "scope_manual" = input$w_iuu_scope_manual,
                                 "allow_sdt" = input$w_iuu_allow_sdt,
                                 "sdt_ldc" = input$w_iuu_sdt_ldc,
                                 "sdt_what_ldc" = input$w_iuu_sdt_what_ldc,
                                 "sdt_time_delay_ldc" = input$w_iuu_sdt_time_delay_ldc,
                                 "sdt_developing" = input$w_iuu_sdt_developing,
                                 "sdt_what_developing" = input$w_iuu_sdt_what_developing,
                                 "sdt_time_delay_developing" = input$w_iuu_sdt_time_delay_developing,
                                 "sdt_sve" = input$w_iuu_sdt_sve,
                                 "sdt_what_sve" = input$w_iuu_sdt_what_sve,
                                 "sdt_time_delay_sve" = input$w_iuu_sdt_time_delay_sve)
    
    rv_custom_policy$oa <- list("definitions" = input$w_oa_definitions,
                                "scope" = input$w_oa_scope,
                                "scope_select" = input$w_oa_scope_select,
                                "scope_manual" = input$w_oa_scope_manual,
                                "hs_cutoff" = input$w_oa_hs_cutoff,
                                "length_cutoff" = input$w_oa_length_cutoff,
                                "tonnage_cutoff" = input$w_oa_tonnage_cutoff,
                                "engine_cutoff" = input$w_oa_engine_cutoff,
                                "allow_sdt" = input$w_oa_allow_sdt,
                                "sdt_ldc" = input$w_oa_sdt_ldc,
                                "sdt_what_ldc" = input$w_oa_sdt_what_ldc,
                                "sdt_hs_cutoff_ldc" = input$w_oa_sdt_hs_cutoff_ldc,
                                "sdt_time_delay_ldc" = input$w_oa_sdt_time_delay_ldc,
                                "sdt_developing" = input$w_oa_sdt_developing,
                                "sdt_what_developing" = input$w_oa_sdt_what_developing,
                                "sdt_hs_cutoff_developing" = input$w_oa_sdt_hs_cutoff_developing,
                                "sdt_time_delay_developing" = input$w_oa_sdt_time_delay_developing,
                                "sdt_sve" = input$w_oa_sdt_sve,
                                "sdt_what_sve" = input$w_oa_sdt_what_sve,
                                "sdt_hs_cutoff_sve" = input$w_oa_sdt_hs_cutoff_sve,
                                "sdt_time_delay_sve" = input$w_oa_sdt_time_delay_sve)
    
    rv_custom_policy$overcap <- list("definitions" = input$w_overcap_definitions,
                                     "scope" = input$w_overcap_scope,
                                     "scope_select" = input$w_overcap_scope_select,
                                     "scope_manual" = input$w_overcap_scope_manual,
                                     "hs_cutoff" = input$w_overcap_hs_cutoff,
                                     "length_cutoff" = input$w_overcap_length_cutoff,
                                     "tonnage_cutoff" = input$w_overcap_tonnage_cutoff,
                                     "engine_cutoff" = input$w_overcap_engine_cutoff,
                                     "allow_sdt" = input$w_overcap_allow_sdt,
                                     "sdt_ldc" = input$w_overcap_sdt_ldc,
                                     "sdt_what_ldc" = input$w_overcap_sdt_what_ldc,
                                     "sdt_hs_cutoff_ldc" = input$w_overcap_sdt_hs_cutoff_ldc,
                                     "sdt_time_delay_ldc" = input$w_overcap_sdt_time_delay_ldc,
                                     "sdt_developing" = input$w_overcap_sdt_developing,
                                     "sdt_what_developing" = input$w_overcap_sdt_what_developing,
                                     "sdt_hs_cutoff_developing" = input$w_overcap_sdt_hs_cutoff_developing,
                                     "sdt_time_delay_developing" = input$w_overcap_sdt_time_delay_developing,
                                     "sdt_sve" = input$w_overcap_sdt_sve,
                                     "sdt_what_sve" = input$w_overcap_sdt_what_sve,
                                     "sdt_hs_cutoff_sve" = input$w_overcap_sdt_hs_cutoff_sve,
                                     "sdt_time_delay_sve" = input$w_overcap_sdt_time_delay_sve)
    
    rv_custom_policy$cap_tier <- list("on_off" = input$w_cap_on_off,
                                      "subsidy_types" = input$w_cap_subsidy_types,
                                      "tier_number" = input$w_cap_tier_number,
                                      "tier_system" = input$w_tier_system,
                                      "two_tier_cutoff" = input$w_two_tier_cutoff,
                                      "three_tier_cutoff" = input$w_three_tier_cutoff,
                                      "tier1_cap_rule" = input$w_tier1_cap_rule,
                                      "tier2_cap_rule" = input$w_tier2_cap_rule,
                                      "tier3_cap_rule" = input$w_tier3_cap_rule,
                                      "tier1_cap_value" = input$w_tier1_cap_value,
                                      "tier1_cap_fishers" = input$w_tier1_cap_fishers,
                                      "tier1_cap_percent" = input$w_tier1_cap_percent,
                                      "tier2_cap_value" = input$w_tier2_cap_value,
                                      "tier2_cap_fishers" = input$w_tier1_cap_fishers,
                                      "tier2_cap_percent" = input$w_tier2_cap_percent,
                                      "tier3_cap_value" = input$w_tier3_cap_value,
                                      "tier3_cap_fishers" = input$w_tier3_cap_fishers,
                                      "tier3_cap_percent" = input$w_tier3_cap_percent)
    
  })
  
  ### Ui Output: Custom policy description container ------------------
  rv_custom_policy_description <- reactiveValues()
  
  ### Ui Output: Custom policy description ------------------
  observe({
    
    rv_custom_policy_description$name <- paste0(
      "<b class='big'>", "Name: ", "</b>", "<small>", rv_custom_policy$name, "</br></small>")
    
    rv_custom_policy_description$iuu_summary <- paste0(
      IUUSummaryText(iuu = rv_custom_policy$iuu,
                     text = text,
                     wid = wid,
                     country_choices = wto_members_and_observers), "</br>")
    
    rv_custom_policy_description$oa_summary <- paste0(
      OASummaryText(oa = rv_custom_policy$oa,
                    text = text,
                    wid = wid,
                    country_choices = wto_members_and_observers), "</br>")
    
    rv_custom_policy_description$overcap_summary <- paste0(
      OvercapSummaryText(overcap = rv_custom_policy$overcap,
                         cap_tier = rv_custom_policy$cap_tier,
                         text = text,
                         wid = wid,
                         country_choices = wto_members_and_observers), "</br>")
    
    # Generate output 
    output$custom_policy <- renderUI({
      
      paste0(rv_custom_policy_description$name,
             rv_custom_policy_description$iuu_summary,
             rv_custom_policy_description$oa_summary,
             rv_custom_policy_description$overcap_summary) %>%
        lapply(htmltools::HTML)
    
    })
  
  })
  
  ### Render UI - Custom policy warning - missing name ------
  output$custom_name_warning <- renderUI({
    
    if(input$w_run_name == ""){
      
    paste0("<b style='color:red;'><i>", "Error: Please enter a name for your policy.", "</b></i></br>") %>% 
        lapply(htmltools::HTML)
      
    }else{
      ""
    }
  })
  
  ### Background happenings: Run custom policy ------
  
  observeEvent(input$ab_run_model_custom, {
    
    req(input$w_run_name != "")
    
    # Create run id
    last_run_id <- which(LETTERS == rv_policy_id$id)
    new_run_id <- LETTERS[last_run_id + 1]
    
    # Get run name
    run_name <- rv_custom_policy$name

    # Update selection tracker so new run is selected
    isolate(rv_selected_result$id <- new_run_id)

    # Progress bar
    withProgress(message = 'Processing selection - please wait', value = 0.01, {

      ### Step 1: Policy selections ---

      # IUU
      iuu <- rv_custom_policy$iuu

      # OA
      oa <- rv_custom_policy$oa

      # Overcap
      overcap <- rv_custom_policy$overcap

      # Cap/Tier
      cap_tier = rv_custom_policy$cap_tier

      incProgress(0.25)

      ### Find fleets ---
      fleet <-  CreateFleets(
        vessel_list = vessel_dat,
        iuu = iuu,
        oa = oa,
        overcap = overcap,
        cap_tier = cap_tier,
        managed_threshold = managed_cutoff,
        subsidy_types_all = subsidy_types_sorted_sumaila,
        cap_tier_lookup = cap_tier_lookup_table,
        country_lookup = country_lookup)
      
      # Make list
      fleet_list <- fleet$summary %>%
        group_by(region) %>%
        group_split()
      names(fleet_list) <- colnames(bio_dat)[-c(1:2)]
      
      
      incProgress(0.75)

      ### Run Model ---
      out <- pmap_df(list(fleet = fleet_list,
                          region = names(fleet_list),
                          bio_param = bio_dat_list),
                     BioEconModel,
                     end_year = 2100,
                     return = "all")
      

      # Store time series results both globally and regionally
      out_all <- out %>%
        dplyr::filter(Year > 2018) %>%
        dplyr::filter(Variable %in% c("biomass", "catches_total", "revenue_total")) %>%
        group_by(Year, Variable, Fleet) %>%
        mutate(Diff = case_when(BAU != 0 ~ (Reform - BAU)/abs(BAU),
                                TRUE ~ 0),
               BAU_global = sum(BAU, na.rm = T),
               Reform_global = sum(Reform, na.rm = T),
               Diff_global = case_when(BAU_global != 0 ~ (Reform_global - BAU_global)/abs(BAU_global),
                                       TRUE ~ 0)) %>%
        ungroup() %>%
        mutate(Id = new_run_id,
               Name = run_name,
               Type = "Custom",
               Description = "Custom policy")

      # Just extract global difference in the last time step
      out_last <- out_all %>%
        dplyr::filter(Year == end_year) %>%
        group_by(Year, Variable, Id, Name, Type, Description) %>%
        summarize(Value = unique(Diff_global)*100) %>%
        ungroup() %>%
        spread(Variable, Value) %>%
        rename(Biomass = biomass,
               Catches = catches_total,
               Revenue = revenue_total)

      # Create description 
      policy_description <- paste0(rv_custom_policy_description$name,
                                   rv_custom_policy_description$iuu_summary,
                                   rv_custom_policy_description$oa_summary,
                                   rv_custom_policy_description$overcap_summary)

      # Fill in new tibble row
      new_result <- tibble(id = new_run_id,
                           name = run_name,
                           iuu = list(iuu),
                           oa = list(oa),
                           overcap = list(overcap),
                           cap_tier = list(cap_tier),
                           policy_description = list(policy_description),
                           fleet_summary = list(remove_all_bad_fleet_summary),
                           results_timeseries = list(out_all),
                           results_last = list(out_last))

      # Add to results reactive object
      isolate(rv_results$run <- rbind(rv_results$run, new_result))

      # Update policy id tracker 
      rv_policy_id$id <- new_run_id
      
      # Advance progress marker
      incProgress(0.95)

    }) # close progress
    
    # Switch back to the results plot
    updateTabItems(session, "menu_items", "selected-results")

  })
  

  ### -----------------------
  ### 03. methods-process ---
  ### -----------------------
  
  ### Downlaod buttons ---------------------
  
  # Download methods PDF
  output$db_download_methods <- downloadHandler(
    filename = "SubsidyExplorer_methods.pdf",
    content = function(file) {
      file.copy("www/SubsidyExplorer_methods.pdf", file)
    }
  )
  
  ### -------------------------
  ### 04a. global-subsidies ---
  ### -------------------------
  
  ### Info button: Subsidy types --------------
  observeEvent(input$info_global_subsidies_subsidy_types, {
                   
                   shinyalert(title = text$item_label[text$item_id == "subsidy-types-to-include"],
                              text = "Text goes here",
                              size = "l",
                              closeOnEsc = TRUE,
                              closeOnClickOutside = TRUE,
                              html = TRUE,
                              type = "",
                              showConfirmButton = TRUE,
                              showCancelButton = FALSE,
                              confirmButtonText = "OK",
                              confirmButtonCol = "#0d5ba2",
                              timer = 0,
                              animation = TRUE)
                   
                 })
  
  ### Info button: Global subsidy map --------------
  observeEvent(input$info_global_subsidies_map, {
                    
                    shinyalert(title = text$item_label[text$item_id == "global-subsidies"],
                               text = "Text goes here",
                               size = "l",
                               closeOnEsc = TRUE,
                               closeOnClickOutside = TRUE,
                               html = TRUE,
                               type = "",
                               showConfirmButton = TRUE,
                               showCancelButton = FALSE,
                               confirmButtonText = "OK",
                               confirmButtonCol = "#0d5ba2",
                               timer = 0,
                               animation = TRUE)
                    
                  })
  
  ### Update checkboxGroupInputs: Select all --------------------
  observeEvent(input$ab_global_subsidies_select_all, {
    
    updateCheckboxGroupInput(session,
                             "w_global_subsidies_good_types",
                             selected = subsidy_types_sorted_sumaila[1:3])
    
    updateCheckboxGroupInput(session,
                             "w_global_subsidies_ugly_types",
                             selected = subsidy_types_sorted_sumaila[11:13])
    
    updateCheckboxGroupInput(session,
                             "w_global_subsidies_bad_types",
                             selected = subsidy_types_sorted_sumaila[4:10])
    
  })
  
  ### Update checkboxGroupInputs: Clear all
  observeEvent(input$ab_global_subsidies_clear_all, {
    
    x <- character(0)
    
    updateCheckboxGroupInput(session,
                             "w_global_subsidies_good_types",
                             selected = x)
    
    updateCheckboxGroupInput(session,
                             "w_global_subsidies_ugly_types",
                             selected = x)
    
    updateCheckboxGroupInput(session,
                             "w_global_subsidies_bad_types",
                             selected = x)
    
  })
  
  ### Leaflet map: Global map of fisheries subsidies with hover boxes ---------------------
  output$global_subsidies_map <- renderLeaflet({
    
    req(input$w_global_subsidies_good_types)
    req(input$w_global_subsidies_ugly_types)
    req(input$w_global_subsidies_bad_types)
    
    selected_subsidy_types <- c(input$w_global_subsidies_good_types, input$w_global_subsidies_ugly_types, input$w_global_subsidies_bad_types)
    
    req(length(selected_subsidy_types) > 0)
    
    # Define colors
    global_subsidies_map_pal <- colorNumeric(palette = "YlOrRd",
                                             log10(c(100, 10e9)))
    
    
    # Filter data
    global_subsidies_map_dat <- subsidy_dat %>%
      dplyr::filter(variable == "subsidies_Sumaila") %>%
      dplyr::filter(type %in% c(selected_subsidy_types)) %>%
      dplyr::filter(!is.na(value) & value > 0) %>%
      group_by(iso3, display_name, category, category_name, type, type_name) %>%
      summarize(value = sum(value, na.rm = T)) %>%
      group_by(iso3, display_name) %>%
      mutate(included_types = paste0(type_name[type_name != "Total"], collapse = ";</br>")) %>%
      ungroup() %>%
      group_by(iso3, display_name, category, category_name, included_types) %>%
      summarize(value = sum(value, na.rm = T))
      
    # Join to world polygons
    global_subsidies_map_dat_shp <- world %>%
      dplyr::filter(!admin_iso3 %in% eu_countries) %>%
      left_join(global_subsidies_map_dat, by = c("admin_iso3" = "iso3")) %>%
      na.omit()
    
    # Hover text for world polygons
    global_subsidies_map_text_shp <- paste0(
      "<b>","State: ", "</b>",  global_subsidies_map_dat_shp$display_name,
      "</br>",
      "<b>", "Est. fisheries subsidies (2018 US$):", "</b>", " $", format(round(global_subsidies_map_dat_shp$value, 0), big.mark = ","),
      "</br>",
      "<b>", "Matching subsidy type(s): ", "</b>", global_subsidies_map_dat_shp$included_types
    ) %>%
      lapply(htmltools::HTML)
    
    # Join to points for small island nations
    global_subsidies_map_dat_points <- world_small_countries %>%
      dplyr::select(sov_iso3, admin_iso3, area_km, center) %>%
      left_join(global_subsidies_map_dat, by = c("admin_iso3" = "iso3")) %>%
      na.omit()
    st_geometry(global_subsidies_map_dat_points) <- global_subsidies_map_dat_points$center
    
    # Hover text for points
    global_subsidies_map_text_points <- paste0(
      "<b>","State: ", "</b>",  global_subsidies_map_dat_points$display_name,
      "</br>",
      "<b>", "Est. fisheries subsidies (2018 US$):", "</b>", " $", format(round(global_subsidies_map_dat_points$value, 0), big.mark = ","),
      "</br>",
      "<b>", "Matching subsidy type(s): ", "</b>", global_subsidies_map_dat_points$included_types
    ) %>%
      lapply(htmltools::HTML)
    
    # Map
    leaflet('global_subsidies_map', options = leafletOptions(minZoom = 3)) %>% 
      addProviderTiles("CartoDB.VoyagerNoLabels") %>% 
      addCircles(data = global_subsidies_map_dat_points,
                 color = ~global_subsidies_map_pal(log10(value)),
                 fillOpacity = 0.8,
                 stroke = "white",
                 weight = 2,
                 radius = 200000,
                 highlight = highlightOptions(weight = 5,
                                              color = "#666",
                                              fillOpacity = 1,
                                              bringToFront = FALSE),
                 label = global_subsidies_map_text_points,
                 labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                          padding = "3px 8px"),
                                             textsize = "13px",
                                             direction = "auto")) %>%
      addPolygons(data = global_subsidies_map_dat_shp, 
                  fillColor = ~global_subsidies_map_pal(log10(value)),
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = FALSE),
                  label = global_subsidies_map_text_shp,
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")) %>%
      setView(0,20, zoom = 3) %>%
      addLegend("bottomright", 
                pal = global_subsidies_map_pal,
                values = log10(c(100, 10e9)),
                labels = round(log10(c(100, 10e9)), 0),
                title = "Est. fisheries subsidies<br>(2018 US$)",
                opacity = 1,
                labFormat = labelFormat(prefix = "$",
                                        transform = function(x) 10^(x)
                )
      )
    
  })
  
  ### ------------------------------
  ### 03b. country-fishery-stats ---
  ### ------------------------------
  
  ### Info button: Countries and territories --------------
  observeEvent(input$info_country_fishery_stats_territories, {
                    
                    shinyalert(title = "WTO Members and Observers",
                               text = "Text goes here",
                               size = "l",
                               closeOnEsc = TRUE,
                               closeOnClickOutside = TRUE,
                               html = TRUE,
                               type = "",
                               showConfirmButton = TRUE,
                               showCancelButton = FALSE,
                               confirmButtonText = "OK",
                               confirmButtonCol = "#0d5ba2",
                               timer = 0,
                               animation = TRUE)
                    
                  })
  
  ### UI output: Name of selected country header ---------------------
  output$country_fishery_stats_selected_country_name <- renderUI({
    
    req(input$w_country_fishery_stats_selected_country)

    out <- names(wto_members_and_observers[wto_members_and_observers == input$w_country_fishery_stats_selected_country])
    
    tags$h3(out)
  })
  
  ### Plotly figure: Fisheries subsidies by type ---------------------
  output$country_fishery_stats_subsidies_plot <- renderPlotly({
    
    req(input$w_country_fishery_stats_selected_country)
    
    # Filter OECD data and add Sumaila data
    country_fishery_stats_subsidies_plot_dat <- subsidy_dat %>%
      dplyr::filter(iso3 == input$w_country_fishery_stats_selected_country)
    country_fishery_stats_subsidies_plot_dat$value[is.na(country_fishery_stats_subsidies_plot_dat$value)] <- 0
    
    req(nrow(country_fishery_stats_subsidies_plot_dat) > 0)
    
    # Make plot  
    country_fishery_stats_subsidies_plot <- ggplot()+
      geom_col(data = country_fishery_stats_subsidies_plot_dat, aes(x = source, y = value, fill = type_name, 
                                                        text = paste0("<b>","State: ","</b>", display_name,
                                                                      "<br>",
                                                                      "<b>","Type: ","</b>", type_name,
                                                                      "<br>",
                                                                      "<b>","Data source: ","</b>", source,
                                                                      "<br>",
                                                                      "<b>","Est. fisheries subsidies (US$):","</b>", format(round(value, 0), big.mark = ","),
                                                                      "<br>",
                                                                      "<b>", "Year: ", "</b>", year)))+
      scale_fill_manual(values = myColors[names(myColors) %in% country_fishery_stats_subsidies_plot_dat$type_name])+
      scale_y_continuous(expand = c(0,0), name = "Est. fisheries subsidies (US$)", 
                         labels = function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE))+
      geom_vline(xintercept = 0, size = 1)+
      coord_flip()+
      theme_bw()+
      labs(x = "")+
      theme(legend.title = element_blank(),
            legend.position = "none",
            axis.text.y = element_text(face = "bold", angle = 90))
    
    # Convert to plotly
    gg <- ggplotly(country_fishery_stats_subsidies_plot, tooltip="text")
    
    # Create legend
    leg <- list(font = list(size = 10, color = "#000"),
                x = 100,
                y = 0.9,
                yanchor = "top")
    
    # Add plotly legend
    gg <- gg %>%
      layout(legend = leg)
    
    # Return plot
    gg
    
  })
  
  ### Plotly figure: FAO Marine Capture Production ---------------------
  output$country_fishery_stats_production_plot <- renderPlotly({
    
    req(input$w_country_fishery_stats_selected_country)
    
    # Filter data
    country_fishery_stats_production_plot_dat <- capture_production_dat_fao %>%
      dplyr::filter(iso3 == input$w_country_fishery_stats_selected_country)
    
    req(nrow(country_fishery_stats_production_plot_dat) > 0)
    
    # Make plot
    country_fishery_stats_production_plot <- country_fishery_stats_production_plot_dat %>%
      ggplot()+
      aes(x = year, y = value/1000, fill = isscaap_group)+
      geom_area()+
      geom_area(aes(text = paste0("<b>","Year: ","</b>", year,
                                  "<br>",
                                  "<b>", "ISSCAAP group: ", "</b>", isscaap_group,
                                  "<br>",
                                  "<b>", "Capture production (tonnes): ", "</b>", format(round(value, 0), big.mark = ","),
                                  "<br>",
                                  "<b>", "% of annual total: ", "</b>", round(prop_annual_total *100, 2))))+
      scale_y_continuous(expand = c(0,0),
                         name = "Capture production (tonnes, thousands)", 
                         labels = function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE))+
      scale_x_continuous(expand = c(0,0))+
      theme_bw()+
      labs(x = "Year")+
      theme(legend.title = element_blank(),
            legend.position = "none")
    
    # Convert to plotly
    gg <- ggplotly(country_fishery_stats_production_plot, tooltip = "text") %>%
      style(hoveron = "points")
    
    # Create Legend
    leg <- list(font = list(size = 10, color = "#000"),
                x = 100,
                y = 0.9,
                yanchor = "top")
    
    # Plotly syntax to adjust hover spike lines
    gg <- gg %>%
      layout(xaxis = list(
        showspikes = TRUE,
        spikemode = "across",
        spikedash = "solid",
        spikesnap = 'compare',
        spikethickness = 1,
        hovermode = 'compare'),
        legend = leg)
    
    # Return plot
    gg
    
  })
  
  ### Plotly figure: Estimated landed value ---------------------
  output$country_fishery_stats_landed_value_plot <- renderPlotly({
    
    req(input$w_country_fishery_stats_selected_country)
    
    # Filter data
    country_fishery_stats_landed_value_plot_dat <- landed_value_dat_tot %>%
      dplyr::filter(iso3 == input$w_country_fishery_stats_selected_country)
    
    req(nrow(country_fishery_stats_landed_value_plot_dat) > 0)
    
    # Make plot
    country_fishery_stats_landed_value_plot <- country_fishery_stats_landed_value_plot_dat %>%
      ggplot()+
      aes(x = year, y = value/1e6)+
      geom_area(fill = '#3c8dbc')+
      geom_area(aes(text = paste0("<b>","Year: ","</b>", year,
                                  "<br>",
                                  "<b>", "Estimated landed value (US$): ", "</b>", "$", format(round(value, 0), big.mark = ","))))+
      scale_y_continuous(expand = c(0,0),
                         name = "Estimated landed value (US$, millions)", 
                         labels = function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE))+
      scale_x_continuous(expand = c(0,0))+
      theme_bw()+
      labs(x = "Year")+
      theme(legend.title = element_blank(),
            legend.position = "none")
    
    # Convert to plotly
    gg <- ggplotly(country_fishery_stats_landed_value_plot, tooltip = "text") %>%
      style(hoveron = "points")
    
    # Create Legend
    leg <- list(font = list(size = 10, color = "#000"),
                x = 100,
                y = 0.9,
                yanchor = "top")
    
    # Plotly syntax to adjust hover spike lines
    gg <- gg %>%
      layout(xaxis = list(
        showspikes = TRUE,
        spikemode = "across",
        spikedash = "solid",
        spikesnap = 'compare',
        spikethickness = 1,
        hovermode = 'compare'),
        legend = leg)
    
    # Return plot
    gg
    
  })
  
  ### Plotly figure: World Bank Population ---------------------
  output$country_fishery_stats_pop_plot <- renderPlotly({

    req(input$w_country_fishery_stats_selected_country)

    # Filter data
    country_fishery_stats_pop_plot_dat <- demographic_dat %>%
      dplyr::filter(iso3 == input$w_country_fishery_stats_selected_country) %>%
      dplyr::filter(variable %in% c("population"))
    
    req(nrow(country_fishery_stats_pop_plot_dat) > 0)
    req(all(is.na(country_fishery_stats_pop_plot_dat$value)) == F)

    # Make plot
    country_fishery_stats_pop_plot <- ggplot(country_fishery_stats_pop_plot_dat)+
      aes(x = year, y = value/1e6)+
      geom_line(color = '#3c8dbc')+
      geom_point(aes(text = paste0("<b>","Year: ","</b>", year,
                                          "<br>",
                                          "<b>", "Population: ", "</b>", format(round(value,0), big.mark = ",", scientific = F))),
                 alpha = 0, color = '#3c8dbc')+
      scale_y_continuous(name = "Population (persons, millions)",
                         labels = function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE))+
      scale_x_continuous(expand = c(0,0))+
      theme_bw()+
      labs(x = "Year")+
      theme(legend.title = element_blank(),
            legend.position = "none")

    # Convert to plotly
    gg <- ggplotly(country_fishery_stats_pop_plot, tooltip="text") %>%
      style(
        traces = 2,
        hoverlabel = list(bgcolor = "white")
      )

    gg <- gg %>%
      layout(xaxis = list(
        showspikes = TRUE,
        spikemode = "across",
        spikecolor = '#3c8dbc',
        spikedash = "solid",
        spikethickness = 1))

    # Plot object to return
    gg

  })
  
  ### Plotly figure: Fishers and fisheries employment ---------------------
  output$country_fishery_stats_fisher_plot <- renderPlotly({
    
    req(input$w_country_fishery_stats_selected_country)
    
    # Filter data
    country_fishery_stats_fisher_plot_dat <- demographic_dat %>%
      dplyr::filter(iso3 == input$w_country_fishery_stats_selected_country) %>%
      dplyr::filter(variable %in% c("fishers", "fishers_fte"))
    
    req(nrow(country_fishery_stats_fisher_plot_dat) > 0)
    req(all(is.na(country_fishery_stats_fisher_plot_dat$value)) == F)
    
    # Make dummy values for missing fishers data
    if(nrow(country_fishery_stats_fisher_plot_dat %>% dplyr::filter(variable == "fishers")) == 0){
      
      country_fishery_stats_fisher_plot_dat <- country_fishery_stats_fisher_plot_dat %>%
        bind_rows(
          tibble(
            iso3 = input$w_country_fishery_stats_selected_country,
            year = seq(2000, 2018, by = 1),
            variable = "fishers",
            value = NA,
            units = NA,
            source = NA
          )
        )
    }
    
    # Make dummy values for missing full-time-equivalent data
    if(nrow(country_fishery_stats_fisher_plot_dat %>% dplyr::filter(variable == "fishers_fte")) == 0){
      
      country_fishery_stats_fisher_plot_dat <- country_fishery_stats_fisher_plot_dat %>%
        bind_rows(
          tibble(
            iso3 = input$w_country_fishery_stats_selected_country,
            year = 2003,
            variable = "fishers_fte",
            value = NA,
            units = NA,
            source = NA
          )
        )
    }
    
    # Make plot
    country_fishery_stats_fisher_plot <- ggplot()+
      aes(x = year, y = value)+
      geom_line(data = country_fishery_stats_fisher_plot_dat %>% dplyr::filter(variable == "fishers"), color = 'navy')+
      geom_point(data = country_fishery_stats_fisher_plot_dat %>% dplyr::filter(variable == "fishers"),
                 aes(text = paste0("<b>","Year: ","</b>", year,
                                   "<br>",
                                   "<b>", "Fishers: ", "</b>", format(round(value,0), big.mark = ",", scientific = F))),
                 alpha = 0, color = 'navy')+
      geom_point(data = country_fishery_stats_fisher_plot_dat %>% dplyr::filter(variable == "fishers_fte"),
                 aes(text = paste0("<b>","Year: ","</b>", year,
                                   "<br>",
                                   "<b>", "Full-time-equivalent fisheries jobs: ", "</b>", format(round(value,0), big.mark = ",", scientific = F))),
                 alpha = 1, color = 'navy')+
      scale_y_continuous(name = "Fishers (persons)",
                         labels = function(x) format(x, big.mark = ",", scientific = FALSE))+
      scale_x_continuous(expand = c(0,0))+
      theme_bw()+
      labs(x = "Year")+
      theme(legend.title = element_blank(),
            legend.position = "none")
    
    # Convert to plotly
    gg <- ggplotly(country_fishery_stats_fisher_plot, tooltip="text")
    
    gg <- gg %>%
      layout(xaxis = list(
        showspikes = TRUE,
        spikemode = "across",
        spikecolor = '#3c8dbc',
        spikedash = "solid",
        spikethickness = 1))
    
    # Plot object to return
    gg
    
  })
  
  ### Plotly figure: GDP ---------------------
  output$country_fishery_stats_gdp_plot <- renderPlotly({
    
    req(input$w_country_fishery_stats_selected_country)
    
    # Filter data
    country_fishery_stats_gdp_plot_dat <- demographic_dat %>%
      dplyr::filter(iso3 == input$w_country_fishery_stats_selected_country) %>%
      dplyr::filter(variable %in% c("gdp", "gdp_ffa"))
    
    req(nrow(country_fishery_stats_gdp_plot_dat) > 0)
    req(all(is.na(country_fishery_stats_gdp_plot_dat$value)) == F)
    
    # Make dummy values for missing data? 
    if(nrow(country_fishery_stats_gdp_plot_dat %>% dplyr::filter(variable == "gdp_ffa")) == 0){
      
      country_fishery_stats_gdp_plot_dat <- country_fishery_stats_gdp_plot_dat %>%
        bind_rows(
          tibble(
            iso3 = input$w_country_fishery_stats_selected_country,
            year = seq(2000, 2018, by = 1),
            variable = "gdp_ffa",
            value = NA,
            units = NA,
            source = NA
          )
        )
    }
    
    # Make plot
    country_fishery_stats_gdp_plot <- ggplot()+
      aes(x = year, y = value/1e9)+
      geom_area(data = country_fishery_stats_gdp_plot_dat %>% dplyr::filter(variable == "gdp"), fill = '#3c8dbc')+
      geom_point(data = country_fishery_stats_gdp_plot_dat %>% dplyr::filter(variable == "gdp"),
                 aes(text = paste0("<b>","Year: ","</b>", year,
                                   "<br>",
                                   "<b>", "GDP - Total (US$): ", "</b>", "$", format(round(value,0), big.mark = ",", scientific = F))),
                 alpha = 0, color = '#3c8dbc')+
      geom_area(data = country_fishery_stats_gdp_plot_dat %>% dplyr::filter(variable == "gdp_ffa"), fill = 'navy')+
      geom_point(data = country_fishery_stats_gdp_plot_dat %>% dplyr::filter(variable == "gdp_ffa"),
                 aes(text = paste0("<b>","Year: ","</b>", year,
                                   "<br>",
                                   "<b>", "GDP - Fisheries, Forestry, and Agriculture (US$): ", "</b>", "$", format(round(value,0), big.mark = ",", scientific = F))),
                 alpha = 0, color = 'navy')+
      scale_y_continuous(name = "GDP (US$, billions)",
                         labels = function(x) format(x, big.mark = ",", scientific = FALSE))+
      scale_x_continuous(expand = c(0,0))+
      theme_bw()+
      labs(x = "Year")+
      theme(legend.title = element_blank(),
            legend.position = "none")
    
    # Convert to plotly
    gg <- ggplotly(country_fishery_stats_gdp_plot, tooltip="text")
    
    gg <- gg %>%
      layout(xaxis = list(
        showspikes = TRUE,
        spikemode = "across",
        spikecolor = '#3c8dbc',
        spikedash = "solid",
        spikethickness = 1))
    
    # Plot object to return
    gg
    
  })
  
  ### ------------------------------
  ### 03c. compare-fishery-stats ---
  ### ------------------------------
  
  ### Info button: Subsidy types --------------
  observeEvent(input$info_compare_fishery_stats_subsidy_types, {
                   
                   shinyalert(title = text$item_label[text$item_id == "subsidy-types-to-include"],
                              text = "Text goes here",
                              size = "l",
                              closeOnEsc = TRUE,
                              closeOnClickOutside = TRUE,
                              html = TRUE,
                              type = "",
                              showConfirmButton = TRUE,
                              showCancelButton = FALSE,
                              confirmButtonText = "OK",
                              confirmButtonCol = "#0d5ba2",
                              timer = 0,
                              animation = TRUE)
                   
                 })
  
  ### Info button: Countries and territories --------------
  observeEvent(input$info_compare_fishery_stats_territories, {
                   
                   shinyalert(title = "WTO Members and Observers",
                              text = "Text goes here",
                              size = "l",
                              closeOnEsc = TRUE,
                              closeOnClickOutside = TRUE,
                              html = TRUE,
                              type = "",
                              showConfirmButton = TRUE,
                              showCancelButton = FALSE,
                              confirmButtonText = "OK",
                              confirmButtonCol = "#0d5ba2",
                              timer = 0,
                              animation = TRUE)
                   
                 })
  
  ### Update checkboxGroupInputs: Select all
  observeEvent(input$ab_compare_fishery_stats_select_all, {
    
    updateCheckboxGroupInput(session,
                             "w_compare_fishery_stats_good_types",
                             selected = subsidy_types_sorted_sumaila[1:3])
    
    updateCheckboxGroupInput(session,
                             "w_compare_fishery_stats_ugly_types",
                             selected = subsidy_types_sorted_sumaila[11:13])
    
    updateCheckboxGroupInput(session,
                             "w_compare_fishery_stats_bad_types",
                             selected = subsidy_types_sorted_sumaila[4:10])
    
  })
  
  ### Update checkboxGroupInputs: Clear all
  observeEvent(input$ab_compare_fishery_stats_clear_all, {
    
    x <- character(0)
    
    updateCheckboxGroupInput(session,
                             "w_compare_fishery_stats_good_types",
                             selected = x)
    
    updateCheckboxGroupInput(session,
                             "w_compare_fishery_stats_ugly_types",
                             selected = x)
    
    updateCheckboxGroupInput(session,
                             "w_compare_fishery_stats_bad_types",
                             selected = x)
    
  })
  
  ### UI output: Name of selected country header ---------------------
  output$compare_fishery_stats_selected_country_name <- renderUI({
    
    req(input$w_compare_fishery_stats_selected_country)
    
    out <- names(wto_members_and_observers[wto_members_and_observers == input$w_compare_fishery_stats_selected_country])
    
    tags$h3(out)
  })
  
  ### Update input: Remove selected state from list of comparison states --------------------------
  observe({
    
    # Removed selected country from the list of choices
    new_choices <- wto_members_and_observers[wto_members_and_observers == input$w_compare_fishery_stats_selected_country]
    
    # Update input
    updateSelectizeInput(session, 
                         "w_compare_fishery_stats_select_manual",
                         choices = new_choices)
  })
  
  ### Plotly figure: Compare fishery stats ---------------------
  output$compare_fishery_stats_bar_plot <- renderPlotly({
    
    req(input$w_compare_fishery_stats_selected_country)
    req(input$w_compare_fishery_stats_plot_variable)
    req(input$w_compare_fishery_stats_method)
    
    if(!(input$w_compare_fishery_stats_plot_variable %in% c("landings", "revenue"))){
      
      selected_subsidy_types <- c(input$w_compare_fishery_stats_good_types, 
                                  input$w_compare_fishery_stats_ugly_types,
                                  input$w_compare_fishery_stats_bad_types)
      req(length(selected_subsidy_types) > 0)
      
    }
    
    # Plot arguments: 1 = variable name, 2 = hover/x-axis caption, 3 = rounding digits, 4 = units prefix.
    compare_fishery_stats_bar_plot_args <- switch(
      input$w_compare_fishery_stats_plot_variable,
      "subsidies" = list("subsidies_Sumaila", "Est. fisheries subsidies (2018 US$)", 0, "$"),
      "landings" = list("capture_production", "Capture production (mt, 2018)", 0, ""),
      "revenue" = list("landed_value", "Est. landed value (2018 US$)", 0, "$"),
      "subsidies_per_landing" = list("subsidies_per_production", "Fisheries subsidies per tonne of capture production (2018 US$/tonne)", 2, "$"),
      "subsidies_per_revenue" = list("subsidies_per_landed_value", "Ratio of fisheries subsidies to landed value", 2, ""), 
      "subsidies_per_capita" = list("subsidies_per_capita", "Fisheries subsidies per capita (2018 US$/person)", 2, "$"),
      "subsidies_per_gdp" = list("subsidies_per_gdp", "Ratio of fisheries subsidies to GDP", 4, ""),
      "subsidies_per_fte" = list("subsidies_per_fte", "Fisheries subsidies per full-time-equivalent fisheries jobs (2018 US$/FTE)", 2, "$"))
    
    # Filter data by selected variable and by selected subsidy type(s) [if applicable]
    compare_fishery_stats_bar_plot_dat <- combined_fishery_stats_dat %>%
      dplyr::filter(variable == compare_fishery_stats_bar_plot_args[[1]]) %>%
      dplyr::filter(if(input$w_compare_fishery_stats_plot_variable %in% c("landings", "revenue")) is.na(type) else type %in% c(selected_subsidy_types)) %>%
      group_by(iso3, display_name, variable) %>%
      mutate(tot_value = sum(value, na.rm = T)) %>%
      ungroup() %>%
      mutate(rank = dense_rank(desc(tot_value)),
             display_name = fct_rev(fct_reorder(display_name, tot_value)),
             iso3 = fct_rev(fct_reorder(iso3, tot_value)))

    # Filter for the top 10 countries
    if(input$w_compare_fishery_stats_method == "top10"){

      compare_fishery_stats_bar_plot_dat <- compare_fishery_stats_bar_plot_dat %>%
        dplyr::filter(rank <= 10 | iso3 == input$w_compare_fishery_stats_selected_country) %>%
        mutate(color = ifelse(iso3 == input$w_compare_fishery_stats_selected_country, "red", NA))
      compare_fishery_stats_bar_plot_dat$value[is.na(compare_fishery_stats_bar_plot_dat$value)] <- 0

      # Filter for manually selected states
    }else if(input$w_compare_fishery_stats_method == "select"){
      
      compare_fishery_stats_bar_plot_dat <- compare_fishery_stats_bar_plot_dat %>%
        dplyr::filter(iso3 %in% input$w_compare_fishery_stats_select_manual | iso3 == input$w_compare_fishery_stats_selected_country) %>%
        mutate(color = ifelse(iso3 == input$w_compare_fishery_stats_selected_country, "red", NA))
      compare_fishery_stats_bar_plot_dat$value[is.na(compare_fishery_stats_bar_plot_dat$value)] <- 0

    }

    # Require at least one matching entry
    req(nrow(compare_fishery_stats_bar_plot_dat) > 0)
    
    
    ## Make plots
    if(!(input$w_compare_fishery_stats_plot_variable %in% c("landings", "revenue"))){
      
      compare_fishery_stats_bar_plot <- ggplot()+
        geom_col(data = compare_fishery_stats_bar_plot_dat, aes(x = display_name, y = value, fill = type_name,
                                                                text = paste0("<b>","State: ","</b>", display_name,
                                                                              "<br>",
                                                                              "<b>","Subsidy Type: ","</b>", type_name,
                                                                              "<br>",
                                                                              "<b>", compare_fishery_stats_bar_plot_args[[2]],": ","</b>",
                                                                              compare_fishery_stats_bar_plot_args[[4]], format(round(value, compare_fishery_stats_bar_plot_args[[3]]), big.mark = ","))))+
        scale_fill_manual(values = myColors[names(myColors) %in% compare_fishery_stats_bar_plot_dat$type_name])+
        scale_y_continuous(expand = c(0,0), name = compare_fishery_stats_bar_plot_args[[2]],
                           labels = function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE))+
        coord_flip()+
        theme_bw()+
        labs(x = "")+
        theme(legend.title = element_blank(),
              legend.position = "none")
      
      
    }else{
      
      compare_fishery_stats_bar_plot <- ggplot()+
        geom_col(data = compare_fishery_stats_bar_plot_dat, aes(x = display_name, y = value, 
                                                                text = paste0("<b>","State: ","</b>", display_name,
                                                                              "<br>",
                                                                              "<b>","Subsidy Type: ","</b>", type_name,
                                                                              "<br>",
                                                                              "<b>", compare_fishery_stats_bar_plot_args[[2]],": ","</b>",
                                                                              compare_fishery_stats_bar_plot_args[[4]], format(round(value, compare_fishery_stats_bar_plot_args[[3]]), big.mark = ","))),
                 fill = "#0d5ba2")+
        scale_y_continuous(expand = c(0,0), name = compare_fishery_stats_bar_plot_args[[2]],
                           labels = function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE))+
        coord_flip()+
        theme_bw()+
        labs(x = "")+
        theme(legend.title = element_blank(),
              legend.position = "none")
      
    }
    
    # Convert to plotly
    gg <- ggplotly(compare_fishery_stats_bar_plot, tooltip="text")

    # Return plot
    gg
    
  })
  
  ### ---------------------------------
  ### 04d. global-fishing-footprint ---
  ### ---------------------------------
  
  ### Info button: Global map of fishing effort --------------
  observeEvent(input$info_global_fishing_footprint_map, {
    
    shinyalert(title = text$item_label[text$item_id == "global-fishing-footprint"],
               text = "Text goes here",
               size = "l",
               closeOnEsc = TRUE,
               closeOnClickOutside = TRUE,
               html = TRUE,
               type = "",
               showConfirmButton = TRUE,
               showCancelButton = FALSE,
               confirmButtonText = "OK",
               confirmButtonCol = "#0d5ba2",
               timer = 0,
               animation = TRUE
    )
    
  })
  
  ### Leaflet map: Global map of fishing effort with hover boxes ---------------------
  output$global_fishing_footprint_map <- renderLeaflet({
    
    # Summarize data
    global_fishing_footprint_map_dat <- vessel_dat %>%
      group_by(eez_hs_code) %>%
      summarize(vessels = n_distinct(ssvid),
                flag_states = n_distinct(flag_iso3),
                fishing_h = sum(fishing_hours_eez_fao_ter, na.rm = T),
                fishing_KWh = sum(fishing_KWh_eez_fao_ter, na.rm = T))
    
    global_fishing_footprint_map_dat_shp <- eez_fao %>%
      left_join(global_fishing_footprint_map_dat, by = c("eez_hs_code" = "eez_hs_code")) %>%
      dplyr::filter(!is.na(fishing_KWh) & fishing_KWh > 0)
    
    global_fishing_footprint_map_text <- paste0(
      "<b>","Location: ","</b>", global_fishing_footprint_map_dat_shp$name,"</b>",
      "<br/>",
      "<b>", "Area type: ","</b>",global_fishing_footprint_map_dat_shp$type, "</b>",
      "</br>",
      "<b>", "State: ", "</b>", global_fishing_footprint_map_dat_shp$trrtry1, "</b>",
      "<br/>",
      "<b>", "Sovereign state: ", "</b>", global_fishing_footprint_map_dat_shp$sovrgn1,
      "<br/>",
      "<b>", "Active vessels: ", "</b>", global_fishing_footprint_map_dat_shp$vessels,
      "<br/>",
      "<b>", "Fishing effort (hours): ", "</b>", format(round(global_fishing_footprint_map_dat_shp$fishing_h, 0), big.mark = ","),
      "<br/>",
      "<b>", "Fishing effort (kWh): ", "</b>", format(round(global_fishing_footprint_map_dat_shp$fishing_KWh, 0), big.mark = ","),
      "<br/>",
      "<b>", "Unique vessel flag states: ", "</b>", global_fishing_footprint_map_dat_shp$flag_states) %>%
      lapply(htmltools::HTML)
    
    # Chloropleth color palette for global effort map
    global_fishing_footprint_map_pal <- colorNumeric(palette = "YlOrRd",
                                          log10(global_fishing_footprint_map_dat_shp$fishing_KWh))
    
    # Map
    leaflet('global_fishing_footprint_map', options = leafletOptions(minZoom = 3)) %>%
      addProviderTiles("CartoDB.VoyagerNoLabels") %>%
      addPolygons(data = global_fishing_footprint_map_dat_shp,
                  fillColor = ~global_fishing_footprint_map_pal(log10(fishing_KWh)),
                  fillOpacity = 1,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = TRUE),
                  label = global_fishing_footprint_map_text,
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")) %>%
      setView(0,20, zoom = 3) %>%
      addLegend("bottomright", 
                pal = global_fishing_footprint_map_pal, 
                values = log10(global_fishing_footprint_map_dat_shp$fishing_KWh),
                title = "Fishing effort<br>(kWh)",
                opacity = 1,
                labFormat = labelFormat(
                  transform = function(x) 10^(x)))
    
  })
  

})
