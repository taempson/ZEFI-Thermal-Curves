
## Copied from: https://discourse.mc-stan.org/t/select-chains-and-iterations-post-sampling-in-brms/6822/8
drop_chains <- function(brm_fit, chains_to_drop) {
  # brm_fit is the output of brms::brm

  sim = brm_fit$fit@sim  # Handy shortcut
  sim$samples <- sim$samples[-chains_to_drop]
  
  # Update the meta-info
  sim$chains = sim$chains - length(chains_to_drop)
  sim$warmup2 = sim$warmup2[-chains_to_drop]
  
  # Add the modified sim back to x
  brm_fit$fit@sim = sim
  brm_fit
}

keep_chains <- function(brm_fit, chains_to_keep) {
  # brm_fit is the output of brms::brm

  sim = brm_fit$fit@sim  # Handy shortcut
  sim$samples <- sim$samples[chains_to_keep]
  
  # Update the meta-info
  sim$chains = length(chains_to_keep)
  sim$warmup2 = sim$warmup2[chains_to_keep]
  
  # Add the modified sim back to x
  brm_fit$fit@sim = sim
  brm_fit
  
}

which_tbl_row <- function(filter_male = FALSE, x0_flag = "individual", y0_flag = "individual", disp_flag = "uniform_1", disp_value = 0.01, model = "two_piece", tbl = fit_tbl) {
  which( tbl$filter_male %in% filter_male & 
          tbl$x0_flag %in% x0_flag &
          tbl$y0_flag %in% y0_flag &
          tbl$disp_flag %in% disp_flag &
          tbl$disp_value %in% disp_value &
          tbl$model %in% model
          # tbl$sampling_dist %in% sampling &
        )
}

clean_var_names <- function(fit) {
  fit %>% setNames(gsub("b_", "", names(.)) %>%
             gsub("(x0|s0|y0|disp)_male(T[0-9]{3})", "\\2_\\1", .) %>%
             gsub("__", "_", .) %>%
             gsub("r_male_(x0|s0|y0|disp)\\[(T[0-9]{3}),Intercept\\]", "\\2_\\1_r", .) %>%
             gsub("\\.", " ", .))
}

## Generate data for plotting expected values
generate_epred_data <- function(fit_brms = NULL, ndraws = 1000, ncores = 4) {

    ## Create grid of x values for epred/predictions

  data_fit_brms <- fit_brms$data
  data_grid <- data_fit_brms %>%
    group_by(male) %>%
    data_grid(x = seq_range(c(20, 46), n = 51))


  ## add expected values
  data_epred <- data_grid %>%
    add_epred_draws(object = fit_brms, ndraws = ndraws, cores = ncores)

  return(data_epred)
}

## Generate data for plotting range of predicted values
generate_pred_data <- function(fit_brms = NULL, ndraws = 1000, ncores = 4) {

  data_fit_brms <- fit_brms$data
  
  data_grid <- data_fit_brms %>%
    group_by(male) %>%
    data_grid(x = seq_range(c(20, 45.9), n = 51)) %>%  #, .model = fit_brms) %>% 
    ungroup() 

  ## add simulated values
  data_pred <- data_grid %>%
    add_predicted_draws(object = fit_brms, ndraws = ndraws, ncores = ncores )

  return(data_pred)
  }


plot_epred<- function(data_epred, male_vec, desc, data_obs = NULL) {
                            ## Plot
  ## Only plot specified males
  data_epred_tmp <- data_epred %>%
    filter(male %in% male_vec)
  plot_tmp <- ggplot(data = data_epred_tmp, aes(x = x, y = .epred)) + #, color = male)) +
    ## Combine Scatter Plots and Model vs Data Plots
    stat_lineribbon(aes(y = .epred), .width = c(.95, .8, 0.5, 0.25), color = "#08519C") +
    scale_fill_brewer(palette = "Greys") 

    #scale_color_manual(values = colors_male) +
    #scale_color_brewer(palette = "Set2") +

  if(!is.null(data_obs)){

    data_obs_tmp <- data_obs %>% filter(male %in% male_vec)    
    plot_tmp <- plot_tmp +
      geom_point(data = data_obs_tmp,
                 aes(x = x, y = y), color = "red")
    title <- "Expected Values & Data vs. Temp"
    y_max <- max(data_obs_tmp$y)*1.1
  } else {

    title <- "Expected Values vs. Temp"
    y_max <- NA
  }

  plot_tmp <- plot_tmp +
    ylim(0, y_max) +
    facet_wrap(vars(male)) +
    labs(title = title, subtitle = desc)
  
  return(plot_tmp)
}

plot_pred <- function(data_pred, male_vec, desc, data_obs = NULL) {
                            ## Plot
  ## Only plot specified males
  data_pred_tmp <- data_pred %>%
    filter(male %in% male_vec)
  plot_tmp <- ggplot(data = data_pred_tmp, aes(x = x, y = .prediction)) + #, color = male)) +
    ## Combine Scatter Plots and Model vs Data Plots
    stat_lineribbon(aes(y = .epred), .width = c(.95, .8, .5, 0.25), color = "#08519C") +
    scale_fill_brewer(palette = "Greys", direction =  -1)

    #scale_color_manual(values = colors_male) +
    #scale_color_brewer(palette = "Set2") +

  if(!is.null(data_obs)){

    data_obs_tmp <- data_obs %>% filter(male %in% male_vec)    
    plot_tmp <- plot_tmp +
      geom_point(data = data_obs_tmp,
                 aes(x = x, y = y), color = "red")
    title <- "Predicted Values & Data vs. Temp"
    y_max <- max(data_obs_tmp$y)*1.1
  } else {
    ymax <- NA
    title <- "Predicted Values vs. Temp"
  }


  plot_tmp <- plot_tmp +
    ylim(0, y_max) +
    facet_wrap(vars(male)) +
    labs(title = title, subtitle = desc)
  
  return(plot_tmp)
}
