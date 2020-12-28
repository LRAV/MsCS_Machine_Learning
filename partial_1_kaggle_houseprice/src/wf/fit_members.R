fit_members <- function (model_stack, verbose = FALSE, ...) 
{
  
  fit_member <- function (name, wflows, members_map, train_dat) 
  {
    name <- stringr::str_replace_all(name, "`", "")
    member_row <- members_map %>% dplyr::filter(value == name)
    member_params <- wflows[[member_row$name.x]] %>% dials::parameters() %>% 
      dplyr::pull(id)
    needs_finalizing <- length(member_params) != 0
    if (needs_finalizing) {
      member_metrics <- members_map %>% dplyr::filter(value == name)
      member_wf <- wflows[[member_metrics$name.x]]
      new_member <- tune::finalize_workflow(member_wf, member_metrics[, 
                                                                      member_params]) %>% generics::fit(data = train_dat)
    }
    else {
      member_model <- members_map %>% dplyr::filter(value == 
                                                      name) %>% dplyr::select(name.x) %>% dplyr::pull()
      new_member <- generics::fit(wflows[[member_model[1]]], 
                                  data = train_dat)
    }
    new_member
  }
  
  
  stacks:::check_model_stack(model_stack)
  dat <- model_stack[["train"]]
  member_names <- stacks:::.get_glmn_coefs(model_stack[["coefs"]][["fit"]], 
                                  model_stack[["coefs"]][["spec"]][["args"]][["penalty"]]) %>% 
    dplyr::filter(estimate != 0 & terms != "(Intercept)") %>% 
    dplyr::pull(terms)
  if (model_stack[["mode"]] == "classification") {
    member_dict <- sanitize_classification_names(model_stack, 
                                                 member_names)
    member_names <- member_dict$new %>% unique()
  }
  metrics_dict <- tibble::enframe(model_stack[["model_metrics"]]) %>% 
    tidyr::unnest(cols = value) %>% dplyr::mutate(.config = if (".config" %in% 
                                                                colnames(.)) {
      .config
    }
    else {
      NA_character_
    }, .config = gsub(pattern = c("Model|Recipe"), replacement = "", 
                      x = .config, ), .config = dplyr::case_when(!is.na(.config) ~ 
                                                                   paste0(name, .config), TRUE ~ paste0(name, "1"))) %>% 
    dplyr::filter(.metric %in% c("rmse", "roc_auc"))
  if (model_stack[["mode"]] == "regression") {
    members_map <- tibble::enframe(model_stack[["cols_map"]]) %>% 
      tidyr::unnest(cols = value) %>% dplyr::full_join(metrics_dict, 
                                                       by = c(value = ".config"))
  }else {
    members_map <- tibble::enframe(model_stack[["cols_map"]]) %>% 
      tidyr::unnest(cols = value) %>% dplyr::full_join(member_dict, 
                                                       by = c(value = "old")) %>% dplyr::filter(!is.na(new)) %>% 
      dplyr::select(name, value = new) %>% dplyr::filter(!duplicated(.$value)) %>% 
      dplyr::full_join(metrics_dict, by = c(value = ".config"))
  }
  member_fits <- purrr::map(member_names, fit_member, wflows = model_stack[["model_defs"]], 
                            members_map = members_map, train_dat = dat)
  model_stack[["member_fits"]] <- setNames(member_fits, member_names)
  if (stacks:::model_stack_constr(model_stack)) {
    model_stack
  }
}
