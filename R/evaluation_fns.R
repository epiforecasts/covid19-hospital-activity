
# Pairwise comparison and ranking pipeline --------------------------------

eval_pipeline <- function(raw_scores,
                          models,
                          by = c("horizon", "model", "forecast_from", "id"),
                          summarise_by,
                          baseline = "baseline"){
  
  pw_out <- scoringutils::pairwise_comparison(scores = raw_scores %>% dplyr::filter(model %in% models),
                                              baseline = baseline,
                                              by = by,
                                              summarise_by = summarise_by)
  rank_group <- setdiff(summarise_by, "model")
  
  if(length(rank_group) > 0){
    
    out <- pw_out %>%
      dplyr::select(summarise_by, rwis = relative_skill, swis = scaled_rel_skill) %>%
      unique() %>%
      group_by_at(rank_group) %>%
      dplyr::mutate(rank = rank(swis, ties.method = "min")) %>%
      dplyr::ungroup()
    
  } else {
    
    out <- pw_out %>%
      dplyr::select(summarise_by, rwis = relative_skill, swis = scaled_rel_skill) %>%
      unique() %>%
      dplyr::mutate(rank = rank(swis, ties.method = "min")) %>%
      dplyr::ungroup()
    
  }
  
  out <- out %>%
    dplyr::mutate(model = ordered(model, models))
  
  return(out)
  
}
