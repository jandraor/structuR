#' Determine pathway dominance over time
#'
#' @param impact_df A data frame returned by \code{struc_eval_impact}
#'
#' @return A data frame
#' @export
#'
#' @examples
#' impact_df <- data.frame(time                = c(1, 8),
#' I__x__y__f1         = c(-0.001087266, -0.953119253),
#' I__y__y__f1         = c(1.9991130, 1.3627462),
#' I__y__y__f2         = c(-1, -1),
#' pos_impact          = c(1.9991130, 1.3627462),
#' neg_impact          = c(-1.001087, -1.953119),
#' total_impact        = c(0.9980257, -0.5903731),
#' dominant_behaviour  = c(1, -1))
#' struc_dominance(impact_df)
struc_dominance <- function(impact_df) {

  cols     <- colnames(impact_df)
  pathways <- cols[!cols %in% c("time", "pos_impact","neg_impact",
                                "total_impact", "dominant_behaviour")]

  combn_df    <- pathway_combn(pathways)
  pathways_ts <- remove_dominated_impacts(impact_df, pathways)

  evaluated_pathways <- evaluate_pathways_over_time(combn_df, pathways_ts)
  opposing_df        <- create_opposing_df(impact_df)

  determine_dominance(evaluated_pathways, opposing_df)
}


pathway_combn <- function(pathways) {

  n_path <- length(pathways)

  purrr::map_dfr(1:n_path, function(i) {
    combns  <- utils::combn(pathways, i)
    formula <- apply(combns, MARGIN = 2,  paste, collapse = " + ")

    data.frame(combn = formula, n = i)
  })
}

remove_dominated_impacts <- function(impact_df, pathways) {

  n_pathways   <- length(pathways)

  for(i in seq_len(n_pathways)) {
    imp_name   <- pathways[[i]]
    comparison <- sign(impact_df[[imp_name]]) == impact_df[["dominant_behaviour"]]


    impact_df[imp_name] <- ifelse(comparison, impact_df[[imp_name]], NA)
  }

  impact_df[, c("time", pathways)]
}

evaluate_pathways_over_time <- function(combn_df, pathways_ts) {

  combn_list <- purrr::transpose(combn_df)

  purrr::map_df(combn_list, function(row) {

    vals      <- with(pathways_ts, eval(parse(text = row$combn)))

    data.frame(combn = row$combn, n = row$n, time = pathways_ts$time,
               impact = vals)

  }) -> evaluated_pathways

  evaluated_pathways[order(evaluated_pathways$time), ]
}

create_opposing_df <- function(impact_df) {

  opposing_impact <- ifelse(impact_df$dominant_behaviour == 1,
                            impact_df$neg_impact,
                            impact_df$pos_impact)

  data.frame(time = impact_df$time, opposing_impact = opposing_impact)
}

determine_dominance <- function(evaluated_pathways, opposing_df) {

  # the magnitude of candidate dominant impacts is always positive
  ep        <- evaluated_pathways[!is.na(evaluated_pathways$impact), ]
  ep$impact <- abs(ep$impact)

  # the magnitude of opposing impacts is always negative
  opposing_df$opposing_impact <- abs(opposing_df$opposing_impact) * -1

  comparison_df <- merge(ep, opposing_df, by = "time", all.x = TRUE)

  comparison_df$dominates <- with(comparison_df,
                                  ifelse(impact + opposing_impact > 0, TRUE,
                                         FALSE))

  raw_dominant_combn <- comparison_df[comparison_df$dominates == TRUE,]

  by(raw_dominant_combn, raw_dominant_combn$time, function(df) {

    min_n   <- min(df$n)
    df      <- df[df$n == min_n, , drop = FALSE]
    max_imp <- max(df$impact)

    df[df$impact == max_imp, , drop = FALSE]
  }) -> dominant_list

  dominant_combn <- do.call(rbind, dominant_list)

  dominant_combn$combn     <- gsub(" \\+ ", ",", dominant_combn$combn)
  dominant_combn           <- dominant_combn[, c("time", "combn")]
  colnames(dominant_combn) <- c("time", "dominant_pathway")

  dominant_combn
}
