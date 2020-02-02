context("Find directed cycles")

library(igraph)


#===============================================================================
# SIR MODEL
#===============================================================================
graph_dfs <- list(
  edges = data.frame(stringsAsFactors = FALSE,
                     from = c("IR", "RR", "RR", "IR", "Susceptible", "Infected",
                              "cpi", "ce", "probability", "Infected"),
                     to = c("Infected", "Infected", "Recovered", "Susceptible",
                            "probability", "cpi", "ce", "IR", "IR", "RR")
                           ),
  nodes = data.frame(stringsAsFactors = FALSE,
                     name = c("Infected", "Recovered", "Susceptible",
                              "probability", "cpi", "ce", "IR", "RR"),
                     type = c("stock", "stock", "stock", "variable", "variable",
                              "variable", "variable", "variable")
           )
)

gr <- graph_from_data_frame(graph_dfs$edges, directed = T,
                            vertices = graph_dfs$nodes)

#===============================================================================
# INVENTORY-LABOUR
#===============================================================================

graph_dfs <- list(
  edges = data.frame(stringsAsFactors=FALSE,
                  from = c("Prod_Rate", "Shipment_Rate", "Hiring_Rate", "Quit_Rate",
                           "Vac_Creation_Rate", "Vac_Closure_Rate",
                           "Prod_Start_Rate", "Prod_Rate", "Vac", "Work_in_process_Inv",
                           "Labor", "Labor", "Hiring_Rate", "Desired_Inv_Coverage",
                           "Desired_Inv", "Inv", "Prod_Adjust_from_Inv",
                           "Desired_Prod", "Desired_WIP", "Work_in_process_Inv",
                           "Adjustment_WIP", "Desired_Prod", "Desired_Prod_Start_Rate",
                           "Desired_Labor", "Labor", "Adjust_For_Labor",
                           "Quit_Rate", "Desired_Hiring_Rate", "Desired_Vac", "Vac",
                           "Adjust_For_Vac", "Desired_Hiring_Rate"),
                    to = c("Inv", "Inv", "Labor", "Labor", "Vac", "Vac",
                           "Work_in_process_Inv", "Work_in_process_Inv",
                           "Hiring_Rate", "Prod_Rate", "Prod_Start_Rate", "Quit_Rate",
                           "Vac_Closure_Rate", "Desired_Inv",
                           "Prod_Adjust_from_Inv", "Prod_Adjust_from_Inv", "Desired_Prod",
                           "Desired_WIP", "Adjustment_WIP", "Adjustment_WIP",
                           "Desired_Prod_Start_Rate", "Desired_Prod_Start_Rate",
                           "Desired_Labor", "Adjust_For_Labor", "Adjust_For_Labor",
                           "Desired_Hiring_Rate", "Desired_Hiring_Rate", "Desired_Vac",
                           "Adjust_For_Vac", "Adjust_For_Vac", "Vac_Creation_Rate",
                           "Vac_Creation_Rate"),
                  type = c("flow", "flow", "flow", "flow", "flow", "flow", "flow",
                           "flow", "info_link", "info_link", "info_link",
                           "info_link", "info_link", "info_link", "info_link",
                           "info_link", "info_link", "info_link", "info_link",
                           "info_link", "info_link", "info_link", "info_link",
                           "info_link", "info_link", "info_link", "info_link", "info_link",
                           "info_link", "info_link", "info_link", "info_link")
          ),
  nodes = data.frame(stringsAsFactors=FALSE,
                  name = c("Inv", "Labor", "Vac", "Work_in_process_Inv",
                           "Desired_Inv_Coverage", "Hiring_Rate", "Prod_Rate",
                           "Prod_Start_Rate", "Quit_Rate", "Shipment_Rate",
                           "Vac_Closure_Rate", "Desired_Inv", "Prod_Adjust_from_Inv",
                           "Desired_Prod", "Desired_WIP", "Adjustment_WIP",
                           "Desired_Prod_Start_Rate", "Desired_Labor",
                           "Adjust_For_Labor", "Desired_Hiring_Rate", "Desired_Vac",
                           "Adjust_For_Vac", "Vac_Creation_Rate"),
                  type = c("stock", "stock", "stock", "stock", "variable", "variable",
                           "variable", "variable", "variable", "variable",
                           "variable", "variable", "variable", "variable",
                           "variable", "variable", "variable", "variable", "variable",
                           "variable", "variable", "variable", "variable"),
              equation = c("Prod_Rate-Shipment_Rate", "Hiring_Rate-Quit_Rate",
                           "Vac_Creation_Rate-Vac_Closure_Rate",
                           "Prod_Start_Rate-Prod_Rate", "2+2", "Vac/8",
                           "Work_in_process_Inv/8", "Labor*0.25*40", "Labor/100", "10000",
                           "Hiring_Rate", "10000*Desired_Inv_Coverage",
                           "(Desired_Inv-Inv)/12", "10000+Prod_Adjust_from_Inv", "Desired_Prod*8",
                           "(Desired_WIP-Work_in_process_Inv)/6",
                           "Adjustment_WIP+Desired_Prod", "Desired_Prod_Start_Rate/(0.25*40)",
                           "(Desired_Labor-Labor)/19", "Adjust_For_Labor+Quit_Rate",
                           "Desired_Hiring_Rate*8", "(Desired_Vac-Vac)/4",
                           "Adjust_For_Vac+Desired_Hiring_Rate")
          )
)

gr_IL <- graph_from_data_frame(graph_dfs$edges, directed = T,
                               vertices = graph_dfs$nodes)

test_that("the function returns the correct number of directed cycles", {
  directed_cycles <- find_directed_cycles(gr)
  n_loops <- length(directed_cycles)
  expect_equal(n_loops, 3)

  # Inventory-Labour model
  actual_value <- length(find_directed_cycles(gr_IL))
  expected_value <- 14
  expect_equal(actual_value, expected_value)
})
