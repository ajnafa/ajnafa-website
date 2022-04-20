#----------------------------Data Simulation Example----------------------------
#-Author: A. Jordan Nafa--------------------------------Created: April 9, 2022-#
#-R Version: 4.1.2-------------------------------------Revised: April 18, 2022-#

# Set Project Options----
options(
  digits = 4, # Significant figures output
  scipen = 999, # Disable scientific notation
  repos = getOption("repos")["CRAN"],
  brms.backend = "cmdstanr",
  max.print = 200
)

# Load the necessary libraries----
pacman::p_load(
  "tidyverse",
  "data.table",
  "dtplyr",
  "ggdag",
  "dagitty",
  "latex2exp",
  "kableExtra",
  install = FALSE
)

#------------------------------------------------------------------------------#
#--------------------DAG for the Data Generation Process------------------------
#------------------------------------------------------------------------------#

# Specify the DAG for the data generation process
sim_dag <- dagify(
  x_t ~ x_tm1 + z_t + y_tm1,
  x_tm1 ~ x_tm2 + z_tm1 + y_tm2,
  x_t1 ~ x_t + y_t,
  z_t ~ z_tm1 + x_tm1 + y_tm1,
  z_tm1 ~ z_tm2 + x_tm2 + y_tm2,
  z_t1 ~ z_t + x_t + y_t,
  y_t ~ x_t + x_tm1 + z_t,
  y_tm1 ~ x_tm1 + z_tm1,
  y_tm2 ~ x_tm2 + z_tm2,
  coords = list(
    x = c(z_tm2 = -1, z_tm1 = 0, z_t = 1, z_t1 = 2, x_tm2 = -1,
          x_tm1 = 0, x_t = 1, x_t1 = 2, y_tm1 = 0.25, y_t = 1.25, y_tm2 = -0.25),
    y = c(z_tm2 = 0, z_tm1 = 0, z_t = 0, z_t1 = 0, x_tm2 = 4,
          x_tm1 = 4, x_t = 4, x_t1 = 4, y_tm1 = 2, y_t = 2, y_tm2 = 2)
  ),
  labels = c(z_tm2 = "bold(Z[t - 2])", x_tm2 = "bold(X[t - 2])", z_t1 = "bold(...)",
             x_t1 = "bold(...)", x_tm1 = "bold(X[t - 1])", x_t = "bold(X[t])",
             z_tm1 = "bold(Z[t - 1])", z_t = "bold(Z[t])", y_tm1 = "bold(Y[t - 1])",
             y_t = "bold(Y[t])", y_tm2 = "bold(Y[t - 2])"),
  exposure = c("x_t"),
  outcome = c("y_t")
) %>%
  # Create a tidy data frame from the DAG
  tidy_dagitty() %>%
  # Set Node Status
  node_status() %>%
  # Set node adjacency
  node_ancestors(.var = "y_t")

fig1_sim_dag <- sim_dag |>
  # Create Path-Specific colors and line types
  mutate(
    # Line type for the edges
    .edge_linetype = case_when(
      name %in% c("x_tm1", "z_t") & to %in% c("y_t", "x_t") ~ 2,
      TRUE ~ 1
    ),
    # Color for the edges
    .edge_colour = case_when(
      name == "x_t" & to == "y_t" ~ "blue",
      name %in% c("x_tm1", "z_t") & to %in% c("y_t", "x_t") ~ "red",
      TRUE ~ "black"
    )
  ) |>
  # Generate the DAG plot for figure 1
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  # Add the graph edges
  geom_dag_edges(aes(edge_linetype = .edge_linetype, edge_colour = .edge_colour), edge_width = 1) +
  # Add the graph nodes
  geom_dag_node(alpha = 0) +
  # Add the graph text
  geom_dag_text(aes(label = label), parse = TRUE, size = 8, color = "black", family = "serif") +
  # Apply theme settings
  theme_dag(base_size = 16, base_family = "serif", title = element_text(size = 25)) +
  # Add labels to the plot
  labs(
    title = TeX(r'(Figure 1 DAG for the Contemporaneous Treatment Effect of $X_{t}$ on $Y_{t}$)', bold = T),
    caption = "Figure adapted from Blackwell and Glynn (2018)"
  )

# Save the generated plot object as a .jpeg file
ggsave(
  filename = "Figure_1_DAG_Example.jpeg",
  plot = fig1_sim_dag,
  device = "jpeg",
  path = "E:/Users/Dropbox/Personal-Website/ajnafa-website/content/blog/04-09-2022-multi-level-simulation/figures/",
  width = 16,
  height = 8,
  units = "in",
  dpi = "retina",
  limitsize = FALSE
)

fig2_sim_dag <- sim_dag |>
  # Create Path-Specific colors and line types
  mutate(
    # Color for the edges
    .text_colour = case_when(
      name %in% c("x_t", "y_t") ~ "#0000FF",
      name %in% c("x_tm1", "z_t") ~ "#008B45",
      TRUE ~ "#000000"
    ),
    # Color for the edges
    .edge_colour = case_when(
      name == "x_t" & to == "y_t" ~ "#0000FF",
      name %in% c("x_tm1", "z_t") ~ "white",
      TRUE ~ "black"
    )
  ) |>
  # Generate the DAG plot for figure 2
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  # Add the graph edges
  geom_dag_edges(
    aes(edge_colour = .edge_colour),
    edge_width = 1) +
  # Add the graph nodes
  geom_dag_node(alpha = 0) +
  # Add the graph text
  geom_dag_text(
    aes(label = label, color = .text_colour),
    parse = TRUE, size = 8, family = "serif"
  ) +
  # Apply text color settings
  scale_color_manual(
    values = c("#000000", "#0000FF", "#008B45"),
    labels = c("Unadjusted Nodes", "Identified Path", "Adjusted Nodes")
  ) +
  # Apply theme settings
  theme_dag(
    base_size = 16,
    base_family = "serif",
    title = element_text(size = 25),
    legend.text = element_text(size = 18),
    legend.position = "bottom"
  ) +
  # Add labels to the plot
  labs(title = TeX(r'(Figure 2 DAG for the Contemporaneous Effect of $X_{t}$ on $Y_{t}$ Adjusting for $X_{t-1}$ and $Z_{t}$)', bold = T)) +
  guides(color = guide_legend(
    title = "Node Status",
    title.position = "left"
  ))

# Save the generated plot object as a .jpeg file
ggsave(
  filename = "Figure_2_DAG_Example.jpeg",
  plot = fig2_sim_dag,
  device = "jpeg",
  path = "E:/Users/Dropbox/Personal-Website/ajnafa-website/content/blog/04-09-2022-multi-level-simulation/figures/",
  width = 16,
  height = 8,
  units = "in",
  dpi = "retina",
  limitsize = FALSE
)

#------------------------------------------------------------------------------#
#-------------------------------Simulating Data---------------------------------
#------------------------------------------------------------------------------#

# Define the Dimensions for the Simulated Data
sim_dims <- tibble(
  Countries = seq(10, 85, 25),  # Number of Countries
  Periods = seq(12, 57, 15) # Number of Time Periods
) |>
  # Expand the data for each combinations of dimensions
  expand(Countries, Periods) |>
  # Add Total Observations and a dataset ID
  mutate(N = Countries*Periods, data_id = 1:n())

# Print the data
head(sim_dims)

# Set the rng seed to ensure values are reproducible
set.seed(123456)

sim_data <- sim_dims |>
  # Nest the tibble by dataset
  nest(data = c(Countries, Periods)) |>
  # Simulate each of the datasets
  mutate(datasets = map(
    .x = data,
    ~ .x |>
      # Expand the data intol panel format
      expand_grid(country = 1:Countries, period = 1:Periods) |>
      # Group the data by country
      group_by(country) |>
      # Simulate baseline values for the parameters
      mutate(
        # Baseline Values for Z
        Z_base = rnorm(n(), rnorm(1, 0, 1), rexp(1, 0.5)),
        # Baseline Values for X
        X_base = rnorm(n(), rnorm(1, 0, 1), rexp(1, 0.5)),
        # Baseline Values for Y
        Y_base = rnorm(n(), rnorm(1, 0, 1), rexp(1, 0.5)),
        # Lagged Baseline Values for Each of the Parameters
        across(
          ends_with("_base"),
          .fns = list(
            lag_1 = ~ lag(.x, n = 1L),
            lag_2 = ~ lag(.x, n = 2L)
          ),
          .names = "{.col}_{.fn}"
        ),
        # Floor periods at 3 to accomodate the dropped lags
        across(c(period, Periods), ~ .x - 2)
      ) |>
      # Ungroup the data
      ungroup() |>
      # Drop missing values
      drop_na()
  )) |>
  # Unnest the dimensions columns
  unnest(cols = data)

# Print the first dataset
head(sim_data$datasets[[1]])

# Print the first dataset
head(sim_data$datasets[[1]])

# Simulate a vector of "true" coefficient values for each parameter
beta <- runif(20, -5, 5)

# Simulate the model dependencies based on the DAG
sim_df <- sim_data |>
  mutate(
    datasets = map(
      .x = datasets,
      ~ .x |>
        transmute(
          # Identifiers
          across(Countries:period, ~ .x),
          # Data for Y[t-2] ~ Y_Lag_2_Base + 5.914*Z[t-2] + -3.570*X[t-2]
          Y_Lag_2 = Y_base_lag_2 + 5.914*Z_base_lag_2 + -3.570*X_base_lag_2,
          # Data for Z[t-1] ~ Z_Lag_Base + X[t-2]*beta[1] + Y[t-2]*beta[2] + Z[t-2]*beta[3]
          Z_Lag = Z_base_lag_1 + Z_base_lag_2*beta[1] + Y_Lag_2*beta[2] + X_base_lag_2*beta[3],
          # Data for X[t-1] ~ X_Lag_Base + X[t-2]*beta[4] + Y[t-2]*beta[5] + Z[t-2]*beta[6]
          X_Lag = X_base_lag_1 + X_base_lag_2*beta[3] + Y_Lag_2*beta[5] + Z_Lag*beta[6],
          # Data for Y[t-1] ~ Y_Lag_Base + X[t-1]*beta[7] + Z[t-1]*beta[8] + Noise
          Y_Lag = Y_base_lag_1 + X_Lag*beta[7] + Z_Lag*beta[8] + rnorm(n(), 0, 1),
          # Data for Z[t] ~ Z_Base + X[t-1]*beta[9] + Z[t-1]*beta[10] + Y[t-1]*beta[11] + Noise
          Z = Z_base + X_Lag*beta[9] + Z_Lag*beta[10] + Y_Lag*beta[11] + rnorm(n(), 0, 1),
          # Data for X[t] ~ X_Base + X[t-1]*beta[12] + Z[t]*beta[13] + Y[t-1]*beta[14] + Noise
          X = X_base + X_Lag*beta[12] + Z*beta[13] + Y_Lag*beta[14] + rnorm(n(), 0, 1),
          # Data for Y[t] ~ Y_Base + X[t]*beta[15] + X[t-1]*beta[16] + Z[t]*beta[17] + Noise
          Y = Y_base + X*beta[15] + X_Lag*beta[16] + Z*beta[17] + rnorm(n(), 0, 1)
        )
    ))

# Print the first dataset
head(sim_df$datasets[[1]])

# Print the first dataset
head(sim_df$datasets[[1]])

# Check the parameter values for Y
print(beta[15:17])

# A Function for checking parameter recovery
# A Function for checking parameter recovery
bias_table <- function(.ols_equation, .fe_equation, .re_equation, .data_list, .term, .truth) {

  # OLS Estimates for Each Dataset
  ols_models <- map_dfr(
    .x = .data_list,
    # Estimate an ols model for each dataset
    ~ lm(.ols_equation, .x) |>
      # Tidy the output
      broom::tidy() |>
      # Filter the term of interest
      filter(term == .term) |>
      # Transmute the required columns
      transmute(
        countries = unique(.x$Countries),
        periods = unique(.x$Periods),
        N = countries*periods,
        ols_est = estimate,
        ols_bias = abs(estimate - .truth)
      )
  )

  # OLS Estimates with Unit FEs for Each Dataset
  fe_models <- map_dfr(
    .x = .data_list,
    # Estimate an ols model for each dataset
    ~ lm(.fe_equation, .x) |>
      # Tidy the output
      broom::tidy() |>
      # Filter the term of interest
      filter(term == .term) |>
      # Transmute the required columns
      transmute(
        countries = unique(.x$Countries),
        periods = unique(.x$Periods),
        N = countries*periods,
        fe_est = estimate,
        fe_bias = abs(estimate - .truth)
      )
  )

  # Random Intercepts Models with Country REs for Each Dataset
  re_models <- map_dfr(
    .x = .data_list,
    # Estimate a random intercepts model for each dataset
    ~ lme4::lmer(.re_equation, .x) |>
      # Tidy the output
      broom.mixed::tidy() |>
      # Filter the term of interest
      filter(term == .term) |>
      # Transmute the required columns
      transmute(
        countries = unique(.x$Countries),
        periods = unique(.x$Periods),
        N = countries*periods,
        re_est = estimate,
        re_bias = abs(estimate - .truth)
      )
  )

  # Combine the data frames into one
  ols_models |>
    left_join(fe_models, by = c("countries", "periods", "N")) |>
    left_join(re_models, by = c("countries", "periods", "N"))
}

param_recovery <- bias_table(
  .ols_equation = Y ~ X + X_Lag + Z,
  .fe_equation = Y ~ X + X_Lag + Z + as.factor(country),
  .re_equation = Y ~ X + X_Lag + Z + (1 | country),
  .data_list = sim_df$datasets,
  .term = "X",
  .truth = beta[15]
)

kable(
  param_recovery,
  col.names = c("Countries", "Periods", "N", "Estimate", "Bias", "Estimate", "Bias", "Estimate", "Bias"),
  align = "c",
  digits = 4
) %>%
  kable_classic(html_font = "serif") %>%
  add_header_above(c("Dimensions" = 3, "Complete Pooling" = 2, "No Pooling" = 2, "Partial Pooling" = 2)) %>%
  add_header_above(c(" " = 3, "Parameter Estimates" = 6)) %>%
  footnote(number = "Bias is the difference between the true value of the parameter and the estimated coefficient for each dataset in absolute terms. The true parameter value for the effect of $X_{t}$ on $Y_{t}$ is approximately -2.3039.")
