# hasn't changed

    Code
      stress_test_arguments
    Output
      # A tibble: 13 x 6
         name                    type      default allowed                 min   max  
         <chr>                   <chr>     <chr>   <chr>                   <chr> <chr>
       1 asset_type              character <NA>    equity, bonds, loans    <NA>  <NA> 
       2 baseline_scenario       character NPS     NPS                     <NA>  <NA> 
       3 shock_scenario          character SDS     SDS                     <NA>  <NA> 
       4 lgd_senior_claims       double    0.45    <NA>                    0.3   0.6  
       5 lgd_subordinated_claims double    0.75    <NA>                    0.6   0.9  
       6 risk_free_rate          double    0.02    <NA>                    0     0.05 
       7 discount_rate           double    0.07    <NA>                    0.015 0.1  
       8 growth_rate             double    0.03    <NA>                    0.01  0.99 
       9 div_netprofit_prop_coef double    1       <NA>                    0.8   1    
      10 shock_year              double    2030    <NA>                    2025  2035 
      11 fallback_term           double    2       <NA>                    1     5    
      12 scenario_geography      character Global  AdvancedEconomies, Afr~ <NA>  <NA> 
      13 use_company_terms       logical   FALSE   TRUE, FALSE             <NA>  <NA> 

