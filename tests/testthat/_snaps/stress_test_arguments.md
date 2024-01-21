# hasn't changed

    Code
      stress_test_arguments
    Output
      # A tibble: 13 x 6
         name                    type      default allowed              min   max  
         <chr>                   <chr>     <chr>   <chr>                <chr> <chr>
       1 asset_type              character <NA>    equity, bonds, loans <NA>  <NA> 
       2 lgd                     double    0.45    <NA>                 0.3   0.9  
       3 risk_free_rate          double    0.02    <NA>                 0     0.05 
       4 discount_rate           double    0.07    <NA>                 0.015 0.1  
       5 growth_rate             double    0.03    <NA>                 0.01  0.099
       6 div_netprofit_prop_coef double    1       <NA>                 0.8   1    
       7 shock_year              double    2030    <NA>                 2025  2035 
       8 start_year              double    2022    <NA>                 2022  2022 
       9 settlement_factor       double    1       <NA>                 0     1    
      10 exp_share_damages_paid  double    0.027   <NA>                 0     1    
      11 scc                     double    40      <NA>                 0     10000
      12 market_passthrough      double    0       <NA>                 0     1    
      13 financial_stimulus      double    1       <NA>                 1     5    

