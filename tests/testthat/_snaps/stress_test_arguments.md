# hasn't changed

    Code
      stress_test_arguments
    Output
      # A tibble: 16 x 6
         name                    type      default       allowed           min   max  
         <chr>                   <chr>     <chr>         <chr>             <chr> <chr>
       1 asset_type              character <NA>          equity, bonds, l~ <NA>  <NA> 
       2 baseline_scenario       character WEO2021_STEPS WEO2021_STEPS, G~ <NA>  <NA> 
       3 shock_scenario          character WEO2021_SDS   WEO2021_SDS, WEO~ <NA>  <NA> 
       4 lgd                     double    0.45          <NA>              0.3   0.9  
       5 risk_free_rate          double    0.02          <NA>              0     0.05 
       6 discount_rate           double    0.07          <NA>              0.015 0.1  
       7 growth_rate             double    0.03          <NA>              0.01  0.099
       8 div_netprofit_prop_coef double    1             <NA>              0.8   1    
       9 shock_year              double    2030          <NA>              2025  2035 
      10 scenario_geography      character Global        AdvancedEconomie~ <NA>  <NA> 
      11 start_year              double    2021          <NA>              2021  2021 
      12 settlement_factor       double    1             <NA>              0     1    
      13 exp_share_damages_paid  double    0.027         <NA>              0     1    
      14 scc                     double    40            <NA>              0     10000
      15 carbon_price_model      character no_carbon_tax no_carbon_tax, N~ <NA>  <NA> 
      16 market_passthrough      double    0             <NA>              0     1    

