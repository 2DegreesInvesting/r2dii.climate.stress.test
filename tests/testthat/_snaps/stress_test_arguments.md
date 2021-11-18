# hasn't changed

    Code
      stress_test_arguments
    Output
      # A tibble: 9 x 6
        name                    type      default allowed              min   max  
        <chr>                   <chr>     <chr>   <chr>                <chr> <chr>
      1 asset_type              character <NA>    equity, bonds, loans <NA>  <NA> 
      2 lgd_senior_claims       double    0.45    <NA>                 0.3   0.6  
      3 lgd_subordinated_claims double    0.75    <NA>                 0.6   0.9  
      4 risk_free_rate          double    0.02    <NA>                 0     0.05 
      5 discount_rate           double    0.02    <NA>                 -0.01 0.05 
      6 div_netprofit_prop_coef double    1       <NA>                 0.8   1    
      7 shock_year              double    2030    <NA>                 2025  2035 
      8 term                    double    2       <NA>                 1     10   
      9 company_exclusion       logical   TRUE    TRUE, FALSE          <NA>  <NA> 

