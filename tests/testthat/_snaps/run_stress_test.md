# if `asset_type` is longer than 1 uses default with a warning

    Code
      out <- run_stress_test()
    Warning <warning>
      `asset_type` should be of length 1.
      x It is of length 3: bonds, equity, loans.
      i Using default: bonds.
      i Suppress this warning e.g. with: `run_stress_test(asset_type = 'bonds')`.

