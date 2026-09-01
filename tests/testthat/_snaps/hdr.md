# hdr_regions errors for dimensions greater than 2

    Code
      hdr_regions(dist, 0.5)
    Condition
      Error in `hdr_regions()`:
      ! hdr_regions() is not implemented for dimensions greater than 2

# hdr_regions errors for multiple distributions

    Code
      hdr_regions(dist, 0.5)
    Condition
      Error in `hdr_regions()`:
      ! hdr_regions() requires a single dist_kde distribution

# hdr_regions errors for a non-kde distribution

    Code
      hdr_regions(distributional::dist_normal(), 0.5)
    Condition
      Error in `hdr_regions()`:
      ! object must be a dist_kde object

