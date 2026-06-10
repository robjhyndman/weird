# outlier_map errors for an unsupported object

    Code
      outlier_map(object = lm(v1 ~ v2, Y))
    Condition
      Error in `outlier_map()`:
      ! `object` must be the output of prcomp() or an rrcov Pca* function

# outlier_map errors when data is missing for a prcomp object

    Code
      outlier_map(pca)
    Condition
      Error in `prcomp_distances()`:
      ! `data` is required to compute orthogonal distances for a prcomp object.

# outlier_map errors when scores or loadings are missing

    Code
      outlier_map(scores = pca$x, data = Y)
    Condition
      Error in `outlier_map()`:
      ! Supply `object`, or both `scores` and `loadings`.

