MODULE SF_SPARSE
  USE SF_SPARSE_ARRAY_CSR
  USE SF_SPARSE_ARRAY_CSC
  USE SF_SPARSE_COMMON
  !USE SF_SPARSE_ARRAY_ALGEBRA
  private

  public :: sparse_dmatrix_csr, sparse_zmatrix_csr  
  public :: sparse_dmatrix_csc, sparse_zmatrix_csc
  public :: assignment(=)
  public :: operator(+)
  public :: operator(-)
  public :: operator(*)
  public :: operator(/)
  public :: operator(.x.)
  public :: kron
  public :: shape
  public :: transpose
  public :: hconjg
  public :: matmul
  
END MODULE SF_SPARSE
