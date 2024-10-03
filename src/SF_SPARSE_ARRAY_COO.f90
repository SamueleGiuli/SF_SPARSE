MODULE SF_SPARSE_ARRAY_COO
  use iso_fortran_env, only: dp=>real64
  USE SF_SPARSE_COMMON
  implicit none
  public :: sf_sparse_dmatrix_coo,sf_sparse_cmatrix_coo, sf_init_matrix_coo, sf_delete_matrix_coo, sf_insert_element_coo, sf_dump_matrix_coo

!COO MATRIX
  type sf_sparse_dmatrix_coo
     integer,dimension(:),allocatable :: rows
     integer,dimension(:),allocatable :: cols
     real(dp),dimension(:),allocatable :: vals
     integer                                   :: Size
     integer                                   :: Nrow
     integer                                   :: Ncol
     logical                                   :: status=.false.
  end type sf_sparse_dmatrix_coo
  
  type sf_sparse_cmatrix_coo
     integer,dimension(:),allocatable     :: rows
     integer,dimension(:),allocatable     :: cols
     complex(dp),dimension(:),allocatable :: vals
     integer                                   :: Size
     integer                                   :: Nrow
     integer                                   :: Ncol
     logical                                   :: status=.false.
  end type sf_sparse_cmatrix_coo
  
  !INIT SPARSE MATRICES 
  interface sf_init_matrix_coo
     module procedure :: sf_init_dmatrix_coo
     module procedure :: sf_init_cmatrix_coo
  end interface sf_init_matrix_coo
  
  !DELETE SPARSE MATRIX 
  interface sf_delete_matrix_coo
     module procedure :: sf_delete_dmatrix_coo
     module procedure :: sf_delete_cmatrix_coo
  end interface sf_delete_matrix_coo


  !INSERT ELEMENTS
  interface sf_insert_element_coo
     module procedure :: sf_insert_delement_coo
     module procedure :: sf_insert_celement_coo
  end interface sf_insert_element_coo

  
  !DUMP SPARSE MATRIX INTO STANDARD MATRIX
  interface sf_dump_matrix_coo
     module procedure :: sf_dump_dmatrix_coo
     module procedure :: sf_dump_cmatrix_coo
  end interface sf_dump_matrix_coo

contains  
  !+------------------------------------------------------------------+
  !PURPOSE:  initialize the sparse matrix list
  !+------------------------------------------------------------------+
  subroutine sf_init_dmatrix_coo(sparse,N,N1)
    type(sf_sparse_dmatrix_coo),intent(inout) :: sparse
    integer                               :: N
    integer,optional                      :: N1
    integer                               :: i
    !
    !put here a delete statement to avoid problems
    if(sparse%status)stop "sf_init_dmatrix_coo: already allocated can not init"
    !
    sparse%Nrow=N
    sparse%Ncol=N
    sparse%Size=0
    if(present(N1))sparse%Ncol=N1
    !
    allocate(sparse%rows(0))
    allocate(sparse%cols(0))
    allocate(sparse%vals(0))
    !
    sparse%status=.true.
    !
  end subroutine sf_init_dmatrix_coo
  
subroutine sf_init_cmatrix_coo(sparse,N,N1)
    type(sf_sparse_cmatrix_coo),intent(inout) :: sparse
    integer                               :: N
    integer,optional                      :: N1
    integer                               :: i
    !
    !put here a delete statement to avoid problems
    if(sparse%status)stop "sf_init_cmatrix_coo: already allocated can not init"
    !
    sparse%Nrow=N
    sparse%Ncol=N
    sparse%Size=0
    if(present(N1))sparse%Ncol=N1
    !
    allocate(sparse%rows(0))
    allocate(sparse%cols(0))
    allocate(sparse%vals(0))
    !
    sparse%status=.true.
    !
  end subroutine sf_init_cmatrix_coo

  !+------------------------------------------------------------------+
  !PURPOSE: delete an entire sparse matrix
  !+------------------------------------------------------------------+
  subroutine sf_delete_dmatrix_coo(sparse)
    type(sf_sparse_dmatrix_coo),intent(inout) :: sparse
    integer                           :: i
    !
    if(.not.sparse%status)return !stop "Error SPARSE/sf_delete_matrix: sparse is not allocated."
    !
    deallocate(sparse%rows)
    deallocate(sparse%cols)
    deallocate(sparse%vals)
    !
    sparse%Nrow=0
    sparse%Ncol=0
    sparse%Size=0
    sparse%status=.false.
  end subroutine sf_delete_dmatrix_coo

    subroutine sf_delete_cmatrix_coo(sparse)
    type(sf_sparse_cmatrix_coo),intent(inout) :: sparse
    integer                           :: i
    !
    if(.not.sparse%status)return !stop "Error SPARSE/sf_delete_matrix: sparse is not allocated."
    !
    deallocate(sparse%rows)
    deallocate(sparse%cols)
    deallocate(sparse%vals)
    !
    sparse%Nrow=0
    sparse%Ncol=0
    sparse%Size=0
    sparse%status=.false.
  end subroutine sf_delete_cmatrix_coo


  !+------------------------------------------------------------------+
  !PURPOSE: insert an element value at position (i,j) in the sparse matrix
  !+------------------------------------------------------------------+
  subroutine sf_insert_delement_coo(sparse,value,i,j)
    type(sf_sparse_dmatrix_coo),intent(inout) :: sparse
    real(dp),intent(in)                    :: value
    integer,intent(in)                    :: i,j
    integer                               :: k
    logical                               :: present
    !
    present=.false.
    do k=1,sparse%Size !Find position if present
       if( (i==sparse%rows(k)).and.(j==sparse%cols(k)))then
          present=.true.
          exit
       end if
    end do
    !
    if(present)then                            ! Add if present
       sparse%vals(k) = sparse%vals(k) + value !
    else
       call add_to(sparse%rows,i)
       call add_to(sparse%cols,j)
       call add_to(sparse%vals,value)
       sparse%Size = sparse%Size +1
    endif
    !
    if(sparse%Size > sparse%Ncol*sparse%Nrow)stop "sf_insert_delement_coo ERROR: sparse%Size > sparse%Ncol*sparse%Nrow"
    !
  end subroutine sf_insert_delement_coo

  subroutine sf_insert_celement_coo(sparse,value,i,j)
    type(sf_sparse_cmatrix_coo),intent(inout) :: sparse
    complex(dp),intent(in)                 :: value
    integer,intent(in)                    :: i,j
    integer                               :: k
    logical                               :: present
    !
    present=.false.
    do k=1,sparse%Size !Find position if present
       if( (i==sparse%rows(k)).and.(j==sparse%cols(k)))then
          present=.true.
          exit
       end if
    end do
    !
    if(present)then                            ! Add if present
       sparse%vals(k) = sparse%vals(k) + value !
    else
       call add_to(sparse%rows,i)
       call add_to(sparse%cols,j)
       call add_to(sparse%vals,value)
       sparse%Size = sparse%Size +1
    endif
    !
    if(sparse%Size > sparse%Ncol*sparse%Nrow)stop "sf_insert_celement_coo ERROR: sparse%Size > sparse%Ncol*sparse%Nrow"
    !
  end subroutine sf_insert_celement_coo
  
  !+------------------------------------------------------------------+
  !PURPOSE: dump a sparse matrix into a regular 2dim array
  !+------------------------------------------------------------------+
  subroutine sf_dump_dmatrix_coo(sparse,matrix)
    type(sf_sparse_dmatrix_coo),intent(in)   :: sparse
    real(dp),dimension(:,:),intent(inout) :: matrix
    real(dp)                              :: val
    integer                              :: i,col,row,Ndim1,Ndim2
    !
    Ndim1=size(matrix,1)
    Ndim2=size(matrix,2)
    !
    if(sparse%Nrow/=Ndim1 .OR. sparse%Ncol/=Ndim2)stop "Warning SPARSE/dump_matrix: dimensions error"
    !
    do i=1,sparse%Size
       row=sparse%rows(i);  col=sparse%cols(i)
       matrix(row,col) = matrix(row,col) + sparse%vals(i)
    enddo
  end subroutine sf_dump_dmatrix_coo

  subroutine sf_dump_cmatrix_coo(sparse,matrix)
    type(sf_sparse_cmatrix_coo),intent(in)      :: sparse
    complex(dp),dimension(:,:),intent(inout) :: matrix
    complex(dp)                              :: vals
    integer                                 :: i,col,row,Ndim1,Ndim2
    !
    Ndim1=size(matrix,1)
    Ndim2=size(matrix,2)
    !
    if(sparse%Nrow/=Ndim1 .OR. sparse%Ncol/=Ndim2)stop "Warning SPARSE/dump_matrix: dimensions error"
    !
    do i=1,sparse%Size
       row=sparse%rows(i);  col=sparse%cols(i)
       matrix(row,col) = matrix(row,col) + sparse%vals(i)
    enddo
  end subroutine sf_dump_cmatrix_coo

END MODULE SF_SPARSE_ARRAY_COO
