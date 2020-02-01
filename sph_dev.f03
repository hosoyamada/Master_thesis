program main
  implicit none
  integer, parameter :: NT=3
  integer, parameter :: NP=4
  integer, parameter :: LAT=4
  integer, parameter :: LON=3
  integer :: j,k
  real :: f_raw(NT,NP) = reshape( (/1.,2.,3.,4.,5.,6.,7.,8.,9.,10.,11.,12./), shape(f_raw))
  real :: f(LAT,LON)

  do k = 1, NP
    do j = 1, NT
      print *, "f(",j,",",k,")=", f_raw(j,k)
    end do
  end do

  call liner_interpolation(f_raw,f)

  do k = 1, LON
    do j = 1, LAT
      print *, "f(",j,",",k,")=", f(j,k)
    end do
  end do

  contains

  subroutine liner_interpolation(f_,f)
    real, intent(in)  :: f_(:,:)
    real, intent(out) :: f(:,:)
    real, parameter :: PI=3.14159265
    integer :: j_,k_,j,k,count_j,count_k
    integer :: north,south,west,east
    real :: dt_,dp_,dt,dp
    real :: theta, phi
    real :: theta_1, phi_1, theta_2, phi_2
    real :: alpha, beta

    dt_ = PI/(NT-1)
    dp_ = 2*PI/(NP-1)
    dt = PI/(LAT-1)
    dp = 2*PI/(LON-1)
    count_j = 1
    count_k = 1

    do j=1, LAT
      do k=1, LON
        theta = dt*real(j-1)
        phi   = dp*real(k-1)
        north=-1
        west=-1
        ! find neighbors
        do j_ = 1, NT
          theta_1 = dt_*real(j_-1)
          theta_2 = dt_*real(j_)
          if(theta_1 <= theta .and. &
             theta <= theta_2) then
             alpha = (theta-theta_1)/dt_
             north=j_
             south=j_+1
             exit
          end if
        end do
        do k_ = 1, NP
          phi_1 = dp_*real(k_-1)
          phi_2 = dp_*real(k_)
          if(phi_1 <= phi .and. &
             phi <= phi_2) then
             beta = (phi-phi_1)/dp_
             west=k_
             east=k_+1
             exit
          end if
        end do
        if(north==-1 .or. west==-1) then
          print *,"error: Not find neighbors"
        end if
        !print *, "LAT=", j, "LON=", k
        !print *, "alpha=", alpha, "beta=", beta
        !print *, "north=", north
        !print *, "south=", south
        !print *, "west=", west
        !print *, "east=", east
        ! caluculate f
        f(j,k)=f_(north, west)*(1-alpha)*(1-beta) &
              +f_(south, west)*alpha*(1-beta)     &
              +f_(north, east)*(1-alpha)*beta     &
              +f_(south, east)*alpha*beta
      end do
    end do
  end subroutine liner_interpolation
end program main
