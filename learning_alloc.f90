MODULE parameters
IMPLICIT NONE

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!
! In this module I declare all variables of interest for the completion of the program.
!
! 1st Block. Parameters and discount factors are here.
!
! 2nd Block. Dimensions are detailed here. The choice set, the parameter space dimension, the number of
! points in the ratings grid, the number of weeks and the number of types are here. The first
! type is for those with a full season in the first season. The second type is for mid season
! replacement. (Type=1,2,...J)
! "total_markov" accounts for how many lines have to be read from the Markov Chain txt
!
! 3rd Block. Tools are here. There are three indices (i,j,k), an epsilon and its inverse (divisions are 
! slower in Fortran), and the maximum number of iterations (set to 2000 as actually the max.
! is reached quite fast).
!
! 4rd Block. Markov chain related variables are specified here. 
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!


!INTEGER, PARAMETER :: dp = selected_real_kind(33, 4931)
INTEGER, PARAMETER :: dp = 8!selected_real_kind(18, 4931) !!With 33 it gives problems of memory.


!!!!!!!!! Parameters
REAL(kind=dp), PARAMETER :: theta_rev_1=9.0, theta_rev_2=-0.3, theta_rev_3=0.00     !Revenues
REAL(kind=dp), PARAMETER :: theta_rev_1_own=9.0, theta_rev_2_own=-0.3, theta_rev_3_own=0.00  !Revenues
REAL(kind=dp), PARAMETER :: opp_cost_1=35.0, opp_cost_2=0.0                 !Opp. cost
REAL(kind=dp), PARAMETER :: opp_cost_1_own=35.0, opp_cost_2_own=0.0                 !Opp. cost
REAL(kind=dp), PARAMETER :: theta_sunk_s1=16.0,  theta_sunk_s1_mid=12.0             !Sunk cost
REAL(kind=dp), PARAMETER :: theta_sunk_s1_own=16.0,  theta_sunk_s1_mid_own=12.0          !Sunk cost
REAL(kind=dp), PARAMETER :: beta=0.99 !Discounts, beta season is beta^12weeks
REAL(kind=dp), PARAMETER :: eulerm=0.57721566490153286
!!!!!!!!! Dimensions
INTEGER, PARAMETER :: dim_choice=2
INTEGER, PARAMETER :: dim_param=6
INTEGER, PARAMETER :: dim_types=6
INTEGER, PARAMETER :: dim_ratings=50 !Going from 0.1 to 5.5. Index=Rating*10-4
INTEGER, PARAMETER :: sim_obs=400
INTEGER, PARAMETER :: max_weeks=154 !22*7=154
INTEGER, PARAMETER :: max_weeks_mid=max_weeks-10
INTEGER, PARAMETER :: max_weeks_qrt=max_weeks-16 
!!!!!!!!! Tools
INTEGER :: g, h, i, j, k !Indices
REAL(kind=dp), PARAMETER :: epsil=10E-6, epsil_2=10E-7, conver_criterion=10E-7
REAL(kind=dp), PARAMETER :: epsil_inv=1.0/epsil, epsil_inv_2=1.0/epsil_2
INTEGER, PARAMETER :: maxIter=2000

!!!!!!!!!Montecarlo
CHARACTER(len=20) :: sample_file
INTEGER, PARAMETER :: samples=1 !150
INTEGER, DIMENSION(samples) ::total_obs



END MODULE parameters

MODULE functions
USE parameters
IMPLICIT NONE
CONTAINS

FUNCTION curr_per_util_full(a,week,rat,theta) !!a takes values 1 or 2 (keep or cancel)
REAL(kind=dp) :: curr_per_util_full 
REAL(kind=dp), DIMENSION(dim_param) :: theta
INTEGER :: a, week, rat, season

season=week/22

IF (a==1) THEN

IF ((week==22) .OR. (week==44) .OR. (week==66) .OR. (week==88) .OR. (week==110) .OR. (week==132)) THEN
curr_per_util_full=(beta**12)*(theta(1)*(rat/10.0)) - theta(3) 
ELSE
curr_per_util_full=theta(1)*(rat/10.0)
END IF

ELSE IF (a==2) THEN

IF ((week==22) .OR. (week==44) .OR. (week==66) .OR. (week==88) .OR. (week==110) .OR. (week==132)) THEN
curr_per_util_full=(beta**12)*theta(2) 
ELSE
curr_per_util_full=theta(2) 
END IF

ELSE
PRINT *, "The choice you input is not in the choice set!"
END IF

END FUNCTION

FUNCTION curr_per_util_mid(a,week,rat,theta) 
REAL(kind=dp) :: curr_per_util_mid 
REAL(kind=dp), DIMENSION(dim_param) :: theta
INTEGER :: a, week, rat, season

season=week/22

IF (a==1) THEN

IF ((week==12) .OR. (week==34) .OR. (week==56) .OR. (week==78) .OR. (week==100) .OR. (week==122)) THEN
curr_per_util_mid=(beta**12)*(theta(1)*(rat/10.0)) - theta(3)
ELSE
curr_per_util_mid=theta(1)*(rat/10.0)
END IF

ELSE IF (a==2) THEN
IF ((week==12) .OR. (week==34) .OR. (week==56) .OR. (week==78) .OR. (week==100) .OR. (week==122)) THEN
curr_per_util_mid=(beta**12)*theta(2) 
ELSE
curr_per_util_mid=theta(2) 
END IF
ELSE
PRINT *, "The choice you input is not in the choice set!"
END IF

END FUNCTION

FUNCTION curr_per_util_qrt(a,week,rat,theta) 
REAL(kind=dp) :: curr_per_util_qrt
REAL(kind=dp), DIMENSION(dim_param) :: theta
INTEGER :: a, week, rat, season

season=week/22

IF (a==1) THEN

IF ((week==6) .OR. (week==28) .OR. (week==50) .OR. (week==72) .OR. (week==94) .OR. (week==116)) THEN
curr_per_util_qrt=(beta**12)*(theta(1)*(rat/10.0)) - theta(3)
ELSE
curr_per_util_qrt=theta(1)*(rat/10.0)
END IF

ELSE IF (a==2) THEN

IF ((week==6) .OR. (week==28) .OR. (week==50) .OR. (week==72) .OR. (week==94) .OR. (week==116)) THEN
curr_per_util_qrt=(beta**12)*theta(2) 
ELSE
curr_per_util_qrt=theta(2) 
END IF

ELSE
PRINT *, "The choice you input is not in the choice set!"
END IF

END FUNCTION

FUNCTION curr_per_util_full_own(a,week,rat,theta) !!a takes values 1 or 2 (keep or cancel)
REAL(kind=dp) :: curr_per_util_full_own
REAL(kind=dp), DIMENSION(dim_param) :: theta
INTEGER :: a, week, rat, season

season=week/22

IF (a==1) THEN

IF ((week==22) .OR. (week==44) .OR. (week==66) .OR. (week==88) .OR. (week==110) .OR. (week==132)) THEN
curr_per_util_full_own=(beta**12)*(theta(4)*(rat/10.0)) - theta(6) 
ELSE
curr_per_util_full_own=theta(4)*(rat/10.0)
END IF

ELSE IF (a==2) THEN
IF ((week==22) .OR. (week==44) .OR. (week==66) .OR. (week==88) .OR. (week==110) .OR. (week==132)) THEN
curr_per_util_full_own=(beta**12)*theta(5) 
ELSE

END IF
ELSE
PRINT *, "The choice you input is not in the choice set!"
END IF

END FUNCTION

FUNCTION curr_per_util_mid_own(a,week,rat,theta) 
REAL(kind=dp) :: curr_per_util_mid_own
REAL(kind=dp), DIMENSION(dim_param) :: theta
INTEGER :: a, week, rat, season

season=week/22

IF (a==1) THEN

IF ((week==12) .OR. (week==34) .OR. (week==56) .OR. (week==78) .OR. (week==100) .OR. (week==122)) THEN
curr_per_util_mid_own=(beta**12)*(theta(4)*(rat/10.0)) - theta(6)
ELSE
curr_per_util_mid_own=theta(4)*(rat/10.0)
END IF

ELSE IF (a==2) THEN
IF ((week==12) .OR. (week==34) .OR. (week==56) .OR. (week==78) .OR. (week==100) .OR. (week==122)) THEN
curr_per_util_mid_own=(beta**12)*theta(5) 

ELSE
curr_per_util_mid_own=theta(5) 
END IF

ELSE
PRINT *, "The choice you input is not in the choice set!"
END IF

END FUNCTION

FUNCTION curr_per_util_qrt_own(a,week,rat,theta) 
REAL(kind=dp) :: curr_per_util_qrt_own
REAL(kind=dp), DIMENSION(dim_param) :: theta
INTEGER :: a, week, rat, season

season=week/22

IF (a==1) THEN

IF ((week==6) .OR. (week==28) .OR. (week==50) .OR. (week==72) .OR. (week==94) .OR. (week==116)) THEN
curr_per_util_qrt_own=(beta**12)*(theta(4)*(rat/10.0)) - theta(6)
ELSE
curr_per_util_qrt_own=theta(4)*(rat/10.0)
END IF

ELSE IF (a==2) THEN
IF ((week==6) .OR. (week==28) .OR. (week==50) .OR. (week==72) .OR. (week==94) .OR. (week==116)) THEN
curr_per_util_qrt_own=(beta**12)*theta(5) 
ELSE
curr_per_util_qrt_own=theta(5) 
END IF

ELSE
PRINT *, "The choice you input is not in the choice set!"
END IF

END FUNCTION

END MODULE functions

MODULE subroutines
USE functions
IMPLICIT NONE
CONTAINS

SUBROUTINE gauss(a,b,x,n)
! Solutions to a system of linear equations A*x=b
! Method: the basic elimination (simple Gauss elimination)
! the original arrays a(n,n) and b(n) will be destroyed
! during the calculation

IMPLICIT NONE
INTEGER :: n
REAL(kind=dp) :: a(n,n), b(n), x(n)
REAL(kind=dp) :: c
INTEGER :: ii, ij, ik
!step 1: forward elimination
DO ik=1, n-1
DO ii=ik+1,n
c=a(ii,ik)/a(ik,ik)
a(ii,ik) = 0.0
b(ii)=b(ii)- c*b(ik)
DO ij=ik+1,n
a(ii,ij) = a(ii,ij)-c*a(ik,ij)
END DO
END DO
END DO
!step 2: back substitution
x(n) = b(n)/a(n,n)
DO ii=n-1,1,-1
c=0.0
DO ij=ii+1,n
c= c + a(ii,ij)*x(ij)
END DO
x(ii) = (b(ii)- c)/a(ii,ii)
END DO
END SUBROUTINE gauss

SUBROUTINE gen_epsilon(values)  !Maybe it doesn't need to be corrected for types.
!!This function generates draw values from the Gumbel Extreme Value Type I distribution.
REAL(kind=dp), DIMENSION(:,:,:), INTENT(OUT) :: values
INTEGER :: index_epsi, index_epsi_2, index_epsi_3
REAL(kind=dp), DIMENSION(SIZE(values,1),SIZE(values,2),SIZE(values,3)) :: realizations
INTEGER, DIMENSION(SIZE(values)) :: seed

CALL RANDOM_SEED(get=seed)
DO index_epsi_3=1,SIZE(values,3)
DO index_epsi_2=1,SIZE(values,2)
DO index_epsi=1,SIZE(values,1)
CALL RANDOM_NUMBER(realizations(index_epsi,index_epsi_2,index_epsi_3))
!values(index_epsi,index_epsi_2,index_epsi_3) = LOG(-LOG(1-realizations(index_epsi,index_epsi_2,index_epsi_3))) + eulerm
values(index_epsi,index_epsi_2,index_epsi_3) = -(LOG(-LOG(realizations(index_epsi,index_epsi_2,index_epsi_3))) + eulerm)
END DO
END DO
END DO

END SUBROUTINE gen_epsilon

SUBROUTINE gen_paths(initial_values,transition_matrix,transition_mean_matrix,transition_prior_matrix,&
&choice_specific_vf,sample_file,total_obs)
INTEGER, DIMENSION(:,:), INTENT(IN) :: initial_values
REAL(kind=dp), DIMENSION(:,:,:,:,:), INTENT(IN) :: transition_matrix, transition_mean_matrix,transition_prior_matrix
REAL(kind=dp), DIMENSION(:,:,:,:,:,:), INTENT(IN) :: choice_specific_vf
INTEGER ::l,m,n,o,p
REAL(kind=dp), DIMENSION(:,:,:), ALLOCATABLE :: values
!REAL(kind=dp), DIMENSION(dim_ratings,max_weeks-1) :: exp_emax
REAL(kind=dp) :: aux2, aux3,aux4,aux5
REAL(kind=dp) :: aux6, aux7, aux8,aux9,aux10,aux11,aux12,aux13
INTEGER, DIMENSION(SIZE(initial_values)*max_weeks) :: seed
REAL(kind=dp), DIMENSION(:,:,:,:,:), ALLOCATABLE :: aux_cdf, aux_cdf_mean, aux_cdf_prior
REAL(kind=dp), DIMENSION(:,:), ALLOCATABLE :: realizations,realizations_mean
INTEGER, DIMENSION(SIZE(initial_values,1),max_weeks-1) :: rat_values, mean_values,prior_values
INTEGER, DIMENSION(SIZE(initial_values,1)) :: types
!!!!!
INTEGER, DIMENSION(max_weeks-1) :: cancels_week, obs_week
CHARACTER(len=20) :: sample_file
INTEGER, INTENT(OUT) :: total_obs

ALLOCATE(values(sim_obs,max_weeks-1,dim_choice))
ALLOCATE(aux_cdf(dim_ratings,dim_ratings,dim_ratings,max_weeks-1,dim_types))
ALLOCATE(aux_cdf_mean(dim_ratings,dim_ratings,dim_ratings,max_weeks-1,dim_types))
ALLOCATE(aux_cdf_prior(dim_ratings,dim_ratings,dim_ratings,max_weeks-1,dim_types))
ALLOCATE(realizations(SIZE(initial_values,1),max_weeks-1))
ALLOCATE(realizations_mean(SIZE(initial_values,1),max_weeks-1))



!!!!!!!!!!!!
CALL gen_epsilon(values)
CALL RANDOM_SEED(get=seed)


!!!!
DO l=1,max_weeks-1
DO p=1,dim_ratings
DO m=1,dim_ratings
aux2=0.0
aux3=0.0
aux6=0.0
aux7=0.0
aux8=0.0
aux9=0.0
DO n=1,dim_ratings
aux2=aux2+transition_matrix(n,m,p,l,1)
aux3=aux3+transition_matrix(n,m,p,l,2)
aux6=aux6+transition_matrix(n,m,p,l,3)
aux7=aux7+transition_matrix(n,m,p,l,4)
aux8=aux8+transition_matrix(n,m,p,l,5)
aux9=aux9+transition_matrix(n,m,p,l,6)
aux_cdf(n,m,p,l,1)=aux2
aux_cdf(n,m,p,l,2)=aux3
aux_cdf(n,m,p,l,3)=aux6
aux_cdf(n,m,p,l,4)=aux7
aux_cdf(n,m,p,l,5)=aux8
aux_cdf(n,m,p,l,6)=aux9

aux4=0.0
aux5=0.0
aux10=0.0
aux11=0.0
aux12=0.0
aux13=0.0
DO o=1,dim_ratings
aux4=aux4+transition_mean_matrix(o,n,m,l,1)
aux5=aux5+transition_mean_matrix(o,n,m,l,2)
aux10=aux10+transition_mean_matrix(o,n,m,l,3)
aux11=aux11+transition_mean_matrix(o,n,m,l,4)
aux12=aux12+transition_mean_matrix(o,n,m,l,5)
aux13=aux13+transition_mean_matrix(o,n,m,l,6)

aux_cdf_mean(o,n,m,l,1)=aux4
aux_cdf_mean(o,n,m,l,2)=aux5
aux_cdf_mean(o,n,m,l,3)=aux10
aux_cdf_mean(o,n,m,l,4)=aux11
aux_cdf_mean(o,n,m,l,5)=aux12
aux_cdf_mean(o,n,m,l,6)=aux13
END DO
END DO

aux2=0.0
aux3=0.0
aux6=0.0
aux7=0.0
aux8=0.0
aux9=0.0

DO n=1,dim_ratings
aux2=aux2+transition_prior_matrix(n,m,p,l,1)
aux3=aux3+transition_prior_matrix(n,m,p,l,2)
aux6=aux6+transition_prior_matrix(n,m,p,l,3)
aux7=aux7+transition_prior_matrix(n,m,p,l,4)
aux8=aux8+transition_prior_matrix(n,m,p,l,5)
aux9=aux9+transition_prior_matrix(n,m,p,l,6)
aux_cdf_prior(n,m,p,l,1)=aux2
aux_cdf_prior(n,m,p,l,2)=aux3
aux_cdf_prior(n,m,p,l,3)=aux6
aux_cdf_prior(n,m,p,l,4)=aux7
aux_cdf_prior(n,m,p,l,5)=aux8
aux_cdf_prior(n,m,p,l,6)=aux9
END DO

END DO
END DO
END DO

!OPEN(UNIT=35, FILE="AUX_CDF.txt")
!DO l=1,max_weeks-1
!DO m=1,dim_ratings
!DO n=1,dim_ratings
!WRITE(35,"(I3,X,I2,X,I2,X,F15.10)") l,m,n,aux_cdf(n,m,l,1)
!END DO
!END DO
!END DO
!CLOSE(UNIT=35)

!!Generate the full story of ratings
DO m=1,SIZE(initial_values,1)
rat_values(m,1)=initial_values(m,1)
mean_values(m,1)=initial_values(m,1)
types(m)=initial_values(m,2)
prior_values(m,1)=7  !!! The very same one I have in the computation of the transition matrices
END DO

DO l=2,max_weeks-1
DO m=1,SIZE(initial_values,1)

CALL RANDOM_NUMBER(realizations(m,l))
CALL RANDOM_NUMBER(realizations_mean(m,l))
!I could call a random number for the prior. But given it's degenerate I'll just
!use the realization of the posterior

DO n=1,dim_ratings
!PRINT *, n,mean_values(m,l-1),prior_values(m,l-1),l-1,types(m)
IF (realizations(m,l)<aux_cdf(n,mean_values(m,l-1),prior_values(m,l-1),l-1,types(m))) THEN
rat_values(m,l)=n
EXIT
END IF
IF (n==dim_ratings) rat_values(m,l)=dim_ratings
END DO


DO o=1,dim_ratings
IF (realizations_mean(m,l)<aux_cdf_mean(o,rat_values(m,l),mean_values(m,l-1),l-1,types(m))) THEN
mean_values(m,l)=o
EXIT
END IF
IF (o==dim_ratings) mean_values(m,l)=dim_ratings
END DO

DO o=1,dim_ratings
IF (realizations_mean(m,l)<aux_cdf_prior(o,mean_values(m,l-1),prior_values(m,l-1),l-1,types(m))) THEN
prior_values(m,l)=o
EXIT
END IF
IF (o==dim_ratings) prior_values(m,l)=dim_ratings
END DO

END DO
END DO

cancels_week=0
obs_week=0


OPEN(UNIT=12,FILE=sample_file)
DO l=1,SIZE(initial_values,1)
DO m=1,max_weeks-1


IF (types(l)==1 .OR. types(l)==4) THEN
obs_week(m)=obs_week(m)+1


IF (choice_specific_vf(rat_values(l,m),mean_values(l,m),prior_values(l,m),1,m,types(l))+values(l,m,1)>&
&choice_specific_vf(rat_values(l,m),mean_values(l,m),prior_values(l,m),2,m,types(l))+values(l,m,2)) THEN
WRITE(12,"(I4,X,I3,X,I2,X,I2,X,I2,X,I1,X,I1)") l, m, rat_values(l,m), mean_values(l,m),prior_values(l,m), 0, types(l)
ELSE
WRITE(12,"(I4,X,I3,X,I2,X,I2,X,I2,X,I1,X,I1)") l, m, rat_values(l,m), mean_values(l,m),prior_values(l,m), 1, types(l)
cancels_week(m)=cancels_week(m)+1
EXIT
END IF 

ELSE IF ((types(l)==2 .OR. types(l)==5) .AND. (m.LE.max_weeks_mid-1)) THEN
obs_week(m)=obs_week(m)+1

IF (choice_specific_vf(rat_values(l,m),mean_values(l,m),prior_values(l,m),1,m,types(l))+values(l,m,1)>&
&choice_specific_vf(rat_values(l,m),mean_values(l,m),prior_values(l,m),2,m,types(l))+values(l,m,2)) THEN
WRITE(12,"(I4,X,I3,X,I2,X,I2,X,I2,X,I1,X,I1)") l, m, rat_values(l,m), mean_values(l,m),prior_values(l,m), 0, types(l)
ELSE
WRITE(12,"(I4,X,I3,X,I2,X,I2,X,I2,X,I1,X,I1)") l, m, rat_values(l,m), mean_values(l,m),prior_values(l,m), 1, types(l)
cancels_week(m)=cancels_week(m)+1
EXIT
END IF 

ELSE IF ((types(l)==3 .OR. types(l)==6) .AND. (m.LE.max_weeks_qrt-1)) THEN
obs_week(m)=obs_week(m)+1

IF (choice_specific_vf(rat_values(l,m),mean_values(l,m),prior_values(l,m),1,m,types(l))+values(l,m,1)>&
&choice_specific_vf(rat_values(l,m),mean_values(l,m),prior_values(l,m),2,m,types(l))+values(l,m,2)) THEN
WRITE(12,"(I4,X,I3,X,I2,X,I2,X,I2,X,I1,X,I1)") l, m, rat_values(l,m), mean_values(l,m),prior_values(l,m), 0, types(l)
ELSE
WRITE(12,"(I4,X,I3,X,I2,X,I2,X,I2,X,I1,X,I1)") l, m, rat_values(l,m), mean_values(l,m),prior_values(l,m), 1, types(l)
cancels_week(m)=cancels_week(m)+1
EXIT
END IF 

END IF

END DO
END DO

CLOSE(UNIT=12)

total_obs=SUM(obs_week)


DO l=1,140
PRINT *, l, cancels_week(l), obs_week(l)
END DO



DEALLOCATE(values)
DEALLOCATE(aux_cdf)
DEALLOCATE(aux_cdf_mean)
DEALLOCATE(aux_cdf_prior)
DEALLOCATE(realizations)
DEALLOCATE(realizations_mean)

END SUBROUTINE gen_paths

SUBROUTINE gen_initial_values(initial_conditions,initial_probabilities,type_prob)
REAL(kind=dp), DIMENSION(:), INTENT(IN) :: type_prob
REAL(kind=dp), DIMENSION(:), INTENT(IN) :: initial_probabilities
INTEGER, DIMENSION(:,:), INTENT(INOUT) :: initial_conditions
REAL(kind=dp), DIMENSION(SIZE(initial_probabilities)) :: aux_cdf
REAL(kind=dp), DIMENSION(dim_types) :: aux_cdf_types
REAL(kind=dp) :: aux_values, realizations(SIZE(initial_conditions,1)), realizations_2(SIZE(initial_conditions,1))
INTEGER :: l,m !,n
INTEGER, DIMENSION(dim_types*SIZE(initial_conditions,1)) :: seed

CALL RANDOM_SEED(get=seed)

!!!!GENERATING TYPES!!!
aux_values=0.0
DO l=1,dim_types
aux_values=aux_values+type_prob(l)
aux_cdf_types(l)=aux_values
END DO

DO l=1,SIZE(initial_conditions,1)
CALL RANDOM_NUMBER(realizations_2(l))
DO m=1,dim_types
IF (realizations_2(l)<aux_cdf_types(m)) THEN
initial_conditions(l,2)=m
EXIT
END IF
END DO
END DO

!!!!GENERATING DRAWS FROM REALIZATIONS!!!
aux_values=0.0
DO l=1,SIZE(initial_probabilities)
aux_values=aux_values+initial_probabilities(l)
aux_cdf(l)=aux_values
END DO

DO l=1,SIZE(initial_conditions,1)
CALL RANDOM_NUMBER(realizations(l))
DO m=1,SIZE(initial_probabilities)
IF (realizations(l)<aux_cdf(m)) THEN
initial_conditions(l,1)=m
EXIT
END IF
END DO
END DO


END SUBROUTINE gen_initial_values

SUBROUTINE compute_value_functions(transition_matrix,transition_mean_matrix,transition_prior_matrix,choice_specific_vf,theta,ccp_0)
REAL(kind=dp), DIMENSION(:,:,:,:), INTENT(IN) :: transition_matrix, transition_mean_matrix,transition_prior_matrix
REAL(kind=dp), DIMENSION(:,:,:,:,:), INTENT(INOUT) :: choice_specific_vf
REAL(kind=dp), DIMENSION(:,:,:,:), ALLOCATABLE :: emax_vf
REAL(kind=dp), DIMENSION(:,:,:,:), INTENT(INOUT) :: ccp_0
REAL(kind=dp) :: aux_f, aux_g, aux_t!, aux_c
REAL(kind=dp), DIMENSION(dim_param), INTENT(IN) :: theta
INTEGER :: l,m,n,o,p,q

ALLOCATE(emax_vf(dim_ratings,dim_ratings,dim_ratings,max_weeks))

choice_specific_vf=0.0
emax_vf=0.0
!!
!
!Here there should be a loop for the last period if the choice specific value is not zero.
!
!!
DO o=1,dim_ratings
DO n=1,dim_ratings
DO m=1,dim_ratings
emax_vf(m,n,o,max_weeks)=curr_per_util_full(2,max_weeks,m,theta)
END DO
END DO
END DO

DO l=max_weeks-1,1,-1
DO p=1,dim_ratings !Current prior
DO m=1,dim_ratings !Current mean
aux_t=0.0

DO q=1,dim_ratings !Future prior
DO o=1,dim_ratings !Future mean
DO n=1,dim_ratings !Future value
aux_t=aux_t+transition_matrix(n,m,p,l)*transition_mean_matrix(o,n,m,l)*transition_prior_matrix(q,m,p,l)*emax_vf(n,o,q,l+1)
END DO
END DO
END DO

DO n=1,dim_ratings !Current ratings
choice_specific_vf(n,m,p,2,l)=curr_per_util_full(2,l,n,theta)
choice_specific_vf(n,m,p,1,l)=curr_per_util_full(1,l,n,theta)+beta*aux_t
aux_f=0.0

!aux_f=EXP(choice_specific_vf(n,m,p,1,l))
!aux_g=aux_f+EXP(choice_specific_vf(n,m,p,2,l))
!CCP_0(n,m,p,l)=aux_f/aux_g
aux_f=EXP(choice_specific_vf(n,m,p,2,l)-choice_specific_vf(n,m,p,1,l))
emax_vf(n,m,p,l)=LOG(1.0+aux_f)+choice_specific_vf(n,m,p,1,l)
CCP_0(n,m,p,l)=1.0/(1.0+aux_f)
END DO
END DO
END DO
END DO

!~OPEN(UNIT=40,FILE="CSVFFULL.txt")
!~DO l=1,max_weeks-1
!~DO m=1,dim_ratings
!~DO n=1,dim_ratings
!~WRITE(40,"(I3,X,I2,X,I2,X,A,X,F16.10,X,A,X,F16.10)") l,m,n, "CSVF 1.", choice_specific_vf(n,m,1,l), &
!~&"CSVF 2. ", choice_specific_vf(n,m,2,l)
!~END DO
!~END DO
!~END DO
!~CLOSE(UNIT=40)

DEALLOCATE(emax_vf)

END SUBROUTINE compute_value_functions

SUBROUTINE compute_value_functions_mid(transition_matrix,transition_mean_matrix,transition_prior_matrix,&
&choice_specific_vf,theta,ccp_0)
REAL(kind=dp), DIMENSION(:,:,:,:), INTENT(IN) :: transition_matrix, transition_mean_matrix,transition_prior_matrix
REAL(kind=dp), DIMENSION(:,:,:,:,:), INTENT(INOUT) :: choice_specific_vf
REAL(kind=dp), DIMENSION(:,:,:,:), ALLOCATABLE :: emax_vf
REAL(kind=dp), DIMENSION(:,:,:,:), INTENT(INOUT) :: ccp_0
REAL(kind=dp) :: aux_f, aux_g, aux_t
REAL(kind=dp), DIMENSION(dim_param), INTENT(IN) :: theta
INTEGER :: l,m,n,o,p,q

ALLOCATE(emax_vf(dim_ratings,dim_ratings,dim_ratings,max_weeks_mid))

choice_specific_vf=0.0
emax_vf=0.0

DO o=1,dim_ratings
DO n=1,dim_ratings
DO m=1,dim_ratings
emax_vf(m,n,o,max_weeks_mid)=curr_per_util_mid(2,max_weeks_mid,m,theta)
END DO
END DO
END DO

DO l=max_weeks_mid-1,1,-1
DO p=1,dim_ratings !Current prior
DO m=1,dim_ratings !Current mean
aux_t=0.0

DO q=1,dim_ratings !Future prior
DO o=1,dim_ratings !Future mean
DO n=1,dim_ratings !Future value
aux_t=aux_t+transition_matrix(n,m,p,l)*transition_mean_matrix(o,n,m,l)*transition_prior_matrix(q,m,p,l)*emax_vf(n,o,q,l+1)
END DO
END DO
END DO

DO n=1,dim_ratings !Current ratings
choice_specific_vf(n,m,p,2,l)=curr_per_util_mid(2,l,n,theta)
choice_specific_vf(n,m,p,1,l)=curr_per_util_mid(1,l,n,theta)+beta*aux_t
aux_f=0.0
!aux_f=EXP(choice_specific_vf(n,m,p,1,l))
!aux_g=aux_f+EXP(choice_specific_vf(n,m,p,2,l))
!emax_vf(n,m,p,l)=LOG(aux_g)
!CCP_0(n,m,p,l)=aux_f/aux_g
aux_f=EXP(choice_specific_vf(n,m,p,2,l)-choice_specific_vf(n,m,p,1,l))
emax_vf(n,m,p,l)=LOG(1.0+aux_f)+choice_specific_vf(n,m,p,1,l)
CCP_0(n,m,p,l)=1.0/(1.0+aux_f)

END DO
END DO
END DO
END DO

!~OPEN(UNIT=40,FILE="CSVFMID.txt")
!~DO l=1,max_weeks_mid-1
!~DO m=1,dim_ratings
!~DO n=1,dim_ratings
!~WRITE(40,"(I3,X,I2,X,I2,X,A,X,F16.10,X,A,X,F16.10)") l,m,n, "CSVF 1.", choice_specific_vf(n,m,1,l), &
!~&"CSVF 2.", choice_specific_vf(n,m,2,l)
!~END DO
!~END DO
!~END DO
!~CLOSE(UNIT=40)

DEALLOCATE(emax_vf)

END SUBROUTINE compute_value_functions_mid

SUBROUTINE compute_value_functions_qrt(transition_matrix,transition_mean_matrix,transition_prior_matrix,&
&choice_specific_vf,theta,ccp_0)
REAL(kind=dp), DIMENSION(:,:,:,:), INTENT(IN) :: transition_matrix, transition_mean_matrix,transition_prior_matrix
REAL(kind=dp), DIMENSION(:,:,:,:,:), INTENT(INOUT) :: choice_specific_vf
REAL(kind=dp), DIMENSION(:,:,:,:), ALLOCATABLE :: emax_vf
REAL(kind=dp), DIMENSION(:,:,:,:), INTENT(INOUT) :: ccp_0
REAL(kind=dp) :: aux_f, aux_g, aux_t
REAL(kind=dp), DIMENSION(dim_param), INTENT(IN) :: theta
INTEGER :: l,m,n,o,p,q

ALLOCATE(emax_vf(dim_ratings,dim_ratings,dim_ratings,max_weeks))

choice_specific_vf=0.0
emax_vf=0.0

DO o=1,dim_ratings
DO n=1,dim_ratings
DO m=1,dim_ratings
emax_vf(m,n,o,max_weeks_qrt)=curr_per_util_qrt(2,max_weeks_qrt,m,theta)
END DO
END DO
END DO

DO l=max_weeks_qrt-1,1,-1
DO p=1,dim_ratings !Current prior
DO m=1,dim_ratings !Current mean
aux_t=0.0

DO q=1,dim_ratings !Future prior
DO o=1,dim_ratings !Future mean
DO n=1,dim_ratings !Future value
aux_t=aux_t+transition_matrix(n,m,p,l)*transition_mean_matrix(o,n,m,l)*transition_prior_matrix(q,m,p,l)*emax_vf(n,o,q,l+1)
END DO
END DO
END DO

DO n=1,dim_ratings !Current ratings
choice_specific_vf(n,m,p,2,l)=curr_per_util_qrt(2,l,n,theta)
choice_specific_vf(n,m,p,1,l)=curr_per_util_qrt(1,l,n,theta)+beta*aux_t
aux_f=0.0
!aux_f=EXP(choice_specific_vf(n,m,p,1,l))
!aux_g=aux_f+EXP(choice_specific_vf(n,m,p,2,l))
!emax_vf(n,m,p,l)=LOG(aux_g)
!CCP_0(n,m,p,l)=aux_f/aux_g
aux_f=EXP(choice_specific_vf(n,m,p,2,l)-choice_specific_vf(n,m,p,1,l))
emax_vf(n,m,p,l)=LOG(1.0+aux_f)+choice_specific_vf(n,m,p,1,l)
CCP_0(n,m,p,l)=1.0/(1.0+aux_f)

END DO
END DO
END DO
END DO

!~OPEN(UNIT=40,FILE="CSVFMID.txt")
!~DO l=1,max_weeks_mid-1
!~DO m=1,dim_ratings
!~DO n=1,dim_ratings
!~WRITE(40,"(I3,X,I2,X,I2,X,A,X,F16.10,X,A,X,F16.10)") l,m,n, "CSVF 1.", choice_specific_vf(n,m,1,l), &
!~&"CSVF 2.", choice_specific_vf(n,m,2,l)
!~END DO
!~END DO
!~END DO
!~CLOSE(UNIT=40)

DEALLOCATE (emax_vf)

END SUBROUTINE compute_value_functions_qrt

SUBROUTINE compute_value_functions_own(transition_matrix,transition_mean_matrix,transition_prior_matrix,&
&choice_specific_vf,theta,ccp_0)
REAL(kind=dp), DIMENSION(:,:,:,:), INTENT(IN) :: transition_matrix, transition_mean_matrix,transition_prior_matrix
REAL(kind=dp), DIMENSION(:,:,:,:,:), INTENT(INOUT) :: choice_specific_vf
REAL(kind=dp), DIMENSION(:,:,:,:), ALLOCATABLE :: emax_vf
REAL(kind=dp), DIMENSION(:,:,:,:), INTENT(INOUT) :: ccp_0
REAL(kind=dp) :: aux_f, aux_g, aux_t!, aux_c
REAL(kind=dp), DIMENSION(dim_param), INTENT(IN) :: theta
INTEGER :: l,m,n,o,p,q

ALLOCATE(emax_vf(dim_ratings,dim_ratings,dim_ratings,max_weeks))

choice_specific_vf=0.0
emax_vf=0.0
!!
!
!Here there should be a loop for the last period if the choice specific value is not zero.
!
!!

DO o=1,dim_ratings
DO n=1,dim_ratings
DO m=1,dim_ratings
emax_vf(m,n,o,max_weeks)=curr_per_util_full_own(2,max_weeks,m,theta)
END DO
END DO
END DO

DO l=max_weeks-1,1,-1
DO p=1,dim_ratings !Current prior
DO m=1,dim_ratings !Current mean
aux_t=0.0

DO q=1,dim_ratings !Future prior
DO o=1,dim_ratings !Future mean
DO n=1,dim_ratings !Future value
aux_t=aux_t+transition_matrix(n,m,p,l)*transition_mean_matrix(o,n,m,l)*transition_prior_matrix(q,m,p,l)*emax_vf(n,o,q,l+1)
END DO
END DO
END DO

DO n=1,dim_ratings !Current ratings
choice_specific_vf(n,m,p,2,l)=curr_per_util_full_own(2,l,n,theta)
choice_specific_vf(n,m,p,1,l)=curr_per_util_full_own(1,l,n,theta)+beta*aux_t
aux_f=0.0
!aux_f=EXP(choice_specific_vf(n,m,p,1,l))
!aux_g=aux_f+EXP(choice_specific_vf(n,m,p,2,l))
!emax_vf(n,m,p,l)=LOG(aux_g)
!CCP_0(n,m,p,l)=aux_f/aux_g
aux_f=EXP(choice_specific_vf(n,m,p,2,l)-choice_specific_vf(n,m,p,1,l))
emax_vf(n,m,p,l)=LOG(1.0+aux_f)+choice_specific_vf(n,m,p,1,l)
CCP_0(n,m,p,l)=1.0/(1.0+aux_f)

END DO
END DO
END DO
END DO

!~OPEN(UNIT=40,FILE="CSVFFULL.txt")
!~DO l=1,max_weeks-1
!~DO m=1,dim_ratings
!~DO n=1,dim_ratings
!~WRITE(40,"(I3,X,I2,X,I2,X,A,X,F16.10,X,A,X,F16.10)") l,m,n, "CSVF 1.", choice_specific_vf(n,m,1,l), &
!~&"CSVF 2. ", choice_specific_vf(n,m,2,l)
!~END DO
!~END DO
!~END DO
!~CLOSE(UNIT=40)

DEALLOCATE(emax_vf)

END SUBROUTINE compute_value_functions_own

SUBROUTINE compute_value_functions_mid_own(transition_matrix,transition_mean_matrix,transition_prior_matrix,&
&choice_specific_vf,theta,ccp_0)
REAL(kind=dp), DIMENSION(:,:,:,:), INTENT(IN) :: transition_matrix, transition_mean_matrix,transition_prior_matrix
REAL(kind=dp), DIMENSION(:,:,:,:,:), INTENT(INOUT) :: choice_specific_vf
REAL(kind=dp), DIMENSION(:,:,:,:), ALLOCATABLE :: emax_vf
REAL(kind=dp), DIMENSION(:,:,:,:), INTENT(INOUT) :: ccp_0
REAL(kind=dp) :: aux_f, aux_g, aux_t
REAL(kind=dp), DIMENSION(dim_param), INTENT(IN) :: theta
INTEGER :: l,m,n,o,p,q


ALLOCATE(emax_vf(dim_ratings,dim_ratings,dim_ratings,max_weeks))
choice_specific_vf=0.0
emax_vf=0.0

DO o=1,dim_ratings
DO n=1,dim_ratings
DO m=1,dim_ratings
emax_vf(m,n,o,max_weeks_mid)=curr_per_util_mid_own(2,max_weeks_mid,m,theta)
END DO
END DO
END DO

DO l=max_weeks_mid-1,1,-1
DO p=1,dim_ratings !Current prior
DO m=1,dim_ratings !Current mean
aux_t=0.0

DO q=1,dim_ratings !Future prior
DO o=1,dim_ratings !Future mean
DO n=1,dim_ratings !Future value
aux_t=aux_t+transition_matrix(n,m,p,l)*transition_mean_matrix(o,n,m,l)*transition_prior_matrix(q,m,p,l)*emax_vf(n,o,q,l+1)
END DO
END DO
END DO

DO n=1,dim_ratings !Current ratings
choice_specific_vf(n,m,p,2,l)=curr_per_util_mid_own(2,l,n,theta)
choice_specific_vf(n,m,p,1,l)=curr_per_util_mid_own(1,l,n,theta)+beta*aux_t
aux_f=0.0
!aux_f=EXP(choice_specific_vf(n,m,p,1,l))
!aux_g=aux_f+EXP(choice_specific_vf(n,m,p,2,l))
!emax_vf(n,m,p,l)=LOG(aux_g)
!CCP_0(n,m,p,l)=aux_f/aux_g
aux_f=EXP(choice_specific_vf(n,m,p,2,l)-choice_specific_vf(n,m,p,1,l))
emax_vf(n,m,p,l)=LOG(1.0+aux_f)+choice_specific_vf(n,m,p,1,l)
CCP_0(n,m,p,l)=1.0/(1.0+aux_f)

END DO
END DO
END DO
END DO
DEALLOCATE(emax_vf)
!~OPEN(UNIT=40,FILE="CSVFMID.txt")
!~DO l=1,max_weeks_mid-1
!~DO m=1,dim_ratings
!~DO n=1,dim_ratings
!~WRITE(40,"(I3,X,I2,X,I2,X,A,X,F16.10,X,A,X,F16.10)") l,m,n, "CSVF 1.", choice_specific_vf(n,m,1,l), &
!~&"CSVF 2.", choice_specific_vf(n,m,2,l)
!~END DO
!~END DO
!~END DO
!~CLOSE(UNIT=40)

END SUBROUTINE compute_value_functions_mid_own

SUBROUTINE compute_value_functions_qrt_own(transition_matrix,transition_mean_matrix,transition_prior_matrix,&
&choice_specific_vf,theta,ccp_0)
REAL(kind=dp), DIMENSION(:,:,:,:), INTENT(IN) :: transition_matrix, transition_mean_matrix,transition_prior_matrix
REAL(kind=dp), DIMENSION(:,:,:,:,:), INTENT(INOUT) :: choice_specific_vf
REAL(kind=dp), DIMENSION(:,:,:,:), ALLOCATABLE :: emax_vf
REAL(kind=dp), DIMENSION(:,:,:,:), INTENT(INOUT) :: ccp_0
REAL(kind=dp) :: aux_f, aux_g, aux_t
REAL(kind=dp), DIMENSION(dim_param), INTENT(IN) :: theta
INTEGER :: l,m,n,o,p,q

ALLOCATE(emax_vf(dim_ratings,dim_ratings,dim_ratings,max_weeks))
choice_specific_vf=0.0
emax_vf=0.0

DO o=1,dim_ratings
DO n=1,dim_ratings
DO m=1,dim_ratings
emax_vf(m,n,o,max_weeks_qrt)=curr_per_util_qrt_own(2,max_weeks_qrt,m,theta)
END DO
END DO
END DO

DO l=max_weeks_qrt-1,1,-1
DO p=1,dim_ratings !Current prior
DO m=1,dim_ratings !Current mean
aux_t=0.0

DO q=1,dim_ratings !Future prior
DO o=1,dim_ratings !Future mean
DO n=1,dim_ratings !Future value
aux_t=aux_t+transition_matrix(n,m,p,l)*transition_mean_matrix(o,n,m,l)*transition_prior_matrix(q,m,p,l)*emax_vf(n,o,q,l+1)
END DO
END DO
END DO

DO n=1,dim_ratings !Current ratings
choice_specific_vf(n,m,p,2,l)=curr_per_util_qrt_own(2,l,n,theta)
choice_specific_vf(n,m,p,1,l)=curr_per_util_qrt_own(1,l,n,theta)+beta*aux_t
aux_f=0.0
!aux_f=EXP(choice_specific_vf(n,m,p,1,l))
!aux_g=aux_f+EXP(choice_specific_vf(n,m,p,2,l))
!emax_vf(n,m,p,l)=LOG(aux_g)
!CCP_0(n,m,p,l)=aux_f/aux_g
aux_f=EXP(choice_specific_vf(n,m,p,2,l)-choice_specific_vf(n,m,p,1,l))
emax_vf(n,m,p,l)=LOG(1.0+aux_f)+choice_specific_vf(n,m,p,1,l)
CCP_0(n,m,p,l)=1.0/(1.0+aux_f)

END DO
END DO
END DO
END DO

!~OPEN(UNIT=40,FILE="CSVFMID.txt")
!~DO l=1,max_weeks_mid-1
!~DO m=1,dim_ratings
!~DO n=1,dim_ratings
!~WRITE(40,"(I3,X,I2,X,I2,X,A,X,F16.10,X,A,X,F16.10)") l,m,n, "CSVF 1.", choice_specific_vf(n,m,1,l), &
!~&"CSVF 2.", choice_specific_vf(n,m,2,l)
!~END DO
!~END DO
!~END DO
!~CLOSE(UNIT=40)
DEALLOCATE(emax_vf)

END SUBROUTINE compute_value_functions_qrt_own

!!!!

SUBROUTINE BHHH_iteration(transition_matrix,transition_mean_matrix,transition_prior_matrix,&
&theta,sample_file,total_obs,log_likelihood,ccp_true) 
!Here the transition matrix include types
REAL(kind=dp), DIMENSION(:,:,:,:,:), INTENT(IN) :: transition_matrix, transition_mean_matrix, transition_prior_matrix
REAL(kind=dp), DIMENSION(:), INTENT(INOUT) :: theta
INTEGER, INTENT(IN) :: total_obs
CHARACTER(len=*), INTENT(IN) :: sample_file
REAL(kind=dp), INTENT(OUT) :: log_likelihood
REAL(kind=dp), DIMENSION(dim_ratings,dim_ratings,dim_ratings,max_weeks-1,dim_types), INTENT(OUT) :: ccp_true
!!!!!!!!!!!!!!
REAL(kind=dp), DIMENSION(SIZE(theta,1), SIZE(theta,1),2) :: theta_partial
!!!!!!!!!!!!!!
REAL(kind=dp), DIMENSION(:,:,:,:,:,:,:), ALLOCATABLE :: ccp 
!!!!!!!!!!!!!!
REAL(kind=dp), DIMENSION(:,:,:,:,:,:), ALLOCATABLE :: log_CCP_jacobian_1, log_CCP_jacobian_4
REAL(kind=dp), DIMENSION(:,:,:,:,:,:), ALLOCATABLE :: log_CCP_jacobian_2, log_CCP_jacobian_5
REAL(kind=dp), DIMENSION(:,:,:,:,:,:), ALLOCATABLE :: log_CCP_jacobian_3, log_CCP_jacobian_6
!!!!!!!!!!!!!!
REAL(kind=dp), DIMENSION(SIZE(theta,1)) :: aux_jacob, mult_hess_jacob, theta_aux, aux_jacob_2!, theta_step, theta_step_2
REAL(kind=dp), DIMENSION(SIZE(theta,1), SIZE(theta,1)) :: aux_hessian
!!!!!!!!!!!!!!
REAL(kind=dp) :: aux_con, log_aux_1, log_aux_2, log_aux_3, log_aux_4!, aux_step_1, aux_step_2
REAL(kind=dp) :: iter_time_0, iter_time_1, inner_time_0, inner_time_1
!!!!!!!!!!!!!!
REAL(kind=dp), DIMENSION(:,:,:,:), ALLOCATABLE :: emax_vf
REAL(kind=dp), DIMENSION(:,:,:,:,:), ALLOCATABLE :: choice_specific_vf
!REAL(kind=dp), DIMENSION(dim_ratings,dim_ratings,dim_choice, max_weeks,dim_types) :: csvf

!!!!!!!!!!!!!!
INTEGER, DIMENSION(total_obs) :: IDshow, Ratings, Cancel, Week, Ratings_ord, Cancel_ord, Week_ord, Seas
INTEGER, DIMENSION(total_obs) :: Mean_Ratings, Prior_Ratings, Prior_Ratings_Ord, Mean_Ratings_Ord, EndSeas, Types, Types_ord
!!!!!!!!!!!!!!
INTEGER ::l,m,n,o,p,q,counter
!!!!!!!!!!!!!!

ALLOCATE(log_CCP_jacobian_1(dim_ratings,dim_ratings,dim_ratings,max_weeks-1,SIZE(theta,1),dim_choice))
ALLOCATE(log_CCP_jacobian_2(dim_ratings,dim_ratings,dim_ratings,max_weeks_mid-1,SIZE(theta,1),dim_choice))
ALLOCATE(log_CCP_jacobian_3(dim_ratings,dim_ratings,dim_ratings,max_weeks_qrt-1,SIZE(theta,1),dim_choice))
ALLOCATE(log_CCP_jacobian_4(dim_ratings,dim_ratings,dim_ratings,max_weeks-1,SIZE(theta,1),dim_choice))
ALLOCATE(log_CCP_jacobian_5(dim_ratings,dim_ratings,dim_ratings,max_weeks_mid-1,SIZE(theta,1),dim_choice))
ALLOCATE(log_CCP_jacobian_6(dim_ratings,dim_ratings,dim_ratings,max_weeks_qrt-1,SIZE(theta,1),dim_choice))
ALLOCATE(ccp(dim_ratings,dim_ratings,dim_ratings,max_weeks-1,SIZE(theta,1),2,dim_types))
ALLOCATE(emax_vf(dim_ratings,dim_ratings,dim_ratings,max_weeks))
ALLOCATE(choice_specific_vf(dim_ratings,dim_ratings,dim_ratings,dim_choice, max_weeks))

!!Simulated data
!~OPEN(UNIT=10,FILE=sample_file)
!~DO l=1,total_obs
!~READ(10,"(I4,X,I3,X,I2,X,I2,X,I2,X,I1,X,I1)")  IDshow(l), Week(l), Ratings(l), Mean_Ratings(l), &
!~&Prior_ratings(l), Cancel(l), Types(l)
!~END DO
!~CLOSE(UNIT=10)

!!Actual data
OPEN(UNIT=10,FILE=sample_file)
DO l=1,total_obs
READ(10,"(2X,I3,3X,I3,4X,I2,4X,I2,4X,I2,X,I1,X,I1)") IDshow(l), Week(l), Ratings(l), Mean_Ratings(l), &
&Prior_ratings(l), Cancel(l), Types(l)
END DO
CLOSE(UNIT=10)

OPEN(UNIT=51, FILE="Warnings.txt")

counter=0
aux_con=5
!!!!!!!!!!!!!ORDERING OBSERVATIONS!!!!!!!!!!!!!!!!!
DO l=1,total_obs
IF (Cancel(l)==1) THEN
counter=counter+1
Ratings_ord(counter)=Ratings(l)
Cancel_ord(counter)=Cancel(l)
Week_ord(counter)=Week(l)
Types_ord(counter)=Types(l)
Mean_Ratings_Ord(counter)=Mean_Ratings(l)
Prior_Ratings_Ord(counter)=Prior_Ratings(l)
END IF
END DO

DO l=1,total_obs
IF (Cancel(l)==0) THEN
counter=counter+1
Ratings_ord(counter)=Ratings(l)
Cancel_ord(counter)=Cancel(l)
Week_ord(counter)=Week(l)
Types_ord(counter)=Types(l)
Mean_Ratings_Ord(counter)=Mean_Ratings(l)
Prior_Ratings_Ord(counter)=Prior_Ratings(l)
END IF
END DO
!!!!!!!!!!!!!ORDERING OBSERVATIONS!!!!!!!!!!!!!!!!!


DO o=1,20
CALL CPU_TIME(iter_time_0)
PRINT *, "Iteration number", o, "Sample", sample_file

!!Initializations!!
!choice_specific_vf=0.0
emax_vf=0.0
!!!!!!

CALL CPU_TIME(inner_time_0)
DO n=1,2
DO m=1,SIZE(theta,1)
DO l=1,SIZE(theta,1)
IF (m==l) THEN
theta_partial(l,m,n)=theta(l)+(2*n-3)*epsil
ELSE
theta_partial(l,m,n)=theta(l)
END IF
END DO

CALL compute_value_functions(transition_matrix(:,:,:,:,1),transition_mean_matrix(:,:,:,:,1),&
&transition_prior_matrix(:,:,:,:,1),choice_specific_vf,theta_partial(:,m,n),ccp(:,:,:,:,m,n,1))       !Type 1
CALL compute_value_functions_mid(transition_matrix(:,:,:,:,2),transition_mean_matrix(:,:,:,:,2),&
&transition_prior_matrix(:,:,:,:,2),choice_specific_vf,theta_partial(:,m,n),ccp(:,:,:,:,m,n,2))       !Type 2
CALL compute_value_functions_qrt(transition_matrix(:,:,:,:,3),transition_mean_matrix(:,:,:,:,3),&
&transition_prior_matrix(:,:,:,:,3),choice_specific_vf,theta_partial(:,m,n),ccp(:,:,:,:,m,n,3))       !Type 3
CALL compute_value_functions_own(transition_matrix(:,:,:,:,4),transition_mean_matrix(:,:,:,:,4),&
&transition_prior_matrix(:,:,:,:,4),choice_specific_vf,theta_partial(:,m,n),ccp(:,:,:,:,m,n,4))       !Type 4
CALL compute_value_functions_mid_own(transition_matrix(:,:,:,:,5),transition_mean_matrix(:,:,:,:,5),&
&transition_prior_matrix(:,:,:,:,5),choice_specific_vf,theta_partial(:,m,n),ccp(:,:,:,:,m,n,5))       !Type 5
CALL compute_value_functions_qrt_own(transition_matrix(:,:,:,:,6),transition_mean_matrix(:,:,:,:,6),&
&transition_prior_matrix(:,:,:,:,6),choice_specific_vf,theta_partial(:,m,n),ccp(:,:,:,:,m,n,6))       !Type 6


!CALL compute_value_functions_all(transition_matrix,transition_mean_matrix,csvf,theta_partial(:,m,n),ccp(:,:,:,m,n,:))



END DO
END DO
CALL CPU_TIME(inner_time_1)
!!!!!!


PRINT *, "Inner loop", inner_time_1-inner_time_0
!!!!
DO n=1,SIZE(theta,1)
DO l=1,max_weeks-1
DO q=1,dim_ratings
DO p=1,dim_ratings
DO m=1,dim_ratings
!!!!!!!!!!!!!!For the first type!!!!!!!!!!!!!

log_aux_1=log(ccp(m,p,q,l,n,2,1))
log_aux_2=log(ccp(m,p,q,l,n,1,1))
log_aux_3=log(1.0-ccp(m,p,q,l,n,2,1))
log_aux_4=log(1.0-ccp(m,p,q,l,n,1,1))

IF (sqrt(aux_con)>10E-2) THEN
!log_CCP_jacobian(m,p,l,n,1,1)=(log_aux_1-log_aux_2)*(0.5*epsil_inv)
!log_CCP_jacobian(m,p,l,n,2,1)=(log_aux_3-log_aux_4)*(0.5*epsil_inv)
log_CCP_jacobian_1(m,p,q,l,n,1)=(log_aux_1-log_aux_2)*(0.5*epsil_inv)
log_CCP_jacobian_1(m,p,q,l,n,2)=(log_aux_3-log_aux_4)*(0.5*epsil_inv)
ELSE
!log_CCP_jacobian(m,p,l,n,1,1)=(log_aux_1-log_aux_2)*(0.5*epsil_inv_2)
!log_CCP_jacobian(m,p,l,n,2,1)=(log_aux_3-log_aux_4)*(0.5*epsil_inv_2)
log_CCP_jacobian_1(m,p,q,l,n,1)=(log_aux_1-log_aux_2)*(0.5*epsil_inv_2)
log_CCP_jacobian_1(m,p,q,l,n,2)=(log_aux_3-log_aux_4)*(0.5*epsil_inv_2)
END IF

!!!!!!!!!!!!!!!!!!Fourth type!!!!!!!!!!!!!!!!
log_aux_1=log(ccp(m,p,q,l,n,2,4))
log_aux_2=log(ccp(m,p,q,l,n,1,4))
log_aux_3=log(1.0-ccp(m,p,q,l,n,2,4))
log_aux_4=log(1.0-ccp(m,p,q,l,n,1,4))
IF (sqrt(aux_con)>10E-2) THEN
!log_CCP_jacobian(m,p,l,n,1,4)=(log_aux_1-log_aux_2)*(0.5*epsil_inv)
!log_CCP_jacobian(m,p,l,n,2,4)=(log_aux_3-log_aux_4)*(0.5*epsil_inv)
log_CCP_jacobian_4(m,p,q,l,n,1)=(log_aux_1-log_aux_2)*(0.5*epsil_inv)
log_CCP_jacobian_4(m,p,q,l,n,2)=(log_aux_3-log_aux_4)*(0.5*epsil_inv)

ELSE
!log_CCP_jacobian(m,p,l,n,1,4)=(log_aux_1-log_aux_2)*(0.5*epsil_inv_2)
!log_CCP_jacobian(m,p,l,n,2,4)=(log_aux_3-log_aux_4)*(0.5*epsil_inv_2)
log_CCP_jacobian_4(m,p,q,l,n,1)=(log_aux_1-log_aux_2)*(0.5*epsil_inv_2)
log_CCP_jacobian_4(m,p,q,l,n,2)=(log_aux_3-log_aux_4)*(0.5*epsil_inv_2)
END IF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IF (l.LE.max_weeks_mid-1) THEN
!!!!!!!!!!!!!For the second type!!!!!!!!!!!!!
log_aux_1=log(ccp(m,p,q,l,n,2,2))
log_aux_2=log(ccp(m,p,q,l,n,1,2))
log_aux_3=log(1.0-ccp(m,p,q,l,n,2,2))
log_aux_4=log(1.0-ccp(m,p,q,l,n,1,2))

IF (sqrt(aux_con)>10E-2) THEN
!log_CCP_jacobian(m,p,l,n,1,2)=(log_aux_1-log_aux_2)*(0.5*epsil_inv)
!log_CCP_jacobian(m,p,l,n,2,2)=(log_aux_3-log_aux_4)*(0.5*epsil_inv)
log_CCP_jacobian_2(m,p,q,l,n,1)=(log_aux_1-log_aux_2)*(0.5*epsil_inv)
log_CCP_jacobian_2(m,p,q,l,n,2)=(log_aux_3-log_aux_4)*(0.5*epsil_inv)
ELSE
!log_CCP_jacobian(m,p,l,n,1,2)=(log_aux_1-log_aux_2)*(0.5*epsil_inv_2)
!log_CCP_jacobian(m,p,l,n,2,2)=(log_aux_3-log_aux_4)*(0.5*epsil_inv_2)
log_CCP_jacobian_2(m,p,q,l,n,1)=(log_aux_1-log_aux_2)*(0.5*epsil_inv_2)
log_CCP_jacobian_2(m,p,q,l,n,2)=(log_aux_3-log_aux_4)*(0.5*epsil_inv_2)
END IF
!!!!!!!!!!!!!For the fifth type!!!!!!!!!!!!!
log_aux_1=log(ccp(m,p,q,l,n,2,5))
log_aux_2=log(ccp(m,p,q,l,n,1,5))
log_aux_3=log(1.0-ccp(m,p,q,l,n,2,5))
log_aux_4=log(1.0-ccp(m,p,q,l,n,1,5))

IF (sqrt(aux_con)>10E-2) THEN
!log_CCP_jacobian(m,p,l,n,1,5)=(log_aux_1-log_aux_2)*(0.5*epsil_inv)
!log_CCP_jacobian(m,p,l,n,2,5)=(log_aux_3-log_aux_4)*(0.5*epsil_inv)
log_CCP_jacobian_5(m,p,q,l,n,1)=(log_aux_1-log_aux_2)*(0.5*epsil_inv)
log_CCP_jacobian_5(m,p,q,l,n,2)=(log_aux_3-log_aux_4)*(0.5*epsil_inv)

ELSE
!log_CCP_jacobian(m,p,l,n,1,5)=(log_aux_1-log_aux_2)*(0.5*epsil_inv_2)
!log_CCP_jacobian(m,p,l,n,2,5)=(log_aux_3-log_aux_4)*(0.5*epsil_inv_2)
log_CCP_jacobian_5(m,p,q,l,n,1)=(log_aux_1-log_aux_2)*(0.5*epsil_inv_2)
log_CCP_jacobian_5(m,p,q,l,n,2)=(log_aux_3-log_aux_4)*(0.5*epsil_inv_2)
END IF
END IF


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IF (l.LE.max_weeks_qrt-1) THEN
!!!!!!!!!!!!!For the third type!!!!!!!!!!!!!
log_aux_1=log(ccp(m,p,q,l,n,2,3))
log_aux_2=log(ccp(m,p,q,l,n,1,3))
log_aux_3=log(1.0-ccp(m,p,q,l,n,2,3))
log_aux_4=log(1.0-ccp(m,p,q,l,n,1,3))

IF (sqrt(aux_con)>10E-2) THEN
!log_CCP_jacobian(m,p,l,n,1,3)=(log_aux_1-log_aux_2)*(0.5*epsil_inv)
!log_CCP_jacobian(m,p,l,n,2,3)=(log_aux_3-log_aux_4)*(0.5*epsil_inv)
log_CCP_jacobian_3(m,p,q,l,n,1)=(log_aux_1-log_aux_2)*(0.5*epsil_inv)
log_CCP_jacobian_3(m,p,q,l,n,2)=(log_aux_3-log_aux_4)*(0.5*epsil_inv)
ELSE
!log_CCP_jacobian(m,p,l,n,1,3)=(log_aux_1-log_aux_2)*(0.5*epsil_inv_2)
!log_CCP_jacobian(m,p,l,n,2,3)=(log_aux_3-log_aux_4)*(0.5*epsil_inv_2)
log_CCP_jacobian_3(m,p,q,l,n,1)=(log_aux_1-log_aux_2)*(0.5*epsil_inv_2)
log_CCP_jacobian_3(m,p,q,l,n,2)=(log_aux_3-log_aux_4)*(0.5*epsil_inv_2)
END IF
!!!!!!!!!!!!!For the sixth type!!!!!!!!!!!!!
log_aux_1=log(ccp(m,p,q,l,n,2,6))
log_aux_2=log(ccp(m,p,q,l,n,1,6))
log_aux_3=log(1.0-ccp(m,p,q,l,n,2,6))
log_aux_4=log(1.0-ccp(m,p,q,l,n,1,6))

IF (sqrt(aux_con)>10E-2) THEN
!log_CCP_jacobian(m,p,l,n,1,6)=(log_aux_1-log_aux_2)*(0.5*epsil_inv)
!log_CCP_jacobian(m,p,l,n,2,6)=(log_aux_3-log_aux_4)*(0.5*epsil_inv)
log_CCP_jacobian_6(m,p,q,l,n,1)=(log_aux_1-log_aux_2)*(0.5*epsil_inv)
log_CCP_jacobian_6(m,p,q,l,n,2)=(log_aux_3-log_aux_4)*(0.5*epsil_inv)
ELSE
!log_CCP_jacobian(m,p,l,n,1,6)=(log_aux_1-log_aux_2)*(0.5*epsil_inv_2)
!log_CCP_jacobian(m,p,l,n,2,6)=(log_aux_3-log_aux_4)*(0.5*epsil_inv_2)
log_CCP_jacobian_6(m,p,q,l,n,1)=(log_aux_1-log_aux_2)*(0.5*epsil_inv_2)
log_CCP_jacobian_6(m,p,q,l,n,2)=(log_aux_3-log_aux_4)*(0.5*epsil_inv_2)
END IF
END IF




END DO
END DO
END DO
END DO
END DO

 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 !!!!!Before inverting and multiplying, getting the summation over all observations!!!!!
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


aux_jacob=0.0
mult_hess_jacob=0.0
aux_hessian=0.0

DO l=1,total_obs
DO m=1,SIZE(theta,1)

IF (types_ord(l)==1) THEN

aux_jacob(m)=aux_jacob(m)+&
&log_CCP_jacobian_1(Ratings_ord(l),Mean_Ratings_Ord(l),Prior_Ratings_Ord(l),Week_ord(l),m,Cancel_ord(l)+1)
DO n=1,SIZE(theta,1)
aux_hessian(m,n)=aux_hessian(m,n)+&
&log_CCP_jacobian_1(Ratings_ord(l),Mean_Ratings_Ord(l),Prior_Ratings_Ord(l),Week_ord(l),m,Cancel_ord(l)+1)*&
&log_CCP_jacobian_1(Ratings_ord(l),Mean_Ratings_Ord(l),Prior_Ratings_Ord(l),Week_ord(l),n,Cancel_ord(l)+1)
END DO

ELSE IF (types_ord(l)==2) THEN

aux_jacob(m)=aux_jacob(m)+&
&log_CCP_jacobian_2(Ratings_ord(l),Mean_Ratings_Ord(l),Prior_Ratings_Ord(l),Week_ord(l),m,Cancel_ord(l)+1)
DO n=1,SIZE(theta,1)
aux_hessian(m,n)=aux_hessian(m,n)+&
&log_CCP_jacobian_2(Ratings_ord(l),Mean_Ratings_Ord(l),Prior_Ratings_Ord(l),Week_ord(l),m,Cancel_ord(l)+1)*&
&log_CCP_jacobian_2(Ratings_ord(l),Mean_Ratings_Ord(l),Prior_Ratings_Ord(l),Week_ord(l),n,Cancel_ord(l)+1)
END DO

ELSE IF (types_ord(l)==3) THEN


aux_jacob(m)=aux_jacob(m)+&
&log_CCP_jacobian_3(Ratings_ord(l),Mean_Ratings_Ord(l),Prior_Ratings_Ord(l),Week_ord(l),m,Cancel_ord(l)+1)
DO n=1,SIZE(theta,1)
aux_hessian(m,n)=aux_hessian(m,n)+&
&log_CCP_jacobian_3(Ratings_ord(l),Mean_Ratings_Ord(l),Prior_Ratings_Ord(l),Week_ord(l),m,Cancel_ord(l)+1)*&
&log_CCP_jacobian_3(Ratings_ord(l),Mean_Ratings_Ord(l),Prior_Ratings_Ord(l),Week_ord(l),n,Cancel_ord(l)+1)
END DO


ELSE IF (types_ord(l)==4) THEN

aux_jacob(m)=aux_jacob(m)+&
&log_CCP_jacobian_4(Ratings_ord(l),Mean_Ratings_Ord(l),Prior_Ratings_Ord(l),Week_ord(l),m,Cancel_ord(l)+1)
DO n=1,SIZE(theta,1)
aux_hessian(m,n)=aux_hessian(m,n)+&
&log_CCP_jacobian_4(Ratings_ord(l),Mean_Ratings_Ord(l),Prior_Ratings_Ord(l),Week_ord(l),m,Cancel_ord(l)+1)*&
&log_CCP_jacobian_4(Ratings_ord(l),Mean_Ratings_Ord(l),Prior_Ratings_Ord(l),Week_ord(l),n,Cancel_ord(l)+1)
END DO

ELSE IF (types_ord(l)==5) THEN

aux_jacob(m)=aux_jacob(m)+&
&log_CCP_jacobian_5(Ratings_ord(l),Mean_Ratings_Ord(l),Prior_Ratings_Ord(l),Week_ord(l),m,Cancel_ord(l)+1)
DO n=1,SIZE(theta,1)
aux_hessian(m,n)=aux_hessian(m,n)+&
&log_CCP_jacobian_5(Ratings_ord(l),Mean_Ratings_Ord(l),Prior_Ratings_Ord(l),Week_ord(l),m,Cancel_ord(l)+1)*&
&log_CCP_jacobian_5(Ratings_ord(l),Mean_Ratings_Ord(l),Prior_Ratings_Ord(l),Week_ord(l),n,Cancel_ord(l)+1)
END DO

ELSE IF (types_ord(l)==6) THEN

aux_jacob(m)=aux_jacob(m)+&
&log_CCP_jacobian_6(Ratings_ord(l),Mean_Ratings_Ord(l),Prior_Ratings_Ord(l),Week_ord(l),m,Cancel_ord(l)+1)
DO n=1,SIZE(theta,1)
aux_hessian(m,n)=aux_hessian(m,n)+&
&log_CCP_jacobian_6(Ratings_ord(l),Mean_Ratings_Ord(l),Prior_Ratings_Ord(l),Week_ord(l),m,Cancel_ord(l)+1)*&
&log_CCP_jacobian_6(Ratings_ord(l),Mean_Ratings_Ord(l),Prior_Ratings_Ord(l),Week_ord(l),n,Cancel_ord(l)+1)
END DO

END IF
END DO
END DO

DO m=1,SIZE(theta,1)
aux_jacob_2(m)=aux_jacob(m)
PRINT *, "jacob", aux_jacob(m)
DO n=1,SIZE(theta,1)
PRINT *, "hess", aux_hessian(m,n)
END DO
END DO
CALL gauss(aux_hessian,aux_jacob,mult_hess_jacob,SIZE(theta,1))


!!!!!!Update theta!!!!!!!!!!



IF(mult_hess_jacob(1) /= mult_hess_jacob(1)) THEN
WRITE(51,"(A,A,F20.10)") sample_file, " had overload. Conv criterion was: ", SQRT(aux_con)
PRINT *, "Overload. Possibly singular matrix"
EXIT
END IF

aux_con=0.0

DO l=1,SIZE(theta,1)
theta_aux(l)=theta(l)
theta(l)=theta(l)+mult_hess_jacob(l)
aux_con=aux_con+((theta_aux(l)-theta(l))*(theta_aux(l)-theta(l)))
END DO

PRINT *, "Convergence criterion:", SQRT(aux_con)

IF (SQRT(aux_con)<10E-6) THEN
PRINT *, "Convergence"
PRINT *, theta
EXIT
END IF

IF (i==maxITER) THEN
PRINT *, "No convergence"
END IF
PRINT *, theta



CALL CPU_TIME(iter_time_1)

PRINT *, "Iteration time", iter_time_1-iter_time_0
END DO
CLOSE(UNIT=51)

!Getting the value of the log-likelihood and the ccp's

CALL compute_value_functions(transition_matrix(:,:,:,:,1),transition_mean_matrix(:,:,:,:,1),&
&transition_prior_matrix(:,:,:,:,1),choice_specific_vf,theta,ccp_true(:,:,:,:,1))     
CALL compute_value_functions_mid(transition_matrix(:,:,:,:,2),transition_mean_matrix(:,:,:,:,2),&
&transition_prior_matrix(:,:,:,:,2),choice_specific_vf,theta,ccp_true(:,:,:,:,2))     
CALL compute_value_functions_qrt(transition_matrix(:,:,:,:,3),transition_mean_matrix(:,:,:,:,3),&
&transition_prior_matrix(:,:,:,:,3),choice_specific_vf,theta,ccp_true(:,:,:,:,3))     
CALL compute_value_functions_own(transition_matrix(:,:,:,:,4),transition_mean_matrix(:,:,:,:,4),&
&transition_prior_matrix(:,:,:,:,4),choice_specific_vf,theta,ccp_true(:,:,:,:,4))     
CALL compute_value_functions_mid_own(transition_matrix(:,:,:,:,5),transition_mean_matrix(:,:,:,:,5),&
&transition_prior_matrix(:,:,:,:,5),choice_specific_vf,theta,ccp_true(:,:,:,:,5))     
CALL compute_value_functions_qrt_own(transition_matrix(:,:,:,:,6),transition_mean_matrix(:,:,:,:,6),&
&transition_prior_matrix(:,:,:,:,6),choice_specific_vf,theta,ccp_true(:,:,:,:,6))     



!CALL compute_value_functions_all(transition_matrix,transition_mean_matrix,csvf,theta,ccp_true)     

log_likelihood=0.0
DO l=1,total_obs
IF (cancel_ord(l)==0) THEN
log_likelihood=log_likelihood+log(ccp_true(Ratings_ord(l),Mean_Ratings_Ord(l),Prior_Ratings_Ord(l),Week_ord(l),Types_ord(l)))
ELSE
log_likelihood=log_likelihood+log(1.0-ccp_true(Ratings_ord(l),Mean_Ratings_Ord(l),Prior_Ratings_Ord(l),Week_ord(l),Types_ord(l)))
END IF
END DO

OPEN(UNIT=10,FILE="Outer_product.txt")
DO m=1,SIZE(theta,1)
WRITE(10,"(I2,X,F15.10,X,A)")  m, aux_jacob(m), "Jacob"
DO n=1,SIZE(theta,1)
WRITE(10,"(I2,X,I2,X,F15.10,X,A)")  m,n, aux_hessian(m,n), "Hessian"
END DO
END DO
CLOSE(UNIT=10)


DEALLOCATE(log_CCP_jacobian_1)
DEALLOCATE(log_CCP_jacobian_2)
DEALLOCATE(log_CCP_jacobian_3)
DEALLOCATE(log_CCP_jacobian_4)
DEALLOCATE(log_CCP_jacobian_5)
DEALLOCATE(log_CCP_jacobian_6)
DEALLOCATE(ccp)
DEALLOCATE(emax_vf)
DEALLOCATE(choice_specific_vf)


END SUBROUTINE BHHH_iteration



END MODULE subroutines

MODULE stat_subroutines
USE subroutines
IMPLICIT NONE
CONTAINS

SUBROUTINE cdf_normal(x,mu,sigma_sq, inv) !! x= value or the quantile, depending on the mode
REAL(kind=dp), INTENT(INOUT) :: x
REAL(kind=dp), INTENT(IN) :: mu, sigma_sq
INTEGER, INTENT(IN) :: inv
REAL(kind=dp), PARAMETER :: eps=10E-8
REAL(kind=dp) :: res_oper, limit, limit_inv, guess!, guess_lag
REAL(kind=dp) :: bound_sup, bound_inf
INTEGER :: m


IF (inv==0) THEN !!! IF 0 give me the CDF
res_oper=0.5
limit=(x-mu)/SQRT(2.0*sigma_sq)
IF (x.LE.mu) THEN
res_oper=res_oper-ERF(-limit)/2.0
ELSE IF (x>mu) THEN
res_oper=res_oper+ERF(limit)/2.0
END IF
!PRINT *, "CDF result is", res_oper
x=res_oper

ELSE IF (inv==1) THEN !!! IF 1 give me the inv_CDF

IF ((x.LE.0.0) .OR. (x.GE.1.0)) THEN
PRINT *, "Quantile out of bounds 0,1"
STOP
END IF
guess=mu


DO m=1,100
limit_inv=(guess-mu)/SQRT(2.0*sigma_sq)
res_oper=0.5

IF (x.LE.mu) THEN
res_oper=res_oper-ERF(-limit_inv)/2.0
ELSE IF (x>mu) THEN
res_oper=res_oper+ERF(limit_inv)/2.0
END IF

IF (ABS(res_oper-x)<eps) THEN
!PRINT *, "Inv CDF result is", guess
x=guess
EXIT
ELSE IF ((res_oper-x>0.0) .AND. (m==1)) THEN
bound_inf=-(mu+100.0*sigma_sq)
bound_sup=guess
ELSE IF ((res_oper-x.LE.0.0) .AND. (m==1)) THEN
bound_sup=(mu+100.0*sigma_sq)
bound_inf=guess
ELSE IF ((res_oper-x>0.0) .AND. (m.NE.1)) THEN
bound_sup=guess
ELSE 
bound_inf=guess
END IF
guess=(bound_sup+bound_inf)/2.0
END DO

ELSE
PRINT *, "Not a valid operation selected"
END IF

END SUBROUTINE

SUBROUTINE compute_transitions(transition_matrix,transition_mean_matrix,transition_prior_matrix,types)
REAL(kind=dp), DIMENSION(:,:,:,:), INTENT(INOUT) :: transition_mean_matrix   !Rat (fut_mean) x Rat(fut_value) x Rat (curr_mean) x Weeks
REAL(kind=dp), DIMENSION(:,:,:,:), INTENT(INOUT) :: transition_matrix        !Ratings (fut_value) x Ratings (mean) x Ratings (Prior) x Weeks
REAL(kind=dp), DIMENSION(:,:,:,:), INTENT(INOUT) :: transition_prior_matrix  !Ratings (fut_prior) x Ratings (curr_mean) x Ratings (Prior) x Weeks
INTEGER, INTENT(IN) :: types
REAL(kind=dp) :: mu_0=1.3, sigma_sq_0=0.7647
REAL(kind=dp), PARAMETER :: sigma_sq=0.164917
REAL(kind=dp) :: aux_cdf, aux_cdf_2, sigma_sq_posterior, r_mean_posterior, sigma_sq_prediction
REAL(kind=dp) :: weight, prob_rt_1,weighted_value
INTEGER:: t,m,n,time,p

time=0
!transition_matrix=0.0
transition_mean_matrix=0.0
transition_prior_matrix=0.0


IF ((types==1) .OR. (types==4)) THEN !!!!!!!!!!!!!!!!!!!!!!!!!! TYPE FULL

!DO t=1,21
!DO m=1,dim_ratings
!DO n=1,dim_ratings
!transition_prior_matrix(INT(10*mu_0),m,n,t)=1.0
!END DO
!END DO
!END DO
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! RAT. VALUES
DO t=1,max_weeks-1
IF ((t==22).OR.(t==44).OR.(t==66).OR.(t==88).OR.(t==110).OR.(t==132)) THEN
sigma_sq_0=0.19549
END IF

IF ((t==23).OR.(t==45).OR.(t==67).OR.(t==89).OR.(t==111).OR.(t==133)) THEN
time=0
END IF

DO p=1,dim_ratings
time=time+1                                       
sigma_sq_posterior=1.0/((1.0/sigma_sq_0)+(time/sigma_sq))   
sigma_sq_prediction=sigma_sq_posterior+sigma_sq          
DO m=1,dim_ratings                                       
aux_cdf_2=0.0                                            
r_mean_posterior=((time*sigma_sq_0)/(sigma_sq+time*sigma_sq_0))*(m/10.0) + &
&((sigma_sq)/(sigma_sq+time*sigma_sq_0))*(p/10.0) !!

DO n=1,dim_ratings-1                                     
aux_cdf=0.05+(n/10.0)                                    
CALL cdf_normal(aux_cdf,r_mean_posterior,sigma_sq_prediction,0) 
prob_rt_1=aux_cdf-aux_cdf_2
transition_matrix(n,m,p,t)=prob_rt_1
aux_cdf_2=aux_cdf                                        
END DO                                                   

transition_matrix(dim_ratings,m,p,t)=1.0-aux_cdf_2
END DO                                                   
END DO
END DO                                                   
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!RAT. MEAN
time=0

DO t=1,max_weeks-1                                       
time=time+1
IF ((t==22).OR.(t==44).OR.(t==66).OR.(t==88).OR.(t==110).OR.(t==132)) THEN
time=0
END IF
weight=REAL(time)/REAL((time+1))                               

DO m=1,dim_ratings !curr. mean index                     
DO n=1,dim_ratings  !fut. rat. index                     

weighted_value=weight*(m/10.0)+(1-weight)*(n/10.0)

weighted_value=weighted_value*10

IF (weighted_value-INT(weighted_value) .LE. 0.5) THEN
transition_mean_matrix(INT(weighted_value),n,m,t)=1.0
ELSE
transition_mean_matrix(INT(weighted_value)+1,n,m,t)=1.0
END IF

END DO
END DO
END DO
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!RAT. PRIOR
DO t=1,max_weeks-1  !Ratings (fut_prior) x Ratings (curr_mean) x Ratings (Prior) x Weeks

DO n=1,dim_ratings
DO m=1,dim_ratings
IF ((t==22).OR.(t==44).OR.(t==66).OR.(t==88).OR.(t==110).OR.(t==132)) THEN

weighted_value=((22.0*sigma_sq_0)/(sigma_sq+22.0*sigma_sq_0))*(m/10.0) + &
&((sigma_sq)/(sigma_sq+22.0*sigma_sq_0))*(n/10.0) !! This should be already the "changed" sigma_sq_0 from before
weighted_value=10.0*weighted_value

IF (weighted_value-INT(weighted_value) .LE. 0.5) THEN
transition_prior_matrix(INT(weighted_value),m,n,t)=1.0
ELSE
transition_prior_matrix(INT(weighted_value)+1,m,n,t)=1.0
END IF

ELSE
transition_prior_matrix(n,m,n,t)=1.0
END IF
END DO
END DO

END DO

ELSE IF ((types==2).OR.(types==5)) THEN !!!!!!!!!!!!!!!!!!!!!!!!!!!!TYPE MID
!DO t=1,11
!DO m=1,dim_ratings
!DO n=1,dim_ratings
!transition_prior_matrix(INT(10*mu_0),m,n,t)=1.0
!END DO
!END DO
!END DO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!RAT. VALUES
DO t=1,max_weeks_mid-1
IF ((t==12).OR.(t==34).OR.(t==56).OR.(t==78).OR.(t==100).OR.(t==122)) THEN
sigma_sq_0=0.19549
END IF

IF ((t==13).OR.(t==35).OR.(t==57).OR.(t==79).OR.(t==101).OR.(t==123)) THEN
time=0
END IF

DO p=1,dim_ratings
time=time+1                                       
sigma_sq_posterior=1.0/((1.0/sigma_sq_0)+(time/sigma_sq))   
sigma_sq_prediction=sigma_sq_posterior+sigma_sq          
DO m=1,dim_ratings                                       
aux_cdf_2=0.0                                            
r_mean_posterior=((time*sigma_sq_0)/(sigma_sq+time*sigma_sq_0))*(m/10.0) + &
&((sigma_sq)/(sigma_sq+time*sigma_sq_0))*(p/10.0) !!

DO n=1,dim_ratings-1                                     
aux_cdf=0.05+(n/10.0)                                    
CALL cdf_normal(aux_cdf,r_mean_posterior,sigma_sq_prediction,0) 
prob_rt_1=aux_cdf-aux_cdf_2
transition_matrix(n,m,p,t)=prob_rt_1
aux_cdf_2=aux_cdf                                        
END DO                                                   

transition_matrix(dim_ratings,m,p,t)=1.0-aux_cdf_2
END DO                                                   
END DO
END DO                                                 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!RAT. MEAN

time=0

DO t=1,max_weeks_mid-1                                       
time=time+1
IF ((t==12).OR.(t==34).OR.(t==56).OR.(t==78).OR.(t==100).OR.(t==122)) THEN
time=0
END IF
weight=REAL(time)/REAL((time+1))                               

DO m=1,dim_ratings !curr. mean index                     
DO n=1,dim_ratings  !fut. rat. index                     

weighted_value=weight*(m/10.0)+(1-weight)*(n/10.0)

weighted_value=weighted_value*10

IF (weighted_value-INT(weighted_value) .LE. 0.5) THEN
transition_mean_matrix(INT(weighted_value),n,m,t)=1.0
ELSE
transition_mean_matrix(INT(weighted_value)+1,n,m,t)=1.0
END IF

END DO
END DO

END DO


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!RAT. PRIOR
DO t=1,max_weeks_mid-1  !Ratings (fut_prior) x Ratings (curr_mean) x Ratings (Prior) x Weeks

DO n=1,dim_ratings
DO m=1,dim_ratings

IF (t==12) THEN


weighted_value=((22.0*sigma_sq_0)/(sigma_sq+22.0*sigma_sq_0))*(m/10.0) + &
&((sigma_sq)/(sigma_sq+22.0*sigma_sq_0))*(n/10.0) !! This should be already the "changed" sigma_sq_0 from before

weighted_value=weighted_value*10.0

IF (weighted_value-INT(weighted_value) .LE. 0.5) THEN
transition_prior_matrix(INT(weighted_value),n,m,t)=1.0
ELSE
transition_prior_matrix(INT(weighted_value)+1,n,m,t)=1.0
END IF


ELSE IF ((t==34).OR.(t==56).OR.(t==78).OR.(t==100).OR.(t==122)) THEN
weighted_value=(22.0/23.0)*(m/10.0)+(1.0/23.0)*(n/10.0)
weighted_value=weighted_value*10.0

IF (weighted_value-INT(weighted_value) .LE. 0.5) THEN
transition_prior_matrix(INT(weighted_value),n,m,t)=1.0
ELSE
transition_prior_matrix(INT(weighted_value)+1,n,m,t)=1.0
END IF

ELSE
transition_prior_matrix(n,m,n,t)=1.0
END IF
END DO
END DO

END DO



ELSE IF ((types==3).OR.(types==6)) THEN
!DO t=1,5
!DO m=1,dim_ratings
!DO n=1,dim_ratings
!transition_prior_matrix(INT(10*mu_0),m,n,t)=1.0
!END DO
!END DO
!END DO


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!RAT. VALUES
DO t=1,max_weeks_qrt-1
IF ((t==6).OR.(t==28).OR.(t==50).OR.(t==72).OR.(t==94).OR.(t==116)) THEN
sigma_sq_0=0.19549
END IF

IF ((t==7).OR.(t==29).OR.(t==51).OR.(t==73).OR.(t==95).OR.(t==117)) THEN
time=0
END IF

DO p=1,dim_ratings
time=time+1                                       
sigma_sq_posterior=1.0/((1.0/sigma_sq_0)+(time/sigma_sq))   
sigma_sq_prediction=sigma_sq_posterior+sigma_sq          
DO m=1,dim_ratings                                       
aux_cdf_2=0.0                                            
r_mean_posterior=((time*sigma_sq_0)/(sigma_sq+time*sigma_sq_0))*(m/10.0) + &
&((sigma_sq)/(sigma_sq+time*sigma_sq_0))*(p/10.0) !!

DO n=1,dim_ratings-1                                     
aux_cdf=0.05+(n/10.0)                                    
CALL cdf_normal(aux_cdf,r_mean_posterior,sigma_sq_prediction,0) 
prob_rt_1=aux_cdf-aux_cdf_2
transition_matrix(n,m,p,t)=prob_rt_1
aux_cdf_2=aux_cdf                                        
END DO                                                   

transition_matrix(dim_ratings,m,p,t)=1.0-aux_cdf_2
END DO                                                   
END DO
END DO                                                 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!RAT. MEAN

time=0

DO t=1,max_weeks_qrt-1                                       
time=time+1
IF ((t==6).OR.(t==28).OR.(t==50).OR.(t==72).OR.(t==94).OR.(t==116)) THEN
time=0
END IF
weight=REAL(time)/REAL((time+1))                               

DO m=1,dim_ratings !curr. mean index                     
DO n=1,dim_ratings  !fut. rat. index                     

weighted_value=weight*(m/10.0)+(1-weight)*(n/10.0)

weighted_value=weighted_value*10

IF (weighted_value-INT(weighted_value) .LE. 0.5) THEN
transition_mean_matrix(INT(weighted_value),n,m,t)=1.0
ELSE
transition_mean_matrix(INT(weighted_value)+1,n,m,t)=1.0
END IF

END DO
END DO

END DO


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!RAT. PRIOR
DO t=1,max_weeks_qrt-1  !Ratings (fut_prior) x Ratings (curr_mean) x Ratings (Prior) x Weeks

DO n=1,dim_ratings
DO m=1,dim_ratings

IF (t==6) THEN
weighted_value=((22.0*sigma_sq_0)/(sigma_sq+22.0*sigma_sq_0))*(m/10.0) + &
&((sigma_sq)/(sigma_sq+22.0*sigma_sq_0))*(n/10.0) !! This should be already the "changed" sigma_sq_0 from before

weighted_value=weighted_value*10.0

IF (weighted_value-INT(weighted_value) .LE. 0.5) THEN
transition_prior_matrix(INT(weighted_value),n,m,t)=1.0
ELSE
transition_prior_matrix(INT(weighted_value)+1,n,m,t)=1.0
END IF


ELSE IF ((t==28).OR.(t==50).OR.(t==72).OR.(t==94).OR.(t==116)) THEN
weighted_value=(22.0/23.0)*(m/10.0)+(1.0/23.0)*(n/10.0)
weighted_value=weighted_value*10.0

IF (weighted_value-INT(weighted_value) .LE. 0.5) THEN
transition_prior_matrix(INT(weighted_value),n,m,t)=1.0
ELSE
transition_prior_matrix(INT(weighted_value)+1,n,m,t)=1.0
END IF

ELSE
transition_prior_matrix(n,m,n,t)=1.0
END IF
END DO
END DO

END DO




END IF
END SUBROUTINE compute_transitions

SUBROUTINE likelihood_ratio_fit(ccp,sample_file,total_obs)
REAL(kind=dp), DIMENSION(:,:,:,:), INTENT(IN) :: ccp
CHARACTER(len=*), INTENT(IN) :: sample_file
REAL(kind=dp), DIMENSION(max_weeks-1) :: statistic
INTEGER, INTENT(IN) :: total_obs
REAL(kind=dp), DIMENSION(max_weeks-1) :: null_cancel, empirical_cancel
INTEGER, DIMENSION(max_weeks-1) :: count_t, count_cancel 
INTEGER, DIMENSION(dim_ratings,dim_ratings,max_weeks-1,dim_types) :: count_state 
INTEGER :: l,m,n,o !,p
INTEGER, DIMENSION(total_obs) :: IDShow, Ratings, Cancel, Week, Mean_Ratings, Types, Seas, EndSeas, Prior_ratings
REAL(kind=dp), DIMENSION(SIZE(statistic,1),dim_choice) :: aux_denominator, aux_numerator

count_t=0
count_state=0
count_cancel=0
null_cancel=0.0


OPEN(UNIT=10,FILE=sample_file)
DO l=1,total_obs
!READ(10,"(I4,X,I3,X,I2,X,I2,X,I1,X,I1)")  IDshow(l), Week(l), Ratings(l), Mean_Ratings(l), Cancel(l), Types(l)
!~READ(10,"(2X,I3,3X,I2,X,I1,X,I1,X,I1,3X,I3,3X)") IDShow(l), Ratings(l), Seas(l), EndSeas(l), Cancel(l), Week(l)
READ(10,"(2X,I3,3X,I3,4X,I2,4X,I2,4X,I2,X,I1,X,I1)") IDshow(l), Week(l), Ratings(l), Mean_Ratings(l), &
&Prior_ratings(l), Cancel(l), Types(l)
count_t(Week(l))=count_t(Week(l))+1  !Summing for each week
count_cancel(Week(l))=count_cancel(Week(l))+Cancel(l)  !Summing each CANCELLATION
count_state(Ratings(l),Mean_Ratings(l),Week(l),Types(l))=count_state(Ratings(l),Mean_Ratings(l),Week(l),Types(l))+1
!Summing across states
END DO
CLOSE(UNIT=10)

DO n=1,max_weeks-1
DO o=1,dim_types
DO l=1,dim_ratings
DO m=1,dim_ratings
null_cancel(n)=null_cancel(n)+((1.0-ccp(m,l,n,o))*(count_state(m,l,n,o)/REAL(count_t(n),8)))
END DO
END DO
END DO
empirical_cancel(n)=count_cancel(n)/REAL(count_t(n))
END DO

DO n=1,SIZE(statistic,1)
aux_denominator(n,1)=1000*(1.0-empirical_cancel(n))
aux_denominator(n,2)=1000*(empirical_cancel(n))
aux_numerator(n,1)=1000*(1.0-null_cancel(n))
aux_numerator(n,2)=1000*(null_cancel(n))

statistic(n)=-2*(count_cancel(n)*log(aux_numerator(n,2)/aux_denominator(n,2)) + &
&(count_t(n)-count_cancel(n))*log(aux_numerator(n,1)/aux_denominator(n,1)))
!WRITE (*,"(I3,X,F15.10,X,F15.10,X,F15.10)") n, statistic(n)
END DO


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! I had P(0|x,mean(x),t,type). I needed to multiply by P(type,x,mean(x)|t) and sum. This implies a four dimensional object.!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END SUBROUTINE likelihood_ratio_fit


!For my application, if I want to test that the parameters of the owned series are the same as those of the not owned ones I can do
!The following: when I compute BHHH I can maximize over half of the parameters assuming that types don't matter. That is, I compute
!the maximum with only half of the value functions, those corresponding to the equivalent durations. That would restrict immediately
!the valid parameters.


END MODULE stat_subroutines



PROGRAM master_thesis
USE stat_subroutines
IMPLICIT NONE
!!!!!!!!! Markov chains
REAL(kind=dp), DIMENSION(dim_ratings) :: initial_conditions
REAL(kind=dp), DIMENSION(dim_types) :: type_prob
REAL(kind=dp), DIMENSION(:,:,:,:,:), ALLOCATABLE::transition_matrix, transition_prior_matrix, transition_mean_matrix
!!!!!!!!! Value functions
REAL(kind=dp), DIMENSION(:,:,:,:,:,:), ALLOCATABLE :: choice_specific_vf
REAL(kind=dp), DIMENSION(:,:,:,:,:), ALLOCATABLE :: ccp_0
!!!!!!!!!Estimation
REAL(kind=dp), DIMENSION(dim_param) :: theta_vec 
REAL(kind=dp) :: llhood, t0, t1  !aux_con
!!!!!!!!!Simulation
INTEGER, DIMENSION(sim_obs,2) :: initial_values
!!!!!!!!! Data
!INTEGER, DIMENSION(n_obs) :: IDShow, Ratings, Seas, EndSeas, Cancel, Week, MaxWeek
!INTEGER, DIMENSION(max_weeks) :: cancels_week, obs_week


ALLOCATE(transition_matrix(dim_ratings,dim_ratings,dim_ratings,max_weeks-1,dim_types))
ALLOCATE(transition_prior_matrix(dim_ratings,dim_ratings,dim_ratings,max_weeks-1,dim_types))
ALLOCATE(transition_mean_matrix(dim_ratings,dim_ratings,dim_ratings,max_weeks-1,dim_types))
ALLOCATE(choice_specific_vf(dim_ratings,dim_ratings,dim_ratings,dim_choice,max_weeks,dim_types))
ALLOCATE(ccp_0(dim_ratings,dim_ratings,dim_ratings,max_weeks-1,dim_types))


!!!!!Initializations!!!!!
theta_vec(1)=theta_rev_1
theta_vec(2)=opp_cost_1
theta_vec(3)=theta_sunk_s1

theta_vec(4)=theta_rev_1_own
theta_vec(5)=opp_cost_1_own
theta_vec(6)=theta_sunk_s1_own

!theta_vec(6)=theta_sunk_s1_own
!!!!!!!!!!!!!!!!!!!!!!!!!


type_prob=1.0/REAL(dim_types)
initial_conditions=1.0/REAL(dim_ratings)
!!!!!!!!!!


!Maybe I can also give as an argument the parameters to be used
CALL compute_transitions(transition_matrix(:,:,:,:,1),transition_mean_matrix(:,:,:,:,1),transition_prior_matrix(:,:,:,:,1),1) 
CALL compute_transitions(transition_matrix(:,:,:,:,2),transition_mean_matrix(:,:,:,:,2),transition_prior_matrix(:,:,:,:,2),2)
CALL compute_transitions(transition_matrix(:,:,:,:,3),transition_mean_matrix(:,:,:,:,3),transition_prior_matrix(:,:,:,:,3),3)
CALL compute_transitions(transition_matrix(:,:,:,:,4),transition_mean_matrix(:,:,:,:,4),transition_prior_matrix(:,:,:,:,4),4)
CALL compute_transitions(transition_matrix(:,:,:,:,5),transition_mean_matrix(:,:,:,:,5),transition_prior_matrix(:,:,:,:,5),5)
CALL compute_transitions(transition_matrix(:,:,:,:,6),transition_mean_matrix(:,:,:,:,6),transition_prior_matrix(:,:,:,:,6),6)

!~OPEN(UNIT=4,FILE="cdf_check.txt")
!~DO h=1,dim_ratings
!~DO i=1,dim_ratings
!~DO k=1,dim_ratings 
!~DO j=1,dim_types
!~WRITE (4,"(3(F7.4))") SUM(transition_matrix(:,h,i,k,j),1),SUM(transition_mean_matrix(:,h,i,k,j),1),&
!~&SUM(transition_prior_matrix(:,h,i,k,j),1)
!~END DO
!~END DO
!~END DO
!~END DO
!~CLOSE(UNIT=4)

CALL compute_value_functions(transition_matrix(:,:,:,:,1),transition_mean_matrix(:,:,:,:,1),&
&transition_prior_matrix(:,:,:,:,1),choice_specific_vf(:,:,:,:,:,1),theta_vec,ccp_0(:,:,:,:,1))
CALL compute_value_functions_mid(transition_matrix(:,:,:,:,2),transition_mean_matrix(:,:,:,:,2),&
&transition_prior_matrix(:,:,:,:,2),choice_specific_vf(:,:,:,:,:,2),theta_vec,ccp_0(:,:,:,:,2))
CALL compute_value_functions_qrt(transition_matrix(:,:,:,:,3),transition_mean_matrix(:,:,:,:,3),&
&transition_prior_matrix(:,:,:,:,3),choice_specific_vf(:,:,:,:,:,3),theta_vec,ccp_0(:,:,:,:,3))
CALL compute_value_functions_own(transition_matrix(:,:,:,:,4),transition_mean_matrix(:,:,:,:,4),&
&transition_prior_matrix(:,:,:,:,4),choice_specific_vf(:,:,:,:,:,4),theta_vec,ccp_0(:,:,:,:,4))
CALL compute_value_functions_mid_own(transition_matrix(:,:,:,:,5),transition_mean_matrix(:,:,:,:,5),&
&transition_prior_matrix(:,:,:,:,5),choice_specific_vf(:,:,:,:,:,5),theta_vec,ccp_0(:,:,:,:,5))
CALL compute_value_functions_qrt_own(transition_matrix(:,:,:,:,6),transition_mean_matrix(:,:,:,:,6),&
&transition_prior_matrix(:,:,:,:,6),choice_specific_vf(:,:,:,:,:,6),theta_vec,ccp_0(:,:,:,:,6))

OPEN(UNIT=999,FILE="totalobs.txt")
DO i=1,samples!! Change this back
WRITE(sample_file,"(A,I3,A)") "sample_",100+i,".txt"      
CALL gen_initial_values(initial_values,initial_conditions,type_prob)
CALL gen_paths(initial_values,transition_matrix,transition_mean_matrix,transition_prior_matrix,&
&choice_specific_vf,sample_file, total_obs(i))
WRITE(999,"(I6)") total_obs(i)
END DO
CLOSE(UNIT=999)


!~OPEN(UNIT=20,FILE="ccp_check.txt")
!~DO h=1,SIZE(CCP_0,1)
!~DO i=1,SIZE(CCP_0,2)
!~DO j=1,SIZE(CCP_0,3)
!~DO k=1,SIZE(CCP_0,4)
!~DO g=1,SIZE(CCP_0,5)
!~WRITE(20,"(I3,X,I3,X,I3,X,I3,X,I3,X,F20.10)") h,i,j,k,g,ccp_0(h,i,j,k,g)
!~END DO
!~END DO
!~END DO
!~END DO
!~END DO
!~CLOSE(UNIT=20)

!This is to estimate the current specification
!Remember to change the "read" part
CALL CPU_TIME(t0)
OPEN(UNIT=1000,FILE="Results_Parameters.txt")
OPEN(UNIT=1001,FILE="Llhood_value.txt")
CALL BHHH_iteration(transition_matrix,transition_mean_matrix,transition_prior_matrix,theta_vec,&
&"Dataset Cleaned FINAL 08 06 16.txt",6548,llhood,ccp_0)
WRITE(1000,"(6(F13.7,X))") theta_vec(1), theta_vec(2), theta_vec(3), theta_vec(4), theta_vec(5), theta_vec(6) 
WRITE(1001,"(A,X,I3,X,A,F20.12)") "Log-likelihood value for sample",i," is ", llhood 
!CALL likelihood_ratio_fit(ccp_0,sample_file,total_obs(i))
CLOSE(UNIT=1001)
CLOSE(UNIT=1000)
CALL CPU_TIME(t1)
PRINT *, "Time was ", t1-t0
!!!!!!!!!!!!!!!!!!

!~CALL CPU_TIME(t0)
!~OPEN(UNIT=1000,FILE="Results_Parameters.txt")
!~OPEN(UNIT=1001,FILE="Llhood_value.txt")
!~OPEN(UNIT=999,FILE="totalobs.txt")
!~DO i=1,samples
!~WRITE(sample_file,"(A,I3,A)") "sample_",100+i,".txt"      !Technicality: we add 100 so that all file names have 3 digit numbers 
!~READ(999,"(I6)"), total_obs(i)
!~CALL BHHH_iteration(transition_matrix,transition_mean_matrix,transition_prior_matrix,theta_vec,sample_file,total_obs(i),&
!~&llhood,ccp_0)
!~WRITE(1000,"(6(F13.7,X))") theta_vec(1), theta_vec(2), theta_vec(3), theta_vec(4), theta_vec(5), theta_vec(6) 
!~WRITE(1001,"(A,X,I3,X,A,F20.12)") "Log-likelihood value for sample",i," is ", llhood 
!~!CALL likelihood_ratio_fit(ccp_0,sample_file,total_obs(i))
!~END DO
!~CLOSE(UNIT=1001)
!~CLOSE(UNIT=1000)
!~CLOSE(UNIT=999)
!~CALL CPU_TIME(t1)
!~PRINT *, "Time was ", t1-t0




END PROGRAM master_thesis