#ifndef USE_ASSERTIONS
#define USE_ASSERTIONS .true.
#endif
module assert_interface
  !! author: Damian Rouson
  !!
  !! Utility for runtime checking of logical assertions.
  !!
  !! Instructions
  !! ------------
  !! Compile with -DUSE_ASSERTIONS=.false. to define the logical parameter named "assertions" and to thereby
  !! facilitate the elimination of assertions during the dead-code removal phase of optimizing compilers:
  !!
  !!    gfortran -cpp -DUSE_ASSERTIONS=.false. -c assertions_interface.f90
  !!
  !! or set the corresponding NO_ASSERTIONS variable defined in this directory's CMakeLists.txt:
  !!
  !!    FC=caf cmake <opencoarrays-source-path> -DNO_ASSERTIONS=ON
  !!
  !! Conditioning assertion calls on the "assertions" compile-time constant enables optimizing compilers
  !! to eliminate assertion calls via dead-code-removal optimiztion.
  !!
  !! Use case 1
  !! ----------
  !!    Pass the optional success argument & check for false return value as an indication of assertion failure:
  !!
  !!    use opencoarrays_assertions_interface, only : assert, assertions
  !!    if (assertions) call assert( 2 > 1, "2 > 1", success)
  !!    if (success/=0) call my_error_handler()
  !!
  !! Use case 2
  !! ----------
  !!    Error-terminate if the assertion fails:
  !!
  !!    use opencoarrays_assertions_interface, only : assert,assertions
  !!    if (assertions) call assert( 2 > 1, "always true inequality")
  !!
  implicit none
  private
  public :: assert
  public :: assertions

  logical, parameter :: assertions=USE_ASSERTIONS

  interface
    pure module subroutine assert(assertion, description, diagnostic_data, success, error_message)
      !! On false assertion, error-terminate or, if present(success), set success = assertion
      implicit none
      logical, intent(in) :: assertion
        !! Most assertions are expressions: call assert(i>0, "i>0")
      character(len=*), intent(in) :: description
        !! Brief statement of what is being asserted -- might be a literal transcription (see above example)
      class(*), intent(in), optional :: diagnostic_data
        !! Optional data to printed if assertion==.false.
      logical, intent(out), optional :: success
        !! Optional copy of the assertion dummy argument
      character(len=:), intent(out), optional, allocatable :: error_message
        !! Optional message allocated only if assertion==.false. .and. present(success)
    end subroutine
  end interface
end module assert_interface
