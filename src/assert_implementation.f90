submodule(assert_interface) assert_implementation

  implicit none

contains

  module procedure assert

    if (.not. assertion) error stop description

  end procedure

end submodule assert_implementation
