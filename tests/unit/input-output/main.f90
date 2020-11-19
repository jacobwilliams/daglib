program main
    use dag_input_output_test, only: &
            dag_input_output_dag_input_output => test_dag_input_output
    use Vegetables_m, only: TestItem_t, testThat, runTests

    implicit none

    call run()
contains
    subroutine run()
        type(TestItem_t) :: tests
        type(TestItem_t) :: individual_tests(1)

        individual_tests(1) = dag_input_output_dag_input_output()
        tests = testThat(individual_tests)

        call runTests(tests)
    end subroutine run
end program
