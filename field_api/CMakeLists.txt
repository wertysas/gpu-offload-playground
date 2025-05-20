
add_executable(struct_wrapped_field_allocs
    scheme_program.F90
    scheme_mod.F90)

target_link_libraries(struct_wrapped_field_allocs
    field_api_dp
    parkind_dp
    fiat
    OpenACC::OpenACC_Fortran
    OpenMP::OpenMP_Fortran
    )

