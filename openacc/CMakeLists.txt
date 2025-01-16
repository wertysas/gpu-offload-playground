
add_executable(openacc_async_ptr open_acc_async_ptr.f90)
target_link_libraries(openacc_async_ptr OpenACC::OpenACC_Fortran)

