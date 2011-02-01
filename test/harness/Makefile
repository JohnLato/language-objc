all:
	make -C ../../examples clean all
	make -C ../

	(cd attributes && make clean all)

	(cd analysis_enum && make clean all)
	(cd analysis_ext_decls && make clean all)
	(cd analysis_local_decls && make clean all)
	(cd analysis_type_check && make clean all)

	(cd bug5_dos_newline && make clean all)
	(cd bug21_sem_typedef && make clean all)
	(cd bug22_file_permission_cpp && make clean all)
	(cd bug30_preserve_int_repr && make)
	(cd bug31_pp_if_else && make)

	(cd bugn6_empty_file && make clean all)
