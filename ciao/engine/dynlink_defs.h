/*
  static void unload_if_present PROTO((char *libname));
  static void add_to_loaded_objects PROTO((char *libname, void *handle));
 */


bool_t prolog_dynlink(Argdecl);
bool_t prolog_dynunlink(Argdecl);  /* JFMC */
