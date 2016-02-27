
bool_t prolog_bootversion(register worker_t *w);
bool_t prolog_sourcepath(register worker_t *w);
bool_t prolog_open(register worker_t *w);
bool_t prolog_close(register worker_t *w);
bool_t prolog_unix_popen(register worker_t *w);
void ENG_perror(char *s);
bool_t prolog_current_input(register worker_t *w);
bool_t prolog_set_input(register worker_t *w);
bool_t prolog_current_output(register worker_t *w);
bool_t prolog_set_output(register worker_t *w);
bool_t prolog_get_stream(register worker_t *w);
bool_t prolog_replace_stream(register worker_t *w);
bool_t prolog_stream_code(register worker_t *w);
bool_t character_count(register worker_t *w);
bool_t line_position(register worker_t *w);
bool_t line_count(register worker_t *w);
