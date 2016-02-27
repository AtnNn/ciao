#if defined(TABLING)
struct l_gen
{
  struct gen *node;
  struct space *space;
  struct space *orig_space;
  struct space *clone_space;
  int *orig_attrs;
  struct l_gen *next;
};

#define CHANGE_SPACE(ARG,SPACE)			\
  {						\
    MAKE_UNDO_CHANGE_SPACE((ARG),(SPACE));	\
    space = (SPACE);				\
  }
#endif
