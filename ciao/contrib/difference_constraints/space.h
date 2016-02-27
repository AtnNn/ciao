#include <limits.h>

#define INI_SIZE_G      10
#define FACTOR_G        2
#define MAX             INT_MAX

struct space
{
  int size;
  int limit;
  int **edges;
  int *pi;
};


struct space *space;

struct space* create_space();
void delete_space(struct space *space);
void print_space(struct space *space);
struct space* clone_space(struct space *s);
struct space* proy_space(Argdecl,int size,tagged_t* vars,int* attrs,int undo);
void print_variable_space(struct space *s, int id);
int isValue_space(struct space*s, int id);
void delay_space(Argdecl,struct space *s, int v);
void reset_space(Argdecl,struct space *s, int x, int y, int v);
void full_abstraction_space(Argdecl,struct space *s, int v1, int v2);
void normalize_space(Argdecl,struct space *s, int i, int j, int L, int U);
int new_diff_var_space(Argdecl,struct space *s);
bool_t add_diff_const_space(Argdecl,struct space *s, int x, int y, int d);
void dijkstra_space(Argdecl,struct space*s, int v);
void get_shortest_path_space(Argdecl,struct space *s, int size, int *orig_vars);
int is_more_general_space(struct space *s1, int size, 
			  struct space *s2, int *vars);
