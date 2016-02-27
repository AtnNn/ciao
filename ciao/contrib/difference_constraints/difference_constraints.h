/////////////////////////////////////////
//// DEFINES ////////////////////////////
/////////////////////////////////////////

#define	TRUE		1
#define	FALSE		0

/////////////////////////////////////////
//// GLOBAL_VARS ////////////////////////
/////////////////////////////////////////

tagged_t args[3];
tagged_t tmp_term1, tmp_term2, undo_term;
int index_macro;

tagged_t atom_incr_dc_num_vars;
tagged_t atom_decr_dc_num_vars;
tagged_t functor_forward_trail;
tagged_t functor_put_dc_value;
tagged_t functor_put_dc_pi;
tagged_t functor_put_dc_space;
tagged_t functor_dbm_id;
