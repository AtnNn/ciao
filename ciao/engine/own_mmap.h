#if defined(HAS_MMAP)

int own_fixed_mmap(void * addr, size_t len);
int own_fixed_munmap(void * addr, size_t len);
 
#endif
