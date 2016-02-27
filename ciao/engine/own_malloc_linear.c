/*
 * new_alloc.c -- New memory manager.
 * AFSID           : $__Header$
 * Author          : Manuel Carro
 * Created On      : Mon Dec 15 13:50:46 1997
 * Last Modified By: Manuel Carro
 * Last Modified On: Thu Jan 19 19:36:42 2006
 * Update Count    : 1030
 * Status          : Seems to work correctly
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#if !defined(Win32i86)
# include <strings.h>
#endif

#if defined(DEBUG)
#include <stdio.h>
#endif

#include "configure.h"
#include "termdefs.h"
#include "streams_defs.h"
#include "debug.h"

#include "own_malloc_defs.h"
#include "mmap_defs.h"

#if defined(USE_OWN_MALLOC)

#if !defined(TRUE)                     /* Probably standalone compilation */
# define TRUE 1
# define FALSE 0
typedef unsigned long int tagged_t;                 /* base to align memory */
typedef int bool_t;
#endif

#if !defined(MIN_MEM_ALLOC)
# define MIN_MEM_ALLOC 32768                /* tagged_t --  from configure.h */
#endif

#define ALIGN sizeof(tagged_t)       /* blocks suitably aligned for any use */
#define TW_TO_CHARS(Tw) (Tw)*ALIGN
#define CHARS_TO_TW(Chars) ((Chars)%ALIGN==0 ? (Chars)/ALIGN : (Chars)/ALIGN+1)

static int min_mem_alloc = MIN_MEM_ALLOC;

/* List of free and asigned blocks. Kept in memory order, to allow
   recompaction upon block freeing.  Use forward and backwards link for
   this.  Compaction is not always possible: blocks of memory can come from
   sparse addresses. */

typedef struct mem_block_str_ mem_block_str_t;
struct mem_block_str_ {
  bool_t block_is_free;                            /* Might be only one bit */
  mem_block_str_t *fwd, *bck;
  mem_block_str_t *next_free, *prev_free;
  int size;                                   /* Measured in tagged_t words */
  tagged_t mem_ptr;                     /* 1st word of the allocated memory */
};

typedef mem_block_str_t MEM_BLOCK;

#define HEADER_SIZE ((CHARS_TO_TW(sizeof(MEM_BLOCK))) - 1)

static MEM_BLOCK *block_list = NULL;
static MEM_BLOCK *free_block_list = NULL;

static MEM_BLOCK *locate_block(int tagged_w);
static tagged_t *reserve_block(int requested_chars, MEM_BLOCK *list);
static void insert_block(MEM_BLOCK *block, 
                         MEM_BLOCK *previous,
                         MEM_BLOCK *next);
static void insert_free_block(MEM_BLOCK *block);
static void remove_free_block(MEM_BLOCK *block);
static MEM_BLOCK *create_new_block(int size_in_chars);
static void dealloc_block(MEM_BLOCK *block);

#define find_mem_block(Ptr) ((MEM_BLOCK *)(ptr - HEADER_SIZE))


void print_mem_map()
{
  MEM_BLOCK *moving = block_list;

  while (moving) {
    fprintf(stderr, "addr: \t %x \t len: \t %8d \t free: %d\n", 
           (unsigned int)moving, 
           moving->size,
           moving->block_is_free);
    moving = moving->fwd;
  }
}


#if defined(HAS_MMAP)
tagged_t *mmap_base = NULL;

void init_own_malloc() {
  tagged_t *returned_base;
  int mmap_size = AddressableSpace;       // In bytes
  MEM_BLOCK *new_block;
  int fd_mmap_file = -1;
  off_t zero_offset = 0;

  // Chunks of code identical to those in configure.c - keep them in sync!
#if !defined(ANONYMOUS_MMAP)
  char *mmap_file = MMAP_FILE;

  if ((fd_mmap_file = open(MMAP_FILE, O_RDWR|O_CREAT)) < 0) {
    fprintf(stderr,
	    "\n\nPANIC: Error opening map file in own_malloc_linear.c\n\n");
    exit(1);
  }
#endif

  mmap_base = (tagged_t *)MallocBase; 
  returned_base = (tagged_t *)mmap((char *)mmap_base, 
		       mmap_size,
		       PROT_READ|PROT_WRITE,
		       MMAP_FLAGS, 
		       fd_mmap_file,
		       zero_offset);
  if (returned_base != mmap_base) {
    fprintf(stderr, "PANIC: cannot mmap() own memory at %p!!!\n", mmap_base);
    fprintf(stderr, "PANIC: returned pointer is %p!!!\n", returned_base);
    exit(-1);
  }
  // What follows is basically a create_new_block which allocates 
  // all memory we can address
  new_block = (MEM_BLOCK *)mmap_base;
  new_block->block_is_free = TRUE;
  new_block->size = CHARS_TO_TW(mmap_size - TW_TO_CHARS(2*HEADER_SIZE));
  insert_block(new_block, NULL, block_list);
  insert_free_block(new_block);
  // print_mem_map();
}
#else
void init_own_malloc() {}
#endif


/* Search for a block with memory enough. Requested size comes in tagged_t
   words.  If no block found, return NULL. */

static MEM_BLOCK *locate_block(requested_tagged_w)
     int requested_tagged_w;
{
  MEM_BLOCK *running;

#if defined(DEBUG)
  if (debug_mem)
    printf("locate_block was requested a block of %d words\n",
           requested_tagged_w);
  if (requested_tagged_w < 1)
    printf("Uh? locate_block was requested a block of %d words!\n",
           requested_tagged_w);
#endif

    running = free_block_list;
    while(running && running->size < requested_tagged_w) 
      running = running->next_free;

#if defined(DEBUG)
  if (debug_mem) {
    if (!running )
      printf("locate_block did not find a block of %d words\n",
             requested_tagged_w);
  }
#endif
  
  return running;
}


/* Given a block which has enough room to allocate the requested chars,
   return pointer to memory area and split the block into two if needed. */

static tagged_t *reserve_block(req_tagged, block)
     int        req_tagged;
     MEM_BLOCK *block;
{
  MEM_BLOCK *new_block;

#if defined(DEBUG)
  if (block->size < req_tagged){
    printf(
    "**** Fatal error: reserve_block received a block smaller than needed\n");
    return NULL;
  }
#endif

  if (block->size > req_tagged + HEADER_SIZE) {
    /* Block is still free -- do not remove from free blocks list  */
    /*
    block->size -= (req_tagged + HEADER_SIZE);
    new_block = (MEM_BLOCK *)((tagged_t *)block + HEADER_SIZE + block->size);
    new_block->block_is_free = FALSE;
    new_block->size = req_tagged;
    insert_block(new_block, block, block->fwd);
    return &(new_block->mem_ptr);
    */
    new_block = (MEM_BLOCK *)((tagged_t *)block + HEADER_SIZE + req_tagged);
    new_block->size = block->size - HEADER_SIZE - req_tagged;
    new_block->block_is_free = TRUE;

    block->size = req_tagged;
    block->block_is_free = FALSE;
    insert_block(new_block, block, block->fwd);
    remove_free_block(block);
    insert_free_block(new_block);
    return &(block->mem_ptr);
  } else {                                    /* Exactly the size we want */
    block->block_is_free = FALSE;
    remove_free_block(block);
    return &(block->mem_ptr);
  }
}


/* Link a block into the list.  previous == NULL if first block, next ==
   NULL if last block . */

static void insert_block(block, previous, next)
     MEM_BLOCK *block, *previous, *next;
{
  block->bck = previous;
  block->fwd = next;
  if (previous != NULL)
    previous->fwd = block;
  else                             /* Then block is the first in the list */
    block_list = block;
  if (next != NULL) next->bck = block;
}


/* Link a block into the free blocks list. */

static void insert_free_block(block)
     MEM_BLOCK *block;
{
  block->prev_free = NULL;
  block->next_free = free_block_list;
  if (free_block_list)
    free_block_list->prev_free = block;
  free_block_list = block;    
#if defined(DEBUG)
  if (debug_mem)
    printf("*Put back block of %d words\n", block->size);
#endif
}

/* Remove a block from the free blocks list. */

static void remove_free_block(block)
     MEM_BLOCK *block;
{
  MEM_BLOCK *previous = block->prev_free,
            *next     = block->next_free;

  if (next)
    next->prev_free = previous;
  if (previous)
    previous->next_free = next;
  else 
    free_block_list = next;
}


/* Create a new block with a given size, rounded upwards to be a multiple of
   an ALIGNed word.  If the creation was sucessful, insert them into the
   general blocks list and into the free blocks list. */

static MEM_BLOCK *create_new_block(int size_in_tagged_w) {
#if defined(HAS_MMAP)
  return NULL;
#else
  MEM_BLOCK *new_block;

  new_block = (MEM_BLOCK *)malloc(TW_TO_CHARS(size_in_tagged_w + HEADER_SIZE));
  
  if (!new_block) {
# if defined(DEBUG)
    printf("malloc: could not allocate %d words of memory\n", 
           size_in_tagged_w + HEADER_SIZE);
# endif
    return NULL;
  }
  new_block->size = size_in_tagged_w;
  new_block->block_is_free = TRUE;
  insert_block(new_block, NULL, block_list);
  insert_free_block(new_block);
  return new_block;
#endif
}

/* Mark a block as unused.  Collapse it with the surronding ones if possible */

static void dealloc_block(block)
     MEM_BLOCK *block;
{
  MEM_BLOCK *next, *prev;

  block->block_is_free = TRUE;

  if (((next = block->fwd) != NULL) &&        /* Check if next block free */
      (next->block_is_free == TRUE) &&
      ((tagged_t *)block + block->size + HEADER_SIZE == (tagged_t *)next)){
    block->size += next->size + HEADER_SIZE;
    if ((block->fwd = next->fwd) != NULL)
      block->fwd->bck = block;
    remove_free_block(next);
  }
  
  if (((prev = block->bck) != NULL) &&
      (prev->block_is_free == TRUE) &&
      ((tagged_t *)prev + prev->size + HEADER_SIZE == (tagged_t *)block)){
    prev->size += block->size + HEADER_SIZE;
    if ((prev->fwd = block->fwd) != NULL)
      prev->fwd->bck = prev;
  } else insert_free_block(block);
}


 /* Our three beloved calls: alloc, dealloc, realloc. */

#define ADJUST_BLOCK(size) (size < min_mem_alloc ? min_mem_alloc : size)

tagged_t *own_malloc(size)
     int size;
{
  MEM_BLOCK *block;
  tagged_t *pointer_returned;
  int size_to_reserve;

#if defined(DEBUG)
  if (size <= 0){
    printf("own_malloc received a request of %d chars... what should I do?\n",
           size);
    return NULL;
  }
#endif
  
  size_to_reserve = CHARS_TO_TW(size);
  if ((block = locate_block(size_to_reserve)) == NULL) {
    block = create_new_block(ADJUST_BLOCK(size_to_reserve));
    if (block == NULL){                     /* malloc could not stand it! */
#if defined(DEBUG)
      printf("own_malloc: could not reserve %d chars\n", size);
#endif
      return NULL;
    }
    /* min_mem_alloc *= 2; */
  }
  pointer_returned = reserve_block(size_to_reserve, block);
#if defined(DEBUG)
  if (debug_mem)
    printf("own_malloc returned %x, %d chars\n", 
           (unsigned int)pointer_returned, size);
#endif

  return pointer_returned;
}

void own_free(ptr)
     tagged_t *ptr;
{
  MEM_BLOCK *block_to_dealloc;

  if ((block_to_dealloc = find_mem_block(ptr))){
    dealloc_block(block_to_dealloc);
  }
}


/* From the manpages:
   realloc() changes the size of the block pointed to by ptr to size bytes
   and returns a pointer to the (possibly moved) block.  The contents will
   be unchanged up to the lesser of the new and old sizes.  If ptr is NULL,
   realloc() behaves like malloc() for the specified size.  If size is zero
   and ptr is not a null pointer, the object pointed to is freed.
*/

tagged_t *own_realloc(ptr,size)                            /* size in chars */
     tagged_t *ptr;
     int     size;
{
  MEM_BLOCK *old_block;
  int        size_to_copy;

  if (ptr == NULL) 
    return own_malloc(size);
  else if (size == 0) {
    own_free(ptr);
    return NULL;
  }

  //  fprintf(stderr, "\nRealloc asked for %d words\n", CHARS_TO_TW(size));

  old_block = find_mem_block(ptr);                /* Gives error messages */

  if (old_block) {
    MEM_BLOCK *new_block = NULL;
    tagged_t    *new_mem_area;
    int        size_in_tagged_w = CHARS_TO_TW(size);
   
    //    fprintf(stderr, "Old block has size %d words\n", old_block->size);
    //    fprintf(stderr, "Space needed %d words\n", size - old_block->size);
    //    fprintf(stderr, "Next block has a size of %d words\n", 
    //            old_block->fwd->size);
    //    if (old_block->fwd->block_is_free) 
    //fprintf("***** Next block has size %d words\n", old_block->fwd->size);

    //    if (size_in_tagged_w > 50 && old_block->fwd && 
    //        old_block->fwd->block_is_free &&
    //    ((old_block->size + old_block->fwd->size) >= size_in_tagged_w))
    //        fprintf(stderr,
    //                " +++ Requesting %d: Can use next block directly!\n",
    //                size_in_tagged_w);

#if defined(FAST_REALLOC)
    next_block = old_block->fwd;
    if (next_block && next_block->block_is_free &&
      ((tagged_t *)old_block+old_block->size+HEADER_SIZE == (tagged_t *)next_block)
   && (old_block->size + next_block->size + HEADER_SIZE >= size_in_tagged_w)){
      // We can use old + next!
      dealloc_block(old_block); // Mark as free and say it is what we need
      new_block = old_block;
    }
#endif
    if ((new_block == NULL) &&   // Could not be found otherwise
        (new_block = locate_block(size_in_tagged_w)) == NULL) {
      new_block = create_new_block(ADJUST_BLOCK(size_in_tagged_w));
      if (new_block == NULL){ // print_mem_map();
#if defined(DEBUG)
        printf("own_realloc: could not reserve %d chars!\n", size);
#endif
        return NULL;
      } 
    }
    
    new_mem_area = reserve_block(size_in_tagged_w, new_block);
    size_to_copy = old_block->size > size_in_tagged_w ?
                   size_in_tagged_w : old_block->size;
    (void)memcpy(new_mem_area, 
                 &(old_block->mem_ptr),
                 TW_TO_CHARS(size_to_copy));
    own_free(ptr);
#if defined(DEBUG)
    if (debug_mem)
      printf("own_realloc returned %x, %d chars\n",
             (unsigned int)new_mem_area, size);
#endif    

    return new_mem_area;
  } else return NULL;
} 

#endif                                                  /* USE_OWN_MALLOC */
