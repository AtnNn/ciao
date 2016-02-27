/*
** configure_float.h
** 
** Made by Edison Mera
** Login   <edison@vaioedison>
** 
** Started on  Thu Jan  4 14:13:40 2007 Edison Mera
** Last update Thu Jan  4 14:17:39 2007 Edison Mera
*/

  unsigned long *lx, *ly, *lz;
  unsigned long mask;
  unsigned int index;
  unsigned int shift;
  unsigned int split;
  int i;
  int orig_size = sizeof(x) / sizeof(mask);
  int size;

  lx = (unsigned long *)&x;
  ly = (unsigned long *)&y;
  lz = (unsigned long *)&z;

  size = orig_size;

  /* It is necessary to init to 0 to avoid garbage in the unused bits: */

  for (i = 0; i < size; i++) {
    lx[i] = 0;
    ly[i] = 0;
    lz[i] = 0;
  }

  /* printf("/\* x=0x%08lX:0x%08lX:0x%08lX *\/\n", lx[0],lx[1],lx[2]); */
  /* printf("/\* y=0x%08lX:0x%08lX:0x%08lX *\/\n", ly[0],ly[1],ly[2]); */
  /* printf("/\* z=0x%08lX:0x%08lX:0x%08lX *\/\n", lz[0],lz[1],lz[2]); */

  x = 1;
  y = -x;
  get_mask_descr(size, lx, ly, &mask, &index, &shift);

  printf("#define IEEE854_MASK_NEGATIVE   0x%08lX\n", mask);
  printf("#define IEEE854_INDEX_NEGATIVE  %d\n", index);
  printf("#define IEEE854_SHIFT_NEGATIVE  %d\n", shift);

  x = 1;
  y = 2;
  get_mask_descr(size, lx, ly, &mask, &index, &shift);

  printf("#define IEEE854_MASK_EXPONENT   0x%08lX\n", mask);
  printf("#define IEEE854_INDEX_EXPONENT  %d\n", index);
  printf("#define IEEE854_SHIFT_EXPONENT  %d\n", shift);

  x = 1;
  y = 1;
  z = 1;

  /* This have mantissa0_length bits (plus a fixed bit if 31) */

  y /= 2;
  x += y;
  get_mask_descr(size, lx, lz, &mask, &index, &split);
  split++;
  for (i=1; i < split; i++) {
    y /=2;
    x +=y;
  }
  get_mask_descr(size, lx, lz, &mask, &index, &shift);
  printf("#define IEEE854_MASK_MANTISSA0_0  0x%08lX\n", mask);
  printf("#define IEEE854_INDEX_MANTISSA0_0 %d\n", index);
  printf("#define IEEE854_SPLIT_MANTISSA0_0 %d\n", mantissa0_length - split);

  z = x;
  for (i=split; i<mantissa0_length; i++) {
    y /= 2;
    x += y;
  }
  get_mask_descr(size, lx, lz, &mask, &index, &shift);
  printf("#define IEEE854_MASK_MANTISSA0_1  0x%08lX\n", mask);
  printf("#define IEEE854_INDEX_MANTISSA0_1 %d\n", index);
  printf("#define IEEE854_SHIFT_MANTISSA0_1 %d\n", shift);

  lx[index] = lx[index] | (mask << shift);

  /* This have 32 bits */
  z = x;
  y /= 2;
  x += y;
  get_mask_descr(size, lx, lz, &mask, &index, &split);
  split++;
  for (i=1; i < split; i++) {
    y /= 2;
    x += y;
  }
  get_mask_descr(size, lx, lz, &mask, &index, &shift);
  printf("#define IEEE854_MASK_MANTISSA1_0  0x%08lX\n", mask);
  printf("#define IEEE854_INDEX_MANTISSA1_0 %d\n", index);
  printf("#define IEEE854_SPLIT_MANTISSA1_0 %d\n", 32 - split);

  z = x;
  for(i=split; i < 32; i++) {
    y /= 2;
    x += y;
  }
  get_mask_descr(size, lx, lz, &mask, &index, &shift);
  printf("#define IEEE854_MASK_MANTISSA1_1  0x%08lX\n", mask);
  printf("#define IEEE854_INDEX_MANTISSA1_1 %d\n", index);
  printf("#define IEEE854_SHIFT_MANTISSA1_1 %d\n", shift);

  printf("#define IEEE854_MANTISSA_LENGTH %d\n\n", mantissa0_length + 32);
