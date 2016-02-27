
/*
  static void shuntVariables(Argdecl);
  static void markTrail(Argdecl);
  static void markFrames(frame_t *frame, bcp_t l);
  static void markChoicepoints(Argdecl);
  static void markVariable(tagged_t *start);
  static void updateRelocationChain(tagged_t *curr, tagged_t *dest);
  static void sweepTrail(Argdecl);
  static void sweepFrames(frame_t *frame, bcp_t l);
  static void sweepChoicepoints(Argdecl);
  static void compressHeap(Argdecl);
 */

bool_t gc_usage(Argdecl);
bool_t gc_mode(Argdecl);
bool_t gc_trace(Argdecl);
bool_t gc_margin(Argdecl);
void compressTrail(Argdecl, bool_t from_gc);
void GarbageCollect(Argdecl);
