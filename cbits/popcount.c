#include <stddef.h>
#include <stdint.h>

size_t popcount (char const * src, size_t len) {
   size_t result = 0; 
   for (size_t i = 0; i < len; i++) {
       result += __builtin_popcount(src[i]);
   }
   return result;
}

#define StgWord uint64_t
#define IS_WORD_ALIGNED(p) \
    ((((uintptr_t) (p)) & (sizeof(StgWord) - 1)) == 0)

size_t popcount2 (char const * src, size_t len) {
   char const *end = src + len;
   size_t result = 0;
   for (; ! IS_WORD_ALIGNED(src); src++) {
       result += __builtin_popcount(*src);
   }
   for (; src + sizeof(StgWord) < end; src += sizeof(StgWord)) {
       result += __builtin_popcountll(*(StgWord *) src);
   }
   for (; src < end; src++) {
       result += __builtin_popcount(*src);
   }
   return result;
}
