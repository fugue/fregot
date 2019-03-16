/* Helper functions for PCRE2.
 *
 * Mostly these help with the fact that we can't easily access defines such as
 * PCRE2_CASELESS from Haskell. */

#include "pcre2.h.in"

uint32_t pcre_simple_compile_options(
        int caseless,
        int multiline,
        int ucp,
        int utf) {
    return 0 |
        (caseless  ? PCRE2_CASELESS  : 0) |
        (multiline ? PCRE2_MULTILINE : 0) |
        (ucp       ? PCRE2_UCP       : 0) |
        (utf       ? PCRE2_UTF       : 0);
}

uint32_t pcre_simple_match_options(
        int notempty_atstart) {
    return 0 |
        (notempty_atstart ? PCRE2_NOTEMPTY_ATSTART : 0);
}
