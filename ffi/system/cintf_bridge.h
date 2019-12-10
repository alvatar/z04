#ifdef __cplusplus
  #define EXTERN extern "C"
#else
  #define EXTERN
#endif

#include "SDL2/SDL_syswm.h"

EXTERN int system_window_init(SDL_SysWMinfo* wmi, int width, int height);
