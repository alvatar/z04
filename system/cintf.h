#ifdef __cplusplus
  #define EXTERN extern "C"
#else
  #define EXTERN
#endif


EXTERN int bgfx_init(void* window, int width, int height);
