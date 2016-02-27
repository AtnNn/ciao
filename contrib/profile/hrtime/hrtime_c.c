/*
C to Prolog Adapter for the hrtime library.

Implemented by Edison Mera.

 */
#include <hrtime.h>

void get_hrtimef(struct hrtime_struct *hr, double *dest) {
  hrtime_t r;
  get_hrtime(hr, &r);
  *dest = (double)r; 
}
void get_hrvtimef(struct hrtime_struct *hr, double *dest) {
  hrtime_t r;
  get_hrvtime(hr, &r);
  *dest = (double)r; 
}
void get_hrutimef(struct hrtime_struct *hr, double *dest) {
  hrtime_t r;
  get_hrutime(hr, &r);
  *dest = (double)r; 
}
void get_hrstimef(struct hrtime_struct *hr, double *dest) {
  hrtime_t r;
  get_hrstime(hr, &r);
  *dest = (double)r; 
}
void get_current_hrtimef(double *dest) {
  hrtime_t r;
  get_current_hrtime(&r);
  *dest = (double)r; 
}

void get_hrtime_selff(double *dest) {
  hrtime_t r;
  get_hrtime_self(&r);
  *dest = (double)r;
}
void get_hrvtime_selff(double *dest) {
  hrtime_t r;
  get_hrvtime_self(&r);
  *dest = (double)r;
}
void get_hrutime_selff(double *dest) {
  hrtime_t r;
  get_hrutime_self(&r);
  *dest = (double)r;
}
void get_hrstime_selff(double *dest) {
  hrtime_t r;
  get_hrstime_self(&r);
  *dest = (double)r;
}

long hrtime_initl() {
  return hrtime_init();
}
long get_hrtime_structl(pid_t pid, struct hrtime_struct **dest) {
  return get_hrtime_struct(pid, dest);
}
long free_hrtime_structl(struct hrtime_struct *hr) {
  return free_hrtime_struct(hr);
}
