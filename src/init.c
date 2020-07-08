
#include <R.h>
#include <Rinternals.h>
#include <stdlib.h>		// for NULL
#include <R_ext/Rdynload.h>

extern void odec(void(* odeparms)(int *, double *));
extern void forcc(void (* odeforcs)(int *, double *));
extern void derivsc(int *neq, double *t, double *y, double *ydot, double *yout, int *ip);

static const R_CMethodDef cfunc[] = {
	{ "odec",	(DL_FUNC) &odec,	1},
	{ "forcc",	(DL_FUNC) &forcc,	1},
	{ "derivsc",	(DL_FUNC) &derivsc,	6},
	{ NULL,		NULL,			0}
};

void R_init_StrathE2E2(DllInfo *dll) {
	R_registerRoutines(dll, NULL, NULL, cfunc, NULL);
	R_useDynamicSymbols(dll, FALSE);
}

