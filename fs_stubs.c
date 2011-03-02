#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>
#include <strings.h>
#include <string.h>
#include <stdio.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/signals.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/unixsupport.h>



CAMLprim value stub_file_actual_size(value path)
{
  CAMLparam1(path);
  CAMLlocal1(ret);
  struct stat buf;
  char err[100];
  const char *c_path = String_val(path);

  if (lstat(c_path, &buf) != 0){
	bzero(err, sizeof(err));
	snprintf(err, sizeof(err) - 1, "lstat failed: %s (%s)", strerror(errno), c_path);
	caml_failwith(err);
  }
  ret = caml_copy_int64(buf.st_blocks * 512);
  CAMLreturn(ret);
}
