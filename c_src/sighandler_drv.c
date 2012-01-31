/* -------------------------------------------------------------------

   Copyright (c) 2012 Andrew Tunnell-Jones. All Rights Reserved.

   This file is provided to you under the Apache License,
   Version 2.0 (the "License"); you may not use this file
   except in compliance with the License.  You may obtain
   a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing,
   software distributed under the License is distributed on an
   "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
   KIND, either express or implied.  See the License for the
   specific language governing permissions and limitations
   under the License.

   -------------------------------------------------------------------- */

#include <signal.h>
#include <string.h>
#include <unistd.h>

#include <erl_driver.h>
#include <erl_interface.h>

#if ERL_DRV_EXTENDED_MAJOR_VERSION < 2
#define ErlDrvSizeT int
#define ErlDrvSSizeT int
#endif

#define SH_DEFINESIG(SIG) if(SIG) printf(SIG)

static int sh_pipe[2];
static void* sh_handlers_old[45];
static int sh_handlers_installed[45];

typedef struct _sh_drv_t {
  ErlDrvPort erl_port;
  ErlDrvTermData term_port;
  ErlDrvTermData term_installed;
  ErlDrvTermData term_removed;
} sh_drv_t;

static int sh_init(void);
static ErlDrvData sh_start(ErlDrvPort port, char* cmd);
static void sh_stop(ErlDrvData handle);
static void sh_io(ErlDrvData port, ErlDrvEvent ev);
static void sh_finish(void);
static ErlDrvSSizeT sh_call(ErlDrvData drv_data,
			    unsigned int sig,
			    char *buf,
			    ErlDrvSizeT len,
			    char **rbuf,
			    ErlDrvSizeT rlen,
			    unsigned int *flags);
static int sh_handler_install(int no);
static int sh_handler_remove(int no);
static void sh_handler(int sig);

static ErlDrvEntry sh_drv_entry = {
    sh_init,                          /* init */
    sh_start,                         /* startup */
    sh_stop,                          /* shutdown */
    NULL,                             /* output */
    sh_io,                            /* ready_input */
    NULL,                             /* ready_output */
    "sighandler_drv",                 /* the name of the driver */
    sh_finish,                        /* finish */
    NULL,                             /* handle */
    NULL,                             /* control */
    NULL,                             /* timeout */
    NULL,                             /* process */
    NULL,                             /* ready_async */
    NULL,                             /* flush */
    sh_call,                          /* call */
    NULL,                             /* event */
    ERL_DRV_EXTENDED_MARKER,          /* ERL_DRV_EXTENDED_MARKER */
    ERL_DRV_EXTENDED_MAJOR_VERSION,   /* ERL_DRV_EXTENDED_MAJOR_VERSION */
    ERL_DRV_EXTENDED_MINOR_VERSION,   /* ERL_DRV_EXTENDED_MINOR_VERSION */
    0                                 /* ERL_DRV_FLAGs */
};

DRIVER_INIT(sighandler_drv) {
  return &sh_drv_entry;
}

static int sh_init(void) {
  if (sh_pipe[0]) return -1;
  if (pipe(sh_pipe) != 0) return -1;

  return 0;
}

static ErlDrvData sh_start(ErlDrvPort port, char* buf) {
  sh_drv_t* retval = (sh_drv_t*) driver_alloc(sizeof(sh_drv_t));
  retval->erl_port = port;
  retval->term_port = driver_mk_port(port);
  driver_select(port, (ErlDrvEvent)(size_t) sh_pipe[0], DO_READ, 1);
  return (ErlDrvData) retval;
}

static void sh_stop(ErlDrvData edd) {
  sh_drv_t* dd = (sh_drv_t*) edd;
  int i;
  driver_select(dd->erl_port, (ErlDrvEvent)(size_t) sh_pipe[0], DO_READ, 0);
  driver_free(dd);
  for (i = 0; i < (46); i++) {
    if (sh_handlers_installed[i]) sh_handler_remove(i);
  }
}

static void sh_io(ErlDrvData edd, ErlDrvEvent ev) {
  sh_drv_t* dd = (sh_drv_t*) edd;
  int sig = 0;
  if ((read(sh_pipe[0], &sig, sizeof(sig)) == 1)) {
    ErlDrvTermData spec[] = {ERL_DRV_PORT, dd->term_port,
			     ERL_DRV_INT, (ErlDrvTermData) sig,
			     ERL_DRV_TUPLE, 2};
    driver_output_term(dd->erl_port, spec, sizeof(spec) / sizeof(spec[0]));
  }
}

static void sh_finish(void) {
  close(sh_pipe[0]);
  close(sh_pipe[1]);
}

static ErlDrvSSizeT sh_call(ErlDrvData drv_data,
			    unsigned int sig,
			    char *buf,
			    ErlDrvSizeT len,
			    char **rbuf,
			    ErlDrvSizeT rlen,
			    unsigned int *flags) {
  ei_term arg;
  int version;
  int index = 0;
  int rindex = 0;
  char* atom_txt = "sig_err";
  if (sig < 1 || sig > 45) goto respond;
  ei_decode_version(buf, &index, &version);
  ei_decode_ei_term(buf, &index, &arg);
  if (arg.ei_type != ERL_ATOM_EXT) return -1;
  if (strncmp(arg.value.atom_name, "toggle", 6) == 0) {
    if (sh_handlers_installed[sig]) {
      if (sh_handler_remove(sig)) atom_txt = "removed";
    } else {
      if (sh_handler_install(sig)) atom_txt = "installed";
    }
  } else if (strncmp(arg.value.atom_name, "status", 6) == 0) {
    atom_txt = sh_handlers_installed[sig] ? "active" : "inactive";
  } else {
    return -1;
  }
 respond:
  ei_encode_version(NULL, &rindex);
  ei_encode_atom(NULL, &rindex, atom_txt);
  if (rindex < rlen) {
    *rbuf = driver_alloc(rindex);
  }
  rindex = 0;
  ei_encode_version(*rbuf, &rindex);
  ei_encode_atom(*rbuf, &rindex, atom_txt);
  return rlen;
}

static int sh_handler_install(int no) {
  if ((sh_handlers_old[no] = signal(no, sh_handler)) == SIG_ERR) {
    return 0;
  }
  sh_handlers_installed[no] = 1;
  return 1;
}

static int sh_handler_remove(int no) {
  signal(no, sh_handlers_old[no]);
  sh_handlers_old[no] = NULL;
  sh_handlers_installed[no] = 0;
  return 1;
}

static void sh_handler(int no) {
  write(sh_pipe[1], &no, 1);
}
