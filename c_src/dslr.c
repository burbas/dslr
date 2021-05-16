#include <stdio.h>
#include <stdint.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <inttypes.h>
#include <ctype.h>
#include <unistd.h>
#include <math.h>
#include <string.h>
#include <gphoto2/gphoto2.h>
#include <gphoto2/gphoto2-camera.h>
#include "erl_nif.h"

static ErlNifResourceType* camera_res = NULL;

struct CameraEnv {
  Camera *camera;
  GPContext *context;
};

static void
camera_cleanup(ErlNifEnv* env, void *arg) {
  struct CameraEnv *ce = arg;

  gp_camera_exit(ce->camera, ce->context);
  enif_release_resource(arg);

}

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
  ErlNifResourceFlags flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
  camera_res = enif_open_resource_type(env, "dslr_nif", "dslr_camera",
				       &camera_cleanup,
				       flags, 0);
  return 0;
}

static ERL_NIF_TERM
capture_image(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  void *ptr;
  char output_file[32];
  if(enif_get_string(env, argv[1], output_file, sizeof(output_file), ERL_NIF_LATIN1) <= 0) {
    return enif_make_tuple2(env,
                            enif_make_atom(env, "error"),
                            enif_make_atom(env, "filepath"));
  }
  int fd, retval;
  if(!enif_get_resource(env, argv[0], camera_res, &ptr)) {
    return enif_make_tuple2(env,
                            enif_make_atom(env, "error"),
                            enif_make_atom(env, "referenced_resource_error"));
  }
  struct CameraEnv *ce = ptr;

  // Take picture
  CameraFile *file;
  CameraFilePath filepath;

  retval = gp_camera_capture(ce->camera, GP_CAPTURE_IMAGE, &filepath, ce->context);
  fd = open(output_file, O_CREAT | O_WRONLY, 0644);
  retval = gp_file_new_from_fd(&file, fd);

  gp_camera_file_get(ce->camera, filepath.folder, filepath.name,
                     GP_FILE_TYPE_NORMAL, file, ce->context);

  // Delete from camera
  gp_camera_file_delete(ce->camera, filepath.folder, filepath.name, ce->context);

  // Return filepath
  return enif_make_tuple2(env,
                          enif_make_atom(env, "ok"),
                          enif_make_string(env, output_file, ERL_NIF_LATIN1));

}

static ERL_NIF_TERM
create_env(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  void *ptr = enif_alloc_resource(camera_res, sizeof(struct CameraEnv));
  struct CameraEnv *ce = ptr;
  int retval;

  ce->context = gp_context_new();

  // Initialize camera
  gp_camera_new(&ce->camera);
  retval = gp_camera_init(ce->camera, ce->context);
  if(retval != GP_OK) {
    return enif_make_tuple2(env,
                            enif_make_atom(env, "error"),
                            enif_make_atom(env, "camera_not_found"));
  }

  // Return object
  ERL_NIF_TERM ret = enif_make_resource(env, ptr);
  enif_release_resource(ptr);
  return ret;
}

static ErlNifFunc nif_funcs[] = {
                                 {"create_env", 0, create_env},
                                 {"capture_photo", 2, capture_image}
};

ERL_NIF_INIT(dslr, nif_funcs, load, NULL, NULL, NULL);
