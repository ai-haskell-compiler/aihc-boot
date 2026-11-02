/* The operating system interface.
 *
 * sys.c is the only file that uses POSIX. All other files use only the
 * C89 standard library.
 */
#ifndef AIHC_SYS_H
#define AIHC_SYS_H

#include "util.h"

/* Whether path is a directory, or a regular file. Both follow symbolic
 * links, as Rust's Path::is_dir and Path::is_file. */
int sys_is_dir(const char *path);
int sys_is_file(const char *path);

/* The names of the entries of a directory (char *), without `.` and
 * `..`, in no special order. Returns 0 on success, or an errno value. */
int sys_list_dir(const char *path, Vec *names);

/* Create a directory and its parents, as Rust's fs::create_dir_all.
 * Returns 0 on success, or an errno value. */
int sys_create_dir_all(const char *path);

/* The first file DIR/name for the directories of PATH, as a fresh string.
 * An empty entry of PATH means the current directory. NULL when PATH is
 * not set or no directory has the file. */
char *sys_find_in_path(const char *name);

/* The process id. */
unsigned long sys_pid(void);

/* Ignore SIGPIPE, so that a write to a closed pipe fails with EPIPE. */
void sys_ignore_sigpipe(void);

/* Run the program argv[0] with the arguments argv (NULL at the end).
 * sys_run looks up a name without `/` in PATH. No shell runs. The
 * standard input comes from the file in_path. The standard output and
 * the standard error go to the files out_path and err_path. A NULL path
 * means /dev/null. On success, sys_run returns 0 and sets *status to the
 * exit status (0 for success), or to 128 plus the signal number when a
 * signal stopped the program. When the program does not start, sys_run
 * returns an errno value. */
int sys_run(char *const argv[], const char *in_path, const char *out_path,
            const char *err_path, int *status);

#endif
