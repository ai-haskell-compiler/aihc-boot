/* The operating system interface, on POSIX. */
#define _POSIX_C_SOURCE 200809L

#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#include "sys.h"

int sys_is_dir(const char *path)
{
    struct stat st;
    return stat(path, &st) == 0 && S_ISDIR(st.st_mode);
}

int sys_is_file(const char *path)
{
    struct stat st;
    return stat(path, &st) == 0 && S_ISREG(st.st_mode);
}

int sys_list_dir(const char *path, Vec *names)
{
    DIR *d = opendir(path);
    struct dirent *ent;
    vec_init(names);
    if (d == NULL) {
        return errno;
    }
    while ((ent = readdir(d)) != NULL) {
        if (strcmp(ent->d_name, ".") == 0 || strcmp(ent->d_name, "..") == 0) {
            continue;
        }
        vec_push(names, xstrdup(ent->d_name));
    }
    closedir(d);
    return 0;
}

int sys_create_dir_all(const char *path)
{
    char *p;
    size_t i;
    size_t n = strlen(path);
    if (n == 0 || sys_is_dir(path)) {
        return 0;
    }
    p = xstrdup(path);
    /* Create every prefix that ends before a `/`, then the whole path. */
    for (i = 1; i <= n; i++) {
        if (i == n || p[i] == '/') {
            char c = p[i];
            p[i] = '\0';
            if (mkdir(p, 0777) != 0 && errno != EEXIST) {
                int err = errno;
                free(p);
                return err;
            }
            p[i] = c;
        }
    }
    free(p);
    return sys_is_dir(path) ? 0 : EEXIST;
}

char *sys_find_in_path(const char *name)
{
    const char *path = getenv("PATH");
    const char *start;
    if (path == NULL) {
        return NULL;
    }
    start = path;
    for (;;) {
        const char *end = strchr(start, ':');
        size_t len = end ? (size_t)(end - start) : strlen(start);
        char *candidate;
        if (len == 0) {
            candidate = xstrdup(name);
        } else if (start[len - 1] == '/') {
            candidate = str_concat(xstrndup(start, len), name);
        } else {
            candidate = str_concat3(xstrndup(start, len), "/", name);
        }
        if (sys_is_file(candidate)) {
            return candidate;
        }
        free(candidate);
        if (end == NULL) {
            return NULL;
        }
        start = end + 1;
    }
}

unsigned long sys_pid(void)
{
    return (unsigned long)getpid();
}

void sys_ignore_sigpipe(void)
{
    signal(SIGPIPE, SIG_IGN);
}

/* Open path (or /dev/null) and move it to fd. Returns 0 or an errno
 * value. */
static int redirect(const char *path, int fd, int flags)
{
    int f = open(path != NULL ? path : "/dev/null", flags, 0666);
    if (f < 0) {
        return errno;
    }
    if (f != fd) {
        if (dup2(f, fd) < 0) {
            return errno;
        }
        close(f);
    }
    return 0;
}

int sys_run(char *const argv[], const char *in_path, const char *out_path,
            const char *err_path, int *status)
{
    int report[2];
    pid_t pid;
    int err = 0;
    ssize_t n;
    int st;
    /* The child writes the errno of a failed start to this pipe. A
     * successful execv closes the pipe, so the parent reads nothing. */
    if (pipe(report) != 0) {
        return errno;
    }
    if (fcntl(report[1], F_SETFD, FD_CLOEXEC) != 0) {
        err = errno;
        close(report[0]);
        close(report[1]);
        return err;
    }
    fflush(NULL);
    pid = fork();
    if (pid < 0) {
        err = errno;
        close(report[0]);
        close(report[1]);
        return err;
    }
    if (pid == 0) {
        close(report[0]);
        err = redirect(in_path, 0, O_RDONLY);
        if (err == 0) {
            err = redirect(out_path, 1, O_WRONLY | O_CREAT | O_TRUNC);
        }
        if (err == 0) {
            err = redirect(err_path, 2, O_WRONLY | O_CREAT | O_TRUNC);
        }
        if (err == 0) {
            signal(SIGPIPE, SIG_DFL);
            execvp(argv[0], argv);
            err = errno;
        }
        n = write(report[1], &err, sizeof err);
        (void)n;
        _exit(127);
    }
    close(report[1]);
    do {
        n = read(report[0], &err, sizeof err);
    } while (n < 0 && errno == EINTR);
    close(report[0]);
    while (waitpid(pid, &st, 0) < 0) {
        if (errno != EINTR) {
            return errno;
        }
    }
    if (n == (ssize_t)sizeof err) {
        return err;
    }
    if (WIFEXITED(st)) {
        *status = WEXITSTATUS(st);
    } else if (WIFSIGNALED(st)) {
        *status = 128 + WTERMSIG(st);
    } else {
        *status = 1;
    }
    return 0;
}
