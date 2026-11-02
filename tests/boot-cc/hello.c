/* A C89 program for the boot C compiler check.
 * It uses the parts of the C library that an interpreter needs:
 * stdio, stdarg, stdlib, string and setjmp. */
#include <setjmp.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static jmp_buf on_error;

static void fail(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stdout, fmt, ap);
    va_end(ap);
    longjmp(on_error, 1);
}

struct cell {
    long value;
    struct cell *next;
};

int main(void)
{
    struct cell *list = NULL;
    struct cell *c;
    long sum = 0;
    int i;
    char buf[32];

    for (i = 1; i <= 10; i++) {
        c = (struct cell *)malloc(sizeof *c);
        if (c == NULL) {
            return 1;
        }
        c->value = i;
        c->next = list;
        list = c;
    }
    while (list != NULL) {
        c = list;
        sum += c->value;
        list = c->next;
        free(c);
    }
    sprintf(buf, "sum=%ld", sum);
    if (strcmp(buf, "sum=55") != 0) {
        return 1;
    }
    printf("%s\n", buf);

    if (setjmp(on_error) == 0) {
        fail("error %d\n", 42);
        return 1;
    }
    printf("hello from tcc\n");
    return 0;
}
