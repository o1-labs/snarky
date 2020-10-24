#include <iostream>
#include <stdarg.h>
#include <time.h>

#include "libff/common/logging.hpp"

int normal_printf(const char* filename,
                  int line,
                  FILE* file,
                  const char* format,
                  va_list args) {
    return vfprintf(file, format, args);
}

int json_printf(const char* filename,
                int line,
                FILE* file,
                const char* format,
                va_list args) {
    time_t now;
    struct tm* now_utc;

    time(&now);
    now_utc = gmtime(&now);
    fprintf(file,
            "{\"timestamp\":\"%d-%02d-%02d %02d:%02d:%02d.%06dZ",
            now_utc->tm_year + 1900,
            now_utc->tm_mon,
            now_utc->tm_mday,
            now_utc->tm_hour,
            now_utc->tm_min,
            now_utc->tm_sec,
            0);
    fprintf (file, "\",\"level\":\"%s\"", "DEBUG");
    fprintf (file,
             ",\"source\":{\"module\":\"%s\",\"location\":\"File \\\"%s\\\", line %d, characters 0-1\"},\"message\":\"",
             "Snarky__Libsnark",
             filename,
             line);
    const int size = 1024;
    char inbuf[size];
    char outbuf[size];
    int ret = vsnprintf(inbuf, 1024, format, args);
    int i;
    int j;
    for (i = j = 0; i < size - 1; i++, j++) {
        if (inbuf[j] == '\0') {
            outbuf[i] = '\0';
            break;
        } else if (inbuf[j] == '\n') {
            outbuf[i] = '\\';
            i++;
            outbuf[i] = 'n';
        } else if (inbuf[j] == '\\') {
            outbuf[i] = '\\';
            i++;
            outbuf[i] = '\\';
        } else {
            outbuf[i] = inbuf[j];
        }
    }
    if (outbuf[i] != '\0') {
        outbuf[i+1] = '\0';
    }
    fputs(outbuf, file);
    fputs("\",\"metadata\":{}}\n", file);
    return ret;
}

int (*libff_printf_deferred)(const char* filename,
                              int line,
                              FILE* file,
                              const char* format,
                              va_list args) = &normal_printf;

FILE *libff_print_dest = stdout;

int may_close = false;

int close_libff_print_dest() {
    if (libff_print_dest) {
        int ret = 0;
        if (may_close) {
            ret = fclose(libff_print_dest);
            may_close = false;
        };
        libff_print_dest = NULL;
        return ret;
    } else {
        return 0;
    }
}

int libff_printf(const char* filename, int line, const char* format, ...) {
    if (libff_print_dest) {
        va_list args;
        va_start(args, format);
        int ret = (*libff_printf_deferred)(filename, line, libff_print_dest, format, args);
        va_end(args);
        return ret;
    }
    else {
        return 0;
    }
}

// extern C
void libff_set_printing_off() {
    close_libff_print_dest();
}

void libff_set_printing_stdout() {
    close_libff_print_dest();
    libff_print_dest = stdout;
}

void libff_set_printing_stderr() {
    close_libff_print_dest();
    libff_print_dest = stderr;
}

void libff_set_printing_file(char *file) {
    close_libff_print_dest();
    may_close = true;
    libff_print_dest = fopen(file, "a");
}

void libff_set_printing_normal() {
    libff_printf_deferred = &normal_printf;
}

void libff_set_printing_json() {
    libff_printf_deferred = &json_printf;
}
