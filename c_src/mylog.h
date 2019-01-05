#pragma once

#ifdef NDEBUG

#define LOG(str)
#define LOGA(fmt, ...)
#define WARN(str)
#define WARNA(fmt, ...)

#else
#define _GNU_SOURCE
#include <unistd.h>

#include <sys/syscall.h>
#include <sys/types.h>

#define LOG(str) enif_fprintf(stdout, "\e[30;1m[%34s#%-5d@(tid:%ld)]\e[0m %s\r\n", __FUNCTION__,__LINE__,syscall(SYS_gettid), str)
#define LOGA(fmt, ...) fprintf(stdout, "\e[30;1m[%34s#%-5d@(tid:%ld)]\e[0m " fmt "\r\n", __FUNCTION__,__LINE__,syscall(SYS_gettid), __VA_ARGS__)
#define WARN(str) enif_fprintf(stdout, "\e[31;1m[%34s#%-5d@(tid:%ld)]\e[0m %s\r\n", __FUNCTION__,__LINE__,syscall(SYS_gettid), str)
#define WARNA(fmt, ...) fprintf(stdout, "\e[31;1m[%34s#%-5d@(tid:%ld)]\e[0m " fmt "\r\n", __FUNCTION__,__LINE__,syscall(SYS_gettid), __VA_ARGS__)

#endif

