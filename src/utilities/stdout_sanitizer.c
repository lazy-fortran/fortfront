#include <stdio.h>

#if defined(_WIN32)
#include <io.h>
#include <fcntl.h>

static int sanitize_fd_windows(int fd) {
    if (_isatty(fd)) {
        return 1;
    }
    if (_lseeki64(fd, 0, SEEK_SET) == -1) {
        return -2;
    }
#if defined(_MSC_VER)
    if (_chsize_s(fd, 0) != 0) {
        if (_chsize(fd, 0) != 0) {
            return -3;
        }
    }
#else
    if (_chsize(fd, 0) != 0) {
        return -3;
    }
#endif
    return 0;
}
#else
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>

static int sanitize_fd_posix(int fd) {
    if (isatty(fd)) {
        return 1;
    }
    if (lseek(fd, 0, SEEK_SET) == (off_t)-1) {
        return -2;
    }
    if (ftruncate(fd, 0) != 0) {
        return -3;
    }
    return 0;
}
#endif

int ff_sanitize_fd(int fd) {
    if (fd < 0) {
        return -1;
    }
    fflush(NULL);
#if defined(_WIN32)
    return sanitize_fd_windows(fd);
#else
    return sanitize_fd_posix(fd);
#endif
}

int ff_sanitize_path(const char *path) {
    int fd;
    int result;

    if (path == NULL) {
        return -10;
    }

#if defined(_WIN32)
    fd = _open(path, _O_RDWR | _O_BINARY);
#else
    fd = open(path, O_RDWR);
#endif
    if (fd < 0) {
        return -11;
    }

    result = ff_sanitize_fd(fd);

#if defined(_WIN32)
    _close(fd);
#else
    close(fd);
#endif

    return result;
}
