/* Kilo -- A very simple editor in less than 1-kilo lines of code (as counted
 *         by "cloc"). Does not depend on libcurses, directly emits VT100
 *         escapes on the terminal.
 *
 * -----------------------------------------------------------------------
 *
 * Copyright (C) 2016 Salvatore Sanfilippo <antirez at gmail dot com>
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 *  *  Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *
 *  *  Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#define KILO_VERSION "0.0.1"

#ifdef __linux__
#define _POSIX_C_SOURCE 200809L
#endif

#include <termios.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <errno.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <unistd.h>
#include <stdarg.h>
#include <fcntl.h>
#include <signal.h>
#include <assert.h>
/* This structure represents a single line in the terminal we are editing. */
typedef struct erow
{
    int idx;     /* Row index in the terminal, zero-based. */
    int size;    /* Size of the row, excluding the null term. */
    char *chars; /* Row content. */
} erow;

/* This structure represents a single chunk of the file */
typedef struct echunk
{
    int idx;             /* Chunk index in the file, zero-based. */
    long init_size;      /* Size of the chunk when it initializes, helps to figure out offset of the next chunk */
    long size;           /* Size of the chunk, including the new-line term and EOF term */
    int dirty;           /* Chunk modified but not saved */
    char *filename;      /* File to save this chunk */
    char *content;       /* Chunk's content */
    FILE *fp;            /* Use to read/write to correspond file */
    struct echunk *prev; /* Previous chunk */
    struct echunk *next; /* Next chunk */
} echunk;

typedef struct ecursor
{
    echunk *in_chunk;
    off_t offset;
} ecursor;

struct editorConfig
{
    int cx, cy;     /* Cursor x and y position in characters */
    int rowoff;     /* Offset of row displayed. */
    int coloff;     /* Offset of column displayed. */
    int screenrows; /* Number of rows that we can show */
    int screencols; /* Number of cols that we can show */
    int numrows;    /* Number of rows */
    int rawmode;    /* Is terminal raw mode enabled? */
    erow *row;      /* Rows */
    int dirty;      /* File modified but not saved. */
    char *filename; /* Currently open filename */
    char statusmsg[80];
    time_t statusmsg_time;

    // Add by EyLinGer
    size_t numchunks;    /* Number of chunks */
    echunk *chunks_head; /* Dummy head of chunk list */
    echunk *last_chunk;  /* last chunk */
    ecursor cc;          /* Content cursor */
    ecursor dsc;         /* Display start cursor */
    ecursor dec;         /* Display start cursor */
    char *line_buffer;   /* Buffer to store a line, its' buffer size depends on windows' width */
    FILE *fp;            /* Currently open file */
    int eof;             /*Have we reached the enf of file?*/
};

static struct editorConfig E;

enum KEY_ACTION
{
    KEY_NULL = 0,    /* NULL */
    CTRL_C = 3,      /* Ctrl-c */
    CTRL_D = 4,      /* Ctrl-d */
    CTRL_F = 6,      /* Ctrl-f */
    CTRL_H = 8,      /* Ctrl-h */
    TAB = 9,         /* Tab */
    CTRL_L = 12,     /* Ctrl+l */
    ENTER = 13,      /* Enter */
    CTRL_Q = 17,     /* Ctrl-q */
    CTRL_S = 19,     /* Ctrl-s */
    CTRL_U = 21,     /* Ctrl-u */
    ESC = 27,        /* Escape */
    BACKSPACE = 127, /* Backspace */
    /* The following are just soft codes, not really reported by the
         * terminal directly. */
    ARROW_LEFT = 1000,
    ARROW_RIGHT,
    ARROW_UP,
    ARROW_DOWN,
    DEL_KEY,
    HOME_KEY,
    END_KEY,
    PAGE_UP,
    PAGE_DOWN
};

/* Utilities*/
size_t numDigit(size_t num)
{
    size_t ans = 0;
    while (num > 0)
    {
        num /= 10;
        ++ans;
    }
    return ans;
}

void initChunk(echunk **pchunk)
{
    assert(*pchunk != NULL);
    (*pchunk)->idx = E.numchunks;
    (*pchunk)->init_size = 0;
    (*pchunk)->size = 0;
    (*pchunk)->dirty = 0;
    (*pchunk)->content = (char *)malloc(4 * 1024 * sizeof(char));
    assert((*pchunk)->content != NULL);
    size_t chunk_fnlen = strlen(E.filename) + strlen(".kilo_chunk") + numDigit(E.numchunks) + 1;
    (*pchunk)->filename = (char *)malloc(chunk_fnlen * sizeof(char));
    assert((*pchunk)->filename != NULL);
    (*pchunk)->fp = NULL;
    (*pchunk)->prev = NULL;
    (*pchunk)->next = NULL;

    if (sprintf((*pchunk)->filename, "%s.kilo_chunk%ld", E.filename, E.numchunks) < 0)
    {
        printf("sprinf failed\n");
        exit(EXIT_FAILURE);
    }
}

void appendChunk(echunk **pchunk)
{
    assert((*pchunk) != NULL);
    E.last_chunk->next = *pchunk;   // ... <->last_chunk->chunk
    (*pchunk)->prev = E.last_chunk; // ... <->last_chunk<->chunk
    (*pchunk)->next = NULL;         // ... <->last_chunk<->chunk->NULL
    E.last_chunk = *pchunk;         // ... <-> ... <->last_chunk(chunk)->NULL
    ++E.numchunks;
}

void deactivateChunk(echunk **pchunk)
{
    if ((*pchunk) != NULL && (*pchunk) != E.chunks_head)
    {
        if ((*pchunk)->content != NULL)
        {
            (*pchunk)->fp = fopen((*pchunk)->filename, "w");
            fwrite((*pchunk)->content, sizeof(char), (*pchunk)->size, (*pchunk)->fp);
            fflush((*pchunk)->fp);
            free((*pchunk)->content);
            (*pchunk)->content = NULL;
            fclose((*pchunk)->fp);
        }
    }
}

void activateChunk(echunk **pchunk)
{
    if ((*pchunk)->content == NULL && (*pchunk) != E.chunks_head)
    {
        (*pchunk)->content = (char *)malloc((*pchunk)->size);
        assert((*pchunk)->content != NULL);
        (*pchunk)->fp = fopen((*pchunk)->filename, "r");
        fread((*pchunk)->content, sizeof(char), (*pchunk)->size, (*pchunk)->fp);
        fclose((*pchunk)->fp);
    }
}

void loadAChunk()
{
    if (E.eof != 1)
    {
        echunk *chunk = (echunk *)malloc(sizeof(echunk));
        initChunk(&chunk);
        size_t nchar = fread(chunk->content, sizeof(char), (4 * 1024 - 1) * sizeof(char), E.fp); // read 4KB once at a time
        chunk->content[nchar] = '\0';
        chunk->init_size = nchar * sizeof(char);
        chunk->size = chunk->init_size;
        appendChunk(&chunk);

        if (feof(E.fp))
        {
            E.eof = 1;
        }
    }
    return;
}

void initRows(erow **prows)
{
    assert((*prows) != NULL);
    for (int i = 0; i < E.screenrows; ++i)
    {
        char *ps = (char *)malloc(E.screencols);
        assert(ps != NULL);
        (*prows)[i].chars = ps;
        (*prows)[i].idx = E.numrows++;
    }
}

void cursorBackward(ecursor *pcursor, int n)
{
    assert(pcursor->in_chunk != NULL);
    int i = 0;
    while (i < n)
    {
        if (pcursor->offset < 0)
        {
            if (pcursor->in_chunk->prev != E.chunks_head)
            {
                activateChunk(&pcursor->in_chunk->prev);
                pcursor->offset = pcursor->in_chunk->prev->size - 1;
                pcursor->in_chunk = pcursor->in_chunk->prev;
                continue;
            }
            else
            {
                return;
            }
        }
        else if (pcursor->offset > 0)
        {
            --pcursor->offset;
            ++i;
        }
        else
        {
            --pcursor->offset;
        }
    }
}

void cursorForward(ecursor *pcursor, int n)
{
    assert(pcursor->in_chunk != NULL);
    int i = 0;
    while (i < n)
    {
        if (pcursor->offset > pcursor->in_chunk->size - 1)
        {
            if (pcursor->in_chunk->next != NULL)
            {
                activateChunk(&pcursor->in_chunk->next);
                pcursor->offset = 0;
                pcursor->in_chunk = pcursor->in_chunk->next;
                continue;
            }
            else
            {
                loadAChunk();
                if (pcursor->in_chunk == E.last_chunk)
                {
                    pcursor->offset = pcursor->in_chunk->size - 1;
                    break;
                }
                continue;
            }
        }
        else if (pcursor->offset < pcursor->in_chunk->size - 1)
        {
            ++pcursor->offset;
            ++i;
        }
        else
        {
            ++pcursor->offset;
        }
    }
}

int backwardALine(void)
{
    assert(E.dsc.in_chunk != NULL);
    int i = 0;
    char c = '\0';
    while (i < E.screencols)
    {
        if (E.dsc.offset < 0)
        {
            activateChunk(&(E.dsc.in_chunk->prev));
            if (E.dsc.in_chunk->prev != E.chunks_head)
            {
                E.dsc.offset = E.dsc.in_chunk->prev->size - 1;
                E.dsc.in_chunk = E.dsc.in_chunk->prev;
                continue;
            }
            else
            {
                E.dsc.offset = 0;
                return i;
            }
        }
        // E.dsc.offset >= 0
        c = E.dsc.in_chunk->content[E.dsc.offset];
        if (c == '\n')
        {
            if (E.dsc.offset == E.dsc.in_chunk->size - 1)
            {
                loadAChunk();
                if (E.dsc.in_chunk == E.last_chunk)
                {
                    return i;
                }
                else
                {
                    E.dsc.in_chunk = E.dsc.in_chunk->next;
                    E.dsc.offset = 0;
                }
            }
            else
            {
                --E.dsc.offset;
                return i;
            }
        }
        else
        {
            if (E.dsc.offset > 0)
            {
                --E.dsc.offset;
                ++i;
            }
            else
            {
                --E.dsc.offset;
            }
        }
    }
    return i;
}

/* Load a 'line' forward from chunks, return how many characters the 'line' contains */
int forwardALine(void)
{
    assert(E.dec.in_chunk != NULL);
    int nchars = 0;
    char c = '\0';
    while (nchars < E.screencols)
    {
        c = E.dec.in_chunk->content[E.dec.offset];
        if (c == '\0')
        {
            loadAChunk();
            if (E.dec.in_chunk == E.last_chunk)
            {
                break;
            }
            else
            {
                E.dec.in_chunk = E.dec.in_chunk->next;
                E.dec.offset = 0;
                continue;
            }
        }
        ++E.dec.offset;
        E.line_buffer[nchars++] = c;
        if (c == '\n')
        {
            break;
        }
    }
    E.line_buffer[nchars] = '\0';
    return nchars;
}

void editorSetStatusMessage(const char *fmt, ...);
void editorMoveCursor(int key);

/* ======================= Low level terminal handling ====================== */

static struct termios orig_termios; /* In order to restore at exit.*/

void disableRawMode(int fd)
{
    /* Don't even check the return value as it's too late. */
    if (E.rawmode)
    {
        tcsetattr(fd, TCSAFLUSH, &orig_termios);
        E.rawmode = 0;
    }
}

/* Called at exit to avoid remaining in raw mode. */
void editorAtExit(void)
{
    echunk *pchunk = E.chunks_head->next;
    while (pchunk != NULL)
    {
        deactivateChunk(&pchunk);
        remove(pchunk->filename);
        pchunk = pchunk->next;
    }
    fwrite("\x1b[2J", sizeof(char), 4, stdout);
    fwrite("\x1b[H", sizeof(char), 3, stdout);
    fflush(stdout);
    disableRawMode(STDIN_FILENO);
}

/* Raw mode: 1960 magic shit. */
int enableRawMode(int fd)
{
    struct termios raw;

    if (E.rawmode)
        return 0; /* Already enabled. */
    if (!isatty(STDIN_FILENO))
        goto fatal;
    atexit(editorAtExit);
    if (tcgetattr(fd, &orig_termios) == -1)
        goto fatal;

    raw = orig_termios; /* modify the original mode */
    /* input modes: no break, no CR to NL, no parity check, no strip char,
     * no start/stop output control. */
    raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
    /* output modes - disable post processing */
    raw.c_oflag &= ~(OPOST);
    /* control modes - set 8 bit chars */
    raw.c_cflag |= (CS8);
    /* local modes - choing off, canonical off, no extended functions,
     * no signal chars (^Z,^C) */
    raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
    /* control chars - set return condition: min number of bytes and timer. */
    raw.c_cc[VMIN] = 0;  /* Return each byte, or zero for timeout. */
    raw.c_cc[VTIME] = 1; /* 100 ms timeout (unit is tens of second). */

    /* put terminal in raw mode after flushing */
    if (tcsetattr(fd, TCSAFLUSH, &raw) < 0)
        goto fatal;
    E.rawmode = 1;
    return 0;

fatal:
    errno = ENOTTY;
    return -1;
}

/* Read a key from the terminal put in raw mode, trying to handle
 * escape sequences. */
int editorReadKey(int fd)
{
    int nread;
    char c, seq[3];
    while ((nread = read(fd, &c, 1)) == 0)
        ;
    if (nread == -1)
        exit(1);

    while (1)
    {
        switch (c)
        {
        case ESC: /* escape sequence */
            /* If this is just an ESC, we'll timeout here. */
            if (read(fd, seq, 1) == 0)
                return ESC;
            if (read(fd, seq + 1, 1) == 0)
                return ESC;

            /* ESC [ sequences. */
            if (seq[0] == '[')
            {
                if (seq[1] >= '0' && seq[1] <= '9')
                {
                    /* Extended escape, read additional byte. */
                    if (read(fd, seq + 2, 1) == 0)
                        return ESC;
                    if (seq[2] == '~')
                    {
                        switch (seq[1])
                        {
                        case '3':
                            return DEL_KEY;
                        case '5':
                            return PAGE_UP;
                        case '6':
                            return PAGE_DOWN;
                        }
                    }
                }
                else
                {
                    switch (seq[1])
                    {
                    case 'A':
                        return ARROW_UP;
                    case 'B':
                        return ARROW_DOWN;
                    case 'C':
                        return ARROW_RIGHT;
                    case 'D':
                        return ARROW_LEFT;
                    case 'H':
                        return HOME_KEY;
                    case 'F':
                        return END_KEY;
                    }
                }
            }

            /* ESC O sequences. */
            else if (seq[0] == 'O')
            {
                switch (seq[1])
                {
                case 'H':
                    return HOME_KEY;
                case 'F':
                    return END_KEY;
                }
            }
            break;
        default:
            return c;
        }
    }
}

/* Use the ESC [6n escape sequence to query the horizontal cursor position
 * and return it. On error -1 is returned, on success the position of the
 * cursor is stored at *rows and *cols and 0 is returned. */
int getCursorPosition(int ifd, int ofd, int *rows, int *cols)
{
    char buf[32];
    unsigned int i = 0;

    /* Report cursor location */
    if (write(ofd, "\x1b[6n", 4) != 4)
        return -1;

    /* Read the response: ESC [ rows ; cols R */
    while (i < sizeof(buf) - 1)
    {
        if (read(ifd, buf + i, 1) != 1)
            break;
        if (buf[i] == 'R')
            break;
        i++;
    }
    buf[i] = '\0';

    /* Parse it. */
    if (buf[0] != ESC || buf[1] != '[')
        return -1;
    if (sscanf(buf + 2, "%d;%d", rows, cols) != 2)
        return -1;
    return 0;
}

/* Try to get the number of columns in the current terminal. If the ioctl()
 * call fails the function will try to query the terminal itself.
 * Returns 0 on success, -1 on error. */
int getWindowSize(int ifd, int ofd, int *rows, int *cols)
{
    struct winsize ws;

    if (ioctl(1, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0)
    {
        /* ioctl() failed. Try to query the terminal itself. */
        int orig_row, orig_col, retval;

        /* Get the initial position so we can restore it later. */
        retval = getCursorPosition(ifd, ofd, &orig_row, &orig_col);
        if (retval == -1)
            goto failed;

        /* Go to right/bottom margin and get position. */
        if (write(ofd, "\x1b[999C\x1b[999B", 12) != 12)
            goto failed;
        retval = getCursorPosition(ifd, ofd, rows, cols);
        if (retval == -1)
            goto failed;

        /* Restore position. */
        char seq[32];
        snprintf(seq, 32, "\x1b[%d;%dH", orig_row, orig_col);
        if (write(ofd, seq, strlen(seq)) == -1)
        {
            /* Can't recover... */
        }
        return 0;
    }
    else
    {
        *cols = ws.ws_col;
        *rows = ws.ws_row;
        return 0;
    }

failed:
    return -1;
}

/* ======================= Editor change implementation ======================= */
/* Insert the specified char at the current prompt position. */
void editorInsertChar(int c)
{
    echunk *chunk = E.cc.in_chunk;
    char *ps = (char *)realloc(chunk->content, chunk->size + 1);
    assert(ps != NULL);
    memmove(ps + E.cc.offset + 1, ps + E.cc.offset, chunk->size - E.cc.offset - 1);
    chunk->content = ps;
    chunk->content[E.cc.offset] = c;
    ++chunk->size;
    ++E.cc.in_chunk->dirty;
    ++E.dirty;
    editorMoveCursor(ARROW_RIGHT);
}

/* Delete the char at the current prompt position. */
void editorDelChar()
{
    echunk *chunk = E.cc.in_chunk;
    memmove(chunk->content + E.cc.offset, chunk->content + E.cc.offset + 1, chunk->size - E.cc.offset - 1);
    char *ps = (char *)realloc(chunk->content, chunk->size - 1);
    assert(ps != NULL);
    chunk->content = ps;
    --E.cc.in_chunk->size;
    ++E.cc.in_chunk->dirty;
    ++E.dirty;
    editorMoveCursor(ARROW_LEFT);
}

/* Load the specified program in the editor memory and returns 0 on success
 * or 1 on error. */
/* TODO: Make this configurable 
    Try to load 4KiB if file size is greater than 4KiB and returns 0 on success or 1 on error.*/
int editorOpen(char *filename)
{
    E.dirty = 0;
    free(E.filename);
    size_t fnlen = strlen(filename) + 1;
    E.filename = (char *)malloc(fnlen);
    memcpy(E.filename, filename, fnlen);

    E.fp = fopen(filename, "r"); // Change by EyLinGer
    if (!E.fp)                   // Change by EyLinGer
    {
        if (errno != ENOENT)
        {
            perror("Opening file");
            exit(1);
        }
        return 1;
    }

    loadAChunk();
    E.cc.in_chunk = E.dec.in_chunk = E.dsc.in_chunk = E.chunks_head->next;
    E.dirty = 0;
    return 0;
}

/* Save the current file on disk. Return 0 on success, 1 on error. */
int editorSave(void)
{
    FILE *fp = NULL;
    echunk *pchunk = E.chunks_head->next;
    char *savename = (char *)malloc(strlen(E.filename) + strlen(".save") + 1);
    if (sprintf(savename, "%s.save", E.filename) < 0)
    {
        printf("sprinf failed\n");
        exit(EXIT_FAILURE);
    }
    fp = fopen(savename, "w");
    while (pchunk != NULL)
    {
        activateChunk(&pchunk);
        fwrite(pchunk->content, sizeof(char), pchunk->size, fp);
        deactivateChunk(&pchunk);
        pchunk->dirty = 0;
        pchunk = pchunk->next;
    }
    if (E.eof != 1)
    {
        char *buf = (char *)malloc(4 * 1024 * sizeof(char));
        size_t nread = 0;
        fpos_t pos;
        fgetpos(E.fp, &pos);
        while (E.eof != 1)
        {
            nread = fread(buf, sizeof(char), 4 * 1024, E.fp);
            fwrite(buf, sizeof(char), nread, fp);
            fflush(fp);
            if (feof(E.fp))
            {
                E.eof = 1;
            }
        }
        fsetpos(E.fp, &pos);
        free(buf);
        E.eof = 0;
    }
    fclose(fp);
    E.dirty = 0;
    editorSetStatusMessage("File Saved!");
    return 0;
}

/* ============================= Terminal update ============================ */

/* We define a very simple "append buffer" structure, that is an heap
 * allocated string where we can append to. This is useful in order to
 * write all the escape sequences in a buffer and flush them to the standard
 * output in a single call, to avoid flickering effects. */
struct abuf
{
    char *b;
    int len;
};

#define ABUF_INIT \
    {             \
        NULL, 0   \
    }

void abAppend(struct abuf *ab, const char *s, int len)
{
    char *new = realloc(ab->b, ab->len + len);

    if (new == NULL)
        return;
    memcpy(new + ab->len, s, len);
    ab->b = new;
    ab->len += len;
}

void abFree(struct abuf *ab)
{
    free(ab->b);
}

/* This function writes the whole screen using VT100 escape characters
 * starting from the logical state of the editor in the global state 'E'. */
void editorRefreshScreen(void)
{
    activateChunk(&E.dsc.in_chunk);
    E.dec = E.dsc;
    struct abuf ab = ABUF_INIT;
    abAppend(&ab, "\x1b[2J", 4);   // clear terminal
    abAppend(&ab, "\x1b[?25l", 6); /* Hide cursor. */
    abAppend(&ab, "\x1b[H", 3);    /* Go home. */
    for (int i = 0; i < E.numrows; ++i)
    {
        int linelen = forwardALine();
        strncpy(E.row[i].chars, E.line_buffer, linelen);
        E.row[i].size = linelen;

        abAppend(&ab, E.row[i].chars, E.row[i].size);
        if (E.row[i].chars[E.row[i].size - 1] == '\n')
        {
            abAppend(&ab, "\r", 1);
        }
        else
        {
            abAppend(&ab, "\r\n", 2);
        }
    }
    abAppend(&ab, "\x1b[7m", 4);
    int msglen = strlen(E.statusmsg);
    if (msglen && time(NULL) - E.statusmsg_time < 5)
    {
        abAppend(&ab, E.statusmsg, msglen <= E.screencols ? msglen : E.screencols);
    }
    abAppend(&ab, "\x1b[0m", 4);
    char buf[32];
    snprintf(buf, sizeof(buf), "\x1b[%d;%dH", E.cy + 1, E.cx + 1);
    abAppend(&ab, buf, strlen(buf));
    abAppend(&ab, "\x1b[?25h", 6); /* Show cursor. */
    fwrite(ab.b, sizeof(char), ab.len, stdout);
    fflush(stdout);
    abFree(&ab);
    deactivateChunk(&E.dsc.in_chunk->prev);
    deactivateChunk(&E.dec.in_chunk->next);
}

/* Set an editor status message for the second line of the status, at the
 * end of the screen. */
void editorSetStatusMessage(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(E.statusmsg, sizeof(E.statusmsg), fmt, ap);
    va_end(ap);
    E.statusmsg_time = time(NULL);
}

/* ========================= Editor events handling  ======================== */

// TODO:MAY CHANGE editorMoveCursor

/* Handle cursor position change because arrow keys were pressed. */
void editorMoveCursor(int key)
{
    int n = 0;
    switch (key)
    {
    case ARROW_LEFT:
        if (E.cx > 0)
        {
            --E.cx;
            cursorBackward(&E.cc, 1);
        }
        break;
    case ARROW_RIGHT:
        if (E.cx < E.row[E.cy].size - 1)
        {
            ++E.cx;
            cursorForward(&E.cc, 1);
        }
        break;
    case ARROW_UP:
        if (E.cy <= 0)
        {
            n = backwardALine();
            cursorBackward(&E.cc, n + E.cx);
        }
        else
        {
            cursorBackward(&E.cc, E.row[E.cy - 1].size + E.cx);
            E.cx = 0;
        }
        E.cy = E.cy <= 0 ? 0 : E.cy - 1;
        break;
    case ARROW_DOWN:
        if (E.cy >= E.numrows - 1)
        {
            cursorForward(&E.dsc, E.row[0].size);
            cursorForward(&E.cc, E.row[E.cy].size - E.cx);
            n = forwardALine();
        }
        else
        {
            cursorForward(&E.cc, E.row[E.cy].size - E.cx);
        }
        E.cx = 0;
        E.cy = E.cy < E.numrows - 1 ? E.cy + 1 : E.numrows - 1;
        break;
    }
}

/* Process events arriving from the standard input, which is, the user
 * is typing stuff on the terminal. */
#define KILO_QUIT_TIMES 3
void editorProcessKeypress(int fd)
{
    /* When the file is modified, requires Ctrl-q to be pressed N times
     * before actually quitting. */
    static int quit_times = KILO_QUIT_TIMES;
    //int times = 0;
    int c = editorReadKey(fd);
    switch (c)
    {
    case CTRL_C: /* Ctrl-c */
        /* We ignore ctrl-c, it can't be so simple to lose the changes
         * to the edited file. */
        break;
    case CTRL_Q: /* Ctrl-q */
        /* Quit if the file was already saved. */
        if (E.dirty && quit_times)
        {
            editorSetStatusMessage("WARNING!!! File has unsaved changes. "
                                   "Press Ctrl-Q %d more times to quit.",
                                   quit_times);
            quit_times--;
            return;
        }
        exit(0);
        break;
    case CTRL_S: /* Ctrl-s */
        editorSetStatusMessage("Saving...");
        editorRefreshScreen();
        editorSave();
        break;
    case CTRL_F:
        //editorFind(fd);
        break;
    case BACKSPACE: /* Backspace */
    case CTRL_H:    /* Ctrl-h */
    case DEL_KEY:
        editorDelChar();
        break;
    case PAGE_UP:
    case PAGE_DOWN:
        /*
        times = E.screenrows;
        while (times--){
            editorMoveCursor(c == PAGE_UP ? ARROW_UP : ARROW_DOWN);
        }*/
        break;

    case ARROW_UP:
    case ARROW_DOWN:
    case ARROW_LEFT:
    case ARROW_RIGHT:
        editorMoveCursor(c);
        break;
    case CTRL_L: /* ctrl+l, clear screen */
        /* Just refresht the line as side effect. */
        break;
    case ESC:
        /* Nothing to do for ESC in this mode. */
        break;
    default:
        editorInsertChar(c);
        break;
    }

    quit_times = KILO_QUIT_TIMES; /* Reset it to the original value. */
}

void updateWindowSize(void)
{
    if (getWindowSize(STDIN_FILENO, STDOUT_FILENO,
                      &E.screenrows, &E.screencols) == -1)
    {
        perror("Unable to query the screen for size (columns / rows)");
        exit(1);
    }
    E.screenrows -= 1; /* Get room for status bar. */
}

void initEditor(void)
{
    E.cx = 0;
    E.cy = 0;
    E.rowoff = 0;
    E.coloff = 0;
    E.numrows = 0;
    E.row = NULL;
    E.dirty = 0;
    E.filename = NULL;
    E.numchunks = 0;
    E.cc.in_chunk = E.dsc.in_chunk = E.dec.in_chunk = NULL; // Add by EyLinGer
    E.cc.offset = E.dsc.offset = E.dec.offset = 0;          // Add by EyLinGer
    updateWindowSize();
    E.line_buffer = (char *)realloc(E.line_buffer, E.screencols * sizeof(char)); // Add by EyLinGer
    assert(E.line_buffer != NULL);                                               // Add by EyLinGer
    E.chunks_head = (echunk *)malloc(sizeof(echunk));                            // Add by EyLinGer
    assert(E.chunks_head != NULL);                                               // Add by EyLinGer
    E.last_chunk = E.chunks_head;                                                // Add by EyLinGer
    E.row = (erow *)malloc(E.screenrows * sizeof(erow));
    initRows(&E.row);
    E.eof = 0;
}

int main(int argc, char **argv)
{
    if (argc != 2)
    {
        fprintf(stderr, "Usage: kilo <filename>\n");
        exit(1);
    }
    initEditor();
    editorOpen(argv[1]);
    enableRawMode(STDIN_FILENO);
    editorSetStatusMessage(
        "HELP: Ctrl-S = save | Ctrl-Q = quit");
    while (1)
    {
        editorRefreshScreen();
        editorProcessKeypress(STDIN_FILENO);
    }
    exit(0);
    return 0;
}
