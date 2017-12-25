@echo off

gcc  main.c       ^
    -std=c99      ^
    -o adoc01.exe ^
    -Wall         ^
    -Wextra       ^
    -Werror       ^
    -fmax-errors=5

if NOT ERRORLEVEL 1 (
    adoc01.exe 311223 1
    REM expected: 6

    adoc01.exe 1212 2
    REM expected: 6

    adoc01.exe 1221 2
    REM expected: 0

    adoc01.exe 123425 2
    REM expected: 4

    adoc01.exe 123123 2
    REM expected: 12

    adoc01.exe 12131415 2
    REM expected: 4
)

