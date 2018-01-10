    .text
    .globl _start
# Entry point of all prust programs: initialize memory manager, call main and
# exit
_start:
    call _brk_init
    call _mm_init
    call main
    mov $0, %rdi  # no error => return code 0
    mov $60, %rax # syscall 60 = exit
    syscall

_panic:
    mov $1, %rdi
    mov $60, %rax
    syscall
