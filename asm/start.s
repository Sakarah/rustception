    .text
    .globl _start
# Entry point of all prust programs: initialize memory manager, call main and
# exit
_start:
    call brk_init
    call mm_init
    call main
    mov $0, %rdi  # no error => return code 0
    mov $60, %rax # syscall 60 = exit
    syscall

