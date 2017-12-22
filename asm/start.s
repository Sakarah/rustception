.file "start.s"
.text
.globl _start
.text
# Entry point of all prust programs: initialize MM, call main and exit
_start:
    call brk_init
    call mm_init
    call main
    mov %rax, %rdi
    mov $60, %rax # syscall 60 = exit
    syscall

