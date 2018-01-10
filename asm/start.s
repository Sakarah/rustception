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

_out_of_range_panic:
    movq $1, %rax
    movq $2, %rdi
    movq $_out_of_range_msg, %rsi
    movq $28, %rdx
    syscall
_panic:
    mov $1, %rdi
    mov $60, %rax
    syscall

    .data
_out_of_range_msg:
    .string "Array access out of range !\n"
