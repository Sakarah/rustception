.file "print.s"
.text
.globl print
.text
# Print %rdx first chars of the string at %rsi
print:
    mov $1, %rax # Syscall 1 = write
    mov $1, %rdi # File descriptior 1 = stdout
    syscall
    ret

