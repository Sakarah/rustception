    .local cur_brk
    .comm cur_brk, 8, 8
    .text

# Must be called once before any sbrk call
_brk_init:
    movq $12, %rax  # Syscall 12 = brk
    movq $0, %rdi   # Do not request any memory
    syscall
    movq %rax, cur_brk  # Store minimal brk in cur_brk
    ret

# Extend the process's data space by %rdi (increment).
# If %rdi is negative, shrink data space by -%rdi.
# Return start of new space allocated, or -1 for errors.
_sbrk:
    movq cur_brk, %rdx # Save the old cur_brk (it is the start of new space)
    addq %rdx, %rdi # Compute the expected new brk (increment + cur_brk)
    movq $12, %rax  # Syscall 12 = brk
    syscall         # Set %rax to new effective brk
    movq %rax, cur_brk  # Store new brk in cur_brk

    cmpq %rax, %rdi # If we do not have enough memory allocated it is an error
    ja .Lsbrkerror      # (ie cur_brk < expected brk)
    movq %rdx, %rax # Return the old brk value
    ret
.Lsbrkerror:
    movq $-1, %rax  # Return -1
    ret
