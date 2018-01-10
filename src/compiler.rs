use ast::{LogicOp, Comp, ArithOp};
use alloc_ast::*;
use std::io::Write;

type Result<T> = ::std::io::Result<T>;

struct Context<'a, W:'a>
{
    out: &'a mut W,
    prgm: &'a Program,
    label: isize,
}

impl<'a, W> Context<'a, W>
{
    fn gen_unique_label(&mut self) -> String
    {
        let label = format!(".Lbl{}", self.label);
        self.label += 1;
        return label;
    }
}

static ASM_BASE: &'static str = include_str!("../asm/prust_base.s");

fn typ_size(typ: Typ) -> usize
{
    match typ
    {
        Typ::Struct(s) => s,
        Typ::Vector => 16,
        Typ::Primitive => 8
    }
}

pub fn compile_program<W:Write>(prgm: &Program, out: &mut W) -> Result<()>
{
    let mut ctx = Context { out, prgm, label:0 };
    write!(ctx.out, "    .text\n")?;
    for (fname, fun) in &prgm.funs
    {
        write!(ctx.out, "    .p2align 4\n")?;
        write!(ctx.out, "{}:\n", fname)?;
        write!(ctx.out, "    pushq %rbp\n")?;
        write!(ctx.out, "    movq %rsp, %rbp\n")?;
        write!(ctx.out, "    subq ${}, %rsp\n", fun.body_stack_size)?;
        compile_block(&fun.body, &mut ctx)?;
        write!(ctx.out, "    movq %rbp, %rsp\n")?;
        write!(ctx.out, "    popq %rbp\n")?;
        write!(ctx.out, "    ret\n\n")?;
    }

    write!(ctx.out, "    .data\n")?;
    for string in &prgm.strings
    {
        write!(ctx.out, "str{}:\n", string.id)?;
        for byte in string.to_str().bytes()
        {
            write!(ctx.out, "    .byte {:#x}\n", byte)?;
        }
    }

    write!(ctx.out, "{}", ASM_BASE)?;
    Ok(())
}

/**
 * Compiles the given block, place the result in %rax.
 */
fn compile_block<W:Write>(block: &Block, ctx: &mut Context<W>) -> Result<()>
{
    if let Typ::Struct(_) = block.expr.typ
    {
        write!(ctx.out, "    pushq %rbx\n")?;
    }
    for instr in &block.instr
    {
        compile_instr(instr, ctx)?;
    }
    if let Typ::Struct(_) = block.expr.typ
    {
        write!(ctx.out, "    popq %rbx\n")?;
    }
    compile_expr(&block.expr, ctx)?;
    Ok(())
}

fn compile_instr<W:Write>(instr: &Instr, ctx: &mut Context<W>) -> Result<()>
{
    match *instr
    {
        Instr::Expression(ref expr) =>
        {
            compile_expr(expr, ctx)?;
        }
        Instr::Let(res_off, ref init_val) =>
        {
            match init_val.typ
            {
                Typ::Primitive =>
                {
                    compile_expr(init_val, ctx)?;
                    write!(ctx.out, "    movq %rax, {}(%rbp)\n", res_off)?;
                }
                Typ::Vector =>
                {
                    compile_expr(init_val, ctx)?;
                    write!(ctx.out, "    movq %rax, {}(%rbp)\n", res_off)?;
                    write!(ctx.out, "    movq %rdx, {}(%rbp)\n", res_off-8)?;
                }
                Typ::Struct(_) =>
                {
                    write!(ctx.out, "    leaq {}(%rbp), %rbx\n", res_off)?;
                    compile_expr(init_val, ctx)?;
                }
            }
        }
        Instr::While(ref cond, ref block) =>
        {
            let loop_start = ctx.gen_unique_label();
            write!(ctx.out, "{}:\n", loop_start)?;
            compile_expr(cond, ctx)?;
            write!(ctx.out, "    testb %al, %al\n")?;
            let loop_end = ctx.gen_unique_label();
            write!(ctx.out, "    jz {}\n", loop_end)?;
            compile_block(block, ctx)?;
            write!(ctx.out, "    jmp {}\n", loop_start)?;
            write!(ctx.out, "{}:\n", loop_end)?;
        }
        Instr::Return(ref expr) =>
        {
            if let Typ::Struct(_) = expr.typ
            {
                write!(ctx.out, "    pushq %rbx\n")?;
            }
            compile_expr(expr, ctx)?;
            // TODO: Free all blocks above
            write!(ctx.out, "    movq %rbp, %rsp\n")?;
            write!(ctx.out, "    popq %rbp\n")?;
            write!(ctx.out, "    ret\n")?
        }
    }
    Ok(())
}

/**
 * Compiles the given expression :
 * Store its result in %rax if it is a primitive type.
 * Store in %rax and %rdx if it is a Vec.
 * Store at position %rbx if it is a struct.
 */
fn compile_expr<W:Write>(expr: &TExpr, ctx: &mut Context<W>) -> Result<()>
{
    match expr.data
    {
        Expr::Constant(value) =>
        {
            // Small optimization for 0
            if value == 0
            {
                write!(ctx.out, "    xorq %rax, %rax\n")?;
            }
            else
            {
                write!(ctx.out, "    movq ${}, %rax\n", value)?;
            }
        }
        Expr::Variable(off) =>
        {
            match expr.typ
            {
                Typ::Primitive =>
                    write!(ctx.out, "    movq {}(%rbp), %rax\n", off)?,
                Typ::Vector =>
                {
                    write!(ctx.out, "    movq {}(%rbp), %rax\n", off)?;
                    write!(ctx.out, "    movq {}(%rbp), %rdx\n", off-8)?;
                }
                Typ::Struct(size) =>
                {
                    let mut copied = 0;
                    while copied < size
                    {
                        write!(ctx.out, "    movq {}(%rbp), %rax\n",
                               off - (copied as isize))?;
                        write!(ctx.out, "    movq %rax, -{}(%rbx)\n", copied)?;
                        copied += 8;
                    }
                }
            }
        }
        Expr::AssignLocal(ref var, ref val) =>
        {
            // TODO: Handle free correctly
            compile_address(var, ctx)?;
            match val.typ
            {
                Typ::Primitive | Typ::Vector =>
                    write!(ctx.out, "    pushq %rax\n")?,
                Typ::Struct(_) => write!(ctx.out, "    movq %rax, %rbx\n")?,
            }
            compile_expr(val, ctx)?;
            match val.typ
            {
                Typ::Primitive =>
                {
                    write!(ctx.out, "    popq %rdi\n")?;
                    write!(ctx.out, "    movq %rax, (%rdi)\n")?;
                }
                Typ::Vector =>
                {
                    write!(ctx.out, "    popq %rdi\n")?;
                    write!(ctx.out, "    movq %rax, (%rdi)\n")?;
                    write!(ctx.out, "    movq %rdx, -8(%rdi)\n")?;
                }
                Typ::Struct(_) => ()
            }
        }
        Expr::FunctionCall(name, ref args) =>
        {
            let fun = ctx.prgm.funs.get(&name).unwrap();
            for arg in args.iter().rev()
            {
                compile_expr(arg, ctx)?;
                match arg.typ
                {
                    Typ::Primitive => write!(ctx.out, "    pushq %rax\n")?,
                    Typ::Vector =>
                    {
                        write!(ctx.out, "    pushq %rax\n")?;
                        write!(ctx.out, "    pushq %rdx\n")?;
                    }
                    Typ::Struct(_) => ()
                }
            }
            write!(ctx.out, "    callq {}\n", name)?;
            write!(ctx.out, "    addq ${}, %rsp\n", fun.args_size)?;
        }
        Expr::Logic(op, ref e1, ref e2) =>
        {
            // && and || are lazy
            let lbl = ctx.gen_unique_label();
            compile_expr(e1, ctx)?;
            write!(ctx.out, "    testb %al, %al\n")?;
            match op
            {
                LogicOp::Or => write!(ctx.out, "    jnz {}\n", lbl)?,
                LogicOp::And => write!(ctx.out, "    jz {}\n", lbl)?,
            }
            compile_expr(e2, ctx)?;
            write!(ctx.out, "{}:\n", lbl)?;
        }
        Expr::Comparison(cmp, ref e1, ref e2) =>
        {
            compile_expr(e1, ctx)?;
            write!(ctx.out, "    pushq %rax\n")?;
            compile_expr(e2, ctx)?;
            write!(ctx.out, "    popq %rdi\n")?;
            write!(ctx.out, "    movl %eax, %esi\n")?;
            // e1 in %rdi and e2 in %rsi
            write!(ctx.out, "    xorl %eax, %eax\n")?; // Reset %rax
            write!(ctx.out, "    cmpl %esi, %edi\n")?;
            match cmp // Set %rax to 0 or 1 (but setcc works with 8-bit data)
            {
                Comp::Equal => write!(ctx.out, "    sete %al\n")?,
                Comp::NotEqual => write!(ctx.out, "    setne %al\n")?,
                Comp::Less => write!(ctx.out, "    setl %al\n")?,
                Comp::LessEq => write!(ctx.out, "    setle %al\n")?,
                Comp::Greater => write!(ctx.out, "    setg %al\n")?,
                Comp::GreaterEq => write!(ctx.out, "    setge %al\n")?,
            }
        }
        Expr::Arithmetic(op, ref e1, ref e2) =>
        {
            compile_expr(e2, ctx)?;
            write!(ctx.out, "    pushq %rax\n")?;
            compile_expr(e1, ctx)?;
            write!(ctx.out, "    popq %rdi\n")?;
            // e1 in %rax and e2 in %rdi
            match op
            {
                ArithOp::Addition =>
                    write!(ctx.out, "    addl %edi, %eax\n")?,
                ArithOp::Substraction =>
                    write!(ctx.out, "    subl %edi, %eax\n")?,
                ArithOp::Multiplication =>
                    write!(ctx.out, "    imull %edi, %eax\n")?,
                ArithOp::Division =>
                {
                    write!(ctx.out, "    cltd\n")?;
                    write!(ctx.out, "    idivl %edi\n")?;
                }
                ArithOp::Remainder =>
                {
                    write!(ctx.out, "    cltd\n")?;
                    write!(ctx.out, "    idivl %edi\n")?;
                    write!(ctx.out, "    movl %edx, %eax\n")?;
                }
            }
        }
        Expr::Minus(ref e) =>
        {
            compile_expr(e, ctx)?;
            write!(ctx.out, "    negl %eax\n")?;
        }
        Expr::Not(ref e) =>
        {
            compile_expr(e, ctx)?;
            write!(ctx.out, "    testb %al, %al\n")?;
            write!(ctx.out, "    setz %al\n")?;
        }
        Expr::Ref(ref e) =>
        {
            compile_address(e, ctx)?;
        }
        Expr::Deref(ref e) =>
        {
            compile_expr(e, ctx)?;
            write!(ctx.out, "    movq (%rax), %rax\n")?;
        }
        Expr::StructConstr(ref fields) =>
        {
            // Return address in %rbx
            for &(ref field_expr, field_offset) in fields
            {
                match field_expr.typ
                {
                    Typ::Primitive =>
                    {
                        compile_expr(field_expr, ctx)?;
                        write!(ctx.out, "    movq %rax, -{}(%rbx)\n",
                               field_offset)?;
                    }
                    Typ::Vector =>
                    {
                        // TODO: Manage vec move
                        compile_expr(field_expr, ctx)?;
                        write!(ctx.out, "    movq %rax, -{}(%rbx)\n",
                               field_offset)?;
                        write!(ctx.out, "    movq %rdx, -{}(%rbx)\n",
                               field_offset-8)?;
                    }
                    Typ::Struct(_) =>
                    {
                        write!(ctx.out, "    subq ${}, %rbx\n", field_offset)?;
                        compile_expr(field_expr, ctx)?;
                        write!(ctx.out, "    addq ${}, %rbx\n", field_offset)?;
                    }
                }
            }
        }
        Expr::Attribute(ref struct_expr, field_offset) =>
        {
            let struct_size = match struct_expr.typ
            {
                Typ::Struct(s) => s,
                _ => panic!("Attribute used on non struct during compile_expr")
            };

            if let Typ::Struct(_) = expr.typ
            {
                write!(ctx.out, "    movq %rbx, %rdx\n")?;
            }

            write!(ctx.out, "    leaq -8(%rsp), %rbx\n")?;
            write!(ctx.out, "    subq ${}, %rsp\n", struct_size)?;

            if let Typ::Struct(_) = expr.typ
            {
                // If we are making a struct, we must save %rbx
                write!(ctx.out, "    pushq %rdx\n")?;
            }

            compile_expr(struct_expr, ctx)?;
            match expr.typ
            {
                Typ::Primitive =>
                    write!(ctx.out, "    movq -{}(%rbx), %rax\n",
                           field_offset)?,
                Typ::Vector =>
                {
                    write!(ctx.out, "    movq -{}(%rbx), %rax\n",
                           field_offset)?;
                    write!(ctx.out, "    movq -{}(%rbx), %rdx\n",
                           field_offset+8)?;
                }
                Typ::Struct(size) =>
                {
                    write!(ctx.out, "    movq %rbx, %rdx\n")?;
                    write!(ctx.out, "    popq %rbx\n")?;
                    // Here %rdx is the position of the generated struct,
                    // %rbx is the wanted sub-struct position
                    let mut copied = 0;
                    while copied < size
                    {
                        write!(ctx.out, "    movq -{}(%rdx), %rax\n",
                               copied + field_offset)?;
                        write!(ctx.out, "    movq %rax, -{}(%rbx)\n", copied)?;
                        copied += 8;
                    }
                }
            }
            write!(ctx.out, "    addq ${}, %rsp\n", struct_size)?;
        }
        Expr::Print(symbol) =>
        {
            let string = symbol.to_str();
            write!(ctx.out, "    movq $1, %rax\n")?;
            write!(ctx.out, "    movq $1, %rdi\n")?;
            write!(ctx.out, "    movq $str{}, %rsi\n", symbol.id)?;
            write!(ctx.out, "    movq ${}, %rdx\n", string.len())?;
            write!(ctx.out, "    syscall\n")?;
        }
        Expr::If(ref cond, ref block_true, ref block_false) =>
        {
            compile_expr(cond, ctx)?;
            write!(ctx.out, "    testb %al, %al\n")?;
            let else_lbl = ctx.gen_unique_label();
            write!(ctx.out, "    jz {}\n", else_lbl)?;
            compile_block(block_true, ctx)?;
            let endif_lbl = ctx.gen_unique_label();
            write!(ctx.out, "    jmp {}\n", endif_lbl)?;
            write!(ctx.out, "{}:\n", else_lbl)?;
            compile_block(block_false, ctx)?;
            write!(ctx.out, "{}:\n", endif_lbl)?;
        }
        Expr::NestedBlock(ref block) => compile_block(block, ctx)?,
        _ => unimplemented!()
    }
    Ok(())
}

/**
 * Put the address of the requested lvalue into %rax.
 * Panics if expr is not a lvalue.
 */
fn compile_address<W: Write>(expr: &TExpr, ctx: &mut Context<W>) -> Result<()>
{
    match expr.data
    {
        Expr::Variable(off) =>
        {
            write!(ctx.out, "    leaq {}(%rbp), %rax\n", off)?;
        }
        Expr::Deref(ref e) =>
        {
            compile_expr(e, ctx)?;
        }
        Expr::Attribute(ref s, field_offset) =>
        {
            compile_address(s, ctx)?;
            if field_offset != 0
            {
                write!(ctx.out, "    subq ${}, %rax\n", field_offset)?;
            }
        }
        Expr::ArrayAccess(ref e, ref i) =>
        {
            let data_size = typ_size(e.typ);
            compile_expr(i, ctx)?;
            write!(ctx.out, "    pushq %rax\n")?;
            // TODO: Return an error if out of bound
            compile_expr(e, ctx)?; // A Vec is a pointer to heap memory
            write!(ctx.out, "    popq %rdi\n")?;
            write!(ctx.out, "    leaq (%rax, %rdi, ${}), %rax\n", data_size)?;
        }
        _ => panic!("Try to compute the address of a rvalue")
    }
    Ok(())
}
