use std::collections::HashMap;
use std::collections::HashSet;
use ast;
use ast::Ident;
use bc_ast;
use bc_ast::Type;
use alloc_ast::*;
use alloc_ast::Expr::*;
use symbol::Symbol;
use std::cmp::max;

struct Context<'a>
{
    stack_size: usize,
    structs: &'a HashMap<Ident, Struct>,
    local_env: HashMap<Ident, isize>,
    strings: HashSet<Symbol>,
}

fn compute_attr_rel_addr(s: &bc_ast::Struct,
                         computed: &mut HashMap<Ident, Struct>,
                         structs: &HashMap<Ident, bc_ast::Struct>)
    -> Struct
{
    let mut total_size = 0;
    let mut fields: HashMap<Ident, usize> = HashMap::new();
    // No assumption is made on the order of the attibutes
    for (id, typ) in &s.fields
    {
        fields.insert(*id, total_size);
        match typ.data {
            Type::Void => total_size += 8,
            Type::Int32 => total_size += 8,
            Type::Bool => total_size += 8,
            Type::Struct(id_sub_struct) =>
            {
                if let Some(sub_struct) = computed.get(&id_sub_struct)
                {
                    total_size += sub_struct.size;
                    continue
                }

                match structs.get(&id_sub_struct)
                {
                    Some(sub_struct) =>
                    {
                        let allocated_sub_struct =
                            compute_attr_rel_addr(sub_struct, computed,
                                                    structs);
                        total_size += allocated_sub_struct.size;
                        computed.insert(id_sub_struct, allocated_sub_struct);
                    }
                    None => panic!("Not define")
                }
            },

            Type::Ref(_,_) | Type::MutRef(_,_) => total_size += 8,
            Type::Vector(_) => total_size += 16,
        }
    }

    Struct
    {
        size: total_size,
        fields: fields
    }
}

fn allocate_structs(structs: &HashMap<Ident, bc_ast::Struct>)
    -> HashMap<Ident, Struct>
{
    let mut allocated_structs : HashMap<Ident, Struct> = HashMap::new();
    for (id, s) in structs.iter()
    {
        if !(allocated_structs.contains_key(&id))
        {
            let alloc_struct =
                compute_attr_rel_addr(s, &mut allocated_structs, &structs);
            allocated_structs.insert(*id, alloc_struct);
        }
    }

    allocated_structs
}

fn typ(t: &bc_ast::Type, ctx: &mut Context) -> Typ
{
    match *t
    {
        Type::Void | Type::Int32 | Type::Bool |
        Type::Ref(_,_) | Type::MutRef(_,_) =>  Typ::Primitive,
        Type::Vector(_) => Typ::Vector,
        Type::Struct(s_id) => match ctx.structs.get(&s_id)
        {
            Some(s) => Typ::Struct(s.size),
            None => panic!("Unknown struct {}", s_id)
        }
    }
}

fn allocate_expr(expr: &bc_ast::TExpr, ctx: &mut Context) -> (TExpr, usize)
{
    /*  We have to remember where to fall back after the expression is
    *   processed.
    */
    let e;
    let mut max_size = ctx.stack_size;

    match expr.data
    {
        bc_ast::Expr::Constant(c) => match c
        {
            ast::Const::Void => e = Constant(0),
            ast::Const::Int32(i32_c) => e = Constant(i32_c as isize),
            ast::Const::Bool(true) => e = Constant(1),
            ast::Const::Bool(false) => e = Constant(0),
        },

        bc_ast::Expr::Variable(l_id) => match ctx.local_env.get(&l_id.data)
        {
            Some(&var_loc) => e = Variable(var_loc),
            None => panic!("Unknown variable {}", l_id.data)
        },

        bc_ast::Expr::Assignment(ref expr_to, ref expr_from) =>
        {
            let (e_to, max_size_1) = allocate_expr(expr_to, ctx);
            let (e_from, max_size_2) = allocate_expr(expr_from, ctx);
            max_size = if max_size_1 > max_size_2 {max_size_1}
                       else {max_size_2};
            e = AssignLocal(Box::new(e_to), Box::new(e_from))
        },

        bc_ast::Expr::FunctionCall(l_id, ref t_args) =>
        {
            let mut allocated_args = Vec::new();
            for arg in t_args
            {
                let (allocated_expr, used_size) = allocate_expr(arg, ctx);
                allocated_args.push(allocated_expr);
                max_size = used_size;
            }
            e = FunctionCall(l_id.data, allocated_args)
        },

        bc_ast::Expr::Logic(logic_op, ref t_expr_1, ref t_expr_2) =>
        {
            let (e1, max_size_1) = allocate_expr(t_expr_1, ctx);
            let (e2, max_size_2) = allocate_expr(t_expr_2, ctx);
            e = Logic(logic_op, Box::new(e1), Box::new(e2));
            max_size = if max_size_1 > max_size_2 {max_size_1} else {max_size_2}
        },

        bc_ast::Expr::Comparison(comp_op, ref t_expr_1, ref t_expr_2) =>
        {
            let (e1, max_size_1) = allocate_expr(t_expr_1, ctx);
            let (e2, max_size_2) = allocate_expr(t_expr_2, ctx);
            e = Comparison(comp_op, Box::new(e1), Box::new(e2));
            max_size = if max_size_1 > max_size_2 {max_size_1} else {max_size_2}
        },

        bc_ast::Expr::Arithmetic(arith_op, ref t_expr_1, ref t_expr_2) =>
        {
            let (e1, max_size_1) = allocate_expr(t_expr_1, ctx);
            let (e2, max_size_2) = allocate_expr(t_expr_2, ctx);
            e = Arithmetic(arith_op, Box::new(e1), Box::new(e2));
            max_size = if max_size_1 > max_size_2 {max_size_1} else {max_size_2}
        },

        bc_ast::Expr::Minus(ref t_expr) =>
        {
            let (expr, used_size) = allocate_expr(t_expr, ctx);
            e = Minus(Box::new(expr));
            max_size = used_size
        },

        bc_ast::Expr::Not(ref t_expr) =>
        {
            let (expr, used_size) = allocate_expr(t_expr, ctx);
            e = Not(Box::new(expr));
            max_size = used_size
        },

        bc_ast::Expr::Deref(ref t_expr) =>
        {
            let (expr, used_size) = allocate_expr(t_expr, ctx);
            e = Deref(Box::new(expr));
            max_size = used_size
        },

        bc_ast::Expr::Ref(ref t_expr) | bc_ast::Expr::MutRef(ref t_expr) =>
        {
            let (expr, used_size) = allocate_expr(t_expr, ctx);
            e = Expr::Ref(Box::new(expr));
            max_size = used_size
        }

        bc_ast::Expr::StructConstr(s_id, ref attributes) =>
        {
            let mut allocated_attr = Vec::new();
            if let Some(s) = ctx.structs.get(&s_id.data)
            {
                for &(l_id, ref t_expr) in attributes
                {
                    match s.fields.get(&l_id.data)
                    {
                        Some(&loc) => {
                            let (expr, used_size) = allocate_expr(
                                t_expr, ctx);
                            allocated_attr.push((expr, loc));
                            max_size = if used_size > max_size {used_size}
                                       else {max_size}
                        }
                        None => panic!("Undefined attribute {} in structure\
                            {}", l_id.data, s_id.data)
                    }
                }
                e = StructConstr(allocated_attr)
            }
            else
            {
                panic!("Undefined struct {}", s_id.data)
            }
        },

        bc_ast::Expr::Attribute(ref t_expr, l_id_attr) =>
        {
            match t_expr.typ
            {
                Type::Struct(s_id) => match ctx.structs.get(&s_id)
                {
                    Some(s) => match s.fields.get(&l_id_attr.data)
                    {
                        Some(&loc) => {
                            let (expr, used_size) = allocate_expr(t_expr, ctx);
                            e = Attribute(Box::new(expr), loc);
                            max_size = used_size
                        },
                        None => panic!("Undefined attribute {} in structure {}",
                            l_id_attr.data, s_id)
                    },
                    None => panic!("I did not process structure {} beforehand",
                        s_id)
                }
                _ => panic!("Expected a structure")
            }
        },

        bc_ast::Expr::VecConstr(ref t_elements) =>
        {
            let mut allocated_vec = Vec::new();
            for t_expr in t_elements
            {
                let (expr, used_size) = allocate_expr(t_expr, ctx);
                max_size = if used_size > max_size {used_size} else {max_size};
                allocated_vec.push(expr);
            }
            e = VecConstr(allocated_vec)
        },

        bc_ast::Expr::ArrayAccess(ref t_expr_array, ref t_expr_pos) =>
        {
            let (array, used_size_array) = allocate_expr(t_expr_array, ctx);
            let (pos, used_size_pos) = allocate_expr(t_expr_pos, ctx);
            max_size = max(used_size_pos, used_size_array);
            e = ArrayAccess(Box::new(array), Box::new(pos))
        },

        bc_ast::Expr::VecLen(ref t_expr) =>
        {
            let (vec, used_size) = allocate_expr(t_expr, ctx);
            e = VecLen(Box::new(vec));
            max_size = used_size
        },

        bc_ast::Expr::Print(symbol) =>
        {
            ctx.strings.insert(symbol);
            e = Print(symbol)
        }

        bc_ast::Expr::If(ref condition_expr, ref if_block, ref else_block) =>
        {
            let (condition, size_condition) = allocate_expr(
                condition_expr, ctx);
            let (alloc_if_block, size_if)  = allocate_block(&if_block, ctx);
            let (alloc_else_block,size_else) = allocate_block(&else_block, ctx);
            max_size = max(max(size_condition, size_if), size_else);
            e = If(Box::new(condition), Box::new(alloc_if_block),
                    Box::new(alloc_else_block))
        },

        bc_ast::Expr::NestedBlock(ref b) =>
        {
            let (block, used_size) = allocate_block(&b, ctx);
            e = NestedBlock(Box::new(block));
            max_size = used_size
        }
    }

    (TExpr { data: e, typ: typ(&expr.typ, ctx) }, max_size)
}

fn allocate_instr(instr: &bc_ast::Instr, ctx: &mut Context) -> (Instr, usize)
{
    match *instr
    {
        bc_ast::Instr::Expression(ref t_expr) =>
        {
            let (e, used_size) = allocate_expr(t_expr, ctx);
            (Instr::Expression(e), used_size)
        }

        bc_ast::Instr::Let(l_id, ref t_expr) =>
        {
            let allocated_point = -(ctx.stack_size as isize);
            ctx.stack_size += match t_expr.typ
            {
                Type::Void | Type::Int32 | Type::Bool | Type::Ref(_,_) |
                Type::MutRef(_,_) => 8,
                Type::Vector(_) => 16,
                Type::Struct(s_id) => match ctx.structs.get(&s_id)
                {
                    Some(s) => s.size,
                    None => panic!("I can't find struct {} definition", s_id)
                }
            };
            let (e, used_size) = allocate_expr(t_expr, ctx);
            ctx.local_env.insert(l_id.data, allocated_point);
            (Instr::Let(allocated_point, e), used_size)
        }

        bc_ast::Instr::While(ref t_cond_expr, ref instr_block) =>
        {
            let (cond_expr, expr_size) = allocate_expr(t_cond_expr, ctx);
            let (instr_block, block_size) =
                allocate_block(&instr_block, ctx);
            (
                Instr::While(cond_expr, Box::new(instr_block)),
                max(expr_size, block_size)
            )
        }

        bc_ast::Instr::Return(ref t_expr) =>
        {
            let (e, used_size) = allocate_expr(t_expr, ctx);
            (Instr::Return(e), used_size)
        }
    }
}

fn allocate_block(block: &bc_ast::Block, ctx: &mut Context) -> (Block, usize)
{
    // Copy the former local environment in order to backup.
    let local_env_backup = ctx.local_env.clone();
    let stack_size_backup = ctx.stack_size;

    let mut instructions = Vec::new();
    let mut max_instr_size = ctx.stack_size;
    for l_instr in &block.instr
    {
        let (instr, instr_size) = allocate_instr(&l_instr.data, ctx);
        instructions.push(instr);
        max_instr_size = max(max_instr_size, instr_size)
    }
    let (e, size) = allocate_expr(&block.expr, ctx);
    max_instr_size = max(max_instr_size, size);

    // Backup the end of the stack
    ctx.stack_size = stack_size_backup;
    ctx.local_env = local_env_backup;

    (Block { instr: instructions, expr: e }, max_instr_size)
}

fn allocate_funs(funs: &HashMap<Ident, bc_ast::Fun>, ctx: &mut Context)
    -> HashMap<Ident, Fun>
{
    let mut allocated_funs: HashMap<Ident, Fun> = HashMap::new();

    for (id, f) in funs
    {
        let mut args : HashMap<Ident, usize> = HashMap::new();
        let mut args_size : usize = 0;

        for arg in &f.sig.arguments
        {
            args.insert(arg.name.data, args_size);
            match arg.typ.data
            {
                Type::Void | Type::Ref(_,_) | Type::MutRef(_,_) |
                Type::Bool | Type::Int32 => args_size += 8,
                Type::Vector(_) => args_size += 16,
                Type::Struct(s_id) => match ctx.structs.get(&s_id)
                {
                    Some(s) => args_size += s.size,
                    None => panic!("Struct {} was not found, but is use in \
                                    {} definition. Aborting !", s_id, id),
                }
            }
            ctx.local_env.insert(arg.name.data, 8+args_size as isize);
        }

        let (body, fun_stack_size) = allocate_block(&f.body, ctx);

        allocated_funs.insert(*id,
            Fun { args, body, body_stack_size: fun_stack_size-8, args_size } );
    }

    allocated_funs
}

pub fn allocate_program(prgm: bc_ast::Program) -> Program
{
    let structs = allocate_structs(&prgm.structs);
    let funs;
    let strings;
    {
        let mut ctx = Context
        {
            stack_size : 8, // We must reserve space for old %rbp
            structs : &structs,
            strings : HashSet::new(),
            local_env : HashMap::new()
        };
        funs = allocate_funs(&prgm.funs, &mut ctx);
        strings = ctx.strings;
    }
    Program { structs, funs, strings }
}
