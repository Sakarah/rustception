use bc_ast;
use std::collections::HashMap;
use ast;
use ast::Ident;
use bc_ast::Type;
use allocator::Expr::*;

struct Program
{
    funs: HashMap<Ident, Fun>,
    structs: HashMap<Ident, Struct>
}

struct Fun
{
    args: HashMap<Ident, usize>,
    body: Block,
    /* Corresponding to the needed size that the caller needs to reserve for
     * returning a struct (equels to 0 in another case)
     */
    ret_size: usize,
}

struct Struct
{
    size: usize,
    fields: HashMap<Ident, usize>
}

struct Block
{
    instr: Vec<Instr>,
    expr: Expr
}

enum Instr
{
    Expression(Expr),
    Let(usize, Expr),
    While(Expr, Box<Block>),
    Return(Expr),
}

enum Expr
{
    AssignLocal(Box<Expr>, Box<Expr>),

    Logic(ast::LogicOp, Box<Expr>, Box<Expr>),
    Comparison(ast::Comp, Box<Expr>, Box<Expr>),
    Arithmetic(ast::ArithOp, Box<Expr>, Box<Expr>),

    Minus(Box<Expr>),
    Not(Box<Expr>),
    Ref(Box<Expr>),
    Deref(Box<Expr>),

    // Last argument of the ArrayAccess is the size of the arguments.
    ArrayAccess(Box<Expr>, Box<Expr>, usize),
    Attribute(Box<Expr>, usize),

    Constant(isize),
    Variable(isize),
    FunctionCall(Ident, Vec<Expr>),
    StructConstr(Vec<(Expr, usize)>),
    VecConstr(Vec<Expr>),
    VecLen(Box<Expr>),
    Print(usize),
    If(Box<Expr>, Box<Block>, Box<Block>),
    NestedBlock(Box<Block>)
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
            Type::Void => (),
            Type::Int32 => total_size += 8,
            Type::Bool => total_size += 16,
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

fn allocate_expr(expr: bc_ast::Expr, stack_size: &mut usize,
                local_env: &mut HashMap<Ident, isize>,
                structs: &HashMap<Ident, Struct>) -> (Expr, usize)
{
    /*  We have to remember where to fall back after the expression is
    *   processed.
    */
    let stack_size_backup = *stack_size;
    let e : Expr;
    let mut max_size = 0;

    match expr
    {
        bc_ast::Expr::Constant(c) => match c
        {
            ast::Const::Void => e = Constant(0),
            ast::Const::Int32(i32_c) => e = Constant(i32_c as isize),
            // ___________ CHECK THE BELOW LINE LATER ___________
            ast::Const::Bool(true) => e = Constant(1),
            ast::Const::Bool(false) => e = Constant(0),
        },

        bc_ast::Expr::Variable(l_id) => match local_env.get(&l_id.data)
        {
            Some(&var_loc) => e = Variable(var_loc),
            None => panic!("Unknown variable {}", l_id.data)
        },

        bc_ast::Expr::Assignment(expr_to, expr_from) =>
        {
            let (e_to, max_size_1) = allocate_expr(
                expr_to.data, stack_size, local_env, structs);
            let (e_from, max_size_2) = allocate_expr(
                expr_from.data, stack_size, local_env, structs);
            max_size = if max_size_1 > max_size_2 {max_size_1}
                       else {max_size_2};
            e = AssignLocal(Box::new(e_to), Box::new(e_from))
        },

        bc_ast::Expr::FunctionCall(l_id, t_args) =>
        {
            let mut allocated_args = Vec::new();
            for arg in t_args
            {
                let (allocated_expr, used_size) = allocate_expr(
                    arg.data, stack_size, local_env, structs);
                allocated_args.push(allocated_expr);
                max_size = if used_size > max_size {used_size} else {max_size};
            }
            e = FunctionCall(l_id.data, allocated_args)
        },

        bc_ast::Expr::Logic(logic_op, t_expr_1, t_expr_2) =>
        {
            let (e1, max_size_1) = allocate_expr(
                t_expr_1.data, stack_size, local_env, structs);
            let (e2, max_size_2) = allocate_expr(
                t_expr_2.data, stack_size, local_env, structs);
            e = Logic(logic_op, Box::new(e1), Box::new(e2));
            max_size = if max_size_1 > (max_size_2 + 8) {max_size_1}
                       else {max_size_2}
        },

        bc_ast::Expr::Comparison(comp_op, t_expr_1, t_expr_2) =>
        {
            let (e1, max_size_1) = allocate_expr(
                t_expr_1.data, stack_size, local_env, structs);
            let (e2, max_size_2) = allocate_expr(
                t_expr_2.data, stack_size, local_env, structs);
            e = Comparison(comp_op, Box::new(e1), Box::new(e2));
            max_size = if max_size_1 > (max_size_2 + 8) {max_size_1}
                       else {max_size_2}
        },

        bc_ast::Expr::Arithmetic(arith_op, t_expr_1, t_expr_2) =>
        {
            let (e1, max_size_1) = allocate_expr(
                t_expr_1.data, stack_size, local_env, structs);
            let (e2, max_size_2) = allocate_expr(
                t_expr_2.data, stack_size, local_env, structs);
            e = Arithmetic(arith_op, Box::new(e1), Box::new(e2));
            max_size = if max_size_1 > (max_size_2 + 8) {max_size_1}
                       else {max_size_2}
        },

        bc_ast::Expr::Minus(t_expr) =>
        {
            let (expr, max_size) = allocate_expr(
                t_expr.data, stack_size, local_env, structs);
            e = Minus(Box::new(expr))
        },

        bc_ast::Expr::Not(t_expr) =>
        {
            let (expr, max_size) = allocate_expr(
                t_expr.data, stack_size, local_env, structs);
            e = Not(Box::new(expr))
        },

        bc_ast::Expr::Deref(t_expr) =>
        {
            let (expr, max_size) = allocate_expr(
                t_expr.data, stack_size, local_env, structs);
            e = Deref(Box::new(expr))
        },

        bc_ast::Expr::Ref(t_expr) | bc_ast::Expr::MutRef(t_expr) =>
        {
            let (expr, max_size) = allocate_expr(
                t_expr.data, stack_size, local_env, structs);
            e = Expr::Ref(Box::new(expr))
        }

        bc_ast::Expr::StructConstr(s_id, attributes) =>
        {
            let mut allocated_attr = Vec::new();
            match structs.get(&s_id.data)
            {
                Some(s) =>
                {
                    for (l_id, t_expr) in attributes
                    {
                        match s.fields.get(&l_id.data)
                        {
                            Some(&loc) => {
                                let (expr, used_size) = allocate_expr(
                                    t_expr.data, stack_size,
                                    local_env, structs);
                                allocated_attr.push((expr, loc));
                                max_size = if used_size > max_size {used_size}
                                           else {max_size}
                            }
                            None => panic!("Undefined attribute {} in structure\
                                {}", l_id.data, s_id.data)
                        }
                    }
                    e = StructConstr(allocated_attr)
                },
                None => panic!("Undefined structure {}", s_id.data)
            }
        },

        bc_ast::Expr::Attribute(t_expr, l_id_attr) =>
        {
            match t_expr.typ
            {
                Type::Struct(s_id) => match structs.get(&s_id)
                {
                    Some(s) => match s.fields.get(&l_id_attr.data)
                    {
                        Some(&loc) => {
                            let (expr, max_size) = allocate_expr(
                                t_expr.data, stack_size, local_env, structs);
                            e = Attribute(Box::new(expr), loc)
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

        bc_ast::Expr::VecConstr(t_elements) =>
        {
            let mut allocated_vec = Vec::new();
            for t_expr in t_elements
            {
                let (expr, used_size) = allocate_expr(
                    t_expr.data, stack_size, local_env, structs);
                max_size = if used_size > max_size {used_size} else {max_size};
                allocated_vec.push(expr);
            }
            e = VecConstr(allocated_vec)
        },

        bc_ast::Expr::ArrayAccess(t_expr_array, t_expr_pos) =>
        {
            let (array, used_size_array) = allocate_expr(
                t_expr_array.data, stack_size, local_env, structs);
            let (pos, used_size_array) = allocate_expr(
                t_expr_array.data, stack_size, local_env, structs);
        }

        _ => panic!("foo")
    }

    *stack_size = stack_size_backup;
    (e, max_size + *stack_size)
}

fn allocate_instr(instr: bc_ast::Instr, stack_size: &mut usize,
                  local_env: &mut HashMap<Ident, isize>,
                  structs: &HashMap<Ident, Struct>) -> Instr
{
    match instr
    {
        bc_ast::Instr::Expression(t_expr) =>
        {
            let (e,_) =
                allocate_expr(t_expr.data, stack_size, local_env, structs);
            Instr::Expression(e)
        }

        bc_ast::Instr::Let(l_id, t_expr) =>
        {
            let allocated_point = *stack_size;
            *stack_size += match t_expr.typ
            {
                Type::Void | Type::Int32 | Type::Bool | Type::Ref(_,_) |
                Type::MutRef(_,_) => 64,
                Type::Vector(_) => 128,
                Type::Struct(s_id) => match structs.get(&s_id)
                {
                    Some(s) => s.size,
                    None => panic!("I can't find struct {} definition", s_id)
                }
            };
            let (e,_) =
                allocate_expr(t_expr.data, stack_size, local_env, structs);
            local_env.insert(l_id.data, allocated_point as isize);
            Instr::Let(allocated_point, e)
        }

        bc_ast::Instr::While(t_expr, instr_block) =>
        {
            panic!("foo")
        }

        bc_ast::Instr::Return(t_expr) =>
        {
            panic!("foo")
        }
    }
}

fn allocate_block(block: bc_ast::Block, stack_size: &mut usize,
                  local_env: &HashMap<Ident, isize>,
                  structs: &HashMap<Ident, Struct>) -> Block
{
    // Copy the former local environment in order to backup.
    let mut new_local_env = local_env.clone();
    let stack_size_backup = *stack_size;

    let mut instructions = Vec::new();
    for l_instr in block.instr
    {
        instructions.push(
            allocate_instr(l_instr.data, stack_size, &mut new_local_env,
                           structs));
    }
    let (e,_) = allocate_expr(block.expr.data, stack_size, &mut new_local_env,
                          structs);

    // Backup the enc of the stack
    *stack_size = stack_size_backup;

    Block
    {
        instr: instructions,
        expr: e
    }
}

/*fn allocate_funs(funs: &HashMap<Ident, bc_ast::Fun>,
                 structs: &HashMap<Ident, Struct>)
    -> HashMap<Ident, Fun>
{
    let mut allocated_funs: HashMap<Ident, Fun> = HashMap::new();
    for (id, f) in funs
    {
        let mut args : HashMap<Ident, usize> = HashMap::new();
        let mut location = 0;
        for arg in f.sig.arguments
        {
            args.insert(arg.name.data, location as usize);
            match arg.typ.data
            {
                Void | Ref(_,_) | MutRef(_,_) =>
                    panic!("Found weird arguments in function definition that \
                            another pass did not catch for unknown reason !"),
                Bool | Int32 => location += 64,
                Vector(_) => location += 128,
                Struct(s_id) => match structs.get(&s_id)
                {
                    Some(s) => location += s.size,
                    None => panic!("Struct {} was not found, but is use in \
                                    {} definition. Aborting !", s_id, id),
                }
            }
        }

        let mut local_env : HashMap<Ident, isize> = HashMap::new();
        for (name, loc) in args
        {
            // ___________ CHECK THE BELOW LINE LATER ___________
            local_env.insert(name, (loc as isize) - (location as isize));
        }

        let mut instructions = Vec::new();
        let mut stack_size = 0;

        for i in f.body.instr
        {
            instructions.push(
                allocate_instr(i.data, &mut stack_size, &mut local_env));
        }
        let b = Block
                {
                    instr: instructions,
                    expr: allocate_expr(f.body.expr.data, &mut stack_size,
                                        &mut local_env)
                };

        let ret_size = match f.sig.return_type.data
        {
            Void | Int32 | Bool => 0,
            // ___________ CHECK THE BELOW LINE LATER ___________
            Vector(_) | Ref(_,_) | MutRef(_,_) => panic!("blablabla"),
            Struct(s_id) => match structs.get(&s_id)
            {
                Some(s) => s.size,
                None => panic!("Function {} return an unknown struct {}",
                                id, s_id)
            }
        };

        allocated_funs.insert(*id, Fun { args: args, body: b,
                                         ret_size: ret_size });
    }

    allocated_funs
}*/
