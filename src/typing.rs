use ast;
use typ_ast;
use location::{Located, Span};
use std::collections::{HashMap,HashSet};

/**
 * Errors that can occur during typing.
 */
#[derive(Debug)]
pub enum TypingError
{
    MultipleFuncDecl(ast::Ident),
    MultipleStructDecl(ast::Ident),
    MultipleFieldDecl(ast::Ident),
    UnknownType(ast::Ident),
    UnknownParametrizedType(ast::Ident),
    MismatchedTypes(typ_ast::Type, typ_ast::Type),
    FunctionReturnBorrowed(typ_ast::Type),
    MultipleArgumentDecl(ast::Ident),
    AssignmentOnRvalue,
    AssignmentOnConstant,
    CannotDeref(typ_ast::Type),
    BorrowOnRvalue,
    MutBorrowOnConstant,
    VariableUnbound(ast::Ident),
    UnknownFunction(ast::Ident),
    WrongNumberOfArguments(usize, usize),
    UnknownMacro(ast::Ident),
    CyclicStruct(ast::Ident),
    BorrowedInsideStruct(ast::Ident, typ_ast::Type),
    InvalidFieldName(ast::Ident, ast::Ident),
    MultipleFieldInit(ast::Ident),
    LackingField(ast::Ident, ast::Ident),
    FieldAccessOnNonStruct(typ_ast::Type),
    UnknownStruct(ast::Ident)
}

type Result<T> = ::std::result::Result<T,Located<TypingError>>;

struct GlobalContext
{
    funs: HashMap<String, typ_ast::FunSignature>,
    structs: HashMap<String, typ_ast::Struct>
}

/**
 * Try to type the given program AST. Return a new typed AST on success or a
 * TypingError on failure.
 */
pub fn type_program(prgm: ast::Program) -> Result<typ_ast::Program>
{
    // First we compute the global context of the program,
    // (ie the function signatures and the structs)
    let mut ctx = GlobalContext { funs: HashMap::new(),
                                  structs: HashMap::new() };

    // Sort between functions and structures
    let mut fun_decl = Vec::new();
    let mut struct_decl = HashMap::new();
    let mut struct_names = HashSet::new();
    for decl in prgm
    {
        match decl
        {
            ast::Decl::Function(f) => fun_decl.push(f),
            ast::Decl::Structure(s) =>
            {
                if !struct_names.insert(s.name.data.clone())
                {
                    return Err(Located::new(
                            TypingError::MultipleStructDecl(s.name.data),
                            s.name.loc));
                }
                struct_decl.insert(s.name.data.clone(), s);
            }
        }
    }

    // Then check that the structs are well formed and add them to the context
    while !struct_decl.is_empty()
    {
        let s = struct_decl.keys().next().unwrap().clone();
        check_struct(&s, &mut struct_decl, &mut ctx, &struct_names)?;
    }

    // Now process functions
    // We keep a separated list for function bodies
    let mut fun_body = Vec::new();

    // We check all function signature types and add them to the global context
    for f in fun_decl
    {
        let sig = check_fun_sig(f.arguments, f.return_type, &struct_names)?;
        if ctx.funs.insert(f.name.data.clone(), sig).is_none()
        {
            fun_body.push((f.name.data, f.body));
        }
        else
        {
            return Err(Located::new(
                TypingError::MultipleFuncDecl(f.name.data),
                f.name.loc));
        }
    }

    // Finally we type all function bodies.
    let mut typ_funs = HashMap::new();
    for (name, body) in fun_body
    {
        let typ_f = type_function(&name, body, &ctx)?;
        typ_funs.insert(name, typ_f);
    }

    let typ_prgm = typ_ast::Program { funs: typ_funs, structs: ctx.structs };
    Ok(typ_prgm)
}

/**
 * Check if the given type 'typ' is well formed in the context 'ctx'.
 * Return a typ_ast::Type corresponding to the well formed type.
 * The location 'loc' is used for error reporting.
 */
fn check_well_formed(typ: &ast::Type, loc: Span, struct_names: &HashSet<String>)
    -> Result<typ_ast::Type>
{
    match *typ
    {
        ast::Type::Void =>
            Ok(typ_ast::Type::Void),
        ast::Type::Basic(ref id) if struct_names.contains(id) =>
            Ok(typ_ast::Type::Struct(id.clone())),
        ast::Type::Basic(ref id) if id == "bool" =>
            Ok(typ_ast::Type::Bool),
        ast::Type::Basic(ref id) if id == "i32" =>
            Ok(typ_ast::Type::Int32),
        ast::Type::Basic(ref id) =>
            Err(Located::new(TypingError::UnknownType(id.clone()), loc)),
        ast::Type::Parametrized(ref id, ref typ) if id == "Vec" =>
        {
            let t = check_well_formed(typ, loc, struct_names)?;
            Ok(typ_ast::Type::Vector(Box::new(t)))
        }
        ast::Type::Parametrized(ref id, _) =>
        {
            Err(Located::new(TypingError::UnknownParametrizedType(id.clone()),
                             loc))
        }
        ast::Type::Ref(ref typ) =>
        {
            let t = check_well_formed(typ, loc, struct_names)?;
            Ok(typ_ast::Type::Ref(Box::new(t)))
        }
        ast::Type::MutRef(ref typ) =>
        {
            let t = check_well_formed(typ, loc, struct_names)?;
            Ok(typ_ast::Type::MutRef(Box::new(t)))
        }
    }
}

/**
 * Return true if 'typ' is a borrowed type
 */
fn is_borrowed(typ: &typ_ast::Type) -> bool
{
    match *typ
    {
        typ_ast::Type::Void | typ_ast::Type::Int32 | typ_ast::Type::Bool |
        typ_ast::Type::Struct(_) | typ_ast::Type::Vector(_) =>
            false,
        typ_ast::Type::Ref(_) | typ_ast::Type::MutRef(_) =>
            true
    }
}

/**
 * Check if the type 'found' can be automatically converted into 'expected'.
 * 'loc' is used for error reporting.
 */
fn check_type(expected: &typ_ast::Type, found: &typ_ast::Type, loc: Span)
    -> Result<()>
{
    match (expected, found)
    {
        (&typ_ast::Type::Void, &typ_ast::Type::Void) => Ok(()),
        (&typ_ast::Type::Int32, &typ_ast::Type::Int32) => Ok(()),
        (&typ_ast::Type::Bool, &typ_ast::Type::Bool) => Ok(()),
        (&typ_ast::Type::Struct(ref id_e),
         &typ_ast::Type::Struct(ref id_f)) if id_e == id_f =>
            Ok(()),
        (&typ_ast::Type::Vector(ref typ_e),&typ_ast::Type::Vector(ref typ_f)) =>
            check_type(typ_e, typ_f, loc),
        (&typ_ast::Type::Ref(ref typ_e), &typ_ast::Type::Ref(ref typ_f)) =>
            check_type(typ_e, typ_f, loc),
        (&typ_ast::Type::MutRef(ref typ_e),&typ_ast::Type::MutRef(ref typ_f)) =>
            check_type(typ_e, typ_f, loc),
        (&typ_ast::Type::Ref(ref typ_e), &typ_ast::Type::MutRef(ref typ_f)) =>
            check_type(typ_e, typ_f, loc),
        _ => Err(Located::new(TypingError::MismatchedTypes(expected.clone(),
            found.clone()), loc))
    }
}

/**
 * Check if all types in the function signature are well formed.
 * Return a typ_ast::FunSignature if it is the case.
 */
fn check_fun_sig(f_args: Vec<ast::Arg>, f_ret: ast::LType,
                 struct_names: &HashSet<String>)
    -> Result<typ_ast::FunSignature>
{
    let ret_type = check_well_formed(&f_ret.data, f_ret.loc, struct_names)?;
    let return_type = Located::new(ret_type, f_ret.loc);

    let mut sig = typ_ast::FunSignature { arguments: Vec::new(), return_type };

    for arg in f_args
    {
        let typ = check_well_formed(&arg.typ.data, arg.typ.loc, struct_names)?;
        let ltyp = Located::new(typ, arg.typ.loc);
        sig.arguments.push(typ_ast::Arg { mutable: arg.mutable, typ:ltyp,
            name: arg.name });
    }

    Ok(sig)
}

#[derive(Clone)]
pub struct FullType
{
    pub mutable: bool,
    pub typ: typ_ast::Type
}

#[derive(Clone)]
struct LocalContext<'a>
{
    vars: HashMap<String, FullType>,
    global: &'a GlobalContext
}

/**
 * Type the given function. (Name must correspond to the actual body.)
 */
fn type_function(f_name: &str, f_body: ast::Block, ctx:&GlobalContext)
    -> Result<typ_ast::Fun>
{
    let f_sig = ctx.funs.get(f_name).unwrap();

    // Check return value
    if is_borrowed(&f_sig.return_type.data)
    {
        return Err(Located::new(
            TypingError::FunctionReturnBorrowed(f_sig.return_type.data.clone()),
            f_sig.return_type.loc));
    }

    // Check arguments
    let mut lctx = LocalContext { vars:HashMap::new(), global:ctx };
    for arg in &f_sig.arguments
    {
        let arg_typ = FullType { mutable: arg.mutable,
                                 typ: arg.typ.data.clone() };
        match lctx.vars.insert(arg.name.data.clone(), arg_typ)
        {
            None => (),
            Some(_) =>
            {
                return Err(Located::new(
                    TypingError::MultipleArgumentDecl(arg.name.data.clone()),
                    arg.name.loc));
            }
        }
    }

    let typ_body = type_block(&f_body, &lctx)?;
    check_type(&f_sig.return_type.data, &typ_body.expr.typ,
               typ_body.expr.loc)?;

    Ok(typ_ast::Fun { sig:f_sig.clone(), body: typ_body })
}

fn type_block(b: &ast::Block, ctx:&LocalContext) -> Result<typ_ast::Block>
{
    let mut lctx = ctx.clone();
    let mut typ_instr = Vec::new();

    for instr in &b.instr
    {
        match instr.data
        {
            ast::Instr::Expression(ref e) =>
            {
                let typ_e = type_expr(e, &lctx)?;
                typ_instr.push(Located::new(typ_ast::Instr::Expression(typ_e),
                                            instr.loc));
            }
            ast::Instr::Let(ref m, ref ident, ref expr) =>
            {
                let typ_expr = type_expr(expr, &lctx)?;
                let var_typ = FullType{ mutable:*m, typ:typ_expr.typ.clone() };
                lctx.vars.insert(ident.data.clone(), var_typ);
                typ_instr.push(Located::new(
                    typ_ast::Instr::Let(ident.clone(), typ_expr), instr.loc));
            }
            ast::Instr::While(ref cond, ref body) =>
            {
                let typ_cond = type_expr(cond, &lctx)?;
                check_type(&typ_ast::Type::Bool, &typ_cond.typ, typ_cond.loc)?;

                let typ_body = type_block(body, &lctx)?;
                typ_instr.push(Located::new(
                    typ_ast::Instr::While(typ_cond, Box::new(typ_body)),
                    instr.loc));
            }
            ast::Instr::If(ref if_expr) =>
            {
                let typ_e = type_ifexpr(if_expr, instr.loc, &lctx)?;
                typ_instr.push(Located::new(typ_ast::Instr::Expression(typ_e),
                                            instr.loc));
            }
            _ => unimplemented!()
        }
    }

    let typ_expr = type_expr(&b.expr, &lctx)?;
    Ok(typ_ast::Block { instr:typ_instr, expr:typ_expr })
}

fn type_expr(e: &ast::LExpr, ctx:&LocalContext) -> Result<typ_ast::TExpr>
{
    match e.data
    {
        ast::Expr::Assignment(ref var, ref val) =>
        {
            let t_var = type_expr(var, ctx)?;
            if !t_var.lvalue
            {
                return Err(Located::new(TypingError::AssignmentOnRvalue,
                                        t_var.loc));
            }
            if !t_var.mutable
            {
                return Err(Located::new(TypingError::AssignmentOnConstant,
                                        t_var.loc));
            }

            let t_val = type_expr(val, ctx)?;
            check_type(&t_var.typ, &t_val.typ, e.loc)?;

            Ok(typ_ast::Typed { typ: typ_ast::Type::Void, mutable: false, data:
                typ_ast::Expr::Assignment(Box::new(t_var), Box::new(t_val)),
                lvalue: false, loc: e.loc })

        }
        ast::Expr::Logic(ref op, ref e1, ref e2) =>
        {
            let t1 = type_expr(e1, ctx)?;
            check_type(&typ_ast::Type::Bool, &t1.typ, t1.loc)?;

            let t2 = type_expr(e2, ctx)?;
            check_type(&typ_ast::Type::Bool, &t2.typ, t2.loc)?;

            Ok(typ_ast::Typed { typ: typ_ast::Type::Bool, mutable: false,
                data: typ_ast::Expr::Logic(*op, Box::new(t1), Box::new(t2)),
                lvalue: false, loc: e.loc })
        }
        ast::Expr::Comparison(ref op, ref e1, ref e2) =>
        {
            let t1 = type_expr(e1, ctx)?;
            check_type(&typ_ast::Type::Int32, &t1.typ, t1.loc)?;

            let t2 = type_expr(e2, ctx)?;
            check_type(&typ_ast::Type::Int32, &t2.typ, t2.loc)?;

            Ok(typ_ast::Typed { typ: typ_ast::Type::Bool, mutable: false, data:
                typ_ast::Expr::Comparison(*op, Box::new(t1), Box::new(t2)),
                lvalue: false, loc: e.loc })

        }
        ast::Expr::Arithmetic(ref op, ref e1, ref e2) =>
        {
            let t1 = type_expr(e1, ctx)?;
            check_type(&typ_ast::Type::Int32, &t1.typ, t1.loc)?;

            let t2 = type_expr(e2, ctx)?;
            check_type(&typ_ast::Type::Int32, &t2.typ, t2.loc)?;

            Ok(typ_ast::Typed { typ: typ_ast::Type::Int32, mutable: false, data:
                typ_ast::Expr::Arithmetic(*op, Box::new(t1), Box::new(t2)),
                lvalue: false, loc: e.loc })

        }
        ast::Expr::Minus(ref e0) =>
        {
            let t0 = type_expr(e0, ctx)?;
            check_type(&typ_ast::Type::Int32, &t0.typ, t0.loc)?;

            Ok(typ_ast::Typed { typ: typ_ast::Type::Int32, mutable: false,
                data: typ_ast::Expr::Minus(Box::new(t0)),
                lvalue: false, loc: e.loc })

        }
        ast::Expr::Not(ref e0) =>
        {
            let t0 = type_expr(e0, ctx)?;
            check_type(&typ_ast::Type::Bool, &t0.typ, t0.loc)?;

        Ok(typ_ast::Typed { typ: typ_ast::Type::Bool, mutable: false,
                data: typ_ast::Expr::Not(Box::new(t0)),
                lvalue: false, loc: e.loc })
        }
        ast::Expr::Deref(ref e0) =>
        {
            let t0 = type_expr(e0, ctx)?;
            let (deref_typ, m) = match t0.typ
            {
                typ_ast::Type::Ref(ref typ) => (*typ.clone(), false),
                typ_ast::Type::MutRef(ref typ) => (*typ.clone(), true),
                _ =>
                {
                    return Err(Located::new(TypingError::CannotDeref(t0.typ),
                                e.loc));
                }
            };

            Ok(typ_ast::Typed { typ: deref_typ, mutable: m,
                data: typ_ast::Expr::Deref(Box::new(t0)),
                lvalue: true, loc: e.loc })
        }
        ast::Expr::Ref(ref e0) =>
        {
            let t0 = type_expr(e0, ctx)?;
            if !t0.lvalue
            {
                return Err(Located::new(TypingError::BorrowOnRvalue, e.loc));
            }

            Ok(typ_ast::Typed { mutable:false, lvalue:false, loc: e.loc,
                typ: typ_ast::Type::Ref(Box::new(t0.typ.clone())),
                data: typ_ast::Expr::Ref(Box::new(t0)) })
        }
        ast::Expr::MutRef(ref e0) =>
        {
            let t0 = type_expr(e0, ctx)?;
            if !t0.lvalue
            {
                return Err(Located::new(TypingError::BorrowOnRvalue, e.loc));
            }
            if !t0.mutable
            {
                return Err(Located::new(TypingError::MutBorrowOnConstant,
                                        e.loc));
            }

            Ok(typ_ast::Typed { mutable: false, lvalue: false, loc: e.loc,
                typ: typ_ast::Type::MutRef(Box::new(t0.typ.clone())),
                data: typ_ast::Expr::MutRef(Box::new(t0)) })
        }
        ast::Expr::Attribute(ref e0, ref attr_name) =>
        {
            let t0 = type_expr(e0, ctx)?;

            let typ = match t0.typ
            {
                typ_ast::Type::Struct(ref struct_name) =>
                {
                    let struc = &ctx.global.structs.get(struct_name).unwrap();
                    // Safe to unwrap here as type_expr should only  ^^^^^^^^
                    // return valid types.

                    match struc.fields.get(&attr_name.data)
                    {
                        None => Err(Located::new(TypingError::InvalidFieldName(
                            attr_name.data.clone(), struct_name.clone()),
                            attr_name.loc)),
                        Some(ref field_typ) => Ok(field_typ.data.clone())
                    }
                }
                _ => Err(Located::new(TypingError::FieldAccessOnNonStruct(
                    t0.typ.clone()), e.loc))
            }?;

            Ok(typ_ast::Typed { mutable: t0.mutable, lvalue: t0.lvalue, typ,
                data: typ_ast::Expr::Attribute(Box::new(t0), attr_name.clone()),
                loc: e.loc })
        }
        ast::Expr::Constant(ref val) =>
        {
            let typ = match *val
            {
                ast::Const::Void => typ_ast::Type::Void,
                ast::Const::Int32(_) => typ_ast::Type::Int32,
                ast::Const::Bool(_) => typ_ast::Type::Bool
            };

            Ok(typ_ast::Typed { typ, mutable: false, lvalue: false, loc: e.loc,
                data: typ_ast::Expr::Constant(val.clone()) } )
        }
        ast::Expr::Variable(ref id) =>
        {
            let (typ, mutable) = match ctx.vars.get(&id.data)
            {
                None =>
                {
                    return Err(Located::new(
                        TypingError::VariableUnbound(id.data.clone()), e.loc));
                }
                Some(ref full_typ) => (full_typ.typ.clone(), full_typ.mutable)
            };

            Ok(typ_ast::Typed { typ, mutable, loc: e.loc, lvalue: true,
                data: typ_ast::Expr::Variable(id.clone()) })
        }
        ast::Expr::FunctionCall(ref fun_name, ref args) =>
        {
            let fun_sig = ctx.global.funs.get(&fun_name.data)
                .ok_or(Located::new(TypingError::UnknownFunction(
                    fun_name.data.clone()), fun_name.loc))?;

            if fun_sig.arguments.len() != args.len()
            {
                return Err(Located::new(TypingError::WrongNumberOfArguments(
                    fun_sig.arguments.len(), args.len()), e.loc));
            }

            let mut typ_args = Vec::new();
            for (param, arg) in fun_sig.arguments.iter().zip(args)
            {
                let arg_typ = type_expr(arg, ctx)?;
                check_type(&param.typ.data, &arg_typ.typ, arg_typ.loc)?;

                typ_args.push(arg_typ);
            }

            Ok(typ_ast::Typed { typ: fun_sig.return_type.data.clone(),
                data: typ_ast::Expr::FunctionCall(fun_name.clone(), typ_args),
                mutable: false, lvalue: false, loc: e.loc })
        }
        ast::Expr::StructConstr(ref struct_name, ref fields) =>
        {
            match ctx.global.structs.get(&struct_name.data)
            {
                None => return Err(Located::new(TypingError::UnknownStruct(
                    struct_name.data.clone()), struct_name.loc)),
                Some(ref struc) =>
                {
                    // Type all the fields given and check if their type match
                    // with the struct
                    let mut found_fields = HashSet::new();
                    let mut field_expr = Vec::new();
                    for &(ref name, ref expr) in fields
                    {
                        let t_expr = type_expr(expr, ctx)?;

                        match struc.fields.get(&name.data)
                        {
                            None => return Err(Located::new(
                                TypingError::InvalidFieldName(name.data.clone(),
                                struct_name.data.clone()), name.loc)),
                            Some(ref typ) =>
                            {
                                check_type(&typ.data, &t_expr.typ, t_expr.loc)?;

                                if !found_fields.insert(name.data.clone())
                                {
                                    return Err(Located::new(
                                        TypingError::MultipleFieldInit(
                                        name.data.clone()), name.loc));
                                }

                                field_expr.push((name.clone(), t_expr));
                            }
                        }
                    }

                    // Check that all fields of the struct are initialized
                    for name in struc.fields.keys()
                    {
                        if !found_fields.contains(name)
                        {
                            return Err(Located::new(TypingError::LackingField(
                                name.clone(), struct_name.data.clone()),
                                e.loc));
                        }
                    }

                    Ok(typ_ast::Typed { mutable: false, lvalue: false,
                        typ: typ_ast::Type::Struct(struct_name.data.clone()),
                        data: typ_ast::Expr::StructConstr(struct_name.clone(),
                        field_expr), loc: e.loc })
                }
            }
        }
        ast::Expr::StringMacro(ref macro_name, ref string) =>
        {
            match macro_name.data.as_ref()
            {
                "print" =>
                    Ok(typ_ast::Typed { typ: typ_ast::Type::Void,
                        data: typ_ast::Expr::Print(string.clone()),
                        mutable: false, lvalue: false, loc: e.loc }),
                _ => Err(Located::new(TypingError::UnknownMacro(
                    macro_name.data.clone()), macro_name.loc))
            }
        }
        ast::Expr::If(ref if_expr) => type_ifexpr(if_expr, e.loc, ctx),
        ast::Expr::NestedBlock(ref block) =>
        {
            let typ_block = type_block(block, ctx)?;
            Ok(typ_ast::Typed { mutable: false, lvalue: false, loc: e.loc,
                typ: typ_block.expr.typ.clone(),
                data: typ_ast::Expr::NestedBlock(Box::new(typ_block)) })
        }
        _ => unimplemented!()
    }
}

fn type_ifexpr(e: &ast::IfExpr, loc: Span, ctx:&LocalContext)
    -> Result<typ_ast::TExpr>
{
    match *e
    {
        ast::IfExpr::Single(ref cond, ref b_if, ref b_else) =>
        {
            let typ_cond = type_expr(cond, ctx)?;
            check_type(&typ_ast::Type::Bool, &typ_cond.typ, typ_cond.loc)?;

            let typ_if = type_block(b_if, ctx)?;
            let typ_else = type_block(b_else, ctx)?;
            check_type(&typ_if.expr.typ, &typ_else.expr.typ,
                       typ_else.expr.loc)?;

            Ok(typ_ast::Typed { typ: typ_if.expr.typ.clone(), mutable: false,
                data: typ_ast::Expr::If(Box::new(typ_cond), Box::new(typ_if),
                    Box::new(typ_else)), lvalue: false, loc })
        }
        ast::IfExpr::Nested(ref cond, ref b_if, ref ifexpr_else) =>
        {
            let typ_cond = type_expr(cond, ctx)?;
            check_type(&typ_ast::Type::Bool, &typ_cond.typ, typ_cond.loc)?;

            let typ_if = type_block(b_if, ctx)?;
            let typ_else = type_ifexpr(ifexpr_else, loc, ctx)?;
            check_type(&typ_if.expr.typ, &typ_else.typ, typ_if.expr.loc)?;

            let block_else = typ_ast::Block{ instr:Vec::new(), expr:typ_else };
            Ok(typ_ast::Typed { typ: typ_if.expr.typ.clone(), mutable: false,
                data: typ_ast::Expr::If(Box::new(typ_cond), Box::new(typ_if),
                    Box::new(block_else)), lvalue: false, loc })
        }
    }
}

/**
 * Take the struct with the given name in struct_decl and check if it is well
 * formed. Then update the global context to include it or return an error.
 * This function is recursive as we might need to compute nested structs.
 * It panics if 'struct_decl' does not contain any element associated to 'name'.
 */
fn check_struct(name: &str, mut struct_decl: &mut HashMap<String, ast::Struct>,
                mut ctx: &mut GlobalContext, struct_names: &HashSet<String>)
    -> Result<()>
{
    let s = struct_decl.remove(name).unwrap();

    let mut fields = HashMap::new();
    for f in s.fields
    {
        let typ = check_well_formed(&f.typ.data, f.typ.loc, struct_names)?;
        match typ
        {
            typ_ast::Type::Void | typ_ast::Type::Bool |
            typ_ast::Type::Int32 | typ_ast::Type::Vector(_) => (),
            typ_ast::Type::Struct(ref name) =>
            {
                if !ctx.structs.contains_key(name)
                {
                    if struct_decl.contains_key(name)
                    {
                        check_struct(name, &mut struct_decl, &mut ctx,
                                     &struct_names)?;
                    }
                    else
                    {
                        return Err(Located::new(TypingError::CyclicStruct(
                            name.clone()), f.typ.loc));
                    }
                }
            }
            typ_ast::Type::Ref(_) | typ_ast::Type::MutRef(_) =>
            {
                return Err(Located::new(
                    TypingError::BorrowedInsideStruct(
                        f.name.data.clone(), typ.clone()), f.typ.loc));
            }
        }

        let ltyp = Located::new(typ, f.typ.loc);
        if fields.insert(f.name.data.clone(), ltyp).is_some()
        {
            return Err(Located::new(TypingError::MultipleFieldDecl(f.name.data),
                f.name.loc));
        }
    }

    ctx.structs.insert(String::from(name), typ_ast::Struct{ fields });
    Ok(())
}

