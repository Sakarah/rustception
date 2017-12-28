use ast;
use typ_ast;
use symbol::Symbol;
use location::{Located, Span};
use std::collections::{HashMap,HashSet};
use std::rc::Rc;
use std::cell::RefCell;

/// Errors that can occur during typing.
#[derive(Debug)]
pub enum TypingError
{
    MultipleFuncDecl(ast::Ident),
    MultipleStructDecl(ast::Ident),
    MultipleFieldDecl(ast::Ident),
    UnknownType(ast::Ident),
    UnknownParametrizedType(ast::Ident),
    NotParametrizedType(ast::Ident),
    MismatchedTypes { found: typ_ast::Type, expected: typ_ast::Type },
    FunctionReturnBorrowed(typ_ast::Type),
    MultipleArgumentDecl(ast::Ident),
    AssignmentOnRvalue,
    AssignmentOnConstant,
    CannotDeref(typ_ast::Type),
    BorrowOnRvalue,
    MutBorrowOnConstant,
    VariableUnbound(ast::Ident),
    UnknownFunction(ast::Ident),
    WrongNumberOfArguments { found: usize, expected:usize },
    UnknownMacro(ast::Ident),
    CyclicStruct(ast::Ident),
    BorrowedInsideStruct(ast::Ident, typ_ast::Type),
    InvalidFieldName { field: ast::Ident, struc: ast::Ident },
    MultipleFieldInit(ast::Ident),
    LackingField { field: ast::Ident, struc: ast::Ident },
    FieldAccessOnNonStruct(typ_ast::Type),
    UnknownStruct(ast::Ident),
    ArrayAccessOnRvalue,
    ArrayAccessOnScalarType(typ_ast::Type),
    UnknownMethod(ast::Ident, typ_ast::Type)
}

type Result<T> = ::std::result::Result<T,Located<TypingError>>;

struct GlobalContext
{
    funs: HashMap<ast::Ident, typ_ast::FunSignature>,
    structs: HashMap<ast::Ident, typ_ast::Struct>
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
                if !struct_names.insert(s.name.data)
                {
                    return Err(Located::new(
                            TypingError::MultipleStructDecl(s.name.data),
                            s.name.loc));
                }
                struct_decl.insert(s.name.data, s);
            }
        }
    }

    // Then check that the structs are well formed and add them to the context
    while !struct_decl.is_empty()
    {
        let s = *struct_decl.keys().next().unwrap();
        check_struct(s, &mut struct_decl, &mut ctx, &struct_names)?;
    }

    // Now process functions
    // We keep a separated list for function bodies
    let mut fun_body = Vec::new();

    // We check all function signature types and add them to the global context
    for f in fun_decl
    {
        let sig = check_fun_sig(f.arguments, f.return_type, &struct_names)?;
        if ctx.funs.insert(f.name.data, sig).is_none()
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
        let typ_f = type_function(name, body, &ctx)?;
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
fn check_well_formed(typ: &ast::Type, loc: Span, struct_names: &HashSet<Symbol>)
    -> Result<typ_ast::Type>
{
    match *typ
    {
        ast::Type::Void =>
            Ok(typ_ast::Type::Void),
        ast::Type::Basic(id) if struct_names.contains(&id) =>
            Ok(typ_ast::Type::Struct(id)),
        ast::Type::Basic(id) if &*id.to_str() == "bool" =>
            Ok(typ_ast::Type::Bool),
        ast::Type::Basic(id) if &*id.to_str() == "i32" =>
            Ok(typ_ast::Type::Int32),
        ast::Type::Basic(id) =>
            Err(Located::new(TypingError::UnknownType(id), loc)),
        ast::Type::Parametrized(id, ref typ) =>
        {
            if struct_names.contains(&id)
            {
                Err(Located::new(TypingError::NotParametrizedType(id), loc))
            }
            else if &*id.to_str() == "Vec"
            {
                let t = check_well_formed(typ, loc, struct_names)?;
                Ok(typ_ast::Type::Vector(Box::new(t)))
            }
            else
            {
                Err(Located::new(TypingError::UnknownParametrizedType(id), loc))
            }
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
 * Return true if 'typ' is a borrowed type or contains a borrowed type
 */
fn contains_borrowed(typ: &typ_ast::Type) -> bool
{
    match *typ
    {
        typ_ast::Type::Void | typ_ast::Type::Int32 | typ_ast::Type::Bool |
        typ_ast::Type::Struct(_) | typ_ast::Type::Unknown(_) =>
            false,
        typ_ast::Type::Ref(_) | typ_ast::Type::MutRef(_) =>
            true,
        typ_ast::Type::Vector(ref t) => contains_borrowed(t)
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
        (&typ_ast::Type::Unknown(ref e_cell), _) =>
        {
            let mut e = e_cell.borrow_mut();
            if let Some(ref typ_e) = *e
            {
                return check_type(typ_e, found, loc);
            }

            // Unification with the other type
            *e = Some(simplify_type(found));
            Ok(())
        }
        (_, &typ_ast::Type::Unknown(ref f_cell)) =>
        {
            let mut f = f_cell.borrow_mut();
            if let Some(ref typ_f) = *f
            {
                return check_type(expected, typ_f, loc);
            }

            *f = Some(simplify_type(expected));
            Ok(())
        }
        _ => Err(Located::new(TypingError::MismatchedTypes {
            expected: expected.clone(), found: found.clone() }, loc))
    }
}

/**
 * Build a copy of 'typ' by replacing discovered unknown types by their real
 * types
 */
fn simplify_type(typ: &typ_ast::Type) -> typ_ast::Type
{
    match *typ
    {
        typ_ast::Type::Void | typ_ast::Type::Int32 | typ_ast::Type::Bool |
        typ_ast::Type::Struct(_) =>
            typ.clone(),
        typ_ast::Type::Vector(ref t) =>
            typ_ast::Type::Vector(Box::new(simplify_type(t))),
        typ_ast::Type::Ref(ref t) =>
            typ_ast::Type::Ref(Box::new(simplify_type(t))),
        typ_ast::Type::MutRef(ref t) =>
            typ_ast::Type::MutRef(Box::new(simplify_type(t))),
        typ_ast::Type::Unknown(ref t_cell) =>
        {
            // Use mut here to avoid infinite recursion
            let t_res = t_cell.try_borrow_mut();
            if let Ok(t_opt) = t_res
            {
                if let Some(ref t) = *t_opt
                {
                    return simplify_type(t);
                }
            }
            typ.clone()
        }
    }
}

/**
 * Check if all types in the function signature are well formed.
 * Return a typ_ast::FunSignature if it is the case.
 */
fn check_fun_sig(f_args: Vec<ast::Arg>, f_ret: ast::LType,
                 struct_names: &HashSet<Symbol>)
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
    vars: HashMap<ast::Ident, FullType>,
    global: &'a GlobalContext,
    return_type: &'a typ_ast::Type
}

/**
 * Type the given function. (Name must correspond to the actual body.)
 */
fn type_function(f_name: Symbol, f_body: ast::Block, ctx:&GlobalContext)
    -> Result<typ_ast::Fun>
{
    let f_sig = ctx.funs.get(&f_name).unwrap();

    // Check return value
    if contains_borrowed(&f_sig.return_type.data)
    {
        return Err(Located::new(
            TypingError::FunctionReturnBorrowed(f_sig.return_type.data.clone()),
            f_sig.return_type.loc));
    }

    // Check arguments
    let mut lctx = LocalContext { vars:HashMap::new(), global:ctx,
        return_type: &f_sig.return_type.data };
    for arg in &f_sig.arguments
    {
        let arg_typ = FullType { mutable: arg.mutable,
                                 typ: arg.typ.data.clone() };
        match lctx.vars.insert(arg.name.data, arg_typ)
        {
            None => (),
            Some(_) =>
            {
                return Err(Located::new(
                    TypingError::MultipleArgumentDecl(arg.name.data),
                    arg.name.loc));
            }
        }
    }

    let typ_body = type_block(&f_body, &lctx)?;
    check_type(&f_sig.return_type.data, &typ_body.expr.typ,
               typ_body.expr.loc)?;

    Ok(typ_ast::Fun { sig: f_sig.clone(), body: typ_body })
}

/**
 * Type the given block
 */
fn type_block<'a>(b: &ast::Block, ctx:&LocalContext<'a>)
    -> Result<typ_ast::Block>
{
    let mut lctx = ctx.clone();
    let mut typ_instr = Vec::new();
    let mut always_return = false;

    for instr in &b.instr
    {
        match instr.data
        {
            ast::Instr::NoOp => (),
            ast::Instr::Expression(ref e) =>
            {
                let typ_e = type_expr(e, &lctx)?;
                always_return |= typ_e.always_return;

                typ_instr.push(Located::new(typ_ast::Instr::Expression(typ_e),
                                            instr.loc));
            }
            ast::Instr::Let(ref m, ref ident, ref expr) =>
            {
                let typ_expr = type_expr(expr, &lctx)?;
                always_return |= typ_expr.always_return;

                let var_typ = FullType{ mutable:*m, typ:typ_expr.typ.clone() };
                lctx.vars.insert(ident.data, var_typ);
                typ_instr.push(Located::new(
                    typ_ast::Instr::Let(*ident, typ_expr), instr.loc));
            }
            ast::Instr::While(ref cond, ref body) =>
            {
                let typ_cond = type_expr(cond, &lctx)?;
                check_type(&typ_ast::Type::Bool, &typ_cond.typ, typ_cond.loc)?;
                always_return |= typ_cond.always_return;

                let typ_body = type_block(body, &lctx)?;
                typ_instr.push(Located::new(
                    typ_ast::Instr::While(typ_cond, Box::new(typ_body)),
                    instr.loc));
            }
            ast::Instr::Return(ref e) =>
            {
                let typ_e = type_expr(e, &lctx)?;
                check_type(lctx.return_type, &typ_e.typ, instr.loc)?;
                always_return = true;

                typ_instr.push(Located::new(typ_ast::Instr::Return(typ_e),
                                            instr.loc));
            }
            ast::Instr::If(ref if_expr) =>
            {
                let typ_e = type_ifexpr(if_expr, instr.loc, &lctx)?;
                always_return |= typ_e.always_return;

                typ_instr.push(Located::new(typ_ast::Instr::Expression(typ_e),
                                            instr.loc));
            }
        }
    }

    let typ_expr = type_expr(&b.expr, &lctx)?;
    always_return |= typ_expr.always_return;

    if always_return
    {
        if let &typ_ast::Type::Void = &typ_expr.typ
        {
            let expr = typ_ast::Typed { always_return: true,
                typ: typ_ast::Type::Unknown(Rc::new(RefCell::new(None))),
                ..typ_expr };
            return Ok(typ_ast::Block { instr:typ_instr, expr });
        }

        let expr = typ_ast::Typed { always_return: true, ..typ_expr };
        return Ok(typ_ast::Block { instr:typ_instr, expr });
    }

    Ok(typ_ast::Block { instr:typ_instr, expr:typ_expr })
}

/**
 * Type the given expression
 */
fn type_expr<'a>(e: &ast::LExpr, ctx:&LocalContext<'a>)
    -> Result<typ_ast::TExpr>
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

            Ok(typ_ast::Typed { typ: typ_ast::Type::Void, mutable: false,
                lvalue: false, loc: e.loc,
                always_return: t_var.always_return || t_val.always_return, data:
                typ_ast::Expr::Assignment(Box::new(t_var), Box::new(t_val))
            })
        }
        ast::Expr::Logic(ref op, ref e1, ref e2) =>
        {
            let t1 = type_expr(e1, ctx)?;
            check_type(&typ_ast::Type::Bool, &t1.typ, t1.loc)?;

            let t2 = type_expr(e2, ctx)?;
            check_type(&typ_ast::Type::Bool, &t2.typ, t2.loc)?;

            Ok(typ_ast::Typed { typ: typ_ast::Type::Bool, mutable: false,
                lvalue: false, loc: e.loc,
                // Lazy evaluation implies that only the first arg always return
                always_return: t1.always_return,
                data: typ_ast::Expr::Logic(*op, Box::new(t1), Box::new(t2)) })
        }
        ast::Expr::Comparison(ref op, ref e1, ref e2) =>
        {
            let t1 = type_expr(e1, ctx)?;
            check_type(&typ_ast::Type::Int32, &t1.typ, t1.loc)?;

            let t2 = type_expr(e2, ctx)?;
            check_type(&typ_ast::Type::Int32, &t2.typ, t2.loc)?;

            Ok(typ_ast::Typed { typ: typ_ast::Type::Bool, mutable: false,
                lvalue: false, loc: e.loc,
                always_return: t1.always_return || t2.always_return, data:
                typ_ast::Expr::Comparison(*op, Box::new(t1), Box::new(t2)) })
        }
        ast::Expr::Arithmetic(ref op, ref e1, ref e2) =>
        {
            let t1 = type_expr(e1, ctx)?;
            check_type(&typ_ast::Type::Int32, &t1.typ, t1.loc)?;

            let t2 = type_expr(e2, ctx)?;
            check_type(&typ_ast::Type::Int32, &t2.typ, t2.loc)?;

            Ok(typ_ast::Typed { typ: typ_ast::Type::Int32, mutable: false,
                lvalue: false, loc: e.loc,
                always_return: t1.always_return || t2.always_return, data:
                typ_ast::Expr::Arithmetic(*op, Box::new(t1), Box::new(t2)),
            })
        }
        ast::Expr::Minus(ref e0) =>
        {
            let t0 = type_expr(e0, ctx)?;
            check_type(&typ_ast::Type::Int32, &t0.typ, t0.loc)?;

            Ok(typ_ast::Typed { typ: typ_ast::Type::Int32, mutable: false,
                always_return: t0.always_return, lvalue: false, loc: e.loc,
                data: typ_ast::Expr::Minus(Box::new(t0))})
        }
        ast::Expr::Not(ref e0) =>
        {
            let t0 = type_expr(e0, ctx)?;
            check_type(&typ_ast::Type::Bool, &t0.typ, t0.loc)?;

        Ok(typ_ast::Typed { typ: typ_ast::Type::Bool, mutable: false,
                lvalue: false, loc: e.loc, always_return: t0.always_return,
                data: typ_ast::Expr::Not(Box::new(t0)) })
        }
        ast::Expr::Deref(ref e0) =>
        {
            let t0 = type_expr(e0, ctx)?;
            let (deref_typ, m) = match t0.typ
            {
                typ_ast::Type::Ref(ref typ) => (simplify_type(typ), false),
                typ_ast::Type::MutRef(ref typ) => (simplify_type(typ), true),
                _ =>
                {
                    return Err(Located::new(TypingError::CannotDeref(t0.typ),
                               e.loc));
                }
            };

            Ok(typ_ast::Typed { typ: deref_typ, mutable: m,
                lvalue: true, loc: e.loc, always_return: t0.always_return,
                data: typ_ast::Expr::Deref(Box::new(t0)) })
        }
        ast::Expr::Ref(ref e0) =>
        {
            let t0 = type_expr(e0, ctx)?;
            if !t0.lvalue
            {
                return Err(Located::new(TypingError::BorrowOnRvalue, e.loc));
            }

            Ok(typ_ast::Typed { mutable:false, lvalue:false, loc: e.loc,
                typ: typ_ast::Type::Ref(Box::new(simplify_type(&t0.typ))),
                always_return: t0.always_return,
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
                typ: typ_ast::Type::MutRef(Box::new(simplify_type(&t0.typ))),
                always_return: t0.always_return,
                data: typ_ast::Expr::MutRef(Box::new(t0)) })
        }
        ast::Expr::ArrayAccess(ref array, ref index) =>
        {
            let typ_index = type_expr(index, ctx)?;
            check_type(&typ_ast::Type::Int32, &typ_index.typ, typ_index.loc)?;

            let typ_array = auto_deref(type_expr(array, ctx)?);
            if !typ_array.lvalue
            {
                return Err(Located::new(TypingError::ArrayAccessOnRvalue,
                                        typ_array.loc));
            }
            match typ_array.typ.clone()
            {
                typ_ast::Type::Vector(ref data_typ) =>
                {
                    Ok(typ_ast::Typed { mutable: typ_array.mutable, lvalue:true,
                        typ: simplify_type(data_typ), loc: e.loc,
                        always_return: typ_index.always_return ||
                            typ_array.always_return,
                        data: typ_ast::Expr::ArrayAccess(Box::new(typ_array),
                            Box::new(typ_index)) })
                }
                t => Err(Located::new(TypingError::ArrayAccessOnScalarType(t),
                         e.loc))
            }
        }
        ast::Expr::Attribute(ref e0, ref attr_name) =>
        {
            let t0 = auto_deref(type_expr(e0, ctx)?);

            let typ = match t0.typ
            {
                typ_ast::Type::Struct(ref struct_name) =>
                {
                    let struc = &ctx.global.structs.get(&struct_name).unwrap();
                    // Safe to unwrap here as type_expr should only   ^^^^^^^^
                    // return valid types.

                    match struc.fields.get(&attr_name.data)
                    {
                        None => Err(Located::new(TypingError::InvalidFieldName {
                            field: attr_name.data, struc: *struct_name },
                            attr_name.loc)),
                        Some(ref field_typ) =>
                            Ok(simplify_type(&field_typ.data))
                    }
                }
                _ => Err(Located::new(TypingError::FieldAccessOnNonStruct(
                    t0.typ.clone()), e.loc))
            }?;

            Ok(typ_ast::Typed { mutable: t0.mutable, lvalue: t0.lvalue, typ,
                loc: e.loc, always_return: t0.always_return,
                data: typ_ast::Expr::Attribute(Box::new(t0), *attr_name) })
        }
        ast::Expr::MethodCall(ref obj, ref method_name, ref args) =>
        {
            let typ_obj = auto_deref(type_expr(obj, ctx)?);
            match typ_obj.typ
            {
                typ_ast::Type::Vector(_) =>
                {
                    match &*method_name.data.to_str()
                    {
                        "len" =>
                        {
                            if args.len() != 0
                            {
                                return Err(Located::new(
                                    TypingError::WrongNumberOfArguments {
                                        expected: 0, found: args.len() },
                                        e.loc));
                            }

                            Ok(typ_ast::Typed { mutable: false, lvalue: false,
                                typ: typ_ast::Type::Int32, loc: e.loc,
                                data: typ_ast::Expr::VecLen(Box::new(typ_obj)),
                                always_return: false })
                        }
                        _ => Err(Located::new(TypingError::UnknownMethod(
                            method_name.data, typ_obj.typ.clone()),
                            method_name.loc))
                    }
                }
                _ => Err(Located::new(TypingError::UnknownMethod(
                    method_name.data, typ_obj.typ.clone()),
                    method_name.loc))
            }
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
                data: typ_ast::Expr::Constant(*val),
                always_return: false })
        }
        ast::Expr::Variable(ref id) =>
        {
            let (typ, mutable) = match ctx.vars.get(&id.data)
            {
                None =>
                {
                    return Err(Located::new(
                        TypingError::VariableUnbound(id.data), e.loc));
                }
                Some(ref full_typ) =>
                    (simplify_type(&full_typ.typ), full_typ.mutable)
            };

            Ok(typ_ast::Typed { typ, mutable, loc: e.loc, lvalue: true,
                data: typ_ast::Expr::Variable(*id),
                always_return: false })
        }
        ast::Expr::FunctionCall(ref fun_name, ref args) =>
        {
            let fun_sig = ctx.global.funs.get(&fun_name.data)
                .ok_or(Located::new(TypingError::UnknownFunction(
                    fun_name.data), fun_name.loc))?;

            if fun_sig.arguments.len() != args.len()
            {
                return Err(Located::new(TypingError::WrongNumberOfArguments {
                    expected: fun_sig.arguments.len(), found: args.len() },
                    e.loc));
            }

            let mut typ_args = Vec::new();
            let mut always_return = false;
            for (param, arg) in fun_sig.arguments.iter().zip(args)
            {
                let arg_typ = type_expr(arg, ctx)?;
                check_type(&param.typ.data, &arg_typ.typ, arg_typ.loc)?;
                always_return |= arg_typ.always_return;

                typ_args.push(arg_typ);
            }

            Ok(typ_ast::Typed { typ: fun_sig.return_type.data.clone(),
                data: typ_ast::Expr::FunctionCall(*fun_name, typ_args),
                mutable: false, lvalue: false, loc: e.loc, always_return })
        }
        ast::Expr::StructConstr(ref struct_name, ref fields) =>
        {
            match ctx.global.structs.get(&struct_name.data)
            {
                None => return Err(Located::new(TypingError::UnknownStruct(
                    struct_name.data), struct_name.loc)),
                Some(ref struc) =>
                {
                    // Type all the fields given and check if their type match
                    // with the struct
                    let mut found_fields = HashSet::new();
                    let mut field_expr = Vec::new();
                    let mut always_return = false;
                    for &(ref name, ref expr) in fields
                    {
                        let t_expr = type_expr(expr, ctx)?;
                        always_return |= t_expr.always_return;

                        match struc.fields.get(&name.data)
                        {
                            None => return Err(Located::new(
                                TypingError::InvalidFieldName { field:name.data,
                                struc: struct_name.data }, name.loc)),
                            Some(ref typ) =>
                            {
                                check_type(&typ.data, &t_expr.typ, t_expr.loc)?;

                                if !found_fields.insert(name.data)
                                {
                                    return Err(Located::new(
                                        TypingError::MultipleFieldInit(
                                        name.data), name.loc));
                                }

                                field_expr.push((*name, t_expr));
                            }
                        }
                    }

                    // Check that all fields of the struct are initialized
                    for name in struc.fields.keys()
                    {
                        if !found_fields.contains(name)
                        {
                            return Err(Located::new(TypingError::LackingField {
                                field: *name, struc: struct_name.data },
                                e.loc));
                        }
                    }

                    Ok(typ_ast::Typed { mutable: false, lvalue: false,
                        typ: typ_ast::Type::Struct(struct_name.data),
                        data: typ_ast::Expr::StructConstr(*struct_name,
                        field_expr), loc: e.loc, always_return })
                }
            }
        }
        ast::Expr::ListMacro(ref macro_name, ref elements) =>
        {
            match &*macro_name.data.to_str()
            {
                "vec" =>
                {
                    let mut vec_type = typ_ast::Type::Unknown(Rc::new(
                        RefCell::new(None)));
                    let mut typ_elements = Vec::new();
                    let mut always_return = false;
                    for elem in elements
                    {
                        let typ_elem = type_expr(elem, ctx)?;

                        // We want the most generic type of the given arguments
                        if check_type(&vec_type, &typ_elem.typ, typ_elem.loc)
                            .is_err()
                        {
                            check_type(&typ_elem.typ, &vec_type, typ_elem.loc)?;
                            vec_type = typ_elem.typ.clone();
                        }
                        always_return |= typ_elem.always_return;

                        typ_elements.push(typ_elem);
                    }

                    Ok(typ_ast::Typed { mutable: false, lvalue: false,
                        typ: typ_ast::Type::Vector(Box::new(vec_type)),
                        data: typ_ast::Expr::VecConstr(typ_elements),
                        loc: e.loc, always_return })
                }
                _ => Err(Located::new(TypingError::UnknownMacro(
                    macro_name.data), macro_name.loc))
            }
        }
        ast::Expr::StringMacro(macro_name, string) =>
        {
            match &*macro_name.data.to_str()
            {
                "print" =>
                    Ok(typ_ast::Typed { typ: typ_ast::Type::Void,
                        data: typ_ast::Expr::Print(string),
                        mutable: false, lvalue: false, loc: e.loc,
                        always_return: false }),
                _ => Err(Located::new(TypingError::UnknownMacro(
                    macro_name.data), macro_name.loc))
            }
        }
        ast::Expr::If(ref if_expr) => type_ifexpr(if_expr, e.loc, ctx),
        ast::Expr::NestedBlock(ref block) =>
        {
            let typ_block = type_block(block, ctx)?;
            Ok(typ_ast::Typed { mutable: false, lvalue: false, loc: e.loc,
                typ: simplify_type(&typ_block.expr.typ),
                always_return: typ_block.expr.always_return,
                data: typ_ast::Expr::NestedBlock(Box::new(typ_block)) })
        }
    }
}

fn auto_deref(e: typ_ast::TExpr) -> typ_ast::TExpr
{
    let typ_opt = match &e.typ
    {
        &typ_ast::Type::Ref(ref t) => Some((simplify_type(t), false)),
        &typ_ast::Type::MutRef(ref t) => Some((simplify_type(t), true)),
        _ => None
    };

    if let Some((typ, mutable)) = typ_opt
    {
        typ_ast::Typed { typ, mutable, lvalue: true, loc: e.loc,
            always_return: e.always_return,
            data: typ_ast::Expr::Deref(Box::new(e)) }
    }
    else { e }
}

fn type_ifexpr<'a>(e: &ast::IfExpr, loc: Span, ctx:&LocalContext<'a>)
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

            Ok(typ_ast::Typed { typ: simplify_type(&typ_if.expr.typ),
                mutable: false, always_return: typ_cond.always_return ||
                    (typ_if.expr.always_return && typ_else.expr.always_return),
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

            let always_return = typ_if.expr.always_return &&
                typ_else.always_return;
            let block_else = typ_ast::Block{ instr:Vec::new(), expr:typ_else };
            Ok(typ_ast::Typed { typ: simplify_type(&typ_if.expr.typ),
                mutable: false, lvalue: false, always_return, loc,
                data: typ_ast::Expr::If(Box::new(typ_cond), Box::new(typ_if),
                    Box::new(block_else)) })
        }
    }
}

/**
 * Take the struct with the given name in struct_decl and check if it is well
 * formed. Then update the global context to include it or return an error.
 * This function is recursive as we might need to compute nested structs.
 * It panics if 'struct_decl' does not contain any element associated to 'name'.
 */
fn check_struct(name: Symbol,
                mut struct_decl: &mut HashMap<Symbol, ast::Struct>,
                mut ctx: &mut GlobalContext,
                struct_names: &HashSet<Symbol>)
    -> Result<()>
{
    let s = struct_decl.remove(&name).unwrap();

    let mut fields = HashMap::new();
    for f in s.fields
    {
        let typ = check_well_formed(&f.typ.data, f.typ.loc, struct_names)?;
        match typ
        {
            typ_ast::Type::Void | typ_ast::Type::Bool | typ_ast::Type::Int32 =>
                (),
            typ_ast::Type::Struct(name) =>
            {
                if !ctx.structs.contains_key(&name)
                {
                    if struct_decl.contains_key(&name)
                    {
                        check_struct(name, &mut struct_decl, &mut ctx,
                                     &struct_names)?;
                    }
                    else
                    {
                        return Err(Located::new(TypingError::CyclicStruct(
                            name), f.typ.loc));
                    }
                }
            }
            typ_ast::Type::Vector(ref t) if !contains_borrowed(t) =>
                (),
            typ_ast::Type::Ref(_) | typ_ast::Type::MutRef(_) |
            typ_ast::Type::Vector(_) =>
            {
                return Err(Located::new(
                    TypingError::BorrowedInsideStruct(
                        f.name.data, typ.clone()), f.typ.loc));
            }
            typ_ast::Type::Unknown(_) =>
                panic!("Unknown type placeholder in struct fields")
        }

        let ltyp = Located::new(typ, f.typ.loc);
        if fields.insert(f.name.data, ltyp).is_some()
        {
            return Err(Located::new(TypingError::MultipleFieldDecl(f.name.data),
                f.name.loc));
        }
    }

    ctx.structs.insert(name, typ_ast::Struct{ fields });
    Ok(())
}

