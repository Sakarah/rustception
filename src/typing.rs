use ast;
use ast::Type;
use typ_ast;
use location::{Located, Span};
use std::collections::HashMap;

/**
 * Errors that can occur during typing.
 */
#[derive(Debug)]
pub enum TypingError
{
    MultipleFuncDecl(ast::Ident),
    MultipleStructDecl(ast::Ident),
    UnknownType(ast::Ident),
    UnknownParametrizedType(ast::Ident),
    MismatchedTypes(ast::Type, ast::Type),
    FunctionReturnBorrowed(ast::Type),
    MultipleArgumentDecl(ast::Ident),
    AssignmentOnRvalue,
    AssignmentOnConstant,
    CannotDeref(ast::Type),
    BorrowOnRvalue,
    MutBorrowOnConstant,
    VariableUnbound(ast::Ident),
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
                                  structs:HashMap::new() };

    // We keep a separated map for function bodies
    let mut funs_body = HashMap::new();

    for decl in prgm
    {
        match decl
        {
            ast::Decl::Function(f) =>
            {
                let sig = typ_ast::FunSignature { arguments:f.arguments,
                                                  return_type:f.return_type };
                match ctx.funs.insert(f.name.data.clone(), sig)
                {
                    None =>
                    {
                        funs_body.insert(f.name.data, f.body);
                    }
                    Some(_) =>
                    {
                        return Err(Located::new(
                            TypingError::MultipleFuncDecl(f.name.data),
                            f.name.loc));
                    }
                }
            },
            ast::Decl::Structure(s) =>
            {
                let struc = typ_ast::Struct { fields: s.fields };
                match ctx.structs.insert(s.name.data.clone(), struc)
                {
                    None => (),
                    Some(_) =>
                    {
                        return Err(Located::new(
                            TypingError::MultipleStructDecl(s.name.data),
                            s.name.loc));
                    }
                }
            }
        }
    }

    // Then we check if all structs are meaningful
    for (name, s) in &ctx.structs
    {
        check_struct(name, s, &ctx)?;
    }

    // Finally we type all function bodies.
    let mut typ_funs = HashMap::new();
    for (name,f) in funs_body
    {
        let typ_f = type_function(&name, f, &ctx)?;
        typ_funs.insert(name, typ_f);
    }

    let typ_prgm = typ_ast::Program { funs: typ_funs, structs: ctx.structs };
    Ok(typ_prgm)
}

/**
 * Check if the given type 'typ' is well formed in the context 'ctx'.
 * The location 'loc' is used for error reporting.
 */
fn check_well_formed(typ: &ast::Type, loc: Span, ctx: &GlobalContext)
    -> Result<()>
{
    match *typ
    {
        ast::Type::Void | ast::Type::Int32 | ast::Type::Bool =>
            Ok(()),
        ast::Type::Basic(ref id) if ctx.structs.contains_key(id) =>
             Ok(()),
        ast::Type::Basic(ref id) =>
            Err(Located::new(TypingError::UnknownType(id.clone()), loc)),
        ast::Type::Parametrized(ref id, ref typ) if id == "Vec" =>
            check_well_formed(typ, loc, ctx),
        ast::Type::Parametrized(ref id, _) =>
            Err(Located::new(TypingError::UnknownParametrizedType(id.clone()),
                             loc)),
        ast::Type::Ref(ref typ) =>
            check_well_formed(typ, loc, ctx),
        ast::Type::MutRef(ref typ) =>
            check_well_formed(typ, loc, ctx)
    }
}

/**
 * Return true if 'typ' is a borrowed type
 */
fn is_borrowed(typ: &ast::Type) -> bool
{
    match *typ
    {
        ast::Type::Void | ast::Type::Int32 | ast::Type::Bool |
        ast::Type::Basic(_) | ast::Type::Parametrized(_,_) =>
            false,
        ast::Type::Ref(_) | ast::Type::MutRef(_) =>
            true
    }
}

/**
 * Check if the type 'found' can be automatically converted into 'expected'.
 * 'loc' is used for error reporting.
 */
fn check_type(expected: &ast::Type, found: &ast::Type, loc: Span) -> Result<()>
{
    match (expected, found)
    {
        (&ast::Type::Void, &ast::Type::Void) => Ok(()),
        (&ast::Type::Int32, &ast::Type::Int32) => Ok(()),
        (&ast::Type::Bool, &ast::Type::Bool) => Ok(()),
        (&ast::Type::Basic(ref id_e), &ast::Type::Basic(ref id_f))
            if id_e == id_f =>
            Ok(()),
        (&ast::Type::Parametrized(ref id_e, ref typ_e),
         &ast::Type::Parametrized(ref id_f, ref typ_f)) if id_e == id_f =>
            check_type(typ_e, typ_f, loc),
        (&ast::Type::Ref(ref typ_e), &ast::Type::Ref(ref typ_f)) =>
            check_type(typ_e, typ_f, loc),
        (&ast::Type::MutRef(ref typ_e), &ast::Type::MutRef(ref typ_f)) =>
            check_type(typ_e, typ_f, loc),
        (&ast::Type::Ref(ref typ_e), &ast::Type::MutRef(ref typ_f)) =>
            check_type(typ_e, typ_f, loc),
        _ => Err(Located::new(TypingError::MismatchedTypes(expected.clone(),
            found.clone()), loc))
    }
}

#[derive(Clone)]
pub struct FullType
{
    pub mutable: bool,
    pub typ: Type
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
    check_well_formed(&f_sig.return_type.data, f_sig.return_type.loc, ctx)?;
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
        check_well_formed(&arg.typ.data, arg.typ.loc, ctx)?;
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
            },
            ast::Instr::Let(ref m, ref ident, ref expr) =>
            {
                let typ_expr = type_expr(expr, &lctx)?;
                let var_typ = FullType{ mutable:*m, typ:typ_expr.typ.clone() };
                lctx.vars.insert(ident.data.clone(), var_typ);
                typ_instr.push(Located::new(
                    typ_ast::Instr::Let(ident.clone(), typ_expr), instr.loc));
            },
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

            Ok(typ_ast::Typed { typ: Type::Void, mutable: false, data:
                typ_ast::Expr::Assignment(Box::new(t_var), Box::new(t_val)),
                lvalue: false, loc: e.loc })

        }
        ast::Expr::Logic(ref op, ref e1, ref e2) =>
        {
            let t1 = type_expr(e1, ctx)?;
            check_type(&ast::Type::Bool, &t1.typ, t1.loc)?;

            let t2 = type_expr(e2, ctx)?;
            check_type(&ast::Type::Bool, &t2.typ, t2.loc)?;

            Ok(typ_ast::Typed { typ: ast::Type::Bool, mutable: false,
                data: typ_ast::Expr::Logic(*op, Box::new(t1), Box::new(t2)),
                lvalue: false, loc: e.loc })
        }
        ast::Expr::Comparison(ref op, ref e1, ref e2) =>
        {
            let t1 = type_expr(e1, ctx)?;
            check_type(&ast::Type::Int32, &t1.typ, t1.loc)?;

            let t2 = type_expr(e2, ctx)?;
            check_type(&ast::Type::Int32, &t2.typ, t2.loc)?;

            Ok(typ_ast::Typed { typ: ast::Type::Bool, mutable: false, data:
                typ_ast::Expr::Comparison(*op, Box::new(t1), Box::new(t2)),
                lvalue: false, loc: e.loc })

        }
        ast::Expr::Arithmetic(ref op, ref e1, ref e2) =>
        {
            let t1 = type_expr(e1, ctx)?;
            check_type(&ast::Type::Int32, &t1.typ, t1.loc)?;

            let t2 = type_expr(e2, ctx)?;
            check_type(&ast::Type::Int32, &t2.typ, t2.loc)?;

            Ok(typ_ast::Typed { typ: ast::Type::Int32, mutable: false, data:
                typ_ast::Expr::Arithmetic(*op, Box::new(t1), Box::new(t2)),
                lvalue: false, loc: e.loc })

        }
        ast::Expr::Minus(ref e0) =>
        {
            let t0 = type_expr(e0, ctx)?;
            check_type(&ast::Type::Int32, &t0.typ, t0.loc)?;

            Ok(typ_ast::Typed { typ: ast::Type::Int32, mutable: false,
                data: typ_ast::Expr::Minus(Box::new(t0)),
                lvalue: false, loc: e.loc })

        }
        ast::Expr::Not(ref e0) =>
        {
            let t0 = type_expr(e0, ctx)?;
            check_type(&ast::Type::Bool, &t0.typ, t0.loc)?;

        Ok(typ_ast::Typed { typ: ast::Type::Bool, mutable: false,
                data: typ_ast::Expr::Not(Box::new(t0)),
                lvalue: false, loc: e.loc })
        }
        ast::Expr::Deref(ref e0) =>
        {
            let t0 = type_expr(e0, ctx)?;
            let (deref_typ, m) = match t0.typ
            {
                ast::Type::Ref(ref typ) => (*typ.clone(), false),
                ast::Type::MutRef(ref typ) => (*typ.clone(), true),
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

            Ok(typ_ast::Typed { typ: ast::Type::Ref(Box::new(t0.typ.clone())),
                mutable: false, data: typ_ast::Expr::Ref(Box::new(t0)),
                lvalue: false, loc: e.loc })
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
                typ: ast::Type::MutRef(Box::new(t0.typ.clone())),
                data: typ_ast::Expr::MutRef(Box::new(t0)) })

        }
        ast::Expr::Constant(ref val) =>
        {
            let typ = match *val
            {
                ast::Const::Void => ast::Type::Void,
                ast::Const::Int32(_) => ast::Type::Int32,
                ast::Const::Bool(_) => ast::Type::Bool
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

/**
 * Check if the struct with the given name and fields is well formed.
 */
fn check_struct(name: &str, s: &typ_ast::Struct, ctx:&GlobalContext)
    -> Result<()>
{
    unimplemented!()
}

