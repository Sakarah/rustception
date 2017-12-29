use location::{Span,Located};
use ast::Ident;
use typ_ast;
use bc_ast;
use bc_ast::Lifetime;
use std::collections::{HashMap, HashSet};

pub enum BorrowError
{
    LifetimeTooShort(Ident),
    BorrowAfterMove(Ident),
    BorrowAfterMutBorrow(Ident),
    MutBorrowAfterBorrow(Ident),
    UsedAfterMove(Ident),
    MovedAfterBorrow(Ident),
    ReassignAfterBorrow(Ident),
    MoveOutOfDerefValue,
    MoveOutOfIndexed,
    MismatchedLifetimes,
    UnresolvedType,
    UnresolvedLifetime,
}

pub type Result<T> = ::std::result::Result<T, Located<BorrowError>>;

/// Return true if the given type is a copy type (i.e. values are never moved).
fn is_copy(typ: &bc_ast::Type) -> bool
{
    use bc_ast::Type;
    match *typ
    {
        Type::Void | Type::Int32 | Type::Bool | Type::Ref(_, _) => true,
        Type::Struct(_) | Type::Vector(_) | Type::MutRef(_, _) => false,
    }
}

/**
 * Get the most external lifetime inside the given type.
 * If there is no borrow, return Lifetime::max_value().
 */
fn get_type_lifetime(typ: &bc_ast::Type) -> Lifetime
{
    use bc_ast::Type;
    match *typ
    {
        Type::Void | Type::Int32 | Type::Bool | Type::Struct(_) =>
            Lifetime::max_value(),
        Type::Vector(ref t) => get_type_lifetime(t),
        Type::Ref(lt, _) | Type::MutRef(lt, _) => lt
    }
}

/// Replace the lifetime by 'lt' if the type is a reference, else return a copy
fn replace_lifetime(typ: &bc_ast::Type, lt: Lifetime) -> bc_ast::Type
{
    use bc_ast::Type;
    match *typ
    {
        Type::Void => Type::Void,
        Type::Int32 => Type::Int32,
        Type::Bool => Type::Bool,
        Type::Struct(s) => Type::Struct(s),
        Type::Vector(ref t) => Type::Vector(t.clone()),
        Type::Ref(_, ref t) => Type::Ref(lt, t.clone()),
        Type::MutRef(_, ref t) => Type::MutRef(lt, t.clone()),
    }
}

/**
 * Check that the lifetimed type `found` can be used where a lifetimed
 * type `exp` is expected.
 */
fn check_type(exp: &bc_ast::Type, found: &bc_ast::Type, loc: Span)
    -> Result<()>
{
    use bc_ast::Type;
    match *exp
    {
        Type::Void | Type::Int32 | Type::Bool | Type::Struct(_) => Ok(()),
        Type::Vector(ref t_exp) =>
        {
            if let Type::Vector(ref t_found) = *found
            {
                check_type(t_exp, t_found, loc)
            }
            else
            {
                panic!("Type error during borrow checking")
            }
        }
        Type::Ref(exp_lt, ref t_exp) | Type::MutRef(exp_lt, ref t_exp) =>
        {
            match *found
            {
                Type::Ref(found_lt, ref t_found) |
                Type::MutRef(found_lt, ref t_found) =>
                {
                    if found_lt <= exp_lt
                    {
                        check_type(t_exp, t_found, loc)
                    }
                    else
                    {
                        Err(Located::new(BorrowError::MismatchedLifetimes, loc))
                    }
                }
                _ => panic!("Type error during borrow checking")
            }
        }
    }
}

/**
 * Convert from typ_ast::Type to bc_ast::Type.
 * Rejects all unresolved unknown types, and assign the lifetime 0 to all
 * references
 */
fn convert_type(typ: &typ_ast::Type, loc: Span) -> Result<bc_ast::Type>
{
    match *typ
    {
        typ_ast::Type::Void => Ok(bc_ast::Type::Void),
        typ_ast::Type::Int32 => Ok(bc_ast::Type::Int32),
        typ_ast::Type::Bool => Ok(bc_ast::Type::Bool),
        typ_ast::Type::Struct(s) => Ok(bc_ast::Type::Struct(s)),
        typ_ast::Type::Vector(ref t) =>
            Ok(bc_ast::Type::Vector(Box::new(convert_type(t, loc)?))),
        typ_ast::Type::Ref(ref t) =>
            Ok(bc_ast::Type::Ref(0, Box::new(convert_type(t, loc)?))),
        typ_ast::Type::MutRef(ref t) =>
            Ok(bc_ast::Type::MutRef(0, Box::new(convert_type(t, loc)?))),
        typ_ast::Type::Unknown(ref t) =>
        {
            match *t.borrow()
            {
                Some(ref t) => convert_type(t, loc),
                None => Err(Located::new(BorrowError::UnresolvedType, loc))
            }
        }
    }
}

/**
 * Convert from typ_ast::Type to bc_ast::Type and use the lifetime 'lt' for the
 * first found reference. Reject unresolved unknown types and nested references.
 * When the lifetime 0 is given, reject if including reference.
 */
fn to_lifetimed_type(typ: &typ_ast::Type, lt: Lifetime, loc: Span)
    -> Result<bc_ast::Type>
{
    match *typ
    {
        typ_ast::Type::Void | typ_ast::Type::Int32 | typ_ast::Type::Bool |
        typ_ast::Type::Struct(_) =>
            convert_type(typ, loc),
        typ_ast::Type::Vector(ref t) =>
            Ok(bc_ast::Type::Vector(Box::new(to_lifetimed_type(t, lt, loc)?))),
        typ_ast::Type::Ref(_) | typ_ast::Type::MutRef(_) if lt == 0 =>
            Err(Located::new(BorrowError::UnresolvedLifetime, loc)),
        typ_ast::Type::Ref(ref t) =>
            Ok(bc_ast::Type::Ref(lt, Box::new(to_lifetimed_type(t,0,loc)?))),
        typ_ast::Type::MutRef(ref t) =>
            Ok(bc_ast::Type::MutRef(lt, Box::new(to_lifetimed_type(t,0,loc)?))),
        typ_ast::Type::Unknown(ref t) =>
        {
            match *t.borrow()
            {
                Some(ref t) => to_lifetimed_type(t, lt, loc),
                None => Err(Located::new(BorrowError::UnresolvedType, loc)),
            }
        }
    }
}

#[derive(Clone)]
struct VarState
{
    name: Ident,
    moved: bool,
    borrowed_for: Option<Lifetime>, // None => not borrowed
    mut_borrowed_for: Option<Lifetime>,
    typ: bc_ast::Type
}

#[derive(Clone, Copy)]
enum LValueAction
{
    Move,
    Borrow,
    BorrowMut,
    Reassign,
    PartialReassign,
    Nothing
}

#[derive(Clone)]
struct Context<'a>
{
    vars_by_lifetime: Vec<VarState>, // Lifetimes are unique for any context
    lifetime_for_name: HashMap<Ident, Vec<Lifetime>>, // Last = current
    borrow_end: Vec<HashSet<Lifetime>>, // Index i = borrow ends with lifetime i
    mut_borrow_end: Vec<HashSet<Lifetime>>,
    lvalue_action: LValueAction,
    borrow_lifetime: Lifetime, // Lifetime of the next encountered borrows
    fun_sigs: &'a HashMap<Ident, typ_ast::FunSignature>
}

impl<'a> Context<'a>
{
    /// Create a new empty context
    fn new(fun_sigs: &HashMap<Ident, typ_ast::FunSignature>) -> Context
    {
        Context { vars_by_lifetime: Vec::new(),
            lifetime_for_name: HashMap::new(), borrow_end: Vec::new(),
            mut_borrow_end: Vec::new(), lvalue_action: LValueAction::Move,
            borrow_lifetime: Lifetime::max_value(), fun_sigs }
    }

    /// Add the given variable to context
    fn add_variable(&mut self, name: Ident, typ: bc_ast::Type)
    {
        let lt = self.next_lifetime();
        let info = VarState { name, typ, moved: false, borrowed_for: None,
            mut_borrowed_for: None };

        #[cfg(feature = "borrow_log")]
        print!("Add variable {} at lifetime #{}\n\n", name, lt);

        self.lifetime_for_name.entry(name).or_insert(Vec::new()).push(lt);
        self.vars_by_lifetime.push(info);
        self.borrow_end.push(HashSet::new());
        self.mut_borrow_end.push(HashSet::new());
    }

    /// Get the variable lifetime
    fn get_lifetime(&self, name: Ident) -> Lifetime
    {
        *self.lifetime_for_name.get(&name).unwrap().last().unwrap()
    }

    /// Get the type of the variable in the current context
    fn get_type(&self, name: Ident) -> &bc_ast::Type
    {
        let var_lt = self.get_lifetime(name);
        &self.vars_by_lifetime[var_lt].typ
    }

    fn next_lifetime(&self) -> Lifetime
    {
        self.vars_by_lifetime.len()
    }

    fn is_moved(&self, name: Ident) -> bool
    {
        let var_lt = self.get_lifetime(name);
        self.vars_by_lifetime[var_lt].moved
    }

    /// Borrow the given variable for the specified lifetime if possible
    fn borrow_for(&mut self, name: Ident, lt: Lifetime, mutable_borrow: bool,
                  loc: Span) -> Result<()>
    {
        #[cfg(feature = "borrow_log")]
        print!("{}:\nBorrow {}{} for lifetime #{}\n\n", loc,
               if mutable_borrow { "mutably " } else { "" }, name, lt);

        let var_lt = self.get_lifetime(name);
        if lt < var_lt
        {
            return Err(Located::new(BorrowError::LifetimeTooShort(name), loc));
        }

        let info = self.vars_by_lifetime.get_mut(var_lt).unwrap();
        if info.moved
        {
            return Err(Located::new(BorrowError::BorrowAfterMove(name), loc));
        }
        else if let Some(_) = info.mut_borrowed_for
        {
            return Err(Located::new(BorrowError::BorrowAfterMutBorrow(name),
                loc));
        }
        else if let Some(old_blt) = info.borrowed_for
        {
            if mutable_borrow
            {
                return Err(Located::new(BorrowError::MutBorrowAfterBorrow(name),
                    loc));
            }
            if lt < old_blt
            {
                info.borrowed_for = Some(lt);
                self.borrow_end[old_blt].remove(&var_lt);
                self.borrow_end[lt].insert(var_lt);
            }
        }
        else if lt < self.borrow_end.len()
        {
            if mutable_borrow
            {
                info.mut_borrowed_for = Some(lt);
                self.mut_borrow_end[lt].insert(var_lt);
            }
            else
            {
                info.borrowed_for = Some(lt);
                self.borrow_end[lt].insert(var_lt);
            }
        }
        Ok(())
    }

    /// Try to declare that the variable has been moved and report errors
    fn move_var(&mut self, name: Ident, loc: Span) -> Result<()>
    {
         #[cfg(feature = "borrow_log")]
        print!("{}:\nMove variable {}\n\n", loc, name);

        let var_lt = self.get_lifetime(name);
        let info = self.vars_by_lifetime.get_mut(var_lt).unwrap();
        if info.moved
        {
            Err(Located::new(BorrowError::UsedAfterMove(name), loc))
        }
        else if let Some(_) = info.borrowed_for
        {
            Err(Located::new(BorrowError::MovedAfterBorrow(name), loc))
        }
        else if let Some(_) = info.mut_borrowed_for
        {
            Err(Located::new(BorrowError::MovedAfterBorrow(name), loc))
        }
        else
        {
            info.moved = true;
            Ok(())
        }
    }

    /// Try to declare that the variable has been (partially or not) replaced
    fn replace_var(&mut self, name: Ident, partial:bool, loc: Span)
        -> Result<()>
    {
        #[cfg(feature = "borrow_log")]
        print!("{}:\nReplace {}{}\n\n", loc, name, if partial { " partially" }
               else { "" });

        let var_lt = self.get_lifetime(name);
        let info = self.vars_by_lifetime.get_mut(var_lt).unwrap();
        if partial && info.moved
        {
            Err(Located::new(BorrowError::UsedAfterMove(name), loc))
        }
        else if let Some(_) = info.borrowed_for
        {
            Err(Located::new(BorrowError::ReassignAfterBorrow(name), loc))
        }
        else if let Some(_) = info.mut_borrowed_for
        {
            Err(Located::new(BorrowError::ReassignAfterBorrow(name), loc))
        }
        else
        {
            info.moved = false;
            Ok(())
        }
    }

    /// Unwind the context up to the specified lifetime.
    fn unwind_up_to(&mut self, lt: Lifetime)
    {
        #[cfg(feature = "borrow_log")]
        print!("Unwind up to lifetime #{}\n\n", lt);

        while self.vars_by_lifetime.len() > lt
        {
            let borr_end = self.borrow_end.pop().unwrap();
            for returned_lt in borr_end
            {
                let returned_var = self.vars_by_lifetime
                                       .get_mut(returned_lt).unwrap();
                returned_var.borrowed_for = None;
            }

            let mut_borr_end = self.mut_borrow_end.pop().unwrap();
            for returned_lt in mut_borr_end
            {
                let returned_var = self.vars_by_lifetime
                                       .get_mut(returned_lt).unwrap();
                returned_var.mut_borrowed_for = None;
            }


            let var = self.vars_by_lifetime.pop().unwrap();
            self.lifetime_for_name.get_mut(&var.name).unwrap().pop();
        }
    }

    /// Merge two contexts after a branching
    fn merge_with(&mut self, mut other: Context)
    {
        #[cfg(feature = "borrow_log")]
        print!("Merging contexts\n\n", lt);

        // For each variable take the worst configuration
        for (var_lt, other_info) in other.vars_by_lifetime.drain(..).enumerate()
        {
            let var_info = &mut self.vars_by_lifetime[var_lt];
            var_info.moved |= other_info.moved;

            if let Some(oth_borr_lt) = other_info.borrowed_for
            {
                if let Some(var_borr_lt) = var_info.borrowed_for
                {
                    if oth_borr_lt < var_borr_lt
                    {
                        self.borrow_end[var_borr_lt].remove(&var_lt);
                        self.borrow_end[oth_borr_lt].insert(var_lt);
                        var_info.borrowed_for = Some(oth_borr_lt);
                    }
                }
                else
                {
                    self.borrow_end[oth_borr_lt].insert(var_lt);
                    var_info.borrowed_for = Some(oth_borr_lt);
                }
            }

            if let Some(oth_borr_lt) = other_info.mut_borrowed_for
            {
                if let Some(var_borr_lt) = var_info.mut_borrowed_for
                {
                    if oth_borr_lt < var_borr_lt
                    {
                        self.mut_borrow_end[var_borr_lt].remove(&var_lt);
                        self.mut_borrow_end[oth_borr_lt].insert(var_lt);
                        var_info.mut_borrowed_for = Some(oth_borr_lt);
                    }
                }
                else
                {
                    self.mut_borrow_end[oth_borr_lt].insert(var_lt);
                    var_info.mut_borrowed_for = Some(oth_borr_lt);
                }
            }
        }
    }
}

/// Run the borrow checker on the given program
pub fn check_program(prgm: typ_ast::Program) -> Result<bc_ast::Program>
{
    let mut structs = HashMap::new();
    for (s_name, struc) in &prgm.structs
    {
        let mut fields = HashMap::new();
        for (f_name, f_typ) in &struc.fields
        {
            let f_btyp = convert_type(&f_typ.data, f_typ.loc)?;
            fields.insert(*f_name, Located::new(f_btyp, f_typ.loc));
        }
        structs.insert(*s_name, bc_ast::Struct { fields });
    }

    let mut funs = HashMap::new();
    for (f_name, f_body) in &prgm.fun_bodies
    {
        let mut ctx = Context::new(&prgm.fun_sigs);

        let f_sig = prgm.fun_sigs.get(f_name).unwrap();
        let mut arguments = Vec::new();
        for arg in &f_sig.arguments
        {
            let a_typ = convert_type(&arg.typ.data, arg.typ.loc)?;
            arguments.push(bc_ast::Arg { mutable: arg.mutable,
                name: arg.name, typ: Located::new(a_typ.clone(),arg.typ.loc) });

            ctx.add_variable(arg.name.data, a_typ);
        }

        let sig = bc_ast::FunSignature { arguments,
            return_type: Located::new(convert_type(&f_sig.return_type.data,
                f_sig.return_type.loc)?, f_sig.return_type.loc) };
        let body = check_block(&f_body, &mut ctx, 0)?;
        funs.insert(*f_name, bc_ast::Fun { sig, body });
    }

    Ok(bc_ast::Program { funs, structs })
}

fn check_block(b: &typ_ast::Block, ctx: &mut Context, block_lifetime: Lifetime)
    -> Result<bc_ast::Block>
{
    // Backup and reset context lvalue action for instructions
    let old_lvalue_act = ctx.lvalue_action;
    ctx.lvalue_action = LValueAction::Move;
    let old_borrow_lt = ctx.borrow_lifetime;
    ctx.borrow_lifetime = Lifetime::max_value();

    let mut instr = Vec::new();
    for b_instr in &b.instr
    {
        instr.push(check_instr(b_instr, ctx)?);
    }

    // Restore lvalue action for final expression
    ctx.lvalue_action = old_lvalue_act;
    ctx.borrow_lifetime = old_borrow_lt;

    let expr = check_expr(&b.expr, ctx)?;
    ctx.unwind_up_to(block_lifetime);

    Ok(bc_ast::Block { instr, expr })
}

fn check_instr(i: &typ_ast::LInstr, ctx: &mut Context)
    -> Result<bc_ast::LInstr>
{
    let new_instr = match i.data
    {
        typ_ast::Instr::Expression(ref expr) =>
            bc_ast::Instr::Expression(check_expr(expr, ctx)?),
        typ_ast::Instr::Let(ref id, ref val) =>
        {
            let new_lt = ctx.next_lifetime();
            // We must create the new lifetime but we don't know the type yet.
            // So temporarily use (), there is no recursive variable anyway.
            ctx.add_variable(id.data, bc_ast::Type::Void);

            // Borrows on right side must be taken at the scope of the new
            // variable.
            ctx.borrow_lifetime = new_lt;
            let new_val = check_expr(val, ctx)?;
            ctx.borrow_lifetime = Lifetime::max_value();

            // Now put the correct type with the variable, if the type is a
            // reference, change the lifetime to the new one.
            ctx.vars_by_lifetime[new_lt].typ = replace_lifetime(&new_val.typ,
                                                                new_lt);

            bc_ast::Instr::Let(*id, new_val)
        }
        typ_ast::Instr::While(ref expr, ref block) =>
        {
            let e = check_expr(expr, ctx)?;

            // We cannot know if the while block will be executed so we must
            // fork the contexts.
            let block_lt = ctx.next_lifetime();
            let mut block_ctx = ctx.clone();
            let b = check_block(block, &mut block_ctx, block_lt)?;
            ctx.merge_with(block_ctx);

            bc_ast::Instr::While(e, Box::new(b))
        }
        typ_ast::Instr::Return(ref expr) =>
            bc_ast::Instr::Return(check_expr(expr, ctx)?),
    };

    Ok(Located::new(new_instr, i.loc))
}

fn check_expr(e: &typ_ast::TExpr, ctx: &mut Context)
    -> Result<bc_ast::TExpr>
{
    let expr;
    let typ;

    match e.data
    {
        typ_ast::Expr::Constant(c) =>
        {
            expr = bc_ast::Expr::Constant(c);
            typ = convert_type(&e.typ, e.loc)?;
        }
        typ_ast::Expr::Variable(ref id) =>
        {
            expr = bc_ast::Expr::Variable(*id);
            typ = ctx.get_type(id.data).clone();

            match ctx.lvalue_action
            {
                LValueAction::Move if is_copy(&typ) => (),
                LValueAction::Move =>
                {
                    ctx.move_var(id.data, e.loc)?;
                }
                LValueAction::Borrow =>
                {
                    let lifetime = ctx.borrow_lifetime;
                    ctx.borrow_for(id.data, lifetime, false, e.loc)?;
                }
                LValueAction::BorrowMut =>
                {
                    let lifetime = ctx.borrow_lifetime;
                    ctx.borrow_for(id.data, lifetime, true, e.loc)?;
                }
                LValueAction::Reassign =>
                {
                    ctx.replace_var(id.data, false, e.loc)?;
                }
                LValueAction::PartialReassign =>
                {
                    ctx.replace_var(id.data, true, e.loc)?;
                }
                LValueAction::Nothing if ctx.is_moved(id.data) =>
                {
                    return Err(Located::new(BorrowError::UsedAfterMove(id.data),
                        e.loc));
                }
                LValueAction::Nothing => ()
            }
        }
        typ_ast::Expr::Assignment(ref var, ref val) =>
        {
            ctx.lvalue_action = LValueAction::Reassign;
            let new_var = check_expr(var, ctx)?;

            ctx.lvalue_action = LValueAction::Move;
            ctx.borrow_lifetime = get_type_lifetime(&new_var.typ);
            let new_val = check_expr(val, ctx)?;
            ctx.borrow_lifetime = Lifetime::max_value();

            check_type(&new_var.typ, &new_val.typ, e.loc)?;

            typ = bc_ast::Type::Void;
            expr = bc_ast::Expr::Assignment(Box::new(new_var),
                                            Box::new(new_val));
        }
        typ_ast::Expr::FunctionCall(name, ref args) =>
        {
            let old_blt = ctx.borrow_lifetime;
            let function_lifetime = ctx.next_lifetime();
            ctx.borrow_lifetime = function_lifetime;
            // Add a dummy variable to create a new lifetime for the function
            ctx.add_variable(name.data, bc_ast::Type::Void);

            let sig = ctx.fun_sigs.get(&name.data).unwrap();
            let mut new_args = Vec::new();
            for (arg, sig_arg) in args.iter().zip(&sig.arguments)
            {
                // Move arguments by default
                ctx.lvalue_action = LValueAction::Move;

                if let typ_ast::Type::MutRef(_) = arg.typ
                {
                    if let typ_ast::Expr::Variable(var_name) = arg.data
                    {
                        // We have a `&mut T` variable used in function call,
                        // we must perform reborrowing to prevent invalid move.
                        // Reborrowing use the mutability requested by the
                        // signature.
                        match sig_arg.typ.data
                        {
                            typ_ast::Type::Ref(_) =>
                                ctx.borrow_for(var_name.data, function_lifetime,
                                           false, arg.loc)?,
                            typ_ast::Type::MutRef(_) =>
                                ctx.borrow_for(var_name.data, function_lifetime,
                                           true, arg.loc)?,
                            _ => panic!("Type error during borrow checking")
                        }

                        // No move after reborrowing
                        ctx.lvalue_action = LValueAction::Nothing;
                    }
                }

                new_args.push(check_expr(arg, ctx)?);
            }

            ctx.unwind_up_to(function_lifetime);
            ctx.borrow_lifetime = old_blt;

            expr = bc_ast::Expr::FunctionCall(name, new_args);
            typ = convert_type(&e.typ, e.loc)?;
        }
        typ_ast::Expr::Logic(op, ref e1, ref e2) =>
        {
            let new_e1 = check_expr(e1, ctx)?;
            let new_e2 = check_expr(e2, ctx)?;

            expr = bc_ast::Expr::Logic(op, Box::new(new_e1), Box::new(new_e2));
            typ = bc_ast::Type::Bool;
        }
        typ_ast::Expr::Comparison(cmp, ref e1, ref e2) =>
        {
            let new_e1 = check_expr(e1, ctx)?;
            let new_e2 = check_expr(e2, ctx)?;

            expr = bc_ast::Expr::Comparison(cmp, Box::new(new_e1),
                                            Box::new(new_e2));
            typ = bc_ast::Type::Bool;
        }
        typ_ast::Expr::Arithmetic(op, ref e1, ref e2) =>
        {
            let new_e1 = check_expr(e1, ctx)?;
            let new_e2 = check_expr(e2, ctx)?;

            expr = bc_ast::Expr::Arithmetic(op, Box::new(new_e1),
                                            Box::new(new_e2));
            typ = bc_ast::Type::Int32;
        }
        typ_ast::Expr::Minus(ref e0) =>
        {
            let new_e0 = check_expr(e0, ctx)?;
            expr = bc_ast::Expr::Minus(Box::new(new_e0));
            typ = bc_ast::Type::Int32;
        }
        typ_ast::Expr::Not(ref e0) =>
        {
            let new_e0 = check_expr(e0, ctx)?;
            expr = bc_ast::Expr::Not(Box::new(new_e0));
            typ = bc_ast::Type::Bool;
        }
        typ_ast::Expr::Deref(ref val) =>
        {
            let old_lvalue_act = ctx.lvalue_action;
            match ctx.lvalue_action
            {
                LValueAction::Move =>
                {
                    let result_typ = convert_type(&e.typ, e.loc)?;
                    if is_copy(&result_typ)
                    {
                        ctx.lvalue_action = LValueAction::Nothing;
                    }
                    else
                    {
                        return Err(Located::new(
                            BorrowError::MoveOutOfDerefValue, e.loc));
                    }
                }
                LValueAction::Borrow | LValueAction::BorrowMut => (),
                LValueAction::Reassign | LValueAction::PartialReassign =>
                {
                    ctx.lvalue_action = LValueAction::Nothing;
                }
                LValueAction::Nothing => (),
            }

            let new_val = check_expr(val, ctx)?;
            ctx.lvalue_action = old_lvalue_act;

            match *&new_val.typ
            {
                bc_ast::Type::Ref(_, ref t) | bc_ast::Type::MutRef(_, ref t) =>
                    typ = (**t).clone(),
                _ => panic!("Type error during borrow checking")
            }
            expr = bc_ast::Expr::Deref(Box::new(new_val));
        }
        typ_ast::Expr::Ref(ref val) =>
        {
            ctx.lvalue_action = LValueAction::Borrow;
            let new_val = check_expr(val, ctx)?;
            ctx.lvalue_action = LValueAction::Move;

            typ = bc_ast::Type::Ref(ctx.borrow_lifetime,
                                    Box::new(new_val.typ.clone()));
            expr = bc_ast::Expr::Ref(Box::new(new_val));
        }
        typ_ast::Expr::MutRef(ref val) =>
        {
            ctx.lvalue_action = LValueAction::BorrowMut;
            let new_val = check_expr(val, ctx)?;
            ctx.lvalue_action = LValueAction::Move;

            typ = bc_ast::Type::MutRef(ctx.borrow_lifetime,
                                       Box::new(new_val.typ.clone()));
            expr = bc_ast::Expr::MutRef(Box::new(new_val));
        }
        typ_ast::Expr::StructConstr(name, ref fields) =>
        {
            let mut new_fields = Vec::new();
            for &(f_name, ref f_expr) in fields
            {
                new_fields.push((f_name, check_expr(f_expr, ctx)?));
            }

            typ = bc_ast::Type::Struct(name.data);
            expr = bc_ast::Expr::StructConstr(name, new_fields);
        }
        typ_ast::Expr::Attribute(ref val, name) =>
        {
            let old_action = ctx.lvalue_action;
            if let LValueAction::Reassign = ctx.lvalue_action
            {
                ctx.lvalue_action = LValueAction::PartialReassign;
            }
            if let LValueAction::Move = ctx.lvalue_action
            {
                if is_copy(&convert_type(&e.typ, e.loc)?)
                {
                    // Do not move the struct if the resulting type is copy
                    ctx.lvalue_action = LValueAction::Nothing;
                }
            }

            let new_val = check_expr(val, ctx)?;

            ctx.lvalue_action = old_action;

            typ = convert_type(&e.typ, e.loc)?; // No references inside struct
            expr = bc_ast::Expr::Attribute(Box::new(new_val), name);
        }
        typ_ast::Expr::VecConstr(ref values) =>
        {
            // We can always replace the outmost reference lifetime by the
            // lifetime of the current variable assigned to. It is not true for
            // nested references though, in those case the type inference seem
            // quite complicated (rustc seems to allow taking a reference before
            // current scope for that). So I just reject nested references
            // inside Vec.
            typ = to_lifetimed_type(&e.typ, ctx.borrow_lifetime, e.loc)?;

            let data_typ = if let bc_ast::Type::Vector(ref t) = typ
            {
                t
            }
            else
            {
                panic!("Type error during borrow checking (expected array)")
            };

            let mut new_values = Vec::new();
            for v_expr in values
            {
                let new_val = check_expr(v_expr, ctx)?;
                check_type(data_typ, &new_val.typ, new_val.loc)?;

                new_values.push(new_val);
            }

            expr = bc_ast::Expr::VecConstr(new_values);
        }
        typ_ast::Expr::ArrayAccess(ref array, ref index) =>
        {
            let old_action = ctx.lvalue_action;
            if let LValueAction::Reassign = ctx.lvalue_action
            {
                ctx.lvalue_action = LValueAction::PartialReassign;
            }
            if let LValueAction::Move = ctx.lvalue_action
            {
                if is_copy(&convert_type(&e.typ, e.loc)?)
                {
                    // Do not move the array if the resulting type is copy
                    ctx.lvalue_action = LValueAction::Nothing;
                }
                else
                {
                    // Refuse to move out of indexed content
                    return Err(Located::new(BorrowError::MoveOutOfIndexed,
                        e.loc));
                }
            }
            let new_array = check_expr(array, ctx)?;

            ctx.lvalue_action = LValueAction::Move;
            let old_borrow_lt = ctx.borrow_lifetime;
            ctx.borrow_lifetime = Lifetime::max_value();

            let new_index = check_expr(index, ctx)?;

            ctx.lvalue_action = old_action;
            ctx.borrow_lifetime = old_borrow_lt;

            if let bc_ast::Type::Vector(ref t) = new_array.typ
            {
                typ = (**t).clone();
            }
            else
            {
                panic!("Type error during borrow checking")
            }
            expr = bc_ast::Expr::ArrayAccess(Box::new(new_array),
                                             Box::new(new_index));
        }
        typ_ast::Expr::VecLen(ref array) =>
        {
            let old_action = ctx.lvalue_action;
            ctx.lvalue_action = LValueAction::Nothing;
            let new_array = check_expr(array, ctx)?;
            ctx.lvalue_action = old_action;

            typ = bc_ast::Type::Int32;
            expr = bc_ast::Expr::VecLen(Box::new(new_array));
        }
        typ_ast::Expr::Print(string) =>
        {
            typ = bc_ast::Type::Void;
            expr = bc_ast::Expr::Print(string);
        }
        typ_ast::Expr::If(ref cond, ref block_true, ref block_false) =>
        {
            let new_cond = check_expr(cond, ctx)?;

            // We cannot know which one of the block_true or block_false will
            // be executed so we must fork the contexts.
            let block_lt = ctx.next_lifetime();
            let mut true_ctx = ctx.clone();
            let b_true = check_block(block_true, &mut true_ctx, block_lt)?;
            let b_false = check_block(block_false, ctx, block_lt)?;
            ctx.merge_with(true_ctx);

            // We can safely replace the first borrow lifetime by the lifetime
            // of the variable beeing assigned (if there is any), and check that
            // this is correct for both sides. It is not correct for nested
            // lifetimes though, so I prefer to always reject they appear.
            typ = to_lifetimed_type(&e.typ, ctx.borrow_lifetime, e.loc)?;
            check_type(&typ, &b_true.expr.typ, e.loc)?;
            check_type(&typ, &b_false.expr.typ, e.loc)?;

            expr = bc_ast::Expr::If(Box::new(new_cond), Box::new(b_true),
                                    Box::new(b_false));
        }
        typ_ast::Expr::NestedBlock(ref block) =>
        {
            let block_lt = ctx.next_lifetime();
            let new_block = check_block(block, ctx, block_lt)?;

            typ = new_block.expr.typ.clone();
            expr = bc_ast::Expr::NestedBlock(Box::new(new_block));
        }
    }

    Ok(bc_ast::TExpr { data: expr, typ, mutable: e.mutable, lvalue: e.lvalue,
        always_return: e.always_return, loc: e.loc })
}
