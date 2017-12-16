use std::collections::HashMap;
use std::cell::RefCell;
use std::rc::Rc;
use std::fmt;
use std::marker::PhantomData;

// This file is highly inspired by rustc source code in libsyntax_pos/symbol.rs

//   ____                  _           _
//  / ___| _   _ _ __ ___ | |__   ___ | |
//  \___ \| | | | '_ ` _ \| '_ \ / _ \| |
//   ___) | |_| | | | | | | |_) | (_) | |
//  |____/ \__, |_| |_| |_|_.__/ \___/|_|
//         |___/
/**
 * A symbol is the representative of a string found in the source file.
 * It is used to store string litterals or identifiers in a manner that
 * allows fast copy or comparison.
 * To create a symbol or convert it back to string use the methods in SymTable.
 */
#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub struct Symbol
{
    id: usize,
    dummy: PhantomData<*const str> // Disable Sync and Send
}

impl Symbol
{
    /// Get the associated symbol for the given string
    pub fn from(s: String) -> Symbol
    {
        SYM_TABLE.with(|tbl| tbl.borrow_mut().get_symbol(s))
    }

    /// Converts the symbol to its associated string
    pub fn to_str(self) -> Rc<str>
    {
        SYM_TABLE.with(|tbl| tbl.borrow().get_str(self))
    }
}

impl fmt::Display for Symbol
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "{}", self.to_str())
    }
}

//   ____                _____     _     _
//  / ___| _   _ _ __ __|_   _|_ _| |__ | | ___
//  \___ \| | | | '_ ` _ \| |/ _` | '_ \| |/ _ \
//   ___) | |_| | | | | | | | (_| | |_) | |  __/
//  |____/ \__, |_| |_| |_|_|\__,_|_.__/|_|\___|
//         |___/
/**
 * A SymTable stores associations between Symbol's and String's.
 * It should be unique across the compilation of a given program.
 */
struct SymTable
{
    map: HashMap<Rc<str>, Symbol>,
    table: Vec<Rc<str>>
}

thread_local!(
    static SYM_TABLE: RefCell<SymTable> = RefCell::new(SymTable::empty())
);

impl SymTable
{
    /// Create an empty SymTable
    fn empty() -> SymTable
    {
        SymTable { map: HashMap::new(), table: Vec::new() }
    }

    /**
     * Get a symbol for the given string.
     * It is guaranteed that two different strings yields different symbols,
     * and two equal strings yields the same symbol.
     */
    fn get_symbol(&mut self, string: String) -> Symbol
    {
        let str_rc = Rc::from(string);
        if let Some(sym) = self.map.get(&str_rc)
        {
            return *sym;
        }

        let new_sym = Symbol { id: self.table.len(), dummy: PhantomData };
        self.map.insert(str_rc.clone(), new_sym);
        self.table.push(str_rc);
        new_sym
    }

    /**
     * Get the string associated to the given Symbol.
     * It is undefined behaviour to call get_str on a Symbol that was not
     * created by get_symbol on this SymTable.
     */
    fn get_str(&self, sym: Symbol) -> Rc<str>
    {
        self.table[sym.id].clone()
    }
}

