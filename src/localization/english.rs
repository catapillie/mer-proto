use super::Lang;
use crate::diagnostics::{DiagnosticKind, Note, Severity};

use colored::Colorize;

pub struct English;

impl Lang for English {
    fn severity_msg(&self, severity: &Severity) -> &str {
        match severity {
            Severity::Error => "error",
            Severity::Warning => "warning",
        }
    }

    #[rustfmt::skip]
    fn diagnostic_msg(&self, kind: &DiagnosticKind) -> String {
        use DiagnosticKind as K;
        match kind {
            K::IllegalCharacter(ill)
                => format!("encountered illegal character {}",
                    format!("{ill:?}").bold(),
                ),
            K::InvalidInteger(e)
                => format!("invalid integer literal ({e})"),
            K::ExpectedToken { found, expected }
                => format!("expected {}, but found {}",
                    expected.to_string().bold(),
                    found.to_string().bold(),
                ),
            K::ExpectedExpression
                => "expected an expression".to_string(),
            K::ExpectedStatement
                => "expected a statement".to_string(),
            K::ExpectedType
                => "expected a type expression".to_string(),
            K::SingletonTypeSyntax
                => "singleton tuple are read as their inner type, so parentheses are unnecessary".to_string(),
            K::GuardNotBoolean
                => "guard is not a boolean".to_string(),
            K::EmptyThenStatement
                => "empty then statement".to_string(),
            K::EmptyElseStatement
                => "empty else statement".to_string(),
            K::ThenWithoutIf
                => "then statement without if".to_string(),
            K::ElseWithoutIfThen
                => "else statement without if-then".to_string(),
            K::EmptyWhileDoStatement
                => "empty while-do statement".to_string(),
            K::EmptyDoWhileStatement
                => "empty do-while statement".to_string(),
            K::DoWithoutWhile
                => "do statement without while".to_string(),
            K::UnknownVariable(name)
                => format!("unknown variable '{}'", name.bold()),
            K::AssigneeMustBeVariable
                => "assignee must be a variable".to_string(),
            K::TooManyVariables(name, count)
                => format!("function '{}' uses {} variables, which is more than the allowed maximum (255)",
                    name.bold(),
                    count.to_string().bold(),
                ),
            K::TooManyTopLevelVariables(count)
                => format!("the top level program uses {} variables, which is more than the allowed maximum (255)",
                    count.to_string().bold(),
                ),
            K::UnusedVariable(name)
                => format!("variable '{}' is never used",
                    name.bold(),
                ),
            K::InvalidArgCount { got, expected } => {
                let left = match expected {
                    0 => format!("function takes in {} arguments", "no".bold()),
                    1 => format!("function takes in {} argument", "one".bold()),
                    _ => format!("function takes in {} arguments", expected.to_string().bold()),
                };
                let right = match got {
                    0 => format!("but receives {}", "none".bold()),
                    1 => format!("but receives {}", "one".bold()),
                    _ => format!("but receives {}", got.to_string().bold()),
                };
                format!("{left}, {right}")
            }
            K::InvalidCallee
                => "callee is not a function, and cannot be called".to_string(),
            K::UnknownType(id)
                => format!("unknown type {}", id.bold()),
            K::TypeMismatch { found, expected }
                => format!("type mismatch of {} into {}",
                    found.to_string().bold(),
                    expected.to_string().bold(),
                ),
            K::InvalidUnaryOperation { op, ty }
                => format!("invalid unary operation {} on type {}",
                    op.to_string().bold(),
                    ty.to_string().bold(),
                ),
            K::InvalidBinaryOperation { op, left, right }
                => format!("invalid binary operation {} between types {} and {}",
                    op.to_string().bold(),
                    left.to_string().bold(),
                    right.to_string().bold(),
                ),
            K::MustReturnValue { expected }
                => format!("a value of type {} must be returned", expected.to_string().bold()),
            K::NotAllPathsReturn
                => "not every path is guaranteed to return".to_string(),
            K::TopLevelMustReturn
                => "the top level program must return unit".to_string(),
            K::UnreachableCode
                => "this code is unreachable".to_string(),
            K::UnallowedVariableCapture { func_name, var_name }
                => format!("function '{}' captures variable '{}', which is not (yet) allowed",
                    func_name.bold(),
                    var_name.bold(),
                ),
            K::InvalidDebugExpression(ty)
                => format!("cannot debug value of type {}",
                    ty.to_string().bold(),
                ),
            K::InvalidDereference(ty)
                => format!("cannot dereference value of type {}",
                    ty.to_string().bold(),
                ),
            K::InvalidImmediateIndex
                => "only tuples and arrays of known size can be access with an immediate index".to_string(),
            K::InvalidIndex
                => "cannot index a value which is not an array".to_string(),
            K::InvalidTupleIndex { len, accessed } => {
                let left = match accessed {
                    0 => format!("cannot access {} value", "zeroth".bold()),
                    1 => format!("cannot access {} value", "first".bold()),
                    _ if accessed % 10 == 1 => format!("cannot access {}st value", accessed.to_string().bold()),
                    _ if accessed % 10 == 2 => format!("cannot access {}nd value", accessed.to_string().bold()),
                    _ => format!("cannot access {}th value", accessed.to_string().bold()),
                };
                let right = format!("of a tuple with {} values", len.to_string().bold());
                format!("{left} {right}")
            }
            K::EmptyArray
                => "empty arrays of known size are not allowed".to_string(),
            K::SingletonArray
                => "singleton arrays are equivalent to their inner value".to_string(),
            K::ArrayMismatchingTypes
                => "values in array must all be of the same type".to_string(),
            K::OutOfRangeArrayIndex { len, index } => {
                let left = match index {
                    0 => format!("cannot access {} value", "zeroth".bold()),
                    1 => format!("cannot access {} value", "first".bold()),
                    _ if index % 10 == 1 => format!("cannot access {}st value", index.to_string().bold()),
                    _ if index % 10 == 2 => format!("cannot access {}nd value", index.to_string().bold()),
                    _ => format!("cannot access {}th value", index.to_string().bold()),
                };
                let right = match len {
                    1 => "of a singleton array".to_string(),
                    _ => format!("of an array with size {}", len.to_string().bold()),
                };
                format!("{left} {right}")
            }
            K::ArrayIndexMustBeInteger
                => "array index must be an integer".to_string(),
            K::OutOfRangeConstantIndex { len, index } => {
                let left = match index {
                    0 => format!("cannot access {} value", "zeroth".bold()),
                    1 => format!("cannot access {} value", "first".bold()),
                    _ if index % 10 == 1 => format!("cannot access {}st value", index.to_string().bold()),
                    _ if index % 10 == 2 => format!("cannot access {}nd value", index.to_string().bold()),
                    _ => format!("cannot access {}th value", index.to_string().bold()),
                };
                let right = match len {
                    1 => "of a singleton array".to_string(),
                    _ => format!("of an array with size {}", len.to_string().bold()),
                };
                format!("{left} {right} (the index is known at compile-time)")
            }
            K::CanBeImmediateIndex
                => "index is known at compile-time and can rewritten as an immediate index".to_string(),
            K::MissingOtherwisePath
                => "case expression without otherwise path".to_string(),
            K::TooManyOtherwisePaths
                => "case expression has more than one otherwise path".to_string(),
            K::LastCasePathIsNotOtherwise
                => "last path of case-expression must be otherwise path".to_string(),
            K::CasePathsTypeMismatch
                => "all paths in case-expression must give the same type".to_string(),
            K::CaseOtherwiseCanBeSimplified
                => "case-otherwise expression has a unique path and can be simplified".to_string(),
        }
    }

    #[rustfmt::skip]
    fn note_msg(&self, note: &Note) -> String {
        use Note as N;
        match note {
            N::Quiet
                => String::new(),
            N::Numbered(num, note)
                => format!("{} {}", format!("[{num}]").bold(), self.note_msg(note)),
            N::Then(note)
                => format!("then {}", self.note_msg(note)),
            N::But(note)
                => format!("but {}", self.note_msg(note)),
            N::So(note)
                => format!("so {}", self.note_msg(note)),
            N::DDDotFront(note)
                => format!("...{}", self.note_msg(note)),
            N::DDDotBack(note)
                => format!("{}...", self.note_msg(note)),
            N::Here
                => "here".to_string(),
            N::Unknown
                => "???".to_string(),
            N::ExpectedToken(expected)
                => format!("expected {}", expected.to_string().bold()),
            N::CanBeRemoved
                => "this can be removed".to_string(),
            N::CanRemoveParentheses
                => "parentheses can be removed".to_string(),
            N::FollowsIf
                => "this should follow an if statement".to_string(),
            N::FollowsIfThen
                => "this should follow an if-then statement".to_string(),
            N::MissingWhile
                => "this must be followed by a while guard".to_string(),
            N::CannotAssign
                => "this cannot be assigned to".to_string(),
            N::MustBeOfType(ty)
                => format!("this must be of type {}", ty.to_string().bold()),
            N::OfType(ty)
                => format!("this is of type {}", ty.to_string().bold()),
            N::Type(ty)
                => ty.to_string().bold().to_string(),
            N::VariableDeclaration(name)
                => format!("variable '{}' is declared here", name.bold()),
            N::VariableType(name, ty)
                => format!(
                    "variable '{}' has type {}",
                    name.bold(),
                    ty.to_string().bold(),
                ),
            N::ArgumentType(name, ty)
                => format!(
                    "argument '{}' has type {}",
                    name.bold(),
                    ty.to_string().bold(),
                ),
            N::NotFunction(ty)
                => format!(
                    "this is not a function, and is of type {}",
                    ty.to_string().bold(),
                ),
            N::FunctionArgs(name, 0)
                => format!(
                    "function '{}' takes in {} arguments",
                    name.bold(),
                    "no".bold(),
                ),
            N::FunctionArgs(name, 1)
                => format!(
                    "function '{}' takes in {} argument",
                    name.bold(),
                    "one".bold(),
                ),
            N::FunctionArgs(name, count)
                => format!(
                    "function '{}' takes in {} arguments",
                    name.bold(),
                    count.to_string().bold(),
                ),
            N::FunctionReturnType(name, ty)
                => format!(
                    "function '{}' returns {}",
                    name.bold(),
                    ty.to_string().bold(),
                ),
            N::FunctionVariableCount(count)
                => format!("uses {} variables", count.to_string().bold()),
            N::ProvidedArgs(count)
                => match count {
                    0 => format!("{} were provided", "none".bold()),
                    1 => format!("{} was provided", "one".bold()),
                    _ => format!("{} were provided", count.to_string().bold()),
                },
            N::VariableCapturedBy(var_name, func_name)
                => format!(
                    "'{}' gets captured by '{}' here",
                    var_name.bold(),
                    func_name.bold(),
                ),
            N::TupleValueCount(len)
                => format!("this tuple contains {} values", len.to_string().bold()),
            N::KnownIndexTooLarge
                => "index known at compile time is too large".to_string(),
            N::CanBeImmediateIndex(index)
                => format!("can be rewritten as immediate index: {}",
                    format!(".{index}").bold(),
                ),
        }
    }
}
