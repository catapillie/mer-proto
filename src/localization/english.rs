use super::Lang;
use crate::{
    com::tokens::{Token, TokenKind},
    diagnostics::{DiagnosticKind, Note, Severity},
};

use colored::Colorize;
use itertools::Itertools;

pub struct English;

impl Lang for English {
    fn token_kind_str(&self, kind: &TokenKind) -> &str {
        use TokenKind as K;
        match kind {
            K::Eof => "end-of-file",
            K::Newline => "newline",
            K::LeftParen => "(",
            K::RightParen => ")",
            K::LeftBracket => "[",
            K::RightBracket => "]",
            K::LeftBrace => "{",
            K::RightBrace => "}",
            K::Dot => ".",
            K::Comma => ",",
            K::Colon => ":",
            K::Underscore => "_",
            K::Equal => "=",
            K::Plus => "+",
            K::PlusPlus => "++",
            K::Minus => "-",
            K::Star => "*",
            K::Slash => "/",
            K::Percent => "%",
            K::Ampersand => "&",
            K::Bar => "|",
            K::Caret => "^",
            K::EqualEqual => "==",
            K::NotEqual => "!=",
            K::LessEqual => "<=",
            K::LessThan => "<",
            K::GreaterEqual => ">=",
            K::GreaterThan => ">",
            K::RightArrow => "->",
            K::IfKw => "'if' keyword",
            K::ThenKw => "'then' keyword",
            K::ElseKw => "'else' keyword",
            K::WhileKw => "'while' keyword",
            K::DoKw => "'do' keyword",
            K::VarKw => "'var' keyword",
            K::FuncKw => "'func' keyword",
            K::ReturnKw => "'return' keyword",
            K::AndKw => "'and' keyword",
            K::OrKw => "'or' keyword",
            K::XorKw => "'xor' keyword",
            K::NotKw => "'not' keyword",
            K::CaseKw => "'case' keyword",
            K::OtherwiseKw => "'otherwise' keyword",
            K::TypeKw => "'type' keyword",
            K::OpaqueKw => "'opaque' keyword",
            K::WithKw => "'with' keyword",
            K::AllocKw => "'alloc' keyword",
            K::TrueKw => "'true' literal",
            K::FalseKw => "'false' literal",
            K::TodoKw => "'todo' keyword",
            K::UnreachableKw => "'unreachable' keyword",
            K::Identifier => "identifier",
            K::Integer => "integer",
            K::MalformedNumeral => "malformed number",
            K::StringLit => "string literal",
            K::DebugKw => "temporary 'debug' keyword",
            K::PrintKw => "temporary 'print' keyword",
        }
    }

    fn token_str(&self, token: &Token) -> String {
        use Token as T;
        match token {
            T::Eof(_, _) => "end-of-file".to_string(),
            T::Newline(_, _) => "newline".to_string(),
            T::LeftParen(_, _) => "(".to_string(),
            T::RightParen(_, _) => ")".to_string(),
            T::LeftBracket(_, _) => "[".to_string(),
            T::RightBracket(_, _) => "]".to_string(),
            T::LeftBrace(_, _) => "{".to_string(),
            T::RightBrace(_, _) => "}".to_string(),
            T::Dot(_, _) => ".".to_string(),
            T::Comma(_, _) => ",".to_string(),
            T::Colon(_, _) => ":".to_string(),
            T::Underscore(_, _) => "_".to_string(),
            T::Equal(_, _) => "=".to_string(),
            T::Plus(_, _) => "+".to_string(),
            T::PlusPlus(_, _) => "++".to_string(),
            T::Minus(_, _) => "-".to_string(),
            T::Star(_, _) => "*".to_string(),
            T::Slash(_, _) => "/".to_string(),
            T::Percent(_, _) => "%".to_string(),
            T::Ampersand(_, _) => "&".to_string(),
            T::Bar(_, _) => "|".to_string(),
            T::Caret(_, _) => "^".to_string(),
            T::EqualEqual(_, _) => "==".to_string(),
            T::NotEqual(_, _) => "!=".to_string(),
            T::LessEqual(_, _) => "<=".to_string(),
            T::LessThan(_, _) => "<".to_string(),
            T::GreaterEqual(_, _) => ">=".to_string(),
            T::GreaterThan(_, _) => "<".to_string(),
            T::RightArrow(_, _) => "->".to_string(),
            T::IfKw(_, _) => "'if' keyword".to_string(),
            T::ThenKw(_, _) => "'then' keyword".to_string(),
            T::ElseKw(_, _) => "'else' keyword".to_string(),
            T::WhileKw(_, _) => "'while' keyword".to_string(),
            T::DoKw(_, _) => "'do' keyword".to_string(),
            T::VarKw(_, _) => "'var' keyword".to_string(),
            T::FuncKw(_, _) => "'func' keyword".to_string(),
            T::ReturnKw(_, _) => "'return' keyword".to_string(),
            T::AndKw(_, _) => "'and' keyword".to_string(),
            T::OrKw(_, _) => "'or' keyword".to_string(),
            T::XorKw(_, _) => "'xor' keyword".to_string(),
            T::NotKw(_, _) => "'not' keyword".to_string(),
            T::CaseKw(_, _) => "'case' keyword".to_string(),
            T::OtherwiseKw(_, _) => "'otherwise' keyword".to_string(),
            T::TypeKw(_, _) => "'type' keyword".to_string(),
            T::OpaqueKw(_, _) => "'opaque' keyword".to_string(),
            T::WithKw(_, _) => "'with' keyword".to_string(),
            T::AllocKw(_, _) => "'alloc' keyword".to_string(),
            T::TrueKw(_, _) => "'true' literal".to_string(),
            T::FalseKw(_, _) => "'false' literal".to_string(),
            T::TodoKw(_, _) => "'todo' keyword".to_string(),
            T::UnreachableKw(_, _) => "'unreachable' keyword".to_string(),
            T::Identifier(s, _) => format!("identifier '{}'", s.0),
            T::Integer(s, _) => format!("{}", s.0),
            T::MalformedNumeral(_, _) => "malformed nulmber".to_string(),
            T::StringLit(_, _) => "string literal".to_string(),
            T::DebugKw(_, _) => "temporary 'debug' keyword".to_string(),
            T::PrintKw(_, _) => "temporary 'print' keyword".to_string(),
        }
    }

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
            K::MissingQuote
                => "string literal is missing a closing delimiter (quotation mark)".to_string(),
            K::InvalidEscapeSequence
                => "invalid character escape sequence".to_string(),
            K::ExpectedToken { found, expected }
                => format!("expected {}, but found {}",
                    self.token_kind_str(expected).bold(),
                    self.token_str(found).bold(),
                ),
            K::ExpectedExpression
                => "expected an expression".to_string(),
            K::ExpectedStatement
                => "expected a statement".to_string(),
            K::ExpectedType
                => "expected a type expression".to_string(),
            K::ExpectedPattern
                => "expected a pattern".to_string(),
            K::SingletonTypeSyntax
                => "singleton tuple are read as their inner type, so parentheses are unnecessary".to_string(),
            K::ExpectedArraySizeOrAmpersand
                => "expected an array size (integer, or ampersand for unspecified size)".to_string(),
            K::ExpectedAccess
                => "expected a field access (immediate index or identifier)".to_string(),
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
            K::NotVariable(name)
                => format!("the name '{}' does not refer to a variable, a function or an opaque type", name.bold()),
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
            K::FunctionRedefinition(name)
                => format!("function '{}' is redefined in the same scope",
                    name.bold(),
                ),
            K::UnknownType(id)
                => format!("unknown type '{}'", id.bold()),
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
            K::InvalidArrayConcatenation { inner_left, inner_right }
                => format!("cannot concatenate two arrays whose inner types are {} and {} respectively",
                    inner_left.to_string().bold(),
                    inner_right.to_string().bold(),
                ),
            K::CannotReturnUnit { expected }
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
            K::CaseThenOtherwiseCanBeSimplified
                => "there exists a simpler syntax for case-then-otherwise expressions".to_string(),
            K::DataStructureRedefinition(name)
                => format!("data structure '{}' is redefined in the same scope",
                    name.bold(),
                ),
            K::CannotMarkAsOpaque
                => "data structures cannot be marked as opaque".to_string(),
            K::NonOpaqueTypeConstructor(name)
                => format!("type '{}' is not opaque, so it does not have a constructor",
                    name.bold(),
                ),
            K::InfiniteDataStructure(name)
                => format!("data structure '{}' is of infinite size (it contains itself without indirection or one of its fields is of infinite size)",
                    name.bold(),
                ),
            K::FieldDeclaredMoreThanOnce(field_name)
                => format!("field '{}' is declared more than once",
                    field_name.bold(),
                ),
            K::InvalidDataStructureExpression
                => "invalid data structure expression".to_string(),
            K::UnknownDataStructure(name)
                => format!("unknown data structure '{}'",
                    name.bold(),
                ),
            K::UnknownFieldInDataStructure { field_name, data_name }
                => format!("field '{}' does not exist in data structure '{}'",
                    field_name.bold(),
                    data_name.bold(),
                ),
            K::FieldSetMoreThanOnce(field_name)
                => format!("field '{}' is set more than once",
                    field_name.bold(),
                ),
            K::FieldsNeverSet(name, fields, last_field) => {
                if fields.is_empty() {
                    format!("field '{}' in data structure '{}' is never set",
                        last_field.bold(),
                        name.bold(),
                    )
                } else {
                    let first_fields = fields
                        .iter()
                        .map(|f| format!("'{}'", f.bold()))
                        .join(", ");
                    format!("fields {first_fields} and '{}' in data structure '{}' are never set",
                        last_field.bold(),
                        name.bold(),
                    )
                }
            }
            K::InvalidFieldAccess(name)
                => format!("cannot access field '{}' of a value which is not a data structure",
                    name.bold(),
                ),
            K::DiscardingWithExpression(ty)
                => format!("with-expression replaces all fields in data structure '{}'",
                    ty.to_string().bold(),
                ),
            K::EmptyWithExpression(ty)
                => format!("with-expression replaces no fields in data structure '{}'",
                    ty.to_string().bold(),
                ),
            K::AliasRedefinition(name)
                => format!("type alias '{}' is redefined in the same scope",
                    name.bold(),
                ),
            K::NonIntegerSize
                => "non-integer size".to_string(),
            K::InvalidPrint(ty)
                => format!("cannot print a value of type {}",
                    ty.to_string().bold(),
                ),
            K::PatternMismatch(pat, ty)
                => format!("pattern {} does not match type {}",
                    pat.to_string().bold(),
                    ty.to_string().bold(),
                ),
            K::TuplePatternMismatch(pat, len)
                => format!("pattern {} does not match a tuple of length {}",
                    pat.to_string().bold(),
                    len.to_string().bold(),
                ),
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
            N::And(note)
                => format!("and {}", self.note_msg(note)),
            N::DDDotFront(note)
                => format!("...{}", self.note_msg(note)),
            N::DDDotBack(note)
                => format!("{}...", self.note_msg(note)),
            N::Here
                => "here".to_string(),
            N::Unknown
                => "???".to_string(),
            N::ExpectedToken(expected)
                => format!("expected {}", self.token_kind_str(expected).bold()),
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
            N::OfTypeButShouldBe(got, expected)
                => format!("this is of type {}, should be {} instead",
                    got.to_string().bold(),
                    expected.to_string().bold(),
                ),
            N::Type(ty)
                => ty.to_string().bold().to_string(),
            N::ReturnsUnit
                => format!("this returns {}", "()".bold()),
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
            N::ArrayLength(size)
                => format!("this is an array of length {}", size.to_string().bold()),
            N::FieldDeclared(field_name)
                => format!("field '{}' is first declared here",
                    field_name.bold(),
                ),
            N::FieldDeclaredAgain(field_name)
                => format!("'{}' is declared again here",
                    field_name.bold(),
                ),
            N::FieldSet(field_name)
                => format!("field '{}' is first set here",
                    field_name.bold(),
                ),
            N::FieldSetAgain(field_name)
                => format!("'{}' is set again here",
                    field_name.bold(),
                ),
            N::FieldType(name, ty)
                => format!("field '{}' is of type {}",
                    name.bold(),
                    ty.to_string().bold(),
                ),
            N::MissingFields(fields, last_field) => {
                if fields.is_empty() {
                    format!("missing field '{}'", last_field.bold())
                } else {
                    let first_fields = fields
                        .iter()
                        .map(|f| format!("'{}'", f.bold()))
                        .join(", ");
                    format!("missing fields {first_fields} and '{}'", last_field.bold())
                }
            }
            N::DataInfiniteSize(name)
                => format!("type '{}' is of infinite size (due to a cycle among one of its fields)",
                    name.bold(),
                ),
            N::NotDataStructure(ty)
                => format!("this is not a data structure, and is of type {}",
                    ty.to_string().bold(),
                ),
            N::DiscardedDataStructure
                => "this data structure is copied then entirely replaced".to_string(),
            N::UnmodifiedDataStructure
                => "this data structure is copied then remains unmodified".to_string(),
            N::InnerTypesMismatch { inner_left, inner_right }
                => format!("array inner types, {} and {}, do not match",
                    inner_left.to_string().bold(),
                    inner_right.to_string().bold(),
                ),
            N::MoreThanOneOtherwisePath
                => "has more than one otherwise path".to_string(),
            N::ShadowedFunction(name)
                => format!("function '{}' is first defined here",
                    name.bold()
                ),
            N::RedefinedFunction
                => "this is the redefinition".to_string(),
            N::ShadowedDataStructure(name)
                => format!("data structure '{}' is first defined here",
                    name.bold()
                ),
            N::RedefinedDataStructure
                => "this is the redefinition".to_string(),
            N::DataStructureMarkedOpaque(name)
                => format!("cannot mark data structure '{}' as opaque",
                    name.bold(),
                ),
            N::ShadowedAlias(name)
                => format!("type alias '{}' is first defined here",
                    name.bold()
                ),
            N::RedefinedAlias
                => "this is the redefinition".to_string(),
            N::MarkedAsOpaque(name)
                => format!("type '{}' is not opaque",
                    name.bold()
                ),
            N::DoesNotHaveConstructor(name)
                => format!("type constructor '{}' does not exist",
                    name.bold()
                ),
            N::PatternMustDescribe(ty)
                => format!("this pattern must describe type {}",
                    ty.to_string().bold(),
                ),
        }
    }
}
