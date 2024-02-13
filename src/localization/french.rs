use super::Lang;
use crate::{
    com::tokens::{Token, TokenKind},
    diagnostics::{DiagnosticKind, Note, Severity},
};

use colored::Colorize;
use itertools::Itertools;

pub struct French;

impl Lang for French {
    fn token_kind_str(&self, kind: &TokenKind) -> &str {
        use TokenKind as K;
        match kind {
            K::Eof => "fin de fichier",
            K::Newline => "fin de ligne",
            K::LeftParen => "(",
            K::RightParen => ")",
            K::LeftBracket => "[",
            K::RightBracket => "]",
            K::LeftBrace => "{",
            K::RightBrace => "}",
            K::Dot => ".",
            K::Comma => ",",
            K::Colon => ":",
            K::Equal => "=",
            K::Plus => "+",
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
            K::At => "@",
            K::RightArrow => "->",
            K::IfKw => "mot-clé 'if'",
            K::ThenKw => "mot-clé 'then'",
            K::ElseKw => "mot-clé 'else'",
            K::WhileKw => "mot-clé 'while'",
            K::DoKw => "mot-clé 'do'",
            K::VarKw => "mot-clé 'var'",
            K::FuncKw => "mot-clé 'func'",
            K::ReturnKw => "mot-clé 'return'",
            K::AndKw => "mot-clé 'and'",
            K::OrKw => "mot-clé 'or'",
            K::XorKw => "mot-clé 'xor'",
            K::NotKw => "mot-clé 'not'",
            K::CaseKw => "mot-clé 'case'",
            K::DataKw => "mot-clé 'data'",
            K::OtherwiseKw => "mot-clé 'otherwise'",
            K::AllocKw => "mot-clé 'alloc'",
            K::TrueKw => "constante 'true'",
            K::FalseKw => "constante 'false'",
            K::TodoKw => "mot-clé 'todo'",
            K::UnreachableKw => "mot-clé 'unreachable'",
            K::Identifier => "identificateur",
            K::Integer => "entier",
            K::MalformedNumeral => "entier mal formé",
            K::DebugKw => "mot-clé temporaire 'debug'",
        }
    }

    fn token_str(&self, token: &Token) -> String {
        use Token as T;
        match token {
            T::Eof(_, _) => "fin de fichier".to_string(),
            T::Newline(_, _) => "fin de ligne".to_string(),
            T::LeftParen(_, _) => "(".to_string(),
            T::RightParen(_, _) => ")".to_string(),
            T::LeftBracket(_, _) => "[".to_string(),
            T::RightBracket(_, _) => "]".to_string(),
            T::LeftBrace(_, _) => "{".to_string(),
            T::RightBrace(_, _) => "}".to_string(),
            T::Dot(_, _) => ".".to_string(),
            T::Comma(_, _) => ",".to_string(),
            T::Colon(_, _) => ":".to_string(),
            T::Equal(_, _) => "=".to_string(),
            T::Plus(_, _) => "+".to_string(),
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
            T::At(_, _) => "@".to_string(),
            T::RightArrow(_, _) => "->".to_string(),
            T::IfKw(_, _) => "mot-clé 'if'".to_string(),
            T::ThenKw(_, _) => "mot-clé 'then'".to_string(),
            T::ElseKw(_, _) => "mot-clé 'else'".to_string(),
            T::WhileKw(_, _) => "mot-clé 'while'".to_string(),
            T::DoKw(_, _) => "mot-clé 'do'".to_string(),
            T::VarKw(_, _) => "mot-clé 'var'".to_string(),
            T::FuncKw(_, _) => "mot-clé 'func'".to_string(),
            T::ReturnKw(_, _) => "mot-clé 'return'".to_string(),
            T::AndKw(_, _) => "mot-clé 'and'".to_string(),
            T::OrKw(_, _) => "mot-clé 'or'".to_string(),
            T::XorKw(_, _) => "mot-clé 'xor'".to_string(),
            T::NotKw(_, _) => "mot-clé 'not'".to_string(),
            T::CaseKw(_, _) => "mot-clé 'case'".to_string(),
            T::DataKw(_, _) => "mot-clé 'data'".to_string(),
            T::OtherwiseKw(_, _) => "mot-clé 'otherwise'".to_string(),
            T::AllocKw(_, _) => "mot-clé 'alloc'".to_string(),
            T::TrueKw(_, _) => "'true' literal".to_string(),
            T::FalseKw(_, _) => "'false' literal".to_string(),
            T::TodoKw(_, _) => "mot-clé 'todo'".to_string(),
            T::UnreachableKw(_, _) => "mot-clé 'unreachable'".to_string(),
            T::Identifier(s, _) => format!("identificateur '{}'", s.0),
            T::Integer(s, _) => format!("{}", s.0),
            T::MalformedNumeral(_, _) => "entier mal formé".to_string(),
            T::DebugKw(_, _) => "mot-clé temporaire 'debug'".to_string(),
        }
    }

    fn severity_msg(&self, severity: &Severity) -> &str {
        match severity {
            Severity::Error => "erreur",
            Severity::Warning => "avertissement",
        }
    }

    #[rustfmt::skip]
    fn diagnostic_msg(&self, kind: &DiagnosticKind) -> String {
        use DiagnosticKind as K;
        match kind {
            K::IllegalCharacter(ill)
                => format!("caractère interdit {} rencontré",
                    format!("{ill:?}").bold(),
                ),
            K::InvalidInteger(e)
                => format!("nombre entier invalide ({e})"),
            K::ExpectedToken { found, expected } => {
                let exp = match is_token_kind_feminine(expected) {
                    true => "attendue",
                    false => "attendu",
                };
                let fnd = match is_token_feminine(found) {
                    true => "trouvée",
                    false => "trouvé",
                };
                format!("{} {exp}, mais {} {fnd}",
                    self.token_kind_str(expected).bold(),
                    self.token_str(found).bold(),
                )
            }
            K::ExpectedExpression
                => "expression attendue".to_string(),
            K::ExpectedStatement
                => "instruction attendue".to_string(),
            K::ExpectedType
                => "type attendu".to_string(),
            K::SingletonTypeSyntax
                => "les tuples singletons sont interprétés comme le type intérieur, donc les parenthèses ne sont pas nécéssaires".to_string(),
            K::ExpectedArraySizeOrAmpersand
                => "taille de tableau attendue (entier, ou esperluette pour une taille non-spécifiée)".to_string(),
            K::ExpectedAccess
                => "accès à un champ attendu (indice immédiat ou identificateur)".to_string(),
            K::GuardNotBoolean
                => "condition non-booléenne".to_string(),
            K::EmptyThenStatement
                => "instruction then vide".to_string(),
            K::EmptyElseStatement
                => "instruction else vide".to_string(),
            K::ThenWithoutIf
                => "instruction then sans instruction if".to_string(),
            K::ElseWithoutIfThen
                => "instruction then sans instruction if-then".to_string(),
            K::EmptyWhileDoStatement
                => "instruction while-do vide".to_string(),
            K::EmptyDoWhileStatement
                => "instruction do-while vide".to_string(),
            K::DoWithoutWhile
                => "instruction do sans while".to_string(),
            K::UnknownVariable(name)
                => format!("variable inconnue '{}'", name.bold()),
            K::AssigneeMustBeVariable
                => "le membre de gauche d'une affectation doit être une variable".to_string(),
            K::TooManyVariables(name, count)
                => format!("la fonction '{}' utilise {} variables, ce qui dépasse le maximum autorisé (255)",
                    name.bold(),
                    count.to_string().bold(),
                ),
            K::TooManyTopLevelVariables(count)
                => format!("le programme au niveau supérieur utilise {} variables, ce qui dépasse le maximum autorisé (255)",
                    count.to_string().bold(),
                ),
            K::UnusedVariable(name)
                => format!("la variable '{}' n'est jamais utilisée",
                    name.bold(),
                ),
            K::InvalidArgCount { got, expected } => {
                let left = match expected {
                    0 => format!("la fonction ne prend {} argument", "aucun".bold()),
                    1 => format!("la fonction prend {} argument", "un".bold()),
                    _ => format!("la fonction prend {} arguments", expected.to_string().bold()),
                };
                let right = match got {
                    0 => format!("mais n'en reçoit {}", "aucun".bold()),
                    1 => format!("mais en reçoit {}", "un".bold()),
                    _ => format!("mais en reçoit {}", got.to_string().bold()),
                };
                format!("{left}, {right}")
            }
            K::InvalidCallee
                => "l'appelé n'est pas une fonction, et ne peut pas être appelé".to_string(),
            K::UnknownType(id)
                => format!("type inconnu {}", id.bold()),
            K::TypeMismatch { found, expected }
                => format!("le type {} ne correspond pas au type {}",
                    found.to_string().bold(),
                    expected.to_string().bold(),
                ),
            K::InvalidUnaryOperation { op, ty }
                => format!("opération unaire {} invalide sur le type {}",
                    op.to_string().bold(),
                    ty.to_string().bold(),
                ),
            K::InvalidBinaryOperation { op, left, right }
                => format!("opération binaire {} invalide entre les types {} et {}",
                    op.to_string().bold(),
                    left.to_string().bold(),
                    right.to_string().bold(),
                ),
            K::MustReturnValue { expected }
                => format!("une valeur de type {} doit être renvoyée", expected.to_string().bold()),
            K::NotAllPathsReturn
                => "certains chemins ne renvoient pas de valeur".to_string(),
            K::TopLevelMustReturn
                => "le programme au niveau supérieur doit renvoyer".to_string(),
            K::UnreachableCode
                => "ce code est inatteignable".to_string(),
            K::UnallowedVariableCapture { func_name, var_name }
                => format!("la fonction '{}' capture la variable '{}', ce qui n'est pas (encore) autorisé",
                    func_name.bold(),
                    var_name.bold(),
                ),
            K::InvalidDebugExpression(ty)
                => format!("impossible de débugger une valeur de type {}",
                    ty.to_string().bold(),
                ),
            K::InvalidDereference(ty)
                => format!("impossible de déréférencer une valeur de type {}",
                    ty.to_string().bold(),
                ),
            K::InvalidImmediateIndex
                => "seuls les tuples et les tableaux à taille connue peuvent être indexés par un indice immédiat".to_string(),
            K::InvalidIndex
                => "impossible d'indexer indirectement une valeur qui n'est pas un tabeau".to_string(),
            K::InvalidTupleIndex { len, accessed } => {
                let left = match accessed {
                    0 => format!("impossible d'accéder à la {} valeur", "zéroième".bold()),
                    1 => format!("impossible d'accéder à la {} valeur", "première".bold()),
                    _ => format!("impossible d'accéder à la {}ème valeur", accessed.to_string().bold()),
                };
                let right = format!("d'un tuple à {} valeurs", len.to_string().bold());
                format!("{left} {right}")
            }
            K::EmptyArray
                => "les tableaux vides à taille connue sont interdits".to_string(),
            K::SingletonArray
                => "les tableaux singletons sont équivalents à leur valeur intérieure".to_string(),
            K::ArrayMismatchingTypes
                => "les valeurs dans un tableau doivent tous être du même type".to_string(),
            K::OutOfRangeArrayIndex { len, index } => {
                let left = match index {
                    0 => format!("impossible d'accéder à la {} valeur", "zéroième".bold()),
                    1 => format!("impossible d'accéder à la {} valeur", "première".bold()),
                    _ => format!("impossible d'accéder à la {}ème valeur", index.to_string().bold()),
                };
                let right = match len {
                    1 => "d'un tableau singleton".to_string(),
                    _ => format!("d'un tableau de taille {}", len.to_string().bold()),
                };
                format!("{left} {right}")
            }
            K::ArrayIndexMustBeInteger
                => "l'indice dans une indexation doit être un entier".to_string(),
            K::OutOfRangeConstantIndex { len, index } => {
                let left = match index {
                    0 => format!("impossible d'accéder à la {} valeur", "zéroième".bold()),
                    1 => format!("impossible d'accéder à la {} valeur", "première".bold()),
                    _ => format!("impossible d'accéder à la {}ème valeur", index.to_string().bold()),
                };
                let right = match len {
                    1 => "d'un tableau singleton".to_string(),
                    _ => format!("d'un tableau de taille {}", len.to_string().bold()),
                };
                format!("{left} {right} (l'indice est connu au temps de compilation)")
            }
            K::CanBeImmediateIndex
                => "l'indice est connu au temps de compilation et peut être remplacé par un indice immédiat".to_string(),
            K::MissingOtherwisePath
                => "l'expression case n'a aucun chemin otherwise".to_string(),
            K::TooManyOtherwisePaths
                => "l'expression case contient plus d'un chemin otherwise".to_string(),
            K::LastCasePathIsNotOtherwise
                => "le dernier chemin d'une expression case doit être un chemin otherwise".to_string(),
            K::CasePathsTypeMismatch
                => "tous les chemins dans une expression case doivent donner le même type".to_string(),
            K::CaseOtherwiseCanBeSimplified
                => "l'expression case-otherwise ne contient qu'un unique chemin, et peut être simplifiée".to_string(),
            K::CaseThenOtherwiseCanBeSimplified
                => "il existe une syntaxe plus simple pour les expressions case-then-otherwise".to_string(),
            K::InvalidDataStructureExpression
                => "expression de structure de données invalide".to_string(),
            K::UnknownDataStructure(name)
                => format!("structure de données '{}' inconnue",
                    name.to_string().bold(),
                ),
            K::UnknownFieldInDataStructure { field_name, data_name }
                => format!("le champ '{}' n'existe pas dans la structure de données '{}'",
                    field_name.bold(),
                    data_name.bold(),
                ),
            K::FieldSetMoreThanOnce(field_name)
                => format!("le champ '{}' est spécifié plus d'une fois",
                    field_name.bold(),
                ),
            K::FieldsNeverSet(name, fields, last_field) => {
                if fields.is_empty() {
                    format!("le champ '{}' de la structure de données '{}' n'est jamais spécifié",
                        last_field.bold(),
                        name.bold(),
                    )
                } else {
                    let first_fields = fields
                        .iter()
                        .map(|f| format!("'{}'", f.bold()))
                        .join(", ");
                    format!("les champs {first_fields} et '{}' de la structure de données '{}' ne sont jamais spécifiés",
                        last_field.bold(),
                        name.bold(),
                    )
                }
            }
            K::InvalidFieldAccess(name)
                => format!("impossible d'accéder au champ '{}' d'une valeur qui n'est pas une structure de données",
                    name.bold(),
                ),
            K::NonIntegerSize
                => "taille non-entière".to_string(),
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
                => format!("puis {}", self.note_msg(note)),
            N::But(note)
                => format!("mais {}", self.note_msg(note)),
            N::So(note)
                => format!("donc {}", self.note_msg(note)),
            N::DDDotFront(note)
                => format!("...{}", self.note_msg(note)),
            N::DDDotBack(note)
                => format!("{}...", self.note_msg(note)),
            N::Here
                => "ici".to_string(),
            N::Unknown
                => "???".to_string(),
            N::ExpectedToken(expected) => {
                let right = match is_token_kind_feminine(expected) {
                    true => "attendue",
                    false => "attendu",
                };
                format!("{} {right}", self.token_kind_str(expected).bold())
            }
            N::CanBeRemoved
                => "ceci peut être retiré".to_string(),
            N::CanRemoveParentheses
                => "les parenthèses peuvent être supprimées".to_string(),
            N::FollowsIf
                => "ceci doit suivre une instruction if".to_string(),
            N::FollowsIfThen
                => "ceci doit suivre une instruction if-then".to_string(),
            N::MissingWhile
                => "ceci doit être suivi d'une condition while".to_string(),
            N::CannotAssign
                => "cette expression ne peut pas recevoir d'affection".to_string(),
            N::MustBeOfType(ty)
                => format!("ceci doit être de type {}", ty.to_string().bold()),
            N::OfType(ty)
                => format!("ceci est de type {}", ty.to_string().bold()),
            N::Type(ty)
                => ty.to_string().bold().to_string(),
            N::VariableDeclaration(name)
                => format!("la variable '{}' est déclarée ici", name.bold()),
            N::VariableType(name, ty)
                => format!(
                    "la variable '{}' est de type {}",
                    name.bold(),
                    ty.to_string().bold(),
                ),
            N::ArgumentType(name, ty)
                => format!(
                    "l'argument '{}' est de type {}",
                    name.bold(),
                    ty.to_string().bold(),
                ),
            N::NotFunction(ty)
                => format!(
                    "ceci n'est pas une fonction, et est de type {}",
                    ty.to_string().bold(),
                ),
            N::FunctionArgs(name, 0)
                => format!(
                    "la fonction '{}' n'accepte {} argument",
                    name.bold(),
                    "aucun".bold(),
                ),
            N::FunctionArgs(name, 1)
                => format!(
                    "la fonction '{}' n'accepte qu'{} argument",
                    name.bold(),
                    "un".bold(),
                ),
            N::FunctionArgs(name, count)
                => format!(
                    "la fonction '{}' accepte {} arguments",
                    name.bold(),
                    count.to_string().bold(),
                ),
            N::FunctionReturnType(name, ty)
                => format!(
                    "la fonction '{}' renvoie {}",
                    name.bold(),
                    ty.to_string().bold(),
                ),
            N::FunctionVariableCount(count)
                => format!("{} variables utilisées", count.to_string().bold()),
            N::ProvidedArgs(count)
                => match count {
                    0 => format!("{} n'est fourni", "aucun".bold()),
                    1 => format!("{} est fourni", "un".bold()),
                    _ => format!("{} sont fournis", count.to_string().bold()),
                },
            N::VariableCapturedBy(var_name, func_name)
                => format!(
                    "'{}' est capturée dans '{}' ici",
                    var_name.bold(),
                    func_name.bold(),
                ),
            N::TupleValueCount(len)
                => format!("ce tuple contient {} valeurs", len.to_string().bold()),
            N::KnownIndexTooLarge
                => "cet indice, connu à la compilation, est trop grand".to_string(),
            N::CanBeImmediateIndex(index)
                => format!("peut être réécrit en indice immédiat: {}",
                    format!(".{index}").bold(),
                ),
            N::FieldSet(field_name)
                => format!("le champ '{}' est d'abord spécifié ici",
                    field_name.bold(),
                ),
            N::FieldSetAgain(field_name)
                => format!("'{}' est spécifié à nouveau ici",
                    field_name.bold(),
                ),
            N::FieldType(name, ty)
                => format!("le champ '{}' est de type {}",
                    name.bold(),
                    ty.to_string().bold(),
                ),
            N::MissingFields(fields, last_field) => {
                if fields.is_empty() {
                    format!("champ manquant '{}'", last_field.bold())
                } else {
                    let first_fields = fields
                        .iter()
                        .map(|f| format!("'{}'", f.bold()))
                        .join(", ");
                    format!("champs manquants {first_fields} et '{}'", last_field.bold())
                }
            }
            N::NotDataStructure(ty)
                => format!("ceci n'est pas une structure de données, et est de type '{}'",
                    ty.to_string().bold(),
                ),
        }
    }
}

fn is_token_kind_feminine(kind: &TokenKind) -> bool {
    matches!(kind, TokenKind::Eof | TokenKind::Newline)
}

fn is_token_feminine(kind: &Token) -> bool {
    matches!(kind, Token::Eof(_, _) | Token::Newline(_, _))
}
