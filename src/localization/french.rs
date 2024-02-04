use super::Lang;
use crate::diagnostics::{DiagnosticKind, Note, Severity};

use colored::Colorize;

pub struct French;

impl Lang for French {
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
            K::ExpectedToken { found, expected }
                => format!("{} attendu, mais {} trouvé",
                    expected.to_string().bold(),
                    found.to_string().bold(),
                ),
            K::ExpectedExpression
                => "expression attendue".to_string(),
            K::ExpectedStatement
                => "instruction attendue".to_string(),
            K::ExpectedType
                => "type attendu".to_string(),
            K::SingletonTypeSyntax
                => "les tuples singletons sont interprétés comme le type intérieur, donc les parenthèses ne sont pas nécéssaires".to_string(),
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
                => "les tableaux singletons sont équivalents à leur valeur interne".to_string(),
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
            N::ExpectedToken(expected)
                => format!("{} attendu", expected.to_string().bold()),
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
                => format!("ceci est de type {}", ty.to_string().bold(),),
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
                    "'{}' est capturée '{}' ici",
                    var_name.bold(),
                    func_name.bold(),
                ),
            N::TupleValueCount(len)
                => format!("ce tuple contient {} valeurs", len.to_string().bold()),
        }
    }
}
