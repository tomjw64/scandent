use pest::{error::Error as PestError, iterators::Pair, Parser};
use pest_derive::*;
use std::borrow::Cow;
use std::error;
use std::fmt;

#[derive(Parser)]
#[grammar = "scandent.pest"]
struct ScandentParser;

pub type ScandentResult<T> = std::result::Result<T, ScandentError>;

#[derive(Debug)]
pub enum ScandentError {
  ParseError(PestError<Rule>),
  UnexpectedRuleError(Rule),
}
impl error::Error for ScandentError {}
impl fmt::Display for ScandentError {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(
      f,
      "{}: {}",
      match &self {
        ScandentError::ParseError(_) => "ParseError",
        ScandentError::UnexpectedRuleError(_) => "UnexpectedRuleError",
      },
      match &self {
        ScandentError::ParseError(err) => err.to_string(),
        ScandentError::UnexpectedRuleError(rule) => format!("{:?}", rule),
      }
    )
  }
}

#[derive(Clone, Debug)]
pub struct Selector {
  pub steps: Vec<Step>,
}

#[derive(Copy, Clone, Debug)]
pub enum Axis {
  Ancestor,
  Parent,
  Descendant,
  Child,
  Next,
  Previous,
  FollowingSibling,
  PrecedingSibling,
  Current,
}
impl Axis {
  fn inverse(&self) -> Axis {
    match self {
      Axis::Ancestor => Axis::Descendant,
      Axis::Parent => Axis::Child,
      Axis::Descendant => Axis::Ancestor,
      Axis::Child => Axis::Parent,
      Axis::Next => Axis::Previous,
      Axis::Previous => Axis::Next,
      Axis::FollowingSibling => Axis::PrecedingSibling,
      Axis::PrecedingSibling => Axis::FollowingSibling,
      Axis::Current => Axis::Current,
    }
  }
}

#[derive(Clone, Debug)]
pub enum Namespace {
  Prefix(String),
  Uri(String),
}

#[derive(Clone, Debug)]
pub enum AttributeRequirementOperation {
  Exists,
  Equals(String),
  Contains(String),
}

#[derive(Copy, Clone, Debug)]
pub struct Formula {
  pub slope: i32,
  pub intercept: i32,
}

#[derive(Clone, Debug)]
pub enum ArgValue {
  StringValue(String),
  FormulaValue(Formula),
}

#[derive(Clone, Debug)]
pub struct CheckFunction {
  pub name: String,
  pub args: Vec<ArgValue>,
}

#[derive(Clone, Debug)]
pub struct NameRequirement {
  pub localname: Option<String>,
  pub namespace: Option<Namespace>,
}

#[derive(Clone, Debug)]
pub struct AttributeRequirement {
  pub name: NameRequirement,
  pub op: AttributeRequirementOperation,
}

#[derive(Clone, Debug)]
pub struct Step {
  pub axis: Axis,
  pub checks: Vec<CheckFunction>,
  pub paths: Vec<Selector>,
  pub attributes: Vec<AttributeRequirement>,
  pub name: NameRequirement,
}

impl Selector {
  fn from_selector_pair(pair: Pair<'_, Rule>) -> ScandentResult<Selector> {
    let parse_steps_result: ScandentResult<Vec<Step>> = pair
      .into_inner()
      .filter_map(|step| match step.as_rule() {
        Rule::step => Some(Step::from_step_pair(step)),
        Rule::EOI => None,
        unexpected => Some(Err(ScandentError::UnexpectedRuleError(unexpected))),
      })
      .collect();
    parse_steps_result.map(|steps| Selector { steps })
  }
  pub fn from_string<'a, T: Into<Cow<'a, str>>>(input: T) -> ScandentResult<Selector> {
    let input_converted = input.into();
    let parse_result = ScandentParser::parse(Rule::selector, &input_converted);
    match parse_result {
      Ok(mut pairs) => Ok(Selector::from_selector_pair(
        pairs.nth(0).expect("Only a single selector supported"),
      )?),
      Err(err) => Err(ScandentError::ParseError(err)),
    }
  }
  pub fn inverse(&self) -> Selector {
    let mut inverse_steps: Vec<Step> = vec![];
    let mut last_axis = Axis::Current;
    for step in self.steps.iter().rev() {
      let step_inverted = {
        let mut clone = step.clone();
        clone.axis = last_axis.inverse();
        last_axis = step.axis;
        clone
      };
      inverse_steps.push(step_inverted)
    }
    let last_step = Step {
      axis: last_axis.inverse(),
      checks: vec![],
      paths: vec![],
      attributes: vec![],
      name: NameRequirement { localname: None, namespace: None },
    };
    inverse_steps.push(last_step);
    Selector { steps: inverse_steps }
  }
}

fn axis_from_pair(pair: Pair<'_, Rule>) -> ScandentResult<Axis> {
  match pair.as_rule() {
    Rule::axis_ancestor => Ok(Axis::Ancestor),
    Rule::axis_parent => Ok(Axis::Parent),
    Rule::axis_following_sibling => Ok(Axis::FollowingSibling),
    Rule::axis_next => Ok(Axis::Next),
    Rule::axis_preceding_sibling => Ok(Axis::PrecedingSibling),
    Rule::axis_previous => Ok(Axis::Previous),
    Rule::axis_descendant => Ok(Axis::Descendant),
    Rule::axis_child => Ok(Axis::Child),
    unexpected => Err(ScandentError::UnexpectedRuleError(unexpected)),
  }
}

impl Step {
  fn from_step_pair(pair: Pair<'_, Rule>) -> ScandentResult<Step> {
    let mut parts = pair.into_inner();
    let axis_pair = parts.nth(0).expect("Grammar should enforce axis placement");
    let axis = axis_from_pair(axis_pair)?;
    let predicate_pair = parts
      .nth(0)
      .expect("Grammar should enforce predicate placement");
    let mut name = NameRequirement {
      localname: None,
      namespace: None,
    };
    let mut checks: Vec<CheckFunction> = vec![];
    let mut paths: Vec<Selector> = vec![];
    let mut attributes: Vec<AttributeRequirement> = vec![];
    for inner_pair in predicate_pair.into_inner() {
      match inner_pair.as_rule() {
        Rule::no_condition => {}
        Rule::name_condition => name = NameRequirement::from_name_pair(inner_pair)?,
        Rule::check_condition => checks.push(CheckFunction::from_check_pair(inner_pair)?),
        Rule::path_condition => paths.push(Selector::from_selector_pair(inner_pair)?),
        Rule::attr_condition => attributes.push(AttributeRequirement::from_attr_pair(inner_pair)?),
        unexpected => return Err(ScandentError::UnexpectedRuleError(unexpected)),
      };
    }
    Ok(Step {
      axis,
      checks,
      paths,
      attributes,
      name,
    })
  }
}

impl AttributeRequirement {
  fn from_attr_pair(pair: Pair<'_, Rule>) -> ScandentResult<AttributeRequirement> {
    let mut parts = pair.into_inner();
    let name =
      NameRequirement::from_name_pair(parts.nth(0).expect("Name must exist and be first"))?;
    let op = match parts.nth(0) {
      None => AttributeRequirementOperation::Exists,
      Some(inner_op) => match inner_op.as_rule() {
        Rule::op_equals => AttributeRequirementOperation::Equals(extract_string(
          parts.nth(0).expect("Grammar enforces value"),
        )?),
        Rule::op_contains => AttributeRequirementOperation::Contains(extract_string(
          parts.nth(0).expect("Grammar enforces value"),
        )?),
        unexpected => return Err(ScandentError::UnexpectedRuleError(unexpected)),
      },
    };
    Ok(AttributeRequirement { name, op })
  }
}

impl CheckFunction {
  fn from_check_pair(pair: Pair<'_, Rule>) -> ScandentResult<CheckFunction> {
    let mut parts = pair.into_inner();
    let name = extract_string_contents(
      parts
        .nth(0)
        .expect("Function name must always be first and present"),
    )?;
    let parse_args_result: ScandentResult<Vec<ArgValue>> = parts
      .map(|arg| match arg.as_rule() {
        Rule::arg => {
          let arg_inner = arg.into_inner().nth(0).expect("Arg cannot be empty");
          match arg_inner.as_rule() {
            Rule::formula => Ok(ArgValue::FormulaValue(Formula::from_formula_pair(
              arg_inner,
            )?)),
            Rule::ident | Rule::string => Ok(ArgValue::StringValue(extract_string(arg_inner)?)),
            unexpected => Err(ScandentError::UnexpectedRuleError(unexpected)),
          }
        }
        unexpected => Err(ScandentError::UnexpectedRuleError(unexpected)),
      })
      .collect();
    Ok(CheckFunction {
      name,
      args: parse_args_result?,
    })
  }
}

impl Formula {
  fn from_formula_pair(pair: Pair<'_, Rule>) -> ScandentResult<Formula> {
    let mut slope = 0;
    let mut sign = 1;
    let mut intercept_abs = 0;
    for inner_pair in pair.into_inner() {
      match inner_pair.as_rule() {
        Rule::formula_n_exp => {
          let mut n_exp_inner = inner_pair.into_inner();
          match n_exp_inner.nth(0) {
            None => slope = 1,
            Some(coefficient) => {
              slope = coefficient
                .as_str()
                .parse::<i32>()
                .expect("Grammar requires integers")
            }
          }
        }
        Rule::formula_minus => sign = -1,
        Rule::formula_plus => sign = 1,
        Rule::formula_constant => {
          intercept_abs = inner_pair
            .as_str()
            .parse::<i32>()
            .expect("Grammar requires integers")
        }
        unexpected => return Err(ScandentError::UnexpectedRuleError(unexpected)),
      }
    }
    Ok(Formula {
      slope,
      intercept: sign * intercept_abs,
    })
  }
}

fn extract_string(pair: Pair<'_, Rule>) -> ScandentResult<String> {
  match pair.as_rule() {
    Rule::string => Ok(
      pair
        .into_inner()
        .nth(0)
        .expect("String must have contents")
        .as_str()
        .to_owned(),
    ),
    Rule::ident => Ok(pair.as_str().to_owned()),
    unexpected => Err(ScandentError::UnexpectedRuleError(unexpected)),
  }
}

fn extract_string_contents(pair: Pair<'_, Rule>) -> ScandentResult<String> {
  let inner = pair
    .into_inner()
    .nth(0)
    .expect("Can't extract string from nothing");
  extract_string(inner)
}

impl NameRequirement {
  fn from_name_pair(pair: Pair<'_, Rule>) -> ScandentResult<NameRequirement> {
    let mut localname = None;
    let mut namespace = None;
    for inner_pair in pair.into_inner() {
      match inner_pair.as_rule() {
        Rule::localname => {
          let value = extract_string_contents(inner_pair)?;
          localname = Some(value);
        }
        Rule::namespace => {
          let namespace_inner = inner_pair
            .into_inner()
            .nth(0)
            .expect("Namespace cannot be empty");
          namespace = match namespace_inner.as_rule() {
            Rule::prefix => Some(Namespace::Prefix(extract_string_contents(namespace_inner)?)),
            Rule::uri => Some(Namespace::Uri(extract_string_contents(namespace_inner)?)),
            unexpected => return Err(ScandentError::UnexpectedRuleError(unexpected)),
          };
        }
        unexpected => return Err(ScandentError::UnexpectedRuleError(unexpected)),
      };
    }
    Ok(NameRequirement {
      localname,
      namespace,
    })
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  fn _tree<'a>(
    pairs: pest::iterators::Pairs<Rule>,
    depth: usize,
    lines: &'a mut Vec<String>,
  ) -> &'a mut Vec<String> {
    let indent = "  ".repeat(depth);
    for pair in pairs {
      lines.push(format!("{}Rule: {:?}", indent, pair.as_rule()));
      lines.push(format!("{}Text: {}", indent, pair.as_str()));
      _tree(pair.into_inner(), depth + 1, lines);
    }
    lines
  }

  #[test]
  fn test_no_condition_explicit() {
    assert!(Selector::from_string("//").is_err());
    assert!(Selector::from_string("//*").is_ok());
  }

  #[test]
  fn test_axes_ok() {
    assert!(Selector::from_string("/div").is_ok());
    assert!(Selector::from_string("//div").is_ok());
    assert!(Selector::from_string("/..div").is_ok());
    assert!(Selector::from_string("/...div").is_ok());
    assert!(Selector::from_string("/>div").is_ok());
    assert!(Selector::from_string("/>>div").is_ok());
    assert!(Selector::from_string("/<div").is_ok());
    assert!(Selector::from_string("/<<div").is_ok());
  }

  #[test]
  fn test_names() {
    assert!(Selector::from_string("/div-ident").is_ok());
    assert!(Selector::from_string("/ns|div").is_ok());
    assert!(Selector::from_string("/_|div").is_ok());
    assert!(Selector::from_string("/''|div").is_ok());
    assert!(Selector::from_string("/'namespace.uri'|div").is_ok());
    assert!(Selector::from_string("/[attr-ident=value]").is_ok());
    assert!(Selector::from_string("/[ns|attr=value]").is_ok());
    assert!(Selector::from_string("/[_|attr=value]").is_ok());
    assert!(Selector::from_string("/['namespace.uri'|attr=value]").is_ok());
  }

  #[test]
  fn test_attr_condition() {
    assert!(Selector::from_string("//div[attr=ident-chars]").is_ok());
    assert!(Selector::from_string("//div[attr=\"double-quoted\"]").is_ok());
    assert!(Selector::from_string("//div[attr='single-quoted']").is_ok());
    assert!(Selector::from_string("//div[attr='']").is_ok());
    assert!(Selector::from_string("//div[attr]").is_ok());
    assert!(Selector::from_string("//div[attr='\"']").is_ok());
    assert!(Selector::from_string("//div[attr=''']").is_err());
    assert!(Selector::from_string("//div[attr=\"\"]").is_ok());
    assert!(Selector::from_string("//div[attr=\"'\"]").is_ok());
    assert!(Selector::from_string("//div[attr=\"\"\"]").is_err());
    assert!(Selector::from_string("//div[attr=illegal&char]").is_err());
    assert!(Selector::from_string("//div[attr='quoted%illegal&char']").is_ok());
  }

  #[test]
  fn whitespace() {
    assert!(Selector::from_string("//div[attr=ident- chars]").is_err());
    assert!(Selector::from_string("//div[attr='ident- chars']").is_ok());
    assert!(Selector::from_string("// div [ attr = ident ] / child").is_ok());
    assert!(Selector::from_string("// div : pseudo ( arg )").is_ok());
    assert!(Selector::from_string("// ns | div").is_ok());
    assert!(Selector::from_string("//div:formula(2 n + 1)").is_err());
    assert!(Selector::from_string("//div:formula(2n + 1)").is_ok());
  }

  #[test]
  fn test_compound() {
    assert!(ScandentParser::parse(
      Rule::selector,
      "//div
      :pseudo(a)
      :pseudo2
      :pseudo3()
      [attr=value]
      {/path}
      :pseudo
      {/path[condition]/..path}
      [attr=value]"
    )
    .is_ok());
    assert!(Selector::from_string("//[attr=value]name-must-be-first").is_err());
    assert!(Selector::from_string("//div/child/..parent[condition]/>next/<previous").is_ok());
  }

  #[test]
  fn test_pseudo() {
    assert!(Selector::from_string("//div:pseudo-without-args").is_ok());
    assert!(Selector::from_string("//div:pseudo-without-args()").is_ok());
    assert!(Selector::from_string("//div:pseudo-with-arg(a)").is_ok());
    assert!(Selector::from_string("//div:pseudo-with-args(a, b, c)").is_ok());
    assert!(Selector::from_string("//div:pseudo-with-formula-args(2n-1, 11, n, n+22)").is_ok());
    assert!(Selector::from_string("//div:pseudo-with-string-args('hello', \"hello\")").is_ok());
  }
}
