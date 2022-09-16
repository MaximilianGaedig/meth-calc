use anyhow::Result;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ShuntingYardError {
	#[error("ParseFloatError: {0}")]
	ParseNumberError(#[from] std::num::ParseFloatError),
	#[error("Unknown operator: {0}")]
	UnknownOperatorError(char),
	#[error("Mismatched Parenthesis")]
	MismatchedParenthesis,
	#[error("BUG: Could not pop operator")]
	PopOperatorError,
	#[error("BUG: Stack Underflow")]
	StackUnderflow,
}

#[derive(Debug, Clone, PartialEq)]
enum Associativity {
	Left,
	Right,
}

#[derive(Debug, Clone, PartialEq)]
struct Operator {
	precedence: u8,
	associativity: Associativity,
}

fn get_operator(c: char) -> Result<&'static Operator, ShuntingYardError> {
	match c {
		'(' => Ok(&Operator {
			precedence: 1,
			associativity: Associativity::Left,
		}),
		')' => Ok(&Operator {
			precedence: 1,
			associativity: Associativity::Left,
		}),
		'+' => Ok(&Operator {
			precedence: 2,
			associativity: Associativity::Left,
		}),
		'-' => Ok(&Operator {
			precedence: 2,
			associativity: Associativity::Left,
		}),
		'*' => Ok(&Operator {
			precedence: 3,
			associativity: Associativity::Left,
		}),
		'/' => Ok(&Operator {
			precedence: 3,
			associativity: Associativity::Left,
		}),
		'^' => Ok(&Operator {
			precedence: 4,
			associativity: Associativity::Right,
		}),
		_ => Err(ShuntingYardError::UnknownOperatorError(c)),
	}
}

fn convert_to_rpn(equation: String) -> Result<String, ShuntingYardError> {
	let mut tokens: Vec<char> = equation.replace(' ', "").chars().collect();
	tokens.reverse();
	#[derive(Debug)]
	enum Token {
		Operand(f64),
		Operator(char),
	}
	let mut output: Vec<Token> = vec![];
	let mut stack: Vec<char> = vec![];

	while !tokens.is_empty() {
		let mut token = tokens.pop().ok_or(ShuntingYardError::StackUnderflow)?;
		if token.is_numeric() || token == '.' || token == ',' {
			let mut number = String::new();
			while token.is_numeric() || token == '.' || token == ',' {
				number.push(token);
				if !tokens.is_empty() {
					token = tokens.pop().ok_or(ShuntingYardError::StackUnderflow)?;
				} else {
					break;
				}
			}
			output.push(Token::Operand(number.replace(',', ".").parse::<f64>()?));
		}
		if token == '(' {
			stack.push(token);
		} else if token == ')' {
			while stack.last().ok_or(ShuntingYardError::StackUnderflow)? != &'(' {
				if stack.is_empty() {
					return Err(ShuntingYardError::MismatchedParenthesis);
				}
				output.push(Token::Operator(stack.pop().ok_or(ShuntingYardError::PopOperatorError)?));
			}
			if *stack.last().ok_or(ShuntingYardError::StackUnderflow)? != '(' {
				return Err(ShuntingYardError::MismatchedParenthesis);
			}
			stack.pop();
		} else if !token.is_numeric() {
			let operator = get_operator(token)?;
			#[allow(clippy::blocks_in_if_conditions)]
			while (|| -> Result<bool, ShuntingYardError> {
				if stack.is_empty() {
					return Ok(false);
				}
				let stack_top = stack.last().ok_or(ShuntingYardError::StackUnderflow)?;
				let top_operator = get_operator(*stack_top)?;
				Ok(
					!stack.is_empty()
						&& stack[stack.len() - 1] != ')'
						&& (operator.precedence < top_operator.precedence
							|| operator.precedence == top_operator.precedence && top_operator.associativity == Associativity::Left),
				)
			})()? {
				output.push(Token::Operator(stack.pop().ok_or(ShuntingYardError::PopOperatorError)?));
			}
			stack.push(token);
		}
	}
	while !stack.is_empty() {
		if *stack.last().ok_or(ShuntingYardError::StackUnderflow)? == '(' {
			return Err(ShuntingYardError::MismatchedParenthesis);
		}
		output.push(Token::Operator(stack.pop().ok_or(ShuntingYardError::PopOperatorError)?));
	}
	Ok(
		output
			.iter()
			.map(|token| match token {
				Token::Operand(operand) => operand.to_string(),
				Token::Operator(operator) => operator.to_string(),
			})
			.collect::<Vec<String>>()
			.join(" "),
	)
}

pub fn solve(equation: String) -> Result<String, ShuntingYardError> {
	let rpn = convert_to_rpn(equation)?;
	let mut stack = vec![];
	for token in rpn.split(' ') {
		if token.parse::<f64>().is_ok() {
			stack.push(token.parse::<f64>()?);
		} else {
			let b = stack.pop().ok_or(ShuntingYardError::StackUnderflow)?;
			let a = stack.pop().ok_or(ShuntingYardError::StackUnderflow)?;
			if token.len() != 1 {
				return Err(ShuntingYardError::UnknownOperatorError('?'));
			}
			let result = match token
				.chars()
				.next()
				.ok_or(ShuntingYardError::UnknownOperatorError('?'))?
			{
				'+' => a + b,
				'-' => a - b,
				'*' => a * b,
				'/' => a / b,
				'^' => a.powf(b),
				_ => {
					return Err(ShuntingYardError::UnknownOperatorError(
						token.chars().next().ok_or(ShuntingYardError::StackUnderflow)?,
					))
				}
			};
			stack.push(result);
		}
	}
	Ok(stack.pop().ok_or(ShuntingYardError::StackUnderflow)?.to_string())
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_no_parenthesis() {
		assert_eq!(convert_to_rpn("1+2".to_string()).unwrap(), "1 2 +");
		assert_eq!(convert_to_rpn("1+2*3".to_string()).unwrap(), "1 2 3 * +");
		assert_eq!(convert_to_rpn("1+2*3^4".to_string()).unwrap(), "1 2 3 4 ^ * +");
		assert_eq!(convert_to_rpn("1+2*3^4-5".to_string()).unwrap(), "1 2 3 4 ^ * + 5 -");
		assert_eq!(
			convert_to_rpn("1+2*3^4-5/6".to_string()).unwrap(),
			"1 2 3 4 ^ * + 5 6 / -"
		);
		assert_eq!(
			convert_to_rpn("1+2*3^4-5/6+7".to_string()).unwrap(),
			"1 2 3 4 ^ * + 5 6 / - 7 +"
		);
		assert_eq!(
			convert_to_rpn("1+2*3^4-5/6+7*8".to_string()).unwrap(),
			"1 2 3 4 ^ * + 5 6 / - 7 8 * +"
		);
		assert_eq!(
			convert_to_rpn("1+2*3^4-5/6+7*8-9".to_string()).unwrap(),
			"1 2 3 4 ^ * + 5 6 / - 7 8 * + 9 -"
		);
		assert_eq!(
			convert_to_rpn("1+2*3^4-5/6+7*8-9/10".to_string()).unwrap(),
			"1 2 3 4 ^ * + 5 6 / - 7 8 * + 9 10 / -"
		);
		assert_eq!(
			convert_to_rpn("1+2*3^4-5/6+7*8-9/10+11".to_string()).unwrap(),
			"1 2 3 4 ^ * + 5 6 / - 7 8 * + 9 10 / - 11 +"
		);
	}
	#[test]
	fn test_parenthesis() {
		assert_eq!(convert_to_rpn("1+(2)".to_string()).unwrap(), "1 2 +");
		assert_eq!(convert_to_rpn("1+(2*3)".to_string()).unwrap(), "1 2 3 * +");
		assert_eq!(convert_to_rpn("1+(2*3)^4".to_string()).unwrap(), "1 2 3 * 4 ^ +");
		assert_eq!(convert_to_rpn("1+(2*3)^4-5".to_string()).unwrap(), "1 2 3 * 4 ^ + 5 -");
		assert_eq!(
			convert_to_rpn("1+(2*3)^4-5/6".to_string()).unwrap(),
			"1 2 3 * 4 ^ + 5 6 / -"
		);
		assert_eq!(
			convert_to_rpn("1+(2*3)^4-5/6+7".to_string()).unwrap(),
			"1 2 3 * 4 ^ + 5 6 / - 7 +"
		);
		assert_eq!(
			convert_to_rpn("1+(2*3)^4-5/6+7*8".to_string()).unwrap(),
			"1 2 3 * 4 ^ + 5 6 / - 7 8 * +"
		);
		assert_eq!(
			convert_to_rpn("1+(2*3)^4-5/6+7*8-9".to_string()).unwrap(),
			"1 2 3 * 4 ^ + 5 6 / - 7 8 * + 9 -"
		);
		assert_eq!(
			convert_to_rpn("1+(2*3)^4-5/6+7*8-9/10".to_string()).unwrap(),
			"1 2 3 * 4 ^ + 5 6 / - 7 8 * + 9 10 / -"
		);
		assert_eq!(
			convert_to_rpn("1+(2*3)^4-5/6+7*8-9/10+11".to_string()).unwrap(),
			"1 2 3 * 4 ^ + 5 6 / - 7 8 * + 9 10 / - 11 +"
		);
		assert_eq!(
			convert_to_rpn("3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3".to_string()).unwrap(),
			"3 4 2 * 1 5 - 2 3 ^ ^ / +"
		);
	}

	#[test]
	fn test_solve() {
		assert_eq!(solve("1+2".to_string()).unwrap(), "3");
		assert_eq!(solve("1+2*3".to_string()).unwrap(), "7");
		assert_eq!(solve("1+2*3^4".to_string()).unwrap(), "163");
		assert_eq!(solve("1+2*3^4-5".to_string()).unwrap(), "158");
		assert_eq!(solve("3-2+22/(33-33)".to_string()).unwrap(), "inf"); // division by zero
		assert_eq!(
			solve("219*(3-2+2/(33-34))/9".to_string()).unwrap(),
			"-24.333333333333332"
		);
		assert_eq!(
			solve("0.5*(3-2+2/(33-34))/9".to_string()).unwrap(),
			"-0.05555555555555555"
		);
		assert_eq!(
			solve("0.5*(3-2+8*2/(0.123452231-34))/9-0.112238912".to_string()).unwrap(),
			"-0.08292241996857071"
		);
		assert_eq!(
			solve("0.5*(3-2+8*2/(0.123452231-34))/9-.112238912".to_string()).unwrap(),
			"-0.08292241996857071"
		);
	}
}
