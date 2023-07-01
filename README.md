# dp
**D**ynamic **P**arser with extensible PEG grammar. Reads as *deep*

# Transparency Rules
* Single unnamed pattern is passed as is:
  `X: x` ~ `X: <value: x> => value`
* Sequence without named arguments results in `{}`:
  `X: x y` ~ `X: x y => {}`
* Sequence with named arguments is wrapped with rule's name
  `X: <value: x> y` ~ `X: <value: x> y => X { value }`

# Rules overriding
You may override rules:
```
X: a
X: b

Y: X // Same as Y: b
```

## Statement rule
Parsing in Repl starts from `Statement` rule.
If you want to extend parser's syntax, you must override it
```
>>> EmptyTuple: '(' ')' => "EmptyTuple"
>>> Statement: Rule | EmptyTuple
// >>> Statement: Statement.clone() | EmptyTuple // TODO: copy rule
>>> ()
"EmptyTuple"
```

## Root rule
Parsing in files starts from `Root` rule.
If you want to extend parser's syntax, you must override it

## Overriding whitespace
Override `Whitespace` rule to change skipped characters
```
Whitespace: /[ ]*/ // Skip only spaces without tabs and newlines
```
There is `context.skip_whitespace` special variable that must be set to `true` for automatic whitespace skipping

# Parsing whitespace characters
Parser first tries to parse without ignoring whitespaces.
This allows you to parse whitespaces, where needed.
```
BinaryOrPrefix
	: '+' /[ ]+/ 'x' => "binary"
	| '+' 'x' => "prefix"
```

# Errors
Create error rules for better errors description. By convention, they should start with `Invalid`.
`@` means at current location,
`@lparen` means at location of `lparen`.
Note that invalid syntax is still parsed and is accepted as valid alternative.
```
Tuple: '(' ')' | InvalidTuple
InvalidTuple: <lparen: '('> =>
	throw ExpectedMatching {
		expected: ')',
		at: @,
		to_match: '(',
		unmatched_at: @lparen,
	}
```

# Debugging
To debug library itself call with env `RUST_LOG=debug` or `RUST_LOG=trace`

# Todo
* [ ] Remove `on_parse` function from rule
* [ ] Move logic to syntax
* [ ] Export/Import rules
* [ ] Rule copy
* [x] Use special rules for whitespace skip
* [ ] Add syntax highlighting
	  * [x] Text as keywords
	  * [x] Regex
	  * [x] Strings
	  * [x] Numbers
	  * [x] Special symbols as operators or punctuators
	  * [x] Variables as identifiers (parameter)
	  * [x] RuleName as types
	  * [ ] Make syntax highlighting smarter
	  * [ ] More token subkinds like `keyword.text`, `operator.lparent`, etc.
	  * [ ] Syntax highlight shouldn't be affected by errors
* [ ] Add documentation
* [ ] Simplify separated patterns
* [ ] Add `#[derive(Rule)]`
      * [ ] Add `#[rule]` attribute
	  * [ ] Add `Self::rule()` method
	  * [ ] Add `Self::parse(str: &str)` method
      * [ ] Implement deserialize
	  * [ ] On enums every variant is alternative
* [ ] Simplify rules
* [ ] Remove ';' at end of rules
* [ ] Use `xxx#tag` to add tags like token kinds to values
* [ ] Fix left recursion