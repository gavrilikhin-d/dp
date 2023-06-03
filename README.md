# dp
**D**ynamic **P**arser with extensible PEG grammar. Reads as *deep*

# Todo
* [ ] Remove parse tree
* [ ] Support `["a", "b", "c"]` syntax
* [ ] Add a way to obtain syntax information from variable name
* [ ] Store source code in context
* [ ] Remove `on_parse` function from rule
* [ ] Move logic to syntax
* [ ] Export/Import rules
* [ ] Change the way rules are added to parsing to be more explicit
* [ ] Add syntax highlighting
* [ ] Support left-recursion
* [ ] Packrat parsing
* [ ] Add documentation
* [ ] Copy rules on reference in code
* [ ] Add `&Rule` as a way to reference a rule without copying it.
* [ ] Simplify separated patterns
* [ ] Add `#[derive(Rule)]`
      * [ ] Add `#[rule]` attribute
	  * [ ] Add `Self::rule()` method
	  * [ ] Add `Self::parse(str: &str)` method
      * [ ] Implement deserialize