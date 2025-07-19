

## About

This program construct a proof for an expression in classical propositional calculus (using the natural deduction variant), if such a proof exists.

## Input Format

The input is a single line—a propositional statement written according to the following grammar:

```
<expression> ::= <variable>
               | '_|_'                  (False constant)
               | '(' <expression> ')'
               | '!' <expression>       (Negation)
               | <expression> '&' <expression> (Conjunction)
               | <expression> '|' <expression> (Disjunction)
               | <expression> '->' <expression> (Implication)
```

    Operators ‘&’ (and) and ‘|’ (or) are left-associative.

    Operator ‘->’ (implies) is right-associative.

    Operators in decreasing order of precedence: ‘&’, ‘|’, ‘->’.

    Variable names do not contain spaces.

    There are no spaces within the symbols of an operator/constant (‘->’ and ‘|’).

    Spaces (and tab characters, which should be treated as spaces) may appear elsewhere.

    The total length of the expression does not exceed 255 characters.

    No more than seven distinct variables are used in the expression.

## Output Format

    If the statement is refutable (not valid): Indicate this fact on a single line by providing a refuting valuation (assigning truth values to variables that make the statement false). Format: [variable_name]=[0|1]; ... (e.g., A=0; B=1; C=0).

    Else (the statement is valid/provable): Output the proof.

        Each line of the proof represents a node in the proof tree. There should be no blank lines (except the last line of the output).

        Child nodes must appear before their parent node.

        Format for each line: [Level] Formula [Rule]

            Level: The node's depth level in the tree, enclosed in square brackets [].

            Formula: The formula at this node.

            Rule: The name of the inference rule applied, enclosed in square brackets [] after a space.

        Use the symbol _|_ (underscore, vertical bar, underscore: ASCII 95, 124, 95) to denote False.

        The statement given in the input file must be the conclusion (root) of the topmost rule.

        In both the input and proof, negation of a term (¬ϕ) must be represented as (ϕ → |).

        You may use the following rules (listed elsewhere, not provided in this text). Premises of rules must appear in the specified order; however, hypotheses within the context can be arbitrarily reordered.

![output semantics](https://github.com/olegggwp/trees_forests/blob/main/image.png?raw=true)