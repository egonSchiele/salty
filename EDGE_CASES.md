In designing any language it's interesting to see what conflicts arise based on combinations you hadn't thought of. Here are some.

    foo := {}

Is this an empty function definition, or is foo returning an empty hash?
It's ambiguous, here's how I decided to define it.

    "foo := {}" `matches` "function foo() {\n}",
    "foo := return {}" `matches` "function foo() {\n    return [];\n}",
    "foo := { {} }" `matches` "function foo() {\n    return [];\n}"

Similarly,

    if 1 == 1 then {}

is this an empty then case or is it saying then []?
