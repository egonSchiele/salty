In designing any language it's interesting to see what conflicts arise based on combinations you hadn't thought of. Here are some.

## Empty Hash

    foo := {}

Is this an empty function definition, or is foo returning an empty hash?
These are more clear:

    foo:= return {}
    foo:= { {} }

Similarly,

    if 1 == 1 then {}

is this an empty then case or is it saying then []?
