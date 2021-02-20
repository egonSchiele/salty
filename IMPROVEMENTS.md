the way I handle optionals leads to a lot of code and can be simplified
chained optionals?

couple edge-case failing specs marked TODO

currently you can only pass a lambda as a func arg if you wrap it in parens, like this:

    React.useEffect((\_ -> window.localStorage.setItem('step', @@activeItem)))

cant do

    React.useEffect(\_ -> window.localStorage.setItem('step', @@activeItem))

because that screws up higher order function call parsing. So if you just add `lambda` to `validFuncArgTypes` all the tests that use HoF in any way will break.
