The goal isn't to have every PHP feature in here, but there are definitely some notable things missing right now that I'd like to add sometime.

- try/catch statements
- pass by reference (`foo = &bar`)
- arrays where the keys are *not* strings. Right now I just support js-style { foo:1, bar:2 }. ES6 lets you put stuff in brackets (https://stackoverflow.com/questions/11508463/javascript-set-object-key-by-variable) so {[foo]:1}
- function arg defaults (foo a=1 := a + 1)

    key and value in for loops:
    foreach ($shops as $k => $v) {
        $v;
    }

Stuff I don't care about:
- switch statements (though guards for functions like haskell would be cool)
