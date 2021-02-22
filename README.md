# Salty

[![Haskell CI](https://github.com/egonSchiele/salty/actions/workflows/haskell.yml/badge.svg?branch=main)](https://github.com/egonSchiele/salty/actions/workflows/haskell.yml)

Salty is a language that compiles to PHP, JavaScript, and JSX.

Salty compiles to readable code, so *there's no long term dependency on it*.

Stop using Salty whenever you want, and you'll be left with the PHP or JavaScript code you'd have probably written instead anyway.

## What can Salty do for you?

Salty is meant to be a pleasure to write and read, and that can be subjective, so see if Salty is right for you!

Here's a PHP function:

```php
/**
 * @param int
 * @param int
 * @return int
 */
public function add(int $a, int $b) {
    return $a + $b;
}
```

Here's the same function in Salty:

```haskell
add :: int -> int -> int
add a b := a + b
```

Here's a `forEach` loop in PHP:

```php
$evens = [];
forEach($numbers as $number) {
    if (isEven($number)) {
        $evens []= $number;
    }
}
```

This is essentially a `select`, and Salty provides higher order functions so you can write it as a select:

```haskell
evens = numbers.select(\number -> isEven(number))
```

Or for getting an array slice, instead of

```php
array_slice($foo, 1)
```

You can do:

```haskell
foo[1:]
```

These are simple examples, but you can find some longer examples [here](/test/LongExamples/Js/SimpleWebPage).

See list of features [here](FEATURES.md): dot notation, implicit returns, guards, multi-assign, hash destructuring, and more.

## Compiling to JSX

Salty provides an alternative to JSX that's similar to [hyperscript-helpers](https://github.com/ohanhi/hyperscript-helpers).

Take this JSX fragment for example:

```jsx
<TodoList title="My Todos">
    <ul>
        <li>todo one</li>
        <li>todo two</li>
        <li>todo three</li>
    </ul>
</TodoList>
```

You can write it in Salty like this:

```ruby
TodoList.new({title: "My Todos"}) do
    ul do
        li "todo one"
        li "todo two"
        li "todo three"
    end
end
```

## Getting Salty
Compile the code yourself or just copy one of the binaries in `bin`.

### Compiling

Install Stack:

https://docs.haskellstack.org/en/stable/install_and_upgrade/

Then:

    make install

## Usage

By default salty reads from stdin and prints to stdout.
You can also give it a file to read like so:

    salty test.salt

And it will print php code on stdout.

Or,

    salty -f test.salt

Will write to `test.php`.

Use

    stack ghci

to play with the parser in ghci.

## Features

See list of features [here](FEATURES.md).

## Vim integration

Add these to your `.vimrc`:

    autocmd BufNewFile,BufRead *.salt set filetype=salty
    autocmd BufEnter *.salt

    " all .salt files are automatically compiled to PHP on write.
    autocmd BufWritePost *.salt silent !salty -f %

    " leader-p will convert the selected salty code to php.
    map <Leader>p :'<,'>!salty<CR>


Syntax highlighting: https://github.com/egonschiele/salt-vim
