# salty

## Usage
Compile the code yourself or just copy one of the binaries in `bin`.

## Compiling

Install Stack:

https://docs.haskellstack.org/en/stable/install_and_upgrade/

Then:

    make install

By default salty reads from stdin and prints to stdout.
You can also give it a file to read like so:

    salty test.salt

And it will print php code on stdout.

    salty -f test.salt

Will write to `test.php`.

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


