<?php
function fib($x) {
    if ($x < 2) {
        return $x;
    } else {
        return fib($x - 1) + fib($x - 2);
    }
}
for ($i = 1; $i <= 10; $i++) {
    var_dump(fib($i));
}