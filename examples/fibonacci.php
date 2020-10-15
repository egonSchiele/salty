<?php
function fib($x) {
    if ($x < 2) {
        return $x;
    } else {
        return (fib($x - 1) + fib($x - 2));
    }
}
$a = 1;
while ($a <= 10) {
    var_dump(fib($a));
    $a = $a + 1;
}
