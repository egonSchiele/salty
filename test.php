<?php
foreach ($foo as $x) {
    print($x);
}
$result = [];
foreach ($foo as $x) {
    $result []= $x + 1;
}
$result = false;
foreach ($foo as $x) {
    if($x->isEven()) {
        $result = true;
        break;
    }
}
$result = true;
foreach ($foo as $x) {
    if(!$x->isEven()) {
        $result = false;
        break;
    }
}
$result = [];
foreach ($foo as $x) {
    if($x->isEven()) {
        $result []= x;
    }
}
public function bar() {
    foreach ($foo as $x) {
        print($x);
    }
    return $result;
}
public function bar() {
    $result = [];
    foreach ($foo as $x) {
        $result []= $x + 1;
    }
    return $result;
}
public function bar() {
    $result = false;
    foreach ($foo as $x) {
        if($x->isEven()) {
            $result = true;
            break;
        }
    }
    return $result;
}
public function bar() {
    $result = true;
    foreach ($foo as $x) {
        if(!$x->isEven()) {
            $result = false;
            break;
        }
    }
    return $result;
}
public function bar() {
    $result = [];
    foreach ($foo as $x) {
        if($x->isEven()) {
            $result []= x;
        }
    }
    return $result;
}
