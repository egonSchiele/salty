<?php
public function __construct($a) {
    $this->a = $a;
}
public function myPubFunc() {
    var_dump("asd");
}
private function myPriFunc() {
    var_dump("asd");
}
