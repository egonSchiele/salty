function isSafe($str, $lists) {
    return $result = false;
    foreach ($lists as $l) {
        if($l->isBlocked(str)) {
            $result = true;
            break;
        }
    };
}

