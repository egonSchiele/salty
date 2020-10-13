isSafe str lists := lists.any(\l -> l.isBlocked(str));
