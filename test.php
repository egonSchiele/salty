<?php
class Compliance_Blocklist {
    function __construct($blocklists) {
        $this->blocklists = $blocklists ?? DataType_Compliance_BlocklistType::getValues();
        function isPerformanceFeatureEnabled() {
            static::$is_performance_feature_enabled = static::$is_performance_feature_enabled ?? Feature::isEnabled(product_compliance.blocklist_perf);
            return static::$is_performance_feature_enabled;
        }
    }
}
