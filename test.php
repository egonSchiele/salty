<?php
class Compliance_Blocklist {
    function __construct($blocklists) {
        $this->blocklists = $blocklists ?? DataType_Compliance_BlocklistType::getValues();
        function isPerformanceFeatureEnabled() {
            static::$is_performance_feature_enabled = static::$is_performance_feature_enabled ?? Feature::isEnabled('product_compliance.blocklist_perf');
            return static::$is_performance_feature_enabled;
        }
        /**
         * @param array|null
         * @param EP_Locale
         */
        function buildCompleteBlocklist(?array $blocklists = null, EP_Locale $loc) {
            $locale = $loc ?? EP_Locale::makeDefaultLocale();
            if (Feature::isEnabled('product_compliance.blocklist_local_cache')) {
                return static::getLocallyCachedBlocklist($blocklists, $locale);
            } else {
                return static::getCachedBlocklist($blocklists, $locale);
            }
        }
    }
}
