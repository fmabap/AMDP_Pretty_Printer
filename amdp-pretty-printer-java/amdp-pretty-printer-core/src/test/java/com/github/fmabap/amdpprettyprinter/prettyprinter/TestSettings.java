package com.github.fmabap.amdpprettyprinter.prettyprinter;

/**
 * Simple ISettings implementation for unit tests.
 * Replaces the ABAP cl_abap_testdouble mock for ZIF_APP_SETTINGS.
 */
class TestSettings implements ISettings {

    private final boolean lineBreakAfterCommaReq;
    private final boolean noLbAtCoSFuDepSfu;
    private final boolean noLbAtCoSFuDepCbrO;
    private final boolean noLbAtCoSFuDepSfuKw;

    TestSettings(boolean lineBreakAfterCommaReq,
            boolean noLbAtCoSFuDepSfu,
            boolean noLbAtCoSFuDepCbrO,
            boolean noLbAtCoSFuDepSfuKw) {
        this.lineBreakAfterCommaReq = lineBreakAfterCommaReq;
        this.noLbAtCoSFuDepSfu = noLbAtCoSFuDepSfu;
        this.noLbAtCoSFuDepCbrO = noLbAtCoSFuDepCbrO;
        this.noLbAtCoSFuDepSfuKw = noLbAtCoSFuDepSfuKw;
    }

    @Override
    public boolean isLineBreakAfterCommaReq() {
        return lineBreakAfterCommaReq;
    }

    @Override
    public boolean isNoLbAtCoSFuDepSfu() {
        return noLbAtCoSFuDepSfu;
    }

    @Override
    public boolean isNoLbAtCoSFuDepCbrO() {
        return noLbAtCoSFuDepCbrO;
    }

    @Override
    public boolean isAlwaysLineBreakAftComma() {
        return false;
    }

    @Override
    public boolean isNoLbAtCoSFuDepSfuKw() {
        return noLbAtCoSFuDepSfuKw;
    }

    @Override
    public boolean isTrace() {
        return true;
    }
}
