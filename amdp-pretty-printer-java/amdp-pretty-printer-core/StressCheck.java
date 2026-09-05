import com.github.fmabap.amdpprettyprinter.prettyprinter.*;
import java.util.*;

public class StressCheck {
    public static void main(String[] args) throws Exception {
        List<String> src = new ArrayList<>();
        src.add("  METHOD sel_data");
        src.add("  BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT.");
        src.add("  lt_result = select");
        int columns = 20000;
        for (int c = 0; c < columns; c++) {
            src.add("      col_" + c + (c == columns - 1 ? "" : ","));
        }
        src.add("    from sflight;");
        src.add("  ENDMETHOD.");
        System.out.println("Tokens (lines): " + src.size());
        long t0 = System.nanoTime();
        List<String> result = new PrettyPrinter().prettyPrint(src, new Settings("4", false));
        long t1 = System.nanoTime();
        System.out.println("OK, result lines: " + result.size() + ", took " + ((t1 - t0) / 1_000_000) + " ms");
    }
}

