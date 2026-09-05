# AMDP Pretty Printer CLI Tool

You can use the AMDP Pretty Printer CLI tool to format multiple files at once.

## Prerequisite

The Path to the Java JRE Version 21 or higher is maintained in the ```PATH``` environment variable.

You can also use the JRE that is shipped with Eclipse. Just search in the Eclipse plugins folder for the java.exe.

The path depends on your Eclipse version. It could be on Windows for example:

```bash
C:\eclipse\plugins\org.eclipse.justj.openjdk.hotspot.jre.full.win32.x86_64_21.0.10.v20260205-0638\jre\bin
```

## Installation

Download the file amdp-pretty-printer-app-*.jar from the [artifacts folder](https://github.com/fmabap/AMDP_Pretty_Printer/tree/main/artifacts).

## Usage

```bash
java -jar <path to the jar file> \
    <source> [target] [--pattern=<glob>] [--no-recursive] [--lb-rule=<0-4>]
```

| Argument | Description |
| --- | --- |
| `path to the jar file` | Path where you stored the jar file |
| `source` | Path to an AMDP source file or directory (required) |
| `target` | Output file or directory (optional; defaults to in-place) |
| `--pattern=<glob>` | File-name filter for directory mode (default: `*.abap`) |
| `--no-recursive` | Do not descend into subdirectories |
| `--lb-rule=<0-4>` | Line-break-after-comma rule (default: `4`) |

**`--lb-rule` values:**

| Value | Behaviour |
| --- | --- |
| `0` | Always insert line break after comma |
| `1` | Never insert line break after comma |
| `2` | Depends on closing bracket only |
| `3` | Depends on closing bracket and sub-function |
| `4` | Depends on closing bracket, sub-function, and keyword |

---

### Examples

You have stored the jar file in "C:\AMDP-Pretty-Printer\amdp-pretty-printer-app-2.1.0.jar".
And you are in the CLI path where your ABAP classes are stored.

#### Single-file mode

Format a file in-place:

```bash
java -jar "C:\AMDP-Pretty-Printer\amdp-pretty-printer-app-2.1.0.jar" zcl_my_amdp.abap
```

Write formatted output to a separate file:

```bash
java -jar "C:\AMDP-Pretty-Printer\amdp-pretty-printer-app-2.1.0.jar" zcl_my_amdp.abap zcl_my_amdp_formatted.abap
```

Disable line breaks after commas:

```bash
java -jar "C:\AMDP-Pretty-Printer\amdp-pretty-printer-app-2.1.0.jar" zcl_my_amdp.abap --lb-rule=1
```

#### Directory mode

Format all `*.abap` files in `src/` in-place (recursively):

```bash
java -jar "C:\AMDP-Pretty-Printer\amdp-pretty-printer-app-2.1.0.jar" src/
```

Format all `*.abap` files from `src/` and write results to `out/` (directory structure is mirrored):

```bash
java -jar "C:\AMDP-Pretty-Printer\amdp-pretty-printer-app-2.1.0.jar" src/ out/
```

Format only the top-level directory, skip subdirectories:

```bash
java -jar "C:\AMDP-Pretty-Printer\amdp-pretty-printer-app-2.1.0.jar" src/ --no-recursive
```

Format all `*.txt` files instead of the default `*.abap`:

```bash
java -jar "C:\AMDP-Pretty-Printer\amdp-pretty-printer-app-2.1.0.jar" src/ --pattern=*.txt
```

Combine options - format `*.abap` files non-recursively, write to a separate directory, use lb-rule 2:

```bash
java -jar "C:\AMDP-Pretty-Printer\amdp-pretty-printer-app-2.1.0.jar" src/ out/ --no-recursive --lb-rule=2
```

---

## Demo

### Input (unformatted)

```abap
CLASS zcl_demo_amdp DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_amdp_marker_hdb.

    CLASS-METHODS get_flights.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_amdp IMPLEMENTATION.

  METHOD get_flights
  BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT
  OPTIONS READ-ONLY
  USING sflight spfli.
  lt_result = select carrid, connid, fldate from sflight where sflight.mandt = SESSION_CONTEXT('CLIENT') and sflight.connid = 'LH' union all select spfli.carrid, spfli.connid, sflight.FLDATE from spfli inner join sflight on sflight.mandt = spfli.mandt and sflight.CARRID = spfli.carrid and sflight.CONNID = spfli.connid where spfli.mandt = SESSION_CONTEXT('CLIENT') and spfli.carrid = 'AB';
  ENDMETHOD.

ENDCLASS.
```

### Output (formatted with `--lb-rule=0`)

```abap
CLASS zcl_demo_amdp DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_amdp_marker_hdb.

    CLASS-METHODS get_flights.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_demo_amdp IMPLEMENTATION.

  METHOD get_flights
  BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT
  OPTIONS READ-ONLY
  USING sflight spfli.
    lt_result =    SELECT carrid,
                          connid,
                          fldate
                     FROM sflight
                    WHERE sflight.mandt = SESSION_CONTEXT('CLIENT')
                      AND sflight.connid = 'LH'
                UNION ALL
                   SELECT spfli.carrid,
                          spfli.connid,
                          sflight.FLDATE
                     FROM spfli
               INNER JOIN sflight
                       ON sflight.mandt = spfli.mandt
                      AND sflight.CARRID = spfli.carrid
                      AND sflight.CONNID = spfli.connid
                    WHERE spfli.mandt = SESSION_CONTEXT('CLIENT')
                      AND spfli.carrid = 'AB';
  ENDMETHOD.

ENDCLASS.
```

Key formatting changes applied by the pretty printer:

- **Keyword casing** - SQL keywords are uppercased (`select` → `SELECT`, `from` → `FROM`, `and` → `AND`, etc.)
- **Clause alignment** - `FROM`, `WHERE`, `AND`, `UNION ALL`, `INNER JOIN`, and `ON` are right-aligned to the same column
- **Column list expansion** - each selected column is placed on its own line (with `--lb-rule=0`)
- **Plain ABAP is untouched** - the class definition, `INTERFACES`, and `CLASS-METHODS` statements are passed through as-is
