# How does it work (technical)

## General

The AMDP Pretty Printer consists like the SAP ABAP Development Tools of
a Rest Service in the SAP Development System that contains the Pretty
Printer logic and an Eclipse Plugin that calls the Rest Service and
adjust the coding. The SAP ADT SDK is used for the adjustment of the
coding and the communication with the SAP Backend.

The AMDP Pretty Printer adjusts only the content of the AMDP methods. It
doesn’t format the ABAP source code.

## The SAP backend logic

The Eclipse Plugin sends a post request to the URL
/sap/bc/adt/zapp/zapp_pretty_printer of your SAP Development System.
This Rest Service is provided by an implementation of the BADI
BADI_ADT_REST_RFC_APPLICATION (Enhancement Implementation
ZAPP_ADT_PRETTY_PRINTER). It calls the formatting logic.

The formatting logic is stored in the package $APP_PRETTY_PRINTER. The
entry point is the method ZCL_APP_PRETTY_PRINTER-\>PRETTY_PRINT.

It calls the scanner class ZCL_APP_SCANNER to scan the source code,
determines the rules and applies them on the source code to format it.
The rules will be applied up to 11 times, because it could be that there
are dependencies between the rules. The rules will be created by the
class ZCL_APP_RULE_FACTORY.

### Scanner logic

The soucre code will be scanned by the class ZCL_APP_SCANNER. Scan means
that it calls the SAP internal command SCAN ABAP-SOURCE to tokenize the
source code. This command is also used by the SAP Standard ABAP Pretty
Printer. The command returns the tokens in a way that it cannot be used
by the rules. Hence, there are several methods that adjust the tokens that
the can be used for the AMDP Pretty Printer. The scanner logic also
determines if a token belongs to a comment and what kind of comment it
is (class ZCL_APP_SCANNER_COMMENT). Additional it determines if the
token belongs to the AMDP method content (class
ZCL_APP_SCANNER_SQLSCRIPT). Therefore, the scanner searches for the
keyword SQLSCRIPT that is not part of a commend and part of the METHOD
command. The source code after the dot of the METHOD command until the
ENDMETHOD command will be formatted by the AMDP Pretty Printer rules.
The rest will be copied 1:1 by the ABAP rule.

Additional there is the class ZCL_APP_SCANNER_DELIMITER it scans the
delimiter (for example spaces) between the tokens that the command SCAN
ABAP-SOURCE returns. If the user enters empty rows in the source code,
then they will be part of the delimiter and the rules normally will keep
them alive in the result of the formatting.

### The rules

The rules will be provided by the class ZCL_APP_RULE_PROVIDER. They are
not defined in a customizing table to deploy the AMDP Pretty Printer
with abapGit. It doesn’t support table content until now.

All rules implement the interface ZIF_APP_RULE and inherit from the
class ZCL_APP_BASE_RULE.

The class ZCL_APP_RULE_FINDER searches the rules for the tokens. On each
token will be only the first found rule be applied. It uses the
following access sequence:

1.  Full specified (Token, Context, Higher Level Context, SQLSCRIPT)

2.  With context (Token, Context, SQLSCRIPT)

3.  Without Token (Context, Higher Level Context, SQLSCRIPT)

4.  With Token (Token, SQLSCRIPT)

5.  Default (SQLSCRIPT)

Hint: Currently all rules have no context. The context could be defined
by a previous rule.

All rules can be find in the package $APP_PRETTY_PRINTER_RULES and
below.
