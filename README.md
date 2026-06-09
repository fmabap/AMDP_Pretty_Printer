# ![Icon](/doc/images/amdp_pretty_printer_icon.png) AMDP Pretty Printer

SAP recommends code push downs of the ABAP logic to the HANA database to have a better performance. One of the possible ways to do this is to use AMDPs (ABAP Managed Database Procedures). They can be developed in Eclipse or in Visual Studio Code. SAP unfortunately does not provide a Pretty Printer (source code formatter) for it. The AMDP Pretty Printer is closing this gap.

## :rotating_light::rotating_light::rotating_light: The AMDP Pretty Printer 2.0 is now released. :rotating_light::rotating_light::rotating_light:

The coding is now Java only. The ABAP coding of the version 1.2.1 has been converted to Java and a new tokenizer has been developed which allows the reuse of the existing logic without the dependency to the SAP Standard Pretty Printer. So there is no longer an installation in the SAP Backend required and it can be used with cloud systems.

There is now also a CLI tool for mass formatting of files and a Visual Studio Code Extension available. All of the tools use the same Java core coding.

If you still need Version 1.2.1, then you can find it in the [releases](https://github.com/fmabap/AMDP_Pretty_Printer/releases/tag/v.1.2.1).

---
### Demo VS Code Extension

![VS Code Extension](/doc/images/vs_code_extension.gif)

---

### Demo Eclipse Plugin

![Eclipse Plugin](/doc/images/amdp_pretty_printer.gif)

---

You can find here the details of the AMDP Pretty Printer artifacts:

- [Eclipse Plugin](./doc/eclipse%20plugin.md)
- [CLI Tool](./doc/cli.md)
- [Visual Studio Code Extension](./doc/vscode%20extension.md)
