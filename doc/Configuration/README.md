# Configuration

## General

It is possible to configure the AMDP Pretty Printer with the transaction
ZAPP_SETTINGS.

There are two different kinds of settings. General settings and user
specific settings.

<img src="images/media/image1.png" alt="settings" width=50%/>

The general settings are valid for all users. But if a user defines his
own settings, then only his settings will be used.

Often several users
work on the same coding, so it is recommended to agree on general
settings in the project and not to use user specific settings.

If nothing is configured, then the default will be used (see the
settings for detail).

## Line Break after Comma Rule

There is currently only the setting “Line Break after Comma Rule”.

<img src="images/media/image2.png" alt="line break after comma rules" width=50%/>

It controls how the AMDP Pretty Printer handles line breaks after the comma.
The following options are possible:

- 0 => add a line break after a comma
- 1 => add no line break after a comma
- 2 => add no line break after a comma at simple functions, if the following criterions are fulfilled:
  - the closing bracket is originally in the same row as the function name
  - a possible sub function contains no comma and no select statement
  - it is one of the following functions:
    - SUBSTRING
    - SUBSTR_AFTER
    - SUBSTR_BEFORE
    - RPAD
    - LPAD
    - CONCAT
    - NULLIF
    - IFNULL

If nothing is configured, then a line break will be added.

### Example option 0 (with line break) vs option 1 (without line break)

The unformatted AMDP source code:

<img src="images/media/image3.png" alt="lb source" wdith=100%/>

The formatted AMDP source code will look like this **with** the line break after the comma:

<img src="images/media/image4.png" alt="with lb" width=75%/>

The formatted AMDP source code will look like this **without** the line break after the comma:

<img src="images/media/image5.png" alt="without lb" width=75%/>

### Examples of option 3 (no line break after comma for simple functions)

No line break in the substring function, because the rtrim function conatains no comma:

<img src="images/media/image6.png" alt="substring without lb before" width=75%/>

<img src="images/media/image7.png" alt="substring without lb after" width=75%/>

Line break in the substring function, because the closing bracket is in the new line:

<img src="images/media/image8.png" alt="substring with lb closing bracket in line before" width=75%/>

<img src="images/media/image9.png" alt="substring with lb closing bracket in line after" width=75%/>

Line break in the substring function, because the sub function concat contains a comma:

<img src="images/media/image10.png" alt="substring with lb comma in sub function before" width=75%/>

<img src="images/media/image11.png" alt="substring with lb comma in sub function after" width=75%/>

