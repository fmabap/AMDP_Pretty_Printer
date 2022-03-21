# Configuration

## General

It is possible to configure the AMDP Pretty Printer with the transaction
ZAPP_SETTINGS.

There are two different kinds of settings. General settings and user
specific settings.

<img src="images/media/image1.png"
style="width:3.48364in;height:2.12935in"/>

The general settings are valid for all users. But if a user defines his
own settings, then only his settings will be used.

Often several users
work on the same coding, so it is recommended to agree on general
settings in the project and not to use user specific settings.

If nothing is configured, then the default will be used (see the
settings for detail).

## No Line Break at Comma

There is currently only the setting “No Line Break at Comma”.

<img src="images/media/image2.png"
style="width:4.07535in;height:1.92933in"/>

It controls if the AMDP Pretty Printer automatically adds a line break
after a comma. If nothing is configured, then it will be added.

### Example

The unformatted AMDP source code:

<img src="images/media/image3.png"
style="width:6.3in;height:0.71319in" />

The formatted AMDP source code will look like this **with** the line
break after the comma:

<img src="images/media/image4.png"
style="width:5.3713in;height:1.90017in"/>

The formatted AMDP source code will look like this **without** the line
break after the comma:

<img src="images/media/image5.png"
style="width:6.04636in;height:1.30428in"/>
