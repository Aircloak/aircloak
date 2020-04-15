# Documentation

- [What it is](#what-it-is)
- [What is it made up of](#what-is-it-made-up-of)
- [Loading into online docs](#loading-into-online-docs)
- [Things to note](#things-to-note)
- [Tools](#tools)

----------------------

## What it is

This folder contains documentation desribing the operation of the
Aircloak system and the underlying anonymization algorithm Diffix.
The documentation has several roles:
* Used by Aircloak engineers as system documentation
* Used in Aircloak Insights online documentation
* Used as stand-alone documentation for customers and public
(e.g. for bounty programs or ArXiv papers)

## What is it made up of

**attacks.md:** Describes attacks that Diffix defends against.
Meant for public consumption.

**diffix.md:** Describes system functionality, and references
sections in attacks.md.  Meant for public consumption.

**attacks-priv.md:** Describes attacks that Diffix does not defend
against. Meant for customers, but not public. This content will be moved
into **attacks.md** as defenses are implemented.

**diffix-priv.md:** Describes system functionality for which there
are known vulnerabilities.  Meant for customers, but not public. This
content will be moved into **diffix.md** as defenses are implemented.

**anonymization.md:** Deprecated. Don't update, but keep because it contains
some information currently not in diffix.md, particularly about uid-based
anonymization.

## Loading into online docs

**diffix.md** and **attack.md** must be copied into the online
documentation directory at `aircloak/air/docs/content/`. Before doing so,
the links to Github issues must be removed.

By convention, these links take the form:

`[ghiXXXX](https://github.com/Aircloak/aircloak/issues/XXXX)`

and always appear on a separate line so that an auto-scrubber can
remove the links before copy.

## Things to note

**diffix.md** contains the following gitbook link:

`{% include "sql/syntax.md" %}`

This link works from the directory `aircloak/air/docs/content/` where
the online documentation resides. In any public copy of **diffix.md**,
the link must be replaced with the actual syntax.

## Tools

TBD
