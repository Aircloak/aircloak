# BOM

This subproject contains scripts generating a Bill of Materials for the whole solution.

- [What it does](#what-it-does)
- [What is it made up of](#what-is-it-made-up-of)
- [Installation](#installation)
- [Running](#running)
- [Validation](#validation)
- [Manually Adding a License](#manually-adding-a-license)
- [Manually Classifying a License](#manually-classifying-a-license)
- [Not Shipped packages](#not-shipped-packages)

## What it does

It generates a JSON file describing all dependencies of the system along with their licenses. This can then be
used to produce a Bill of Materials page in the frontend to fulfill the requirements of license such as the
`MIT`.

## What is it made up of

It is written as a single `mix` task plus supporting code.

Please note that it depends on a patched version of the `hex` library, which it uses to fetch `hex.pm`
metadata. The library is not available through `hex.pm` itself, so it is fetched via git. The patched version
is compiled with debug info so that it works with dialyzer.

## Installation

You need `Elixir` per the `.tool-versions` file in the root of the repository. Then just `mix deps.get`.

## Running

The entry point is the `mix bom` task. See `mix help bom` for usage. The command will produce a JSON file
describing dependencies and their licenses in the following format:

```json
[
  {
    "name": "name of the package",
    "realm": "either 'elixir' or 'node'",
    "license": {
      "type": "one of a list of license types",
      "text": "the full text of the license"
    },
    "version": "a version number or git hash"
  },
  ...
]
```

For a list of possible license types see the function `allowed_type?` in `lib/bom/license`.

## Validation

We perform checks on packages to see that they are sane and that the license they are released under can be
used to include them in our system. If `mix bom` doesn't succeed due to some packages not passing these checks
it will exit with a non-zero status and print out a detailed list of packages with errors.

Possible errors:

### Name empty / realm empty / no license

These are most likely programming errors - the code responsible for the given realm of package didn't produce
the name/realm/license information for that package.

### License empty

An empty license text has been generated for the package. This most pobably means that the code responsible
for automatically recovering the license text from files like `LICENSE` or `README` found an empty section.
This is most likely a programming error unless - only the `:empty` license should have an empty text.

### Forbidden license type :empty

No license text could be obtained for the package. See Manually Adding a License.

### Forbidden license type {:unknown, <hash>}

A license text has been found, but could not be automatically classified. This most likely means that the
package lacks license metadata in `npm`/`hex.pm`/whatever. See Manually Classifying a License.

## Manually Adding a License

You can manually add a license for a package by adding a new entry in the `@licenses` attribute in
`lib/bom/whitelist.ex`. Follow the existing convention for `type`. `text: :standard` means that the text will
be taken from `priv/licenses/generic/<type>`. `text: :provided` means that the text will be taken from
`priv/licenses/<realm>/<package name>` - you should use this one when the package or its website includes some
license file. Make sure the license text file exists.

### Important - BSD 4-clause licenses

The BSD 4-clause license requires that all marketing materials pertaining to the feature where the particular
package is used include a notice that code provided by the package's copyright holder is included. Because of
this weird requirement the license has been deprecated. Nevertheless, some packages might still use it.
Consequently, this license will not be automatically recognized.  When whitelisting such a package try to make
sure that this requirement won't be a problem - for example it's a test-only package.

## Manually Classifying a License

When a license text is automatically found, but a type cannot be found the license is marked as `{:unknown,
<text hash>}`. You can manually set the type of license for that package (after inspecting the package) by
adding that hash to `@type_by_text_digest` in `lib/bom/whitelist.ex`.

## Not Shipped packages

You can mark a package as not shipped by adding it to `@not_shipped` in `lib/bom/whitelist.ex`. It will be
skipped from the generated BOM. Make sure the package is in fact not shipped, for example it's a test-only
dependency.
