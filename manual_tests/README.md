# Manual tests

- [What it is](#what-it-is)
  - [Tests](#tests)
- [Usage](#usage)
- [Maintenance](#maintenance)

----------------------

## What it is

We aim to provide good coverage of our desired functionality using automated tests.
However there is always some manual testing done before a release is made in order to
ensure that "things look good" and work as expected. Some things just can't be automated
(Tableau) whereas others are tricky to fully automate right (UI).
The person doing the "things look good" assesment should not have to come up with a set of
"looks good" criteria each and every time tests are run.
This directory describes expected testing steps and takes the guesswork out of the process.

### Tests

- [Interface tests](interface_tests.md)

## Usage

Run through the test script provided ensuring the steps succeed as expected.
Failure should lead to the creation of issues and a delay of the release.

## Maintenance

When you add new functionality to the system that impacts the user experience of the system,
or has an impact on external tools, you should document it in the appropriate manual test suite
to ensure the functionality is evaluated before a release is made.
