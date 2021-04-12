# LSP server benchmarks

This directory contains a tool which measures the speed of LSP servers.

## Building

Install [Stack][], then run the following command:

    $ stack build

Then, install the LSP servers you want to benchmark:

* **Deno**: Install [Deno][].
* **ESLint**: Run `npm install` in the `eslint/` directory.
* **Flow**: Run `npm install` in the `flow/` directory.
* **RSLint**: Install [RSLint's rslint_lsp crate][install-rslint].
* **TypeScript**: Run `npm install` in the `typescript/` directory.
* **quick-lint-js**: Install `quick-lint-js` in `$PATH`.

## Running

Print a list of benchmarks:

    $ stack run -- --list
    eslint-server/change-wait
    eslint-server/open-wait-close
    quick-lint-js/change-wait
    quick-lint-js/open-wait-close

Run the benchmarks and generate an HTML report with graphs:

    $ stack run -- --output benchmark-report.html

[Deno]: https://deno.land/
[Stack]: https://haskellstack.org/
[install-rslint]: https://rslint.org/guide/
