# Verilog model site

## Dependencies

To install dependencies:

```bash
npm install
```

## Developing

Once you've installed dependencies, start a development server:

```bash
npm run dev
```

## Codegen (optional)

To generate errors data (it also runs when `npm run dev` and `npm run build` by default):

```bash
npm run generate
```

To initialize syntax highlighter:

```bash
npx shiki-codegen \
    --langs verilog \
    --themes github-dark-default \
    --engine javascript \
    ./src/lib/generated/shiki.bundle.ts
```

## Building

To create a production version:

```bash
npm run build
```

You can preview the production build with `npm run preview`.

## Other

### Extract SystemVerilog Examples

To extract SystemVerilog examples from YAML error files into individual `.sv` files:

The script creates files named after example IDs in the `all_examples` directory.

```bash
node scripts/extract_sv_examples.js found_errors all_examples
```
