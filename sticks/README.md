# sticks: a simple static site generator

`sticks` is a tiny and customizable static site generator.
it leverages `pandoc` to great extent, and so consists only of a single bash script.

<!-- : yet manages to provide nigh-total the features of jekyll, and more. this powers both my [website](https://apropos.codes) and my [wiki](https://wiki.apropos.codes). -->

don't use this! use [hakyll](https://jaspervdj.be/hakyll/) instead. it's a library for constructing static site generators: hooking directly into pandoc's haskell library and providing much more reasonable constructs for accomplishing such a task. i just wanted an excuse to learn pandoc proper.

### feature list
- html templating: directly through pandoc.
- support for markdown, restructured text, asciidoc, org-mode...
- renders latex and typst directly to pdfs
- pandoc flavoured markdown, including:
  - lists with preserved order (numbers mean something)
  - line blocks and nested quotes
  - code blocks and syntax highlighting
  - a wide variety of tables
  - inline latex via katex+mathml
  - yaml metadata blocks (!!!)

### usage
pandoc (and by extension sticks) operates on **templates** containing *variables* and *expressions*, and **files** containing *metadata* and *content*.

templates are simply an html, or pdf, or etc file containing the content needed (ex. header and footer material) for a free-standing document. they contain the special `$body$` variable somewhere for file injection. files may be prefixed with a YAML metadata block declaring variables (ex. title, subtitle, author...) for use by templates, and a special `template: ` variable to denote the template to be used.

every project must have a `_templates` folder (configurable location, see `--templates`) containing a `default.html`. this is the template that will be used by files lacking an explicit template. templates are for the *output type*, which in sticks' case is HTML and PDF.

the aforementioned YAML metadata blocks may be placed before files of any type. they look like the following:
```yaml
---
template: default
title: a really cool document
subtitle: https://www.youtube.com/watch?v=dQw4w9WgXcQ
---
```
