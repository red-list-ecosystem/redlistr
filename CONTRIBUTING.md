# CONTRIBUTING

### Please contribute!

We love collaboration! You are welcome to suggest general improvements
to the package structure or to whatsoever. We appreciate any
contribution and collaboration.

#### Code of Conduct

Please note that the **landscapemetrics** project is released with a
[Contributor Code of
Conduct](http://red-list-ecosystem.github.io/redlistr/CODE_OF_CONDUCT.md).
By contributing to this project you agree to abide by its terms.

## How to contribute

### Ask a question ⁉️

Browse the
[documentation](https://red-list-ecosystem.github.io/redlistr/) to see
if you can find a solution. Still stuck? Open an [issue on
GitHub](https://github.com/red-list-ecosystem/redlistr/issues) on
GitHub. We’ll try to do our best to address it, as questions often lead
to better documentation or the discovery of bugs.

If you want to ask a question in private get in contact by
[anikobtoth\<at\>gmail.com](mailto:anikobtoth@gmail.com) or
[jr.ferrer.parisgmail.com](mailto:jr.ferrer.paris@gmail.com).

Please try to include a reproducible example using for example the
[`reprex`](https://reprex.tidyverse.org) package.

### Propose an idea 💡

Take a look at the
[documentation](https://red-list-ecosystem.github.io/redlistr/) and
[issue on GitHub](https://github.com/red-list-ecosystem/redlistr/issues)
list to see if it isn’t included or suggested yet. If not, please open a
new issue!

While we can’t promise to implement your idea, it helps to:

- Explain in detail how it would work.
- Keep the scope as narrow as possible.

### Report a bug 🐛

Report it as an [issue on
GitHub](https://github.com/red-list-ecosystem/redlistr/issues) so we can
fix it. A good bug report makes it easier for us to do so, so please
include:

- The content of
  [`utils::sessionInfo()`](https://rdrr.io/r/utils/sessionInfo.html).
- Any details about your local setup that might be helpful in
  troubleshooting.
- Detailed steps to reproduce the bug

Again, please try to include a reproducible example using for example
the [`reprex`](https://reprex.tidyverse.org) package.

### Improve the documentation 📖

Good documentation makes all the difference, so your help to improve it
is very welcome!

We use [roxygen2](https://cran.r-project.org/package=roxygen2), with
[Markdown
syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/rd-formatting.html),
for documentation.

### Pull request process ↕️

We try to follow the [GitHub
flow](https://guides.github.com/introduction/flow/) for development.

1.  Fork [the repo](https://github.com/red-list-ecosystem/redlistr/) and
    clone it to your computer. To learn more about this process, see
    [this guide](https://guides.github.com/activities/forking/). Don’t
    forget to pull all new changes before starting to work!

2.  Open the RStudio project file (`.Rproj`) and install all development
    dependencies with (e.g., using
    [`devtools::install_dev_deps()`](https://devtools.r-lib.org/reference/install_deps.html)).
    Make sure the package passes R CMD check by running
    [`devtools::check()`](https://devtools.r-lib.org/reference/check.html).

3.  Create a new Git branch and use a name that briefly describes the
    proposed changes.

4.  Make your changes:

    - Write your code.
    - Test your code (bonus points for adding unit tests using the
      [`testthat`](https://testthat.r-lib.org) package).
    - Document your code (see function documentation above).
    - Check your code with
      [`devtools::check()`](https://devtools.r-lib.org/reference/check.html)
      and aim for 0 errors, warnings and notes.

5.  Commit and push your changes.

6.  Submit a [pull
    request](https://guides.github.com/activities/forking/#making-a-pull-request).

New code should follow the tidyverse [style
guide](https://style.tidyverse.org).

#### References

This CONTRIBUTING.md is adapted from
[here](https://gist.github.com/peterdesmet/e90a1b0dc17af6c12daf6e8b2f044e7c)
and
[here](https://github.com/r-lib/usethis/blob/main/inst/templates/tidy-contributing.md).
