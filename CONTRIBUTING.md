# Contributing to tidysheet

Thank you for considering contributing to tidysheet! We welcome contributions from our colleagues in ONS. Here are some guidelines to help you get started.

## Code of Conduct

Please read and follow our Code of Conduct to ensure a welcoming environment for all contributors.

## How to Contribute

### Reporting Issues

If you find a bug or have a feature request, please open an issue and provide as much detail as possible.

### Submitting Changes

1. **Fork the Repository**: Click the "Fork" button at the top right of the repository page.
2. **Create a Branch**: Use a descriptive name for your branch (e.g., `feature/add-new-feature`).
3. **Make Your Changes**: Ensure your code follows our coding standards.
4. **Commit Your Changes**: Write clear and concise commit messages.
5. **Push to Your Fork**: Push your changes to your forked repository.
6. **Open a Pull Request**: Go to the original repository and open a pull request. Provide a detailed description of your changes.

## Coding Standards

- Follow the style guide for the language you are using.
- Write clear, concise, and well-documented code.
- Ensure your code is tested and passes all tests.

## Testing

Run the tests to ensure your changes do not break existing functionality. 

To run test coverage, use the following code:

```r
covr::package_coverage(
    path = ".",
    type = "all",
    combine_types = FALSE,
    relative_path = TRUE,
    quiet = TRUE,
    clean = TRUE,
    line_exclusions = NULL,
    function_exclusions = NULL,
    pre_clean = TRUE
)

output:
tutorial Tests Coverage: 100.00%
R/hello_world.R: 100.00%

tutorial Vignettes Coverage: 0.00%
R/hello_world.R: 0.00%

tutorial Examples Coverage: 0.00%
R/hello_world.R: 0.00%

## Documentation

If your changes affect the documentation, please update it accordingly. This includes the README, inline comments, and any other relevant documentation files.

## Communication

Join our Slack channel or Discord server to discuss issues, ask questions, and collaborate with other contributors ;-)
Actaully, just message us on Teams. We don't have a Discord server.

---

Thank you for your contributions!

```
