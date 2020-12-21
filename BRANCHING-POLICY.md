We use a kind of gitflow for our development process and a certain branch policy.

General rule: avoid polluting the repository. It's a common place to work for many peoples, and a lot of branches makes it very hard to track the process.

- `master` is a main development branch.
- `master` branch should be always valid: compilation is ok, the tests passing, the latest actual code is there. (A broken `master` branch should be fixed immediately.)
- Create feature branches for active development.
- It's allowed for feature branches to be in a broken state.
- Avoid creating branches for no real need. Don't forget to delete your temporary branches from the repository.
- Consider the following subfolders:
    - `feature` for a new feature
    - `fix` for immediate fixes of something
- It's also allowed to have a thematic subfolder.
- Avoid merging to `master`, prefer to rebase your feature branch instead.
- It's allowed to do merges within collective feature branches (otherwise it will be hard for participants to work together).
- WIP comments in personal / feature branches allowed as long as you'll squash these commits before pushing them to `master`.
- The PR for merging should be made before moving `master` to the upcoming changes (for triggering our CI and making Code Reviews).
- Code Reviews are mandatory for the most of the changes.
- Old merged branches should be deleted ASAP.
- The reasonably old branches are not allowed to be kept in the repository.
- Abandoned branches should be deleted.
