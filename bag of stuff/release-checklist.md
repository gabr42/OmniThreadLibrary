# OmniThreadLibrary release checklist

Order matters. Tests run first, on the code that will ship. The Google Drive file ID must exist
before the release commit (it is written into `README.md` and `release-3.xx.txt`, both in git),
and nothing is pushed until the real ZIP is on Google Drive.

## A. Update the code (uncommitted is fine)

1. Update the code: sync `src`, remove unused units, fix bugs, etc.

## B. Test

2. Run `unittests\mbsmoketest.bat` (all compilers, 32-bit and 64-bit, plus unit tests).
3. Run `unittests\mbstresstest.bat`.
   If a fix is needed, make it and re-run both.

From here on only non-code files may change (`history.txt`, `README.md`, release notes,
`makedownload.bat`, docs). Any code change sends you back to step 2.

## C. Prepare the release commit

4. Upload a placeholder `OmniThreadLibrary.zip` to Google Drive and note the file ID.
5. Update the non-code files:
   - Set the release date in `history.txt`.
   - Put the Drive link in `bag of stuff\release-3.xx.txt` (replace `<<TODO: ZIP file id>>`).
   - Update `README.md`: new Drive link and new tag URL (`.../tree/release-3.xx`).
   - Update version references and `.md` docs.
   - `makedownload.bat` must include everything that ships (e.g. `docs\claude-skill\*.md`).
6. Commit locally (code from step 1 plus the files from step 5); do not push.
   Check that the commit contains no build output and no code that was not tested.

## D. Package (still local)

7. Create the tag locally (same name pattern as existing tags, e.g. `release-3.09`).
8. Make a clean worktree of the tag: `git worktree add c:\0\otl-release release-3.xx`.
9. In the worktree run `makedownload.bat` (no old zip may exist there - 7z appends to it).
10. Check the zip:
    - `7z l` shows `docs\claude-skill\*` (9 files);
    - no stray build output;
    - `bag of stuff\` is not in the zip (excluded with `-xr!"bag of stuff"`);
    - `history.txt` and `README.md` are the final versions.
11. Overwrite the placeholder on Google Drive with the real zip (same file ID). Confirm the file ID
    is unchanged and check the link in a private window.

## E. Publish (irreversible)

12. Push `master` and the tag together. A tag is not pushed with the branch: push it explicitly
    (`git push origin master release-3.xx`, or push the tag by hand in GitKraken) and verify with
    `git ls-remote --tags origin release-3.xx`.
13. Create the GitHub release as a draft from the tag, using `release-3.xx.txt` as the text.
    Check it, then publish straight away.
14. Check that the README links on GitHub work (tag URL and Drive link).
15. Remove the worktree: `git worktree remove c:\0\otl-release`.

## Rules

- Do not amend or add commits after step 7. If you must, delete the local tag and go back to step 6
  (or step 2 if code changed).
- Do not push before the real zip is on Drive (step 11); the pushed README links to that file.
