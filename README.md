# Re-doom
An opinionated, cut down core of doom emacs.

## notes:
 Interactive Startup Order:
   > $EMACSDIR/early-init.el
     - inspect CLI args, maybe prep for batch
     - setup performance changes, 
     - add REDOOMLIB to load-path
     - require re-doom-bootstrap
     - require re-doom-lib, ready for init.el
   - hook: `before-init-hook'
   > USER_DIR/init.el
     - call re-doom!
       - run bootstrappers
       - declare each profile:
         - the MODULES_DIR 
         - the ACTIVE-MODULES
         - the PACKAGES-DIR where straight installs these packages (or default $XDG-CACHE)
       - setup `after-init-hook' to load profile, init modules, loadd-packages, config modules
   - hook: `after-init-hook'
     > $XDG-CACHE-HOME/emacs/.local/profiles/$CURRENTPROFILE/$VERSION/init.el (Generated profile of load paths and autoloads)
     > $MODULES_DIR/**/init.el
     > $MODULES_DIR/**/config.el
     > $USER_DIR/post-init.el
   - CLI Args processed:
     - if batch, execute then quit
   - scratch buffer created
   - initial buffer loaded
   - hook: `emacs-startup-hook'
   - hook: `tty-setup-hook'
   - hook: `window-setup-hook'
   - display startup screen
   - advice-hook: `command-line-1' create load report buffer
   > Deferred loads waiting


##  CLI calls:
 1: bootstrap -> install -> compile (--target=, --profile=)
 3: report
 4: run  --debug --stepped --timed --memory
 5: quick run (minimal)
 6: stub-module

 Dependencies:
 Straight.
 Git.


## Module todos:
- performance module: gcmh-mode
- keybind core

