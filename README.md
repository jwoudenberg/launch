# Launch

A launcher based for personal use.

It supports:

- Find and launch applications in desktop files.
- Insert emoji, by typing a colon (:).
- Start any desktop app in nixpkgs, by typing a comma (,).
- Copy a pass(age) password to clipboard, by typing an asterisk (*).

I have it integrated into sway window manager with this configuration (assuming Nix):

```
for_window [app_id="^launcher$"] floating enable, sticky enable, resize set 350 px 350 px, border pixel 3
set $menu exec kitty --class=launcher -- ${pkgs.jwlaunch}/bin/launch

bindsym {
  Mod4+p exec $menu
}
```
