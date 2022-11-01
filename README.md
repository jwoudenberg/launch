# Launch

A launcher based for personal use.

It supports:

- Finding and launching applications in desktop files.
- Inserting emoji, by starting with typing a colon (:).
- Starting any desktop app in nixpkgs, by starting with typing a comma (,).

I have it integrated into sway window manager with this configuration (assuming Nix):

```
for_window [app_id="^launcher$"] floating enable, sticky enable, resize set 350 px 350 px, border pixel 3
set $menu exec kitty --class=launcher -- ${pkgs.jwlaunch}/bin/launch

bindsym {
  Mod4+p exec $menu
}
```
