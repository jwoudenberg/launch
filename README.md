# Launch

A launcher based on FZF made for personal use.

It currently supports:

- Finding and launching applications in desktop files.
- Searching emoji and putting them on your clipboard.

I have it integrated into sway window manager withthis configuration:

```
for_window [app_id="^launcher$"] floating enable, sticky enable, resize set 350 px 350 px, border pixel 3
set $menu exec kitty --class=launcher -- ${pkgs.jwlaunch}/bin/launch

bindsym {
  Mod4+p exec $menu
}
```
