{ chromium
, dmenu
, dunst
, iconThemes
}:

let
  collectDirs = with builtins; dir:
    let contents = readDir dir;
        parts = partition isDir (attrNames contents);
        isDir = item: getAttr item contents == "directory";
        addCurDir = ds: if parts.wrong == [] then ds else [dir] ++ ds;
        f = item: collectDirs "${dir}/${item}";
    in addCurDir ( concatMap f parts.right );

  iconResolutions = ["16x16"];

  iconPath = with builtins;
    concatStringsSep ":"
      ( filter
          (path: any (res: match ".*/${res}/.*" path != null) iconResolutions)
          (concatMap (theme: collectDirs "${theme}/share/icons") iconThemes)
      );

in

dunst.overrideAttrs (old: rec {
  postInstall = ''
    install -Dm755 dunstify $out/bin
    wrapProgram $out/bin/dunst \
      --set GDK_PIXBUF_MODULE_FILE "$GDK_PIXBUF_MODULE_FILE" \
      --add-flags "-conf ${./dunstrc}" \
      --add-flags "-icon_path ${iconPath}" \
      --add-flags "-dmenu ${dmenu}/bin/dmenu" \
      --add-flags "-browser ${chromium}/bin/chromium"
  '';
})
