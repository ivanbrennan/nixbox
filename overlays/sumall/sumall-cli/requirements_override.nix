{ pkgs, python }:

let
  depName = dep: (builtins.parseDrvName dep.name).name;
  depMatch = dep: name: pkgs.lib.hasSuffix ("-" + name) (depName dep);

  removeDependencies = names: deps:
    builtins.filter
    (dep: builtins.all (name: !depMatch dep name) names)
    deps;
in

self: super: {
  "attrs" = python.overrideDerivation super."attrs" (old: {
    propagatedBuildInputs =
      removeDependencies [ "pytest" ] old.propagatedBuildInputs;
    doCheck = false;
  });
}
