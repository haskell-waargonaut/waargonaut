{ nixpkgs, declInput }: let pkgs = import nixpkgs {}; in {
  jobsets = pkgs.runCommand "spec.json" {} ''
    cat <<EOF
    ${builtins.toXML declInput}
    EOF
    cat > $out <<EOF
    {
        "waargonaut": {
            "enabled": 1,
            "hidden": false,
            "description": "waargonaut",
            "nixexprinput": "waargonaut",
            "nixexprpath": "ci/ci.nix",
            "checkinterval": 300,
            "schedulingshares": 1,
            "enableemail": false,
            "emailoverride": "",
            "keepnr": 5,
            "inputs": {
                "waargonaut": { "type": "git", "value": "https://github.com/qfpl/waargonaut", "emailresponsible": false },
                "nixpkgs": { "type": "git", "value": "https://github.com/NixOS/nixpkgs.git release-18.03", "emailresponsible": false }
            }
        }
    }
    EOF
  '';
}
