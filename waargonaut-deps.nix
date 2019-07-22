self: super: {
  haskellPackages = super.haskellPackages.override (old: {
    overrides = super.lib.composeExtensions (old.overrides or (_: _: {})) (hself: hsuper:
    let
      dc = d: super.haskell.lib.dontCheck d;
    in 
    {
      hw-json-standard-cursor = super.haskell.lib.dontCheck (hsuper.callHackageDirect {
        pkg = "hw-json-standard-cursor";
        ver = "0.2.1.1";
        sha256 = "1jzi9h38bkc6i9yvhc6x0zbdr0qdv1zc6j8iwb4x8qnnps77n6jp";
      } {});

      hw-json-simd = hsuper.callHackageDirect {
        pkg = "hw-json-simd";
        ver = "0.1.0.2";
        sha256 = "1iwdq1dw336s0j62b2jjmbznz882d9283d608ay50qmp1m552d5q";
      } {};

      hedgehog = hsuper.callHackageDirect {
        pkg = "hedgehog";
        ver = "1.0";
        sha256 = "06q1w1pjvhdr6za1n5kjd3zszh4xi2ixrwgclqqqj6nhdiz8y6zj";
      } {};

      hw-hedgehog = super.haskell.lib.doJailbreak (hsuper.callHackageDirect {
        pkg = "hw-hedgehog";
        ver = "0.1.0.3";
        sha256 = "01bmlhb3ns3k9sg3i4q2rx5ab49ns7b2mmq81vg4j6dn5y5hcqkr";
      } {});

      hedgehog-fn = hsuper.callHackageDirect {
        pkg = "hedgehog-fn";
        ver = "1.0";
        sha256 = "1dhfyfycy0wakw4j7rr01a7v70yms7dw3h60k5af7pi9v700wyb4";
      } {};

      tasty-hedgehog = hsuper.callHackageDirect {
        pkg = "tasty-hedgehog";
        ver = "1.0.0.1";
        sha256 = "06mffkvscl8r81hjhsvjlyqa843szgv8fays1l9z4jaw2759glsr";
      } {};

      hw-bits = dc (hsuper.callHackageDirect {
        pkg = "hw-bits";
        ver = "0.7.0.6";
        sha256 = "1x2b6ailplxj9y06clpxizwqr32nq9hf2j53v08m2wymw4k7718x";
      } {});

      hw-rankselect = dc (hsuper.callHackageDirect {
        pkg = "hw-rankselect";
        ver = "0.13.2.0";
        sha256 = "1nvla6i2c9bccsxwk232bz3phidkxjy2ia8cmijac1a0yf6z2lgm";
      } {});
        
      hw-prim = dc (hsuper.callHackageDirect {
        pkg = "hw-prim";
        ver = "0.6.2.30";
        sha256 = "0rp4rgksbksk4jsjgdnsic9rkwgk3g6rk3njhma2h5yyk63pkh7p";
      } {});

      hw-rankselect-base = dc (hsuper.callHackageDirect {
        pkg = "hw-rankselect-base";
        ver = "0.3.2.1";
        sha256 = "0nspjqk6sxja5y4mh9vsppn05q2f89h838zbn9kpypn6x21c95fv";
      } {});

      hw-balancedparens = super.haskell.lib.appendPatch (hsuper.callHackageDirect {
        pkg = "hw-balancedparens";
        ver = "0.3.0.0";
        sha256 = "03l0n2lhpay5jqm4829q58dcqv93n4jq6s3l9b633mflhxrl5nlw";
      } {}) ./nix/patches/remove-testing-library-component.patch;

      hw-fingertree = dc (hsuper.callHackageDirect {
        pkg = "hw-fingertree";
        ver = "0.1.1.0";
        sha256 = "1r3xyw3mz2kf5pqw08apyrk5f08j3bqxrx21w4qq5cmxvr7zmk3q";
      } {});

      hw-excess = dc (hsuper.callHackageDirect {
        pkg = "hw-excess";
        ver = "0.2.2.0";
        sha256 = "1m7ris8lf06kw8znammnvwmvfa6y54smzrp1x8hm17b5vaj8r4xw";
      } {});

      sop-core     = hsuper.callHackage "sop-core" "0.4.0.0" {};
      generics-sop = hsuper.callHackage "generics-sop" "0.4.0.1" {};

      natural            = dc hsuper.natural;
    });
  });
}
