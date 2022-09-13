let upstream =
      https://github.com/garganscript/package-sets/releases/download/v0.1.0/release.dhall
        sha256:86d64cc3698a3fcfe92058765cc90e48581b288aac2fc28a3dce61ed375389f6

let overrides = {=}

let additions = {=}

in  upstream // overrides // additions
