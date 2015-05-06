#-*- mode: sh -*-

function cabal-sanity {
    rm -rf ~/.cabal ~/.ghc
    cabal update
    echo 'require-sandbox: True' >> ~/.cabal/config
}
