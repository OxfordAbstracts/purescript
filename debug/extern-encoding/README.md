# Externs writer measurements

These standalone programs re-encode saved `externs.cbor` files. Each input is decoded and forced before the measured interval. None of the writers changes the compiler. Use the repository's GHC 9.8.4 build with `serialise-0.2.6.1`, `cborg-0.2.10.0` and `text-2.1.1`.

From the repository root, build the compiler, then the probes in a separate directory:

```sh
stack build purescript:exe:purs
work=$(mktemp -d)
probe=$PWD/debug/extern-encoding
stack unpack cborg-0.2.10.0 --to "$work"
src=$work/cborg-0.2.10.0/src
sed 's/^module Codec.CBOR.Write$/module StockWrite/' "$src/Codec/CBOR/Write.hs" > "$work/StockWrite.hs"
patch -o "$work/FastWrite.hs" "$work/StockWrite.hs" < "$probe/direct-string.patch"
for program in ExternEncode CompareWriter ExternCorpus; do
  stack exec -- ghc -O2 -threaded -rtsopts -package purescript -package cborg -package serialise -package ghc-bignum \
    -I"$src/cbits" -DOPTIMIZE_GMP -DHAVE_GHC_BIGNUM -i"$work" -outputdir "$work/$program-obj" \
    "$probe/$program.hs" -o "$work/$program"
done
```

`ExternEncode` compares the stock lazy file sink with `hPutBuilder` writing the installed encoder's output directly to a handle. `CompareWriter` holds that handle sink fixed; `stock` recompiles the published cborg writer next to `inline-text`, which copies small strings directly into the current buffer. `ExternCorpus` rotates writer order by module and measures allocation on the current thread. Its sink is a single reused `/dev/null` handle, not a build's filesystem. Set `file` to an externs file and `output` to the directory containing all module directories:

```sh
"$work/ExternEncode" "$file" "$work/w-" both 4 +RTS -N1 -T -A32m
"$work/CompareWriter" "$file" "$work/w-" both 5 +RTS -N1 -T -A32m
for writer in lazy handle installed stock inline-text; do cmp "$file" "$work/w-$writer" || exit 1; done
"$work/ExternCorpus" "$output" +RTS -N1 -A32m > "$work/corpus.tsv"
awk '{n[$2]++; bytes[$2]+=$3; ns[$2]+=$4} END {for (w in n) print w,n[w],bytes[w]/1e9,ns[w]/1e9}' "$work/corpus.tsv"
```
