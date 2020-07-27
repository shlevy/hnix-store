huper: helf: {
  hnix-store-core =
    helf.callCabal2nix "hnix-store-core" ./hnix-store-core {};
  hnix-store-db =
    helf.callCabal2nix "hnix-store-db" ./hnix-store-db {};
  hnix-store-remote =
    helf.callCabal2nixWithOptions "hnix-store-remote" ./hnix-store-remote "-fio-testsuite" {};
}
