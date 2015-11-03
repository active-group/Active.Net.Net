namespace Active.Net.Net

module Generators =
    open FsCheck
    /// FsCheck generator for System.Net.IPAddress
    let IPAddressGenerator =
        Arb.generate<byte>
        |> Gen.arrayOfLength 4  // IPv4
        |> Gen.map (fun bytes -> new System.Net.IPAddress(bytes))

    /// FsCheck generator for System.Net.NetworkInformation.PhysicalAddress (aka MacAddress.T)
    let PhysicalAddressGenerator =
        Arb.generate<byte>
        |> Gen.arrayOfLength 6 // usually 6 bytes long
        |> Gen.map (fun bytes -> new System.Net.NetworkInformation.PhysicalAddress(bytes))

/// helper type to register IPv4 IPAddresses, MAC addresses generators
type NetGenerators() =
    static member IPAddress() =
        {new FsCheck.Arbitrary<System.Net.IPAddress>() with
            override x.Generator = Generators.IPAddressGenerator
            override x.Shrinker t = Seq.empty }
    static member PhysicalAddress() =
        {new FsCheck.Arbitrary<System.Net.NetworkInformation.PhysicalAddress>() with
            override x.Generator = Generators.PhysicalAddressGenerator
            override x.Shrinker t = Seq.empty}

