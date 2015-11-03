namespace Active.Net.Net

/// helper fns to handle subnets (network address plus subnetmask)
module Subnet =
    open FSharp.Data
    open System.Net

    /// count bits set to 1
    // http://www.fssnip.net/Q
    let popcount (x:byte) =
        let rec go b acc = if b = 0uy then acc else go (b &&& (b-1uy)) (acc+1) //sparse count
        //if b = 0 then acc else go (b.shrl) (acc + (b &&& 1)) //add res of calc
        go x 0

    /// return # of set bits in mask represented by IpAddress
    let prefixLength (ipAddr:IpAddress.T) =
        (IpAddress.address ipAddr).GetAddressBytes ()
        |> Array.map popcount
        |> Array.fold (+) 0

    /// subnet mask from CIDR prefix length
    let subnetMaskFromPrefixLength (classs:int) =
        ((1L <<< classs) - 1L) <<< (32 - classs)
        |> int32
        |> IPAddress.HostToNetworkOrder
        |> uint32
        |> IpAddress.fromUInt32

    /// return address in CIDR notation
    let cidr (addr:IpAddress.T) (mask:IpAddress.T) =
        let prefixLength = prefixLength mask
        // are there still subnet masks where non-MSBs are set?
        let areMSBs = (mask = subnetMaskFromPrefixLength prefixLength)
        if areMSBs
        then sprintf "%s/%d" (IpAddress.toString addr) prefixLength
        else sprintf "%s/%s" (IpAddress.toString addr) (IpAddress.toString mask)

    /// masks out the bits in address which are part of the subnet
    let networkAddress2 (addr:IpAddress.T) (mask:IpAddress.T) =
        Array.map2 (&&&) (IpAddress.toByteArray addr) (IpAddress.toByteArray mask)
        |> IpAddress.fromByteArray

    /// subnet type (network address + subnet mask)
    type T = {networkAddress: IpAddress.T
              subnetMask: IpAddress.T}
             with
             override v.ToString() =
                cidr (v.networkAddress) (v.subnetMask)

    /// network address of subnet
    let networkAddress (v:T) = v.networkAddress
    /// subnet mask of subnet
    let subnetMask (v:T) = v.subnetMask

    /// create Subnet.T from IP address and subnet mask;
    /// will calculate network address from IP address and subnet mask
    let make (address:IpAddress.T) (subnetMask:IpAddress.T) =
        {networkAddress = networkAddress2 address subnetMask
         subnetMask = subnetMask}
    /// create Subnet.T from IP address and CIDR prefix length (eg. data from "192.168.0.1/24")
    /// will calculate network address from IP address and derived subnet mask
    let makeCidr (networkAddress:IpAddress.T) (subnetPrefixLength:int) =
        let subnetMask = subnetMaskFromPrefixLength subnetPrefixLength
        make networkAddress subnetMask
    /// return true if ipAddr is part of subnet, false otherwise
    let contains (subnet:T) (ipAddr:IpAddress.T) =
        networkAddress2 ipAddr (subnetMask subnet) = (networkAddress subnet)
    /// json representation of subnet (array with two values, network address and subnet mask)
    let toJson (v:T) = JsonValue.Array [| IpAddress.toJson v.networkAddress; IpAddress.toJson v.subnetMask |]
    /// Subnet.T from its json representation
    let fromJson (j:JsonValue) =
        match j.AsArray() with
        | [|n;s|] -> make (IpAddress.fromJson n) (IpAddress.fromJson s)
        | _ -> failwith "not a subnet"
    /// string representation of Subnet.T
    let toString (v:T) = v.ToString()
    /// Subnet.T from its string representation
    let fromString (s:string) =
        match s.Split([|'/'|]) with
        | [| net ; subnet |] ->
            let networkAddress = IpAddress.fromString net
            let subnetMask =
                if subnet.Contains(".")
                then IpAddress.fromString subnet // assume dot notation
                else subnetMaskFromPrefixLength (System.Int32.Parse subnet) // assume cidr notation
            make networkAddress subnetMask
        | _ -> failwith ("not in subnet notation: " + s)
    /// Subnet.T from the string representation of an IP address and a subnet mask
    /// will calculate network address from IP address and subnet mask
    let fromStrings (networkAddress:string) (subnetMask:string) =
        make (IpAddress.fromString networkAddress) (IpAddress.fromString subnetMask)

    module Testing =
        open FsCheck
//        let commonMasks =
//            ([0..30]
//            |> List.fold (fun (mask::masks) _ -> ((mask >>> 1) ||| mask)::mask::masks) [0x80000000])
//            |> List.map (System.Net.IPAddress.HostToNetworkOrder >> uint32 >> int64 )
        let commonMasks =
            [4294967295L; 4278190079L; 4244635647L; 4177526783L; 4043309055L;
            3774873599L; 3238002687L; 2164260863L; 16777215L; 16711679L; 16580607L;
            16318463L; 15794175L; 14745599L; 12648447L; 8454143L; 65535L; 65279L;
            64767L; 63743L; 61695L; 57599L; 49407L; 33023L; 255L; 254L; 252L; 248L;
            240L; 224L; 192L; 128L; 0L]
            |> List.map (uint32 >> IpAddress.fromUInt32)
//            ["255.255.255.255"; "255.255.255.254"; "255.255.255.252"; "255.255.255.248";
//            "255.255.255.240"; "255.255.255.224"; "255.255.255.192"; "255.255.255.128";
//            "255.255.255.0"; "255.255.254.0"; "255.255.252.0"; "255.255.248.0";
//            "255.255.240.0"; "255.255.224.0"; "255.255.192.0"; "255.255.128.0";
//            "255.255.0.0"; "255.254.0.0"; "255.252.0.0"; "255.248.0.0"; "255.240.0.0";
//            "255.224.0.0"; "255.192.0.0"; "255.128.0.0"; "255.0.0.0"; "254.0.0.0";
//            "252.0.0.0"; "248.0.0.0"; "240.0.0.0"; "224.0.0.0"; "192.0.0.0";
//            "128.0.0.0"]

        let commonMasksGen = Gen.elements commonMasks

        let generator = gen {
            let! ipAddress = Arb.generate<IpAddress.T>
            let! mask      = Gen.oneof [Arb.generate<IpAddress.T>; commonMasksGen]
            return make ipAddress mask
        }
        let arbitrary = Arb.fromGen generator
