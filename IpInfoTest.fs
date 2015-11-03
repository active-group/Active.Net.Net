namespace Active.Net.Test.Net

/// fns testing modules of Active.Net.Net.IpInfo
module IpAddrTest =
    open NUnit.Framework
    open FsUnit
    open FsCheck
    open FsCheck.NUnit
    open Active.Net.TestUtil

    open System.Configuration
    open FSharp.Data
    open Active.Net.Net

    [<TestFixture>]
    type ``IpInfoTest `` () =
        [<TestFixtureSetUp>]
        /// register IPv4 IPAddresses, MAC addresses generators
        member x.``set up`` () = 
            Arb.register<NetGenerators>() |> ignore

        // properties are named self-explanatory
        
        [<QuietProperty>]
        member x.``Constructing MacAddress from its MacAddress.addressBytes returns the same MacAddress`` macAddr =
            MacAddress.addressBytes macAddr
            |> MacAddress.make
            |> should equal macAddr

        [<Test>] 
        member x.``Test MacAddress.startsWith with examples`` () =
            let macAddr = MacAddress.make [| 2uy; 3uy; 4uy; 5uy|]
            let emptyPrefix = MacAddress.make [||]
            let matchingPrefix1 = MacAddress.make [| 2uy |] // matches right away
            let matchingPrefix2 = MacAddress.make [| 2uy; 3uy |] // matches later
            let nonMatchingPrefix1 = MacAddress.make [| 1uy; |] // does not match right away
            let nonMatchingPrefix2 = MacAddress.make [| 2uy; 1uy |] // does not match later
            let nonMatchingPrefix3 = MacAddress.make [| 2uy; 3uy; 4uy; 5uy; 6uy |] // too long
            should equal (MacAddress.startsWith macAddr emptyPrefix) true
            should equal (MacAddress.startsWith macAddr matchingPrefix1) true
            should equal (MacAddress.startsWith macAddr matchingPrefix2) true
            should equal (MacAddress.startsWith macAddr nonMatchingPrefix1) false
            should equal (MacAddress.startsWith macAddr nonMatchingPrefix2) false

        [<QuietProperty>]
        member x.``MacAddress starts with every of its bytes, but not more`` macAddr =
            let bytes = MacAddress.addressBytes macAddr
            for i in 0 .. bytes.Length-1 do
                let prefix = Array.sub bytes 0 i
                             |> MacAddress.make
                MacAddress.startsWith macAddr prefix
                |> should equal true
            let longPrefix =  // one more byte
                Array.append bytes [|0uy|]
                |> MacAddress.make
            MacAddress.startsWith macAddr longPrefix
            |> should equal false

        [<QuietProperty>]
        member x.``MAC address toString, fromString are inverses`` macAddr =
            MacAddress.toString macAddr
            |> MacAddress.fromString
            |> should equal macAddr

        // cannot quite test MacAddress.fromInterface

        [<QuietProperty>]
        member x.``MAC address toJson, fromJson are inverses`` macAddr =
            MacAddress.toJson macAddr
            |> MacAddress.fromJson
            |> should equal macAddr

        [<QuietProperty>]
        member x.``IpAddress toBytes,fromBytes are inverses`` ipAddr =
            IpAddress.toByteArray ipAddr
            |> IpAddress.fromByteArray
            |> should equal ipAddr

        // IpAddress.fromUInt32 cannot fail

        [<QuietProperty>]
        member x.``IpAddressString matches any stringified IpAddress`` ipAddr =
            match IpAddress.toString ipAddr with
            | IpAddress.IPAddressString addr -> addr |> should equal ipAddr
            | addr -> Assert.Fail("IPAddressString did not match " + addr)

        [<QuietProperty>] 
        member x.``IpAddress toString`` ipAddr =
            let str = IpAddress.toString(ipAddr)
            System.Net.IPAddress.Parse(str)
            |> should equal (IpAddress.address ipAddr)

        [<QuietProperty>]
        member x.``IpAddress toString,fromString are inverses`` ipAddr =
            IpAddress.toString ipAddr
            |> IpAddress.fromString
            |> should equal ipAddr

        [<QuietProperty>] 
        member x.``IpAddress toJson, fromJson are inverses`` addr =
            let json = IpAddress.toJson addr
            (IpAddress.fromJson json) = addr

        [<QuietProperty>]
        member x.``Subnet popcount counts set bits`` b =
            // test with slow but 'obviously' correct implementation
            let rec numSetBits b =
                if b = 0uy then 0uy
                else if (b &&& 1uy) = 1uy
                     then 1uy + numSetBits (b >>> 1)
                     else numSetBits (b >>> 1)
                     
            Subnet.popcount b
            |> should equal (numSetBits b)

        [<Test>]
        member x.``prefixLength works for all prefixes`` () =
            // test if (prefixLength u) = n
            let test (u:uint32) (n:int) =
                Subnet.prefixLength (IpAddress.fromUInt32 u)
                |> should equal n
            // run test for all prefixes n .. 0; u starts with 2^n-1
            let rec prefix u n =
                if n >= 0
                then do test u n
                        prefix (u >>> 1) (n - 1)
            prefix System.UInt32.MaxValue 32 // has all bits set

        [<Test>]
        member x.``subnetMaskFromPrefixLength n creates mask with n bits set for all n = 0..32`` () =
            for n in 0..32 do
                Subnet.subnetMaskFromPrefixLength n
                |> Subnet.prefixLength
                |> should equal n

        [<Test>]
        member x.``subnetMaskFromPrefixLength n creates mask with lower bytes = 0 for all n = 1..32`` () =
            // n = 0 not covered by (n-1)/8+1 formula, but already tested in ``subnetMaskFromPrefixLength n creates mask with n bits set for all n = 0..32``
            for n in 1..32 do
                let numSetBytes = (n-1)/8+1
                let numUnsetBytes = 4 - numSetBytes
                let bytes = Subnet.subnetMaskFromPrefixLength n
                            |> IpAddress.toByteArray
                let zeroes = Array.create numUnsetBytes 0uy
                Array.sub bytes numSetBytes numUnsetBytes
                |> should equal zeroes

        [<QuietProperty>]
        member x.``cidr creates strings that are parseable by Subnet for all prefix lengths 0..32`` ipAddr =
            for n in 0..32 do
                Subnet.cidr ipAddr (Subnet.subnetMaskFromPrefixLength n)
                |> Subnet.fromString
                |> should equal (Subnet.makeCidr ipAddr n)

        [<QuietProperty>]
        member x.``networkAddress2 of a network address is idempotent`` ipAddr mask =
            let networkAddress = Subnet.networkAddress2 ipAddr mask
            Subnet.networkAddress2 networkAddress mask
            |> should equal networkAddress

        [<QuietProperty>]
        member x.``networkAddress after Subnet.make returns network address`` ipAddr mask =
            Subnet.make ipAddr mask
            |> Subnet.networkAddress
            |> should equal (Subnet.networkAddress2 ipAddr mask)

        [<QuietProperty>]
        member x.``subnetMask after Subnet.make returns subnet mask`` ipAddr mask =
            Subnet.make ipAddr mask
            |> Subnet.subnetMask
            |> should equal mask

        [<QuietProperty>]
        member x.``Subnet.makeCidr returns prefix mask`` ipAddr =
            for n in 0..32 do
                Subnet.makeCidr ipAddr n
                |> Subnet.subnetMask
                |> should equal (Subnet.subnetMaskFromPrefixLength n)

        [<QuietProperty>]
        member x.``Subnet's toJson, fromJson are inverses`` ipAddr mask =
            let subnet = Subnet.make ipAddr mask
            subnet
            |> Subnet.toJson
            |> Subnet.fromJson
            |> should equal subnet

        [<QuietProperty>]
        member x.``Subnet's toString, fromString are inverses`` ipAddr mask =
            let subnet = Subnet.make ipAddr mask
            subnet
            |> Subnet.toString
            |> Subnet.fromString
            |> should equal subnet
            
        [<QuietProperty>]
        member x.``Subnet.fromStrings and IpAddress.toString are inverses for each subnet field`` ipAddr mask =
            Subnet.fromStrings (IpAddress.toString ipAddr) (IpAddress.toString mask)
            |> should equal (Subnet.make ipAddr mask)

        // cannot test NetworkInterface module, as we cannot create System.Net.NetworkInterface structures

        [<TestFixtureTearDown>]
        member x.``tear down`` () = ()



