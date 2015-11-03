namespace Active.Net.Net

/// helper fns for System.Net.IPAddress, 
/// plus wrapper object IpAddress, so that it can be used as a map key (needs IComparable)
module IpAddress =
    open System
    open System.Text
    open System.Net
    open System.Net.NetworkInformation
    open FSharp.Data

    /// wrapper type to have comparable IPAddress for map key (?)
    [<CustomComparison>]
    [<CustomEquality>]
//    [<StructuralEquality>]
    type T = {address:IPAddress}
        with
        override x.Equals(y) =
            match y with
            | :? T as y -> x.address = y.address
            | _ -> false
        override x.GetHashCode() = x.address.GetHashCode()
        interface System.IComparable with
            member x.CompareTo y =
                let compareBytes (a:byte[]) (b:byte[]) =
                    if a < b then -1
                    else if a > b then 1
                         else 0
                match y with
                | :? T as y ->
                    compareBytes (x.address.GetAddressBytes()) (y.address.GetAddressBytes())
                | _ -> failwithf "uncomparable: %A with %A" x y
    /// create IpAddress.T from System.Net.IPAddress
    let make (address:IPAddress) = {address=address}
    /// System.Net.IPAddress of IpAddress.T
    let address (x:T) = x.address
    /// address bytes of IpAddress.T
    let toByteArray (x:T) = x.address.GetAddressBytes()
    /// create IpAddress.T from uint32
    let fromUInt32 (address:uint32) = new IPAddress((int64) address) |> make
    /// create IpAddress.T from byte array (4 bytes for IPv4)
    let fromByteArray (bs:byte[]) = new IPAddress(bs) |> make

    /// matches strings that represent ip address in human notation
    // partial active pattern, as not all strings are ip addresses
    let (|IPAddressString|_|) (s:string) : T option =
        let mutable ipAddress = new IPAddress(0L)
        if IPAddress.TryParse(s, &ipAddress)
        then Some <| make ipAddress
        else None
    /// IpAddress.T from string (eg. "127.0.0.1")
    let fromString (addr:string) = IPAddress.Parse addr |> make
    /// string representation of IpAddress.T (eg. "127.0.0.1")
    let toString (addr:T) = (address addr).ToString()
    /// json representation of IpAddress.T (a json string)
    let toJson(ipAddr:T) : JsonValue =
        toString ipAddr |> JsonValue.String 
    /// IpAddress.T from its json representation
    let fromJson(j:JsonValue) : T =
        j.AsString() |> fromString

