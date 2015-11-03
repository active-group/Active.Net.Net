namespace Active.Net.Net

/// helper fns for MacAddress (System.Net.NetworkInformation.PhysicalAddress)
module MacAddress =
    open System
    open FSharp.Data
    open System.Net.NetworkInformation
    type T = PhysicalAddress

    /// create MacAddress.T from byte array (usually 6 bytes, but that is not enforced)
    let make bs = new PhysicalAddress(bs)
    
    /// array of bytes representing MAC address
    let addressBytes (macAddr:T) = macAddr.GetAddressBytes()

    /// true, iff macAddr has the same first bytes as prefix
    let startsWith (macAddr:T) (prefix:T) =
        let macAddrBytes = addressBytes macAddr
        let prefixBytes = addressBytes prefix
        Array.sub macAddrBytes 0 (min (Array.length macAddrBytes) (Array.length prefixBytes)) = prefixBytes

    /// MAC address as string, eg. 0029CF
    let toString (macAddr:T) : string = macAddr.ToString()
    /// MAC address from string, eg. 00-29-CF or 0029CF; only uppercase recognized
    let fromString (s:string) : T = PhysicalAddress.Parse(s)
    /// MAC address of a NetworkInterface
    let fromInterface (iface:NetworkInterface) =
        iface.GetPhysicalAddress() : T

    /// Json representation of MAC address (simple string)
    let toJson (macAddr:T) : JsonValue =
        toString macAddr |> JsonValue.String

    /// MAC address from json representation
    let fromJson (j:JsonValue) : T =
        j.AsString() |> fromString

