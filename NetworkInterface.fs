namespace Active.Net.Net

/// hellper fns for System.Net.NetworkInformation.NetworkInterface
module NetworkInterface =
    open System.Net.NetworkInformation
    type T = NetworkInterface

    /// all network interfaces
    let getAll () : T[] = NetworkInterface.GetAllNetworkInterfaces()
    /// all UnicastIPAddressInformation of interface
    let getUnicastIPAddressesInformations (iface:T) =
        let props = iface.GetIPProperties()
        seq { for addr in props.UnicastAddresses do yield addr }
        |> Array.ofSeq
    /// All IPv4 subnets of the interface (that is, IP network address &&& netmask)
    let networkAddresses (iface:T) =
        getUnicastIPAddressesInformations iface
        |> Array.filter (fun ucast ->
            let address = ucast.Address
            address.AddressFamily = System.Net.Sockets.AddressFamily.InterNetwork (* v4 *))
        |> Array.map (fun ucast -> 
            let address = IpAddress.make ucast.Address
            let mask    = IpAddress.make ucast.IPv4Mask
            Subnet.make address mask)
    /// true iff network interface is up
    let isUp (iface:T) = iface.OperationalStatus = OperationalStatus.Up
    /// true iff network interface is loopback
    let isLoopback (iface:T) = iface.NetworkInterfaceType = NetworkInterfaceType.Loopback
    /// true iff network interface is ethernet
    let isEthernet (iface:T) =
        match iface.NetworkInterfaceType with
        | NetworkInterfaceType.Ethernet
        (* 
        //other "Ethernet" types:
        | NetworkInterfaceType.Ethernet3Megabit
        | NetworkInterfaceType.FastEthernetFx
        | NetworkInterfaceType.FastEthernetT
        | NetworkInterfaceType.GigabitEthernet
        *)
         -> true
        | _ -> false
        
    /// event that is triggered when network information changes
    let networkAddressChanged = NetworkChange.NetworkAddressChanged

    /// event that is triggerd when the network connectivity changes
    let networkAvailibilityChanged = NetworkChange.NetworkAvailabilityChanged
