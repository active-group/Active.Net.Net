namespace Active.Net.Net

module Dns =
    // http://nf2p.com/dot-net/host-name-lookups-with-f/
    /// resolve multiple host names or ip addresses in parallel
    let getHostEntriesParallel : string seq -> System.Net.IPHostEntry option seq =
        Seq.map (fun host-> System.Net.Dns.BeginGetHostEntry(host,null,null))
        >> Seq.toArray
        >> Seq.map (fun asyncResult -> try Some <| System.Net.Dns.EndGetHostEntry(asyncResult) with e -> None)

    /// resolve host name or ip address
    let getHostEntry hostOrIpAddress : System.Net.IPHostEntry option =
        getHostEntriesParallel [hostOrIpAddress] |> Seq.head
