namespace DoubleLinkedList
open DoubleLinkedList
open DoubleLinkedNode
module LRUCache = 
    type LRUCache<'a> when 'a:equality = 
        {
            mutable Map : Map<string,DoubleLinkedNode<(string * 'a)>>;
            mutable List :  DoubleLinkedList<(string * 'a)>;
            Capacity: int
        }
    with
        static member create capacity = 
            {Map = Map.empty; List = DoubleLinkedList.empty; Capacity = capacity}
        member this.Get key :'a option = 
            let node = Map.tryFind key this.Map
            match node with
            | None -> None
            | Some(n) -> 
                this.List<- DoubleLinkedList.removeNode n this.List
                this.List <-DoubleLinkedList.addHead n.Value this.List
                this.Map <- Map.add key this.List.Head.Value this.Map
                let (_,value) = n.Value
                Some(value)
        member this.Put key (value:'a) = 
            let item = this.Get key
            match item with
            | Some(v) -> this.List.Head.Value.Value <- (key, value)
            | None -> 
                this.List <- DoubleLinkedList.addHead (key, value) this.List
                this.Map <- Map.add key this.List.Head.Value this.Map
            if Map.count this.Map > this.Capacity then
                let (lastKey,_) = this.List.Tail.Value.Value
                this.Map <- Map.remove lastKey this.Map
                this.List <- DoubleLinkedList.removeLast this.List
                
