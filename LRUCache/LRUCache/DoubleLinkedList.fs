namespace DoubleLinkedList

module DoubleLinkedNode = 
    type DoubleLinkedNode<'a> = 
        {
            mutable Previous : DoubleLinkedNode<'a> option;
            mutable Next : DoubleLinkedNode<'a> option;
            mutable Value : 'a;
        }
    let remove (node:DoubleLinkedNode<'a>) = 
        match node with
        | { Previous = None; Next = None; Value = x} -> ()
        | { Previous = Some(p); Next = None; Value = x} -> 
            p.Next <- None
        | { Previous = None; Next = Some(n); Value = x} -> 
            n.Previous <- None
        | { Previous = Some(p); Next = Some(n); Value = x} -> 
            p.Next <- Some(n)
            n.Previous <- Some(p)
   
open DoubleLinkedNode
module DoubleLinkedList = 

    type DoubleLinkedList<'a> = { Head:DoubleLinkedNode<'a> option; Tail:DoubleLinkedNode<'a> option; }

    let empty = {Head = None; Tail = None;}

    let isEmpty dl = dl = empty

    let addHead item (dl:DoubleLinkedList<'a>) = 
        match dl with
        | {Head=None;Tail=None} -> 
            let newNode = {Previous = None; Next=None; Value=item}
            {Head=Some(newNode);Tail=Some(newNode)}
        | {Head = Some(n); Tail = l} ->
            let newNode = {Previous = None; Next=Some(n);Value=item}
            n.Previous <- Some(newNode)
            {Head=Some(newNode);Tail=l}
        | {Head = None; Tail = Some(_)} -> failwith "Impossible"

    let removeLast (dl:DoubleLinkedList<'a>) = 
        match dl with
        | {Head=None;Tail=None} -> {Head=None;Tail=None} 
        | {Head = n; Tail = Some(l)} ->
            match l.Previous with
            | None -> {Head=None;Tail=None} 
            | Some(t) -> 
                t.Next <- None
                {Head=n;Tail=Some(t)}
        | {Head = Some(_); Tail = None} -> failwith "Impossible"

    let removeNode (node:DoubleLinkedNode<'a>) (dl:DoubleLinkedList<'a>) = 
        let isEqual = LanguagePrimitives.PhysicalEquality
        let result = match node, dl with
                     | _, {Head=None;Tail=None} -> {Head=None;Tail=None}
                     | node, {Head=Some(h);Tail=Some(t)} when isEqual node h && isEqual node t -> {Head=None;Tail=None}
                     | node, {Head=Some(h);Tail=Some(t)} when not(isEqual node h) && not(isEqual node t)  -> {Head = Some(h); Tail = Some(t)}
                     | node, {Head=Some(h);Tail=Some(t)} when not(isEqual node h) && isEqual node t -> {Head = Some(h); Tail = t.Previous}
                     | node, {Head=Some(h);Tail=Some(t)} when isEqual node h && not(isEqual node t) -> {Head = h.Next; Tail = Some(t)}
                     | _,_ ->failwith "Impossible"
        DoubleLinkedNode.remove node
        result

    let rec toListFromHead (dl:DoubleLinkedList<'a>) = 
        match dl with
        | {Head=None;Tail=_} -> []
        | {Head = Some(n); Tail = t} -> n.Value :: toListFromHead {Head = n.Next; Tail = t }
