namespace UnitTestProject1
open Microsoft.VisualStudio.TestTools.UnitTesting
open DoubleLinkedList
open LRUCache

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member this.AddHead () =
        let mutable list = DoubleLinkedList.empty
        Assert.AreEqual(([]:int list), (DoubleLinkedList.toListFromHead list));
        list <- DoubleLinkedList.addHead 1 list
        Assert.AreEqual([1], (DoubleLinkedList.toListFromHead list));
        list <- DoubleLinkedList.addHead 2 list
        Assert.AreEqual([2;1], (DoubleLinkedList.toListFromHead list));

    [<TestMethod>]
    member this.RemoveLast () =
        let mutable list = DoubleLinkedList.empty
        list <- DoubleLinkedList.addHead 1 list
        list <- DoubleLinkedList.addHead 2 list
        list <- DoubleLinkedList.removeLast list
        Assert.AreEqual([2], (DoubleLinkedList.toListFromHead list));
        list <- DoubleLinkedList.removeLast list
        Assert.AreEqual(([]:int list), (DoubleLinkedList.toListFromHead list));
        list <- DoubleLinkedList.removeLast list
        Assert.AreEqual(([]:int list), (DoubleLinkedList.toListFromHead list));

    [<TestMethod>]
    member this.RemoveNode () =
        let mutable list = DoubleLinkedList.empty
        list <- DoubleLinkedList.addHead 1 list
        let lastNode = list.Head
        list <- DoubleLinkedList.addHead 2 list
        let middleNode = list.Head
        list <- DoubleLinkedList.addHead 3 list
        let firstNode = list.Head

        list <- DoubleLinkedList.removeNode middleNode.Value list
        Assert.AreEqual([3;1], (DoubleLinkedList.toListFromHead list));
        list <- DoubleLinkedList.removeNode lastNode.Value list
        Assert.AreEqual([3], (DoubleLinkedList.toListFromHead list));
        list <- DoubleLinkedList.removeNode firstNode.Value list
        Assert.AreEqual(([]:int list), (DoubleLinkedList.toListFromHead list));

    [<TestMethod>]
    member this.LRUAddRemove () =
        let lruCache = LRUCache<_>.create 3
        lruCache.Put "hello" 10
        lruCache.Put "hello2" 11
        lruCache.Put "hello3" 12
        Assert.AreEqual(Some(10), lruCache.Get "hello")
        lruCache.Put "hello4" 13
        Assert.AreEqual(None, lruCache.Get "hello2")
        Assert.AreEqual(Some(13), lruCache.Get "hello4")
        lruCache.Put "hello4" 15
        Assert.AreEqual(Some(15), lruCache.Get "hello4")
        Assert.AreEqual(Some(10), lruCache.Get "hello")
        Assert.AreEqual(Some(12), lruCache.Get "hello3")
        lruCache.Put "hello5" 20
        Assert.AreEqual(None, lruCache.Get "hello4")
        Assert.AreEqual(Some(10), lruCache.Get "hello")
        Assert.AreEqual(Some(12), lruCache.Get "hello3")
        Assert.AreEqual(Some(20), lruCache.Get "hello5")
