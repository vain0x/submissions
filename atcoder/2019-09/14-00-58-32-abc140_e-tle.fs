module Program

let listEq itemEq xs ys =
  let rec go xs ys =
    match xs, ys with
    | [], [] ->
      true

    | x :: xs, y :: ys ->
      itemEq x y && go xs ys

    | _ ->
      false

  go xs ys

let listReplicate item len =
  let rec go acc i =
    if i = len then
      acc
    else
      go (item :: acc) (i + 1)

  go [] 0

let unitObj = box ()

let intEq (first: int) (second: int) =
  first = second

let intMin (first: int) (second: int) =
  if second < first then second else first

let intMax (first: int) (second: int) =
  if first < second then second else first

// -----------------------------------------------
// Intervals
// -----------------------------------------------

type Interval = int * int

let intervalIsDisjoint (first: Interval) (second: Interval) =
  let xl, xr = first
  let yl, yr = second
  xr <= yl || yr <= xl

let intervalCovers (first: Interval) (second: Interval) =
  let xl, xr = first
  let yl, yr = second
  xl <= yl && yr <= xr

// -----------------------------------------------
// Traits
// -----------------------------------------------

type SegItemTypeTag =
  | SegItemTypeTag

let segItemTypeToAppend (append, _, _, (_: SegItemTypeTag)) = append

let segItemTypeToEmptyNode (_, emptyNode, _, (_: SegItemTypeTag)) = emptyNode

let segItemTypeToNoChildren (_, _, noChildren, (_: SegItemTypeTag)): obj = noChildren

// -----------------------------------------------
// Nodes
// -----------------------------------------------

type SegNodeTag =
  | SegNodeTag

let segNodeNewEmpty emptyItem =
  emptyItem, 0, (-1), unitObj, SegNodeTag

let segItemTypeNew emptyItem append =
  let emptyNode = segNodeNewEmpty emptyItem

  // Type unification.
  assert ([append emptyItem emptyItem; emptyItem] |> List.isEmpty |> not)

  let itemTy = append, emptyNode, box (emptyNode, emptyNode), SegItemTypeTag
  assert ([itemTy |> segItemTypeToAppend; append] |> List.isEmpty |> not)

  itemTy

let segNodeNewLeaf itemTy item =
  let noChildren = itemTy |> segItemTypeToNoChildren
  item, 1, 0, noChildren, SegNodeTag

let segNodeToItem node =
  let item, _, _, _, (_: SegNodeTag) = node
  item

let segNodeToLength node =
  let _, len, _, _, (_: SegNodeTag) = node
  len

let segNodeToHeight node =
  let _, _, height, _, (_: SegNodeTag) = node
  height

let segNodeToChildren node =
  let _, (len: int), (_: int), (children: obj), (_: SegNodeTag) = node
  assert (len >= 2)

  let left, right = unbox children

  // HACK: Unify the item type of children.
  assert ([node; left; right] |> List.isEmpty |> not)

  left, right

let segNodeIsEmpty node =
  segNodeToLength node = 0

let segNodeNew itemTy left right =
  // eprintfn "node new"
  match left |> segNodeToLength, right |> segNodeToLength with
  | 0, 0 ->
    itemTy |> segItemTypeToEmptyNode

  | 0, _ ->
    right

  | _, 0 ->
    left

  | _ ->
    let leftItem, leftLen, leftHeight, _, (_: SegNodeTag) = left
    let rightItem, rightLen, rightHeight, _, (_: SegNodeTag) = right
    assert (leftLen >= 1 && rightLen >= 1)

    let item = (itemTy |> segItemTypeToAppend) leftItem rightItem
    let len = leftLen + rightLen
    let height = 1 + intMax leftHeight rightHeight
    item, len, height, box (left, right), SegNodeTag

let segNodeWithLeft itemTy newLeft node =
  assert (segNodeToLength node >= 1)
  let _, right = node |> segNodeToChildren
  segNodeNew itemTy newLeft right

let segNodeWithRight itemTy newRight node =
  assert (segNodeToLength node >= 1)
  let left, _ = node |> segNodeToChildren
  segNodeNew itemTy left newRight

let segNodeMakeBalanced itemTy node =
  // from:
  //       T
  //      / \
  //     L   R
  //        / \
  //       X   Y
  // to:
  //        [R]
  //        / \
  //      [T]  Y
  //      / \
  //     L   X
  let rotateLeft node =
    if (node |> segNodeToLength) < 2 then
      node
    else

    let _, right = node |> segNodeToChildren
    if (right |> segNodeToLength) < 2 then
      node
    else

    let x, _ = right |> segNodeToChildren
    let u = node |> segNodeWithRight itemTy x
    let t = right |> segNodeWithLeft itemTy u
    t

  // from:
  //         T
  //        / \
  //       L   R
  //      / \
  //     X   Y
  // to:
  //      [L]
  //      / \
  //     X  [T]
  //        / \
  //       Y   R
  let rotateRight node =
    if (node |> segNodeToLength) < 2 then
      node
    else

    let left, _ = node |> segNodeToChildren
    if (left |> segNodeToLength) < 2 then
      node
    else

    let _, y = left |> segNodeToChildren
    let u = node |> segNodeWithLeft itemTy y
    let t = left |> segNodeWithRight itemTy u
    t

  let doubleLeft node =
    if (node |> segNodeToLength) < 2 then
      node
    else

    let _, right = node |> segNodeToChildren
    node |> segNodeWithRight itemTy (right |> rotateRight) |> rotateLeft

  let doubleRight node =
    if (node |> segNodeToLength) < 2 then
      node
    else

    let left, _ = node |> segNodeToChildren
    node |> segNodeWithLeft itemTy (left |> rotateLeft) |> rotateRight

  let toBalance node =
    if (node |> segNodeToLength) < 2 then
      0
    else
      let left, right = node |> segNodeToChildren
      segNodeToHeight right - segNodeToHeight left

  let selfBalance = node |> toBalance
  if (-1) <= selfBalance && selfBalance <= 1 then
    node
  else

  if (node |> segNodeToLength) < 2 then
    node
  else

  let left, right = node |> segNodeToChildren
  if selfBalance >= 2 then
    if (right |> toBalance) < 0 then
      node |> doubleLeft
    else
      node |> rotateLeft
  else
    if (left |> toBalance) > 0 then
      node |> doubleRight
    else
      node |> rotateRight

let segNodeReplicate itemTy len item =
  assert (len >= 0)

  let rec go len =
    if len = 0 then
      itemTy |> segItemTypeToEmptyNode
    else if len = 1 then
      segNodeNewLeaf itemTy item
    else
      let m = len / 2
      let left = go m
      let right = go (len - m)
      segNodeNew itemTy left right

  go len |> segNodeMakeBalanced itemTy

// -----------------------------------------------
// Trees
// -----------------------------------------------

type SegTreeTag =
  | SegTreeTag

let segTreeNew itemTy =
  let emptyNode = itemTy |> segItemTypeToEmptyNode
  itemTy, emptyNode, SegTreeTag

let segTreeReplicate itemTy len item =
  let root = segNodeReplicate itemTy len item
  itemTy, root, SegTreeTag

let segTreeToRoot self =
  let _, node, (_: SegTreeTag) = self
  node

let segTreeToLength self =
  self |> segTreeToRoot |> segNodeToLength

let segTreeToHeight self =
  self |> segTreeToRoot |> segNodeToHeight

let segTreeIsEmpty self =
  self |> segTreeToRoot |> segNodeIsEmpty

let segItemTypeToEmptyItem itemTy =
  itemTy |> segItemTypeToEmptyNode |> segNodeToItem

/// Gets an item at the index.
/// Error if out of range.
let segTreeGet index self =
  let rec go index node =
    let item, len, _, _, (_: SegNodeTag) = node
    if index < 0 || index >= len then
      printfn "ERROR: segNodeGet out of range (index = %d, len = %d)" index len
      exit 1

    assert (len <> 0)
    if len = 1 then
      item
    else

    let left, right = node |> segNodeToChildren
    let leftLen = segNodeToLength left
    if index < leftLen then
      go index left
    else
      go (index - leftLen) right

  self |> segTreeToRoot |> go index

/// Gets the sum of items at the index.
/// The end index is exclusive.
let segTreeSum (ql: int) (qr: int) self =
  let itemTy, root, (_: SegTreeTag) = self

  let rec go (e: Interval) (q: Interval) node =
    if intervalIsDisjoint q e then
      itemTy |> segItemTypeToEmptyItem
    else if intervalCovers q e then
      node |> segNodeToItem
    else
      assert (node |> segNodeIsEmpty |> not)
      let leftNode, rightNode = node |> segNodeToChildren

      let el, er = e
      let m = el + (leftNode |> segNodeToLength)

      let vl = leftNode |> go (el, m) q
      let vr = rightNode |> go (m, er) q
      (itemTy |> segItemTypeToAppend) vl vr

  let len = root |> segNodeToLength
  let q = ql, qr

  if intervalIsDisjoint q (0, len) then
    itemTy |> segItemTypeToEmptyItem
  else

  let e = 0, len
  root |> go e q

/// Replaces an item at the index.
/// Error if out of range.
let segTreeSet index newItem self =
  let itemTy, root, (_: SegTreeTag) = self

  let rec go index node =
    let _, len, height, children, (_: SegNodeTag) = node
    if index < 0 || index >= len then
      printfn "ERROR: segNodeSet out of range (index = %d, len = %d)" index len
      exit 1

    assert (len <> 0)
    if len = 1 then
      newItem, len, height, children, SegNodeTag
    else

    let left, right = node |> segNodeToChildren
    let leftLen = segNodeToLength left
    if index < leftLen then
      node |> segNodeWithLeft itemTy (left |> go index)
    else
      node |> segNodeWithRight itemTy (right |> go (index - leftLen))

  let root = root |> go index
  itemTy, root, SegTreeTag

/// Inserts an item at the index.
let segTreeInsert index newItem self =
  let itemTy, root, (_: SegTreeTag) = self

  let rec go index node =
    let oldItem, len, _, _, (_: SegNodeTag) = node
    assert (index <= len)

    if len = 0 then
      segNodeNewLeaf itemTy newItem
    else if len = 1 then
      let oldLeaf = segNodeNewLeaf itemTy oldItem
      let newLeaf = segNodeNewLeaf itemTy newItem
      let left, right =
        if index = 0 then
          newLeaf, oldLeaf
        else
          assert (index = 1)
          oldLeaf, newLeaf
      segNodeNew itemTy left right
    else
      let left, right = node |> segNodeToChildren

      let leftLen = segNodeToLength left
      let rightLen = segNodeToLength right
      assert (leftLen + rightLen = len)

      let left, right =
        if index < leftLen then
          let left = left |> go index
          assert (segNodeToLength left = leftLen + 1)
          left, right
        else
          let right = right |> go (index - leftLen)
          assert (segNodeToLength right = rightLen + 1)
          left, right

      segNodeNew itemTy left right |> segNodeMakeBalanced itemTy

  let root = root |> go index
  itemTy, root, SegTreeTag

/// Removes an item at the index.
let segTreeRemove index self =
  let itemTy, root, (_: SegTreeTag) = self

  let rec go index node =
    let _, len, _, _, (_: SegNodeTag) = node
    assert (index < len)

    assert (len <> 0)
    if len = 1 then
      itemTy |> segItemTypeToEmptyNode
    else
      let left, right = node |> segNodeToChildren

      let leftLen = segNodeToLength left
      let rightLen = segNodeToLength right
      assert (leftLen + rightLen = len)

      let node =
        if index < leftLen then
          if leftLen < 2 then
            right
          else
            node |> segNodeWithLeft itemTy (left |> go index)
        else
          if rightLen < 2 then
            left
          else
            node |> segNodeWithRight itemTy (right |> go (index - leftLen))

      node |> segNodeMakeBalanced itemTy

  let root = root |> go index
  itemTy, root, SegTreeTag

/// Inserts an item to the bottom.
let segTreePush value self =
  let index = self |> segTreeToLength
  self |> segTreeInsert index value

/// Removes the last item.
let segTreePop self =
  let index = (self |> segTreeToLength) - 1
  self |> segTreeRemove index

/// Creates a segment tree from the list.
let segTreeOfList itemTy xs =
  let rec go t xs =
    match xs with
    | [] ->
      t

    | x :: xs ->
      go (t |> segTreePush x) xs

  go (segTreeNew itemTy) xs

/// Creates a list from the segment tree.
let segTreeToList v =
  let len = v |> segTreeToLength

  let rec go acc i =
    if i = len then
      List.rev acc
    else
      let x = v |> segTreeGet i
      go (x :: acc) (i + 1)

  go [] 0

// -----------------------------------------------
// ABC 140 E
// -----------------------------------------------

// https://atcoder.jp/contests/abc140/tasks/abc140_e

let abc140eSolve n (perm: int[]) =
  let segItemTypeInt = segItemTypeNew 0 (fun _ _ -> 0)

  // add sentinels
  let perm =
    let rec go i t =
      // eprintfn "%d" i
      if i = n then
        t
      else
        t |> segTreeInsert i perm.[i] |> go (i + 1)

    segTreeNew segItemTypeInt
    |> go 0
    |> segTreeInsert 0 (n + 1)
    |> segTreePush (n + 2)
  assert (segTreeToLength perm = n + 2)

  // segment trees for processed items
  // let rec segTreeReplicate itemTy len item =
  //   let rec go i t =
  //     if i = len then
  //       t
  //     else
  //       t |> segTreeInsert i item |> go (i + 1)
  //   go 0 (segTreeNew itemTy)

  let prev =
    segTreeReplicate (segItemTypeNew 0 intMax) (n + 2) 0
  let next =
    segTreeReplicate (segItemTypeNew (n + 1) intMin) (n + 2) (n + 1)

  // inverse of perm
  let pos =
    let rec go pos i =
      if i = n + 2 then
        pos
      else
        let pos = pos |> segTreeSet (perm |> segTreeGet i) i
        go pos (i + 1)

    let pos =
      segTreeReplicate segItemTypeInt (n + 3) 0
    go pos 0

  let rec go sum prev next p =
    if p < 1 then
      sum
    else
      let i = pos |> segTreeGet p

      // if p % (n / 30) = 0 then
      //   eprintfn "p=%d prev=%d next=%d" p (prev |> segTreeToHeight) (next |> segTreeToHeight)

      let x = prev |> segTreeSum 0 i
      let w = prev |> segTreeSum 0 x
      assert (w <= x && x < i)

      let y = next |> segTreeSum (i + 1) (n + 2)
      let z = next |> segTreeSum (y + 1) (n + 2)
      assert (i < y && y <= z)

      let count = (x - w) * (y - i) + (i - x) * (z - y)
      let sum = (int64 sum) + (int64 (count * p))

      // printfn "p=%d (i=%d) w=%d x=%d y=%d z=%d" p i w x y z

      // mark i'th element as processed
      let prev = prev |> segTreeSet i i
      let next = next |> segTreeSet i i

      go sum prev next (p - 1)

  go 0L prev next n
  // (perm |> segTreeToLength)
  // + (prev |> segTreeToLength)
  // + (next |> segTreeToLength)

// let gen n =
//   let rand = System.Random()

//   let arraySwap i j (array: _[]) =
//     let t = array.[i]
//     array.[i] <- array.[j]
//     array.[j] <- t

//   let arrayShuffle array =
//     let rec go i =
//       if i > 0 then
//         let j = rand.Next(0, i + 1)
//         array |> arraySwap i j
//         go (i - 1)
//     go (n - 1)

//   let perm = Array.init n (fun x -> x + 1)
//   arrayShuffle perm
//   printfn "%d" n
//   printfn "%s" (System.String.Join(" ", perm))

[<EntryPoint>]
let main _ =
  let n = stdin.ReadLine() |> int
  let perm = stdin.ReadLine().Split(' ') |> Array.map int
  let m = abc140eSolve n perm
  printfn "%d" m
  0
