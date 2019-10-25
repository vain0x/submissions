module Program

// -----------------------------------------------
// Binary trees
// -----------------------------------------------

// type Tree<'a> =
//   | Leaf of 'a
//   | Node of size:int * Tree<'a> * Tree<'a>

type TreeTag =
  | TreeTag

type Tree = int * obj * TreeTag

let treeId (node: Tree) =
  node

let treeToLength (node: Tree) =
  let len, _, (_: TreeTag) = node
  len

let treeNewLeaf item =
  1, box item, TreeTag

let treeNewNode (left: Tree) (right: Tree) =
  let len = treeToLength left + treeToLength right
  len, box (left, right), TreeTag

let treeLeafToItem (node: Tree) =
  let len, payload, (_: TreeTag) = node
  assert (len = 1)
  unbox payload

let treeNodeToChildren (node: Tree) =
  let len, payload, (_: TreeTag) = node
  assert (len >= 2)
  let left, right = unbox payload
  assert ([node; left; right] |> List.isEmpty |> not)
  left, right

let treeGet index (node: Tree) =
  assert (0 <= index && index < (node |> treeToLength))

  let rec go index node =
    let len = node |> treeToLength
    if len = 1 then
      assert (index = 0)
      node |> treeLeafToItem
    else
      let left, right = node |> treeNodeToChildren
      let leftLen = left |> treeToLength
      if index < leftLen then
        go index left
      else
        go (index - leftLen) right

  go index node

let treeSet index newItem (node: Tree) =
  assert (0 <= index && index < (node |> treeToLength))

  let rec go index node =
    let len = node |> treeToLength
    if len = 1 then
      assert (index = 0)
      treeNewLeaf newItem
    else
      let left, right = node |> treeNodeToChildren
      let leftLen = left |> treeToLength
      let left, right =
        if index < leftLen then
          go index left, right
        else
          left, go (index - leftLen) right
      treeNewNode left right

  go index node

let treeToListAcc acc (node: Tree) =
  let rec go node acc =
    let len = node |> treeToLength
    if len = 1 then
      treeLeafToItem node :: acc
    else
      let left, right = node |> treeNodeToChildren
      acc |> go left |> go right

  go node acc

// -----------------------------------------------
// Zero-less binary digits
// -----------------------------------------------

// type Digit<'a> =
//   | One of Tree<'a>
//   | Two of Tree<'a> * Tree<'a>

[<RequireQualifiedAccess>]
type DigitTag =
  | One
  | Two

let digitNewOne (node: Tree) =
  box node, DigitTag.One

let digitNewTwo (left: Tree) (right: Tree) =
  assert (treeToLength left = treeToLength right)
  box (left, right), DigitTag.Two

let digitToTag digit =
  let _, (tag: DigitTag) = digit
  tag

let digitOneToNode digit: Tree =
  let payload, (tag: DigitTag) = digit
  match tag with
  | DigitTag.One ->
    unbox payload

  | DigitTag.Two ->
    failwith "NEVER"

let digitTwoToNodes digit: Tree * Tree =
  let payload, (tag: DigitTag) = digit
  match tag with
  | DigitTag.One ->
    failwith "NEVER"

  | DigitTag.Two ->
    unbox payload

let digitToLength digit =
  let _, (tag: DigitTag) = digit
  match tag with
  | DigitTag.One ->
    digit |> digitOneToNode |> treeToLength

  | DigitTag.Two ->
    let left, right = digit |> digitTwoToNodes
    treeToLength left + treeToLength right

// -----------------------------------------------
// Random access lists
// -----------------------------------------------

type RalTag =
  | RalTag

let ralNewEmpty defaultItem =
  0, [], defaultItem, RalTag

let ralIsEmpty ral =
  let _, digits, _, (_: RalTag) = ral
  digits |> List.isEmpty

let ralToLength ral =
  let len, _, _, (_: RalTag) = ral
  len

let ralVerify ral =
  let verify () =
    let len, digits, _, (_: RalTag) = ral

    let rec go w ds =
      match ds with
      | [] ->
        ()

      | d :: ds ->
        match d |> digitToTag with
        | DigitTag.One ->
          assert (d |> digitOneToNode |> treeToLength = w)
        | DigitTag.Two ->
          let left, right = d |> digitTwoToNodes
          assert (treeToLength left = w)
          assert (treeToLength right = w)

        go (w * 2) ds

    assert (digits |> List.sumBy digitToLength = len)
    go 1 digits
    true

  assert (verify ())
  ral

let ralPush item ral =
  let len, digits, defaultItem, (_: RalTag) = ral
  assert ([item; defaultItem] |> List.isEmpty |> not)

  let rec go t ds =
    match ds with
    | [] ->
      [digitNewOne t]

    | u :: ds ->
      match digitToTag u with
      | DigitTag.One ->
        digitNewTwo t (digitOneToNode u) :: ds

      | DigitTag.Two ->
        let u =
          let left, right = digitTwoToNodes u
          assert (treeToLength left = treeToLength t)
          assert (treeToLength right = treeToLength t)
          treeNewNode left right
        digitNewOne t :: go u ds

  let digits = go (treeNewLeaf item) digits
  let ral = len + 1, digits, defaultItem, RalTag
  ralVerify ral

// let ralPop ral =
//   let len, digits, defaultItem, (_: RalTag) = ral

//   let item, digits =
//     match digits with
//     | [] ->
//       assert false
//       defaultItem, []

//     | d :: ds ->
//       match d |> digitToTag with
//       | DigitTag.One ->
//         let item = d |> digitOneToNode |> treeLeafToItem defaultItem
//         item, ds

//       | DigitTag.Two ->
//         let left, right = d |> digitTwoToNodes
//         let item = left |> treeLeafToItem defaultItem
//         item, digitNewOne right :: ds

//   let ral = len - 1, digits, defaultItem, RalTag
//   item, ralVerify ral

let ralGet index ral =
  let len, digits, defaultItem, (_: RalTag) = ral
  assert (0 <= index && index < len)

  let rec go index digits =
    match digits with
    | [] ->
      assert false
      defaultItem

    | d :: ds ->
      let digitLen = d |> digitToLength
      if index < digitLen then
        match d |> digitToTag with
        | DigitTag.One ->
          d |> digitOneToNode |> treeGet index

        | DigitTag.Two ->
          let left, right = d |> digitTwoToNodes
          let leftLen = left |> treeToLength
          if index < leftLen then
            left |> treeGet index
          else
            right |> treeGet (index - leftLen)
      else
        go (index - digitLen) ds

  let index = len - index - 1
  go index digits

let ralSet index newItem ral =
  let len, digits, defaultItem, (_: RalTag) = ral
  assert (0 <= index && index < len)

  let rec go index digits =
    match digits with
    | [] ->
      assert false
      digits

    | d :: ds ->
      let digitLen = d |> digitToLength
      if index < digitLen then
        match d |> digitToTag with
        | DigitTag.One ->
          let t = d |> digitOneToNode |> treeSet index newItem
          digitNewOne t :: ds

        | DigitTag.Two ->
          let left, right = d |> digitTwoToNodes
          let leftLen = left |> treeToLength
          let left, right =
            if index < leftLen then
              let left = left |> treeSet index newItem
              left, right
            else
              let right = right |> treeSet (index - leftLen) newItem
              left, right
          digitNewTwo left right :: ds
      else
        d :: go (index - digitLen) ds

  let index = len - index - 1
  let digits = go index digits
  let ral = len, digits, defaultItem, RalTag
  ralVerify ral

let ralReplicate len item =
  let rec go i ral =
    if i = len then
      ral
    else
      ral |> ralPush i |> go (i + 1)

  go 0 (ralNewEmpty item)

let ralToList ral =
  let rec go digits acc =
    match digits with
    | [] ->
      acc

    | d :: ds ->
      match d |> digitToTag with
      | DigitTag.One ->
        d |> digitOneToNode |> treeToListAcc acc |> go ds

      | DigitTag.Two ->
        let left, right = d |> digitTwoToNodes
        let acc = treeToListAcc acc left
        let acc = treeToListAcc acc right
        acc |> go ds

  let _, digits, defaultItem, (_: RalTag) = ral
  let acc = []
  assert ([acc; [defaultItem]] |> List.isEmpty |> not)
  acc |> go digits

// -----------------------------------------------
// ABC 140 E
// -----------------------------------------------

// https://atcoder.jp/contests/abc140/tasks/abc140_e

let i64Zero = 0, 0

let rec i64AddInt (value: int) self =
  let lo, hi = self

  if value < 500000000 && lo < 500000000 then
    lo + value, hi
  else

  let vhi = value / 1000000000
  let vlo = value % 1000000000
  let lo = lo + vlo

  let whi = lo / 1000000000
  let wlo = lo % 1000000000
  wlo, hi + vhi + whi

let i64ToString self =
  let lo, hi = self
  if hi = 0 then
    string lo
  else
    let lo =
      let s = "000000000" + string lo
      s.[s.Length - 9..s.Length - 1]
    string hi + lo

let abc140eSolve n perm =
  let prev, next =
    let rec go i prev next =
      if i > n then
        prev, next
      else
        let prev = prev |> ralPush i
        let next = next |> ralPush i
        go (i + 1) prev next

    let prev = ralNewEmpty 0 |> ralPush 0 |> ralPush 0
    let next = ralNewEmpty 0
    let prev, next = go 1 prev next
    let next = next |> ralPush (n + 1) |> ralPush (n + 1)
    prev, next

  // inverse of perm
  let pos =
    let rec go i perm pos =
      match perm with
      | [] ->
        pos

      | p :: perm ->
        pos |> ralSet (p - 1) (i + 1) |> go (i + 1) perm

    let pos = ralReplicate n 0
    pos |> go 0 perm

  let rec go sum prev next p =
    if p = n then
      sum
    else
      let i = pos |> ralGet p

      let x = prev |> ralGet i
      let w = prev |> ralGet x
      assert (w <= x && x < i)

      let y = next |> ralGet i
      let z = next |> ralGet y
      assert (i < y && y <= z)

      let count = (x - w) * (y - i) + (i - x) * (z - y)
      let sum = sum |> i64AddInt (count * (p + 1))

      let prev = prev |> ralSet y x
      let next = next |> ralSet x y
      go sum prev next (p + 1)

  go i64Zero prev next 0

[<EntryPoint>]
let main _ =
  let n = stdin.ReadLine() |> int
  let perm = stdin.ReadLine().Split(' ') |> Array.map int |> Array.toList
  let m = abc140eSolve n perm
  printfn "%s" (m |> i64ToString)
  0
