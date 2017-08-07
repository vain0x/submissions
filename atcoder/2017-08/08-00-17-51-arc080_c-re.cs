using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

public sealed class BinaryHeap<TValue>
    : IReadOnlyCollection<TValue>
{
    readonly List<TValue> list;
    readonly Func<TValue, TValue, int> compare;

    public int Count
    {
        get { return list.Count; }
    }

    public IEnumerator<TValue> GetEnumerator()
    {
        return list.GetEnumerator();
    }

    System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
    {
        return GetEnumerator();
    }

    public TValue Peek()
    {
        return list[0];
    }

    public void Enqueue(TValue value)
    {
        list.Add(value);
        var i = list.Count - 1;
        while (i > 0)
        {
            // Index of the parent.
            var p = (i - 1) >> 1;

            if (compare(list[p], value) <= 0) break;
            list[i] = list[p];
            i = p;
        }
        list[i] = value;
    }

    public TValue Dequeue()
    {
        var min = list[0];
        var x = list[list.Count - 1];
        var i = 0;
        while (true)
        {
            // Index of children.
            var l = (i << 1) + 1;
            var r = (i << 1) + 2;
            if (l >= list.Count) break;

            // Index of the smaller child.
            var c = r < list.Count && compare(list[r], list[l]) < 0 ? r : l;

            if (compare(list[c], x) >= 0) break;
            list[i] = list[c];
            i = c;
        }
        list[i] = x;
        list.RemoveAt(list.Count - 1);
        return min;
    }

    public BinaryHeap(List<TValue> list, Func<TValue, TValue, int> compare)
    {
        this.list = list;
        this.compare = compare;
    }
}

public static class BinaryHeap
{
    public static BinaryHeap<X> Create<X>(Func<X, X, int> compare)
    {
        return new BinaryHeap<X>(new List<X>(), compare);
    }

    public static BinaryHeap<X> Create<X>()
    {
        return new BinaryHeap<X>(new List<X>(), Comparer<X>.Default.Compare);
    }

    public static BinaryHeap<X> FromEnumerable<X>(IEnumerable<X> xs, Func<X, X, int> compare)
    {
        var list = new List<X>(xs);
        list.Sort(new Comparison<X>(compare));
        return new BinaryHeap<X>(list, compare);
    }

    public static BinaryHeap<X> FromEnumerable<X>(IEnumerable<X> xs)
    {
        return FromEnumerable(xs, Comparer<X>.Default.Compare);
    }
}


public interface ISlicableSegmentTree<T>
    : IReadOnlyList<T>
    , IList<T>
{
    new int Count { get; }
    new T this[int index] { get; set; }

    T Query(int index, int count);
    T Query();

    ISlicableSegmentTree<T> Slice(int index, int count);
}

public abstract class SlicableSegmentTreeBase<T>
    : IReadOnlyList<T>
    , IList<T>
    , System.Collections.IList
{
    readonly IList<T> list;

    static readonly bool isNullableType = default(T) == null;

    private static bool IsCompatibleObject(object value)
    {
        return value is T || (value == null && isNullableType);
    }

    public int Count
    {
        get
        {
            return list.Count;
        }
    }

    public T this[int index]
    {
        get
        {
            return list[index];
        }
        set
        {
            if (index < 0 || index >= list.Count) throw new ArgumentOutOfRangeException("index");
            SetItem(index, value);
        }
    }

    public void Add(T item)
    {
        int index = list.Count;
        InsertItem(index, item);
    }

    public void Clear()
    {
        ClearItems();
    }

    public void CopyTo(T[] array, int index)
    {
        list.CopyTo(array, index);
    }

    public bool Contains(T item)
    {
        return list.Contains(item);
    }

    public IEnumerator<T> GetEnumerator()
    {
        return list.GetEnumerator();
    }

    public int IndexOf(T item)
    {
        return list.IndexOf(item);
    }

    public void Insert(int index, T item)
    {
        if (index < 0 || index > list.Count) throw new ArgumentOutOfRangeException("index");

        InsertItem(index, item);
    }

    public bool Remove(T item)
    {
        int index = list.IndexOf(item);
        if (index < 0) return false;
        RemoveItem(index);
        return true;
    }

    public void RemoveAt(int index)
    {
        if (index < 0 || index >= list.Count) throw new ArgumentOutOfRangeException("index");

        RemoveItem(index);
    }

    protected virtual void ClearItems()
    {
        list.Clear();
    }

    protected virtual void InsertItem(int index, T item)
    {
        list.Insert(index, item);
    }

    protected virtual void RemoveItem(int index)
    {
        list.RemoveAt(index);
    }

    protected virtual void SetItem(int index, T item)
    {
        list[index] = item;
    }

    bool ICollection<T>.IsReadOnly
    {
        get
        {
            return list.IsReadOnly;
        }
    }

    System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
    {
        return ((System.Collections.IEnumerable)list).GetEnumerator();
    }

    bool System.Collections.ICollection.IsSynchronized
    {
        get { return false; }
    }

    object System.Collections.ICollection.SyncRoot
    {
        get
        {
            throw new NotSupportedException();
        }
    }

    void System.Collections.ICollection.CopyTo(Array array, int index)
    {
        var count = list.Count;

        for (int i = 0; i < count; i++)
        {
            array.SetValue(list[i], index + i);
        }
    }

    object System.Collections.IList.this[int index]
    {
        get
        {
            return list[index];
        }
        set
        {
            this[index] = (T)value;
        }
    }

    bool System.Collections.IList.IsReadOnly
    {
        get
        {
            return list.IsReadOnly;
        }
    }

    bool System.Collections.IList.IsFixedSize
    {
        get
        {
            return true;
        }
    }

    int System.Collections.IList.Add(object value)
    {
        Add((T)value);
        return Count - 1;
    }

    bool System.Collections.IList.Contains(object value)
    {
        return IsCompatibleObject(value) && Contains((T)value);
    }

    int System.Collections.IList.IndexOf(object value)
    {
        return IsCompatibleObject(value) ? IndexOf((T)value) : -1;
    }

    void System.Collections.IList.Insert(int index, object value)
    {
        Insert(index, (T)value);
    }

    void System.Collections.IList.Remove(object value)
    {
        if (IsCompatibleObject(value))
        {
            Remove((T)value);
        }
    }

    public SlicableSegmentTreeBase(IList<T> list)
    {
        if (list == null) throw new ArgumentNullException("list");
        this.list = list;
    }
}

[System.Diagnostics.DebuggerDisplay("Count = {Count}")]
public sealed class ConcreteSlicableSegmentTree<T>
    : SlicableSegmentTreeBase<T>
    , ISlicableSegmentTree<T>
{
    [System.Diagnostics.DebuggerDisplay("Count = {Count}")]
    sealed class SliceTree
        : SlicableSegmentTreeBase<T>
        , ISlicableSegmentTree<T>
    {
        readonly ConcreteSlicableSegmentTree<T> parent;
        readonly int offset;
        readonly int itemCount;

        public T Query(int index, int count)
        {
            if (index < 0 || index > itemCount) throw new ArgumentOutOfRangeException("index");
            if (count < 0 || index + count > itemCount) throw new ArgumentOutOfRangeException("count");
            return parent.Query(offset + index, count);
        }

        public T Query()
        {
            return parent.Query(offset, itemCount);
        }

        public ISlicableSegmentTree<T> Slice(int index, int count)
        {
            if (index < 0 || index > itemCount) throw new ArgumentOutOfRangeException("index");
            if (count < 0 || index + count > itemCount) throw new ArgumentOutOfRangeException("count");
            return parent.Slice(offset + index, count);
        }

        public SliceTree(ConcreteSlicableSegmentTree<T> parent, int offset, int itemCount, IList<T> segment)
            : base(segment)
        {
            this.parent = parent;
            this.offset = offset;
            this.itemCount = itemCount;
        }
    }

    readonly T empty;
    readonly Func<T, T, T> append;

    readonly T[] nodes;
    readonly int height;
    readonly int itemCount;

    static int NodeIndexFromItemIndex(int k, int height)
    {
        return (1 << height) - 1 + k;
    }

    int NodeIndexFromItemIndex(int k)
    {
        return NodeIndexFromItemIndex(k, height);
    }

    System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
    {
        return GetEnumerator();
    }

    protected override void SetItem(int index, T item)
    {
        base.SetItem(index, item);

        var i = NodeIndexFromItemIndex(index);

        while (i != 0)
        {
            var parentIndex = (i - 1) / 2;
            var childIndex = parentIndex * 2 + 1;
            nodes[parentIndex] = append(nodes[childIndex], nodes[childIndex + 1]);
            i = parentIndex;
        }
    }

    /// <summary>
    /// Executes a query.
    /// </summary>
    /// <param name="i">The current node's index (in <see cref="nodes"/>).</param>
    /// <param name="nl">The begin of the range corresponding to the current node.</param>
    /// <param name="nr">The end of the range corresponding to the current node.</param>
    /// <param name="ql">The begin of the query range.</param>
    /// <param name="qr">The end of the query range.</param>
    /// <returns></returns>
    T QueryCore(int i, int nl, int nr, int ql, int qr)
    {
        // The query range is in the node range.
        Debug.Assert(nl <= ql && ql < qr && qr <= nr);

        // Middle index of the node range.
        var m = nl + (nr - nl + 1) / 2;

        if (nl == ql && nr == qr)
        {
            return nodes[i];
        }
        else if (qr <= m)
        {
            // Forward to the left subtree because it contains the query range.
            return QueryCore(i * 2 + 1, nl, m, ql, qr);
        }
        else if (m <= ql)
        {
            // Forward to the right subtree.
            return QueryCore(i * 2 + 2, m, nr, ql, qr);
        }
        else
        {
            // Divide the query range at the middle index,
            // evaluate subqueries and merge results.
            Debug.Assert(ql < m && m < qr);

            var l = QueryCore(i * 2 + 1, nl, m, ql, m);
            var r = QueryCore(i * 2 + 2, m, nr, m, qr);
            return append(l, r);
        }
    }

    public T Query(int index, int count)
    {
        if (index < 0 || index > itemCount) throw new ArgumentOutOfRangeException("index");
        if (count < 0 || index + count > itemCount) throw new ArgumentOutOfRangeException("count");
        if (count == 0) return empty;
        return QueryCore(0, 0, 1 << height, index, index + count);
    }

    public T Query()
    {
        return nodes[0];
    }

    public ISlicableSegmentTree<T> Slice(int index, int count)
    {
        if (index < 0 || index > itemCount) throw new ArgumentOutOfRangeException("index");
        if (count < 0 || index + count > itemCount) throw new ArgumentOutOfRangeException("count");

        if (count == 0)
        {
            throw new NotSupportedException("Empty segment tree is not supported.");
        }

        if (index == 0 && itemCount == count)
        {
            return this;
        }

        return
            new SliceTree(
                this,
                index,
                count,
                new ArraySegment<T>(nodes, NodeIndexFromItemIndex(index), count)
            );
    }

    public ConcreteSlicableSegmentTree(T[] nodes, int height, int itemCount, T empty, Func<T, T, T> append)
        : base(new ArraySegment<T>(nodes, NodeIndexFromItemIndex(0, height), itemCount))
    {
        this.nodes = nodes;
        this.height = height;
        this.itemCount = itemCount;
        this.empty = empty;
        this.append = append;
    }
}

public static class SlicableSegmentTree
{
    static int CalculateHeight(int count)
    {
        var h = 0;
        while ((1 << h) < count) h++;
        return h;
    }

    public static ISlicableSegmentTree<X> Create<X>(IEnumerable<X> items, X empty, Func<X, X, X> append)
    {
        var buffer = items as IList<X> ?? items.ToList();
        var itemCount = buffer.Count;
        if (itemCount == 0)
        {
            throw new NotSupportedException("Empty segment tree is not supported.");
        }

        var height = CalculateHeight(itemCount);
        var nodes = new X[(1 << height) * 2 - 1];
        buffer.CopyTo(nodes, (1 << height) - 1);

        for (var i = (1 << height) - 2; i >= 0; i--)
        {
            var l = i * 2 + 1;
            var r = i * 2 + 2;
            nodes[i] = append(nodes[l], nodes[r]);
        }

        return new ConcreteSlicableSegmentTree<X>(nodes, height, itemCount, empty, append);
    }

    public struct Option<T>
    {
        public readonly T Value;
        public readonly bool HasValue;

        public static Option<T> None
        {
            get { return new Option<T>(); }
        }

        public Option(T value)
        {
            Value = value;
            HasValue = true;
        }
    }

    public static ISlicableSegmentTree<Option<X>> FromSemigroup<X>(IEnumerable<X> items, Func<X, X, X> append)
    {
        return
            Create(
                items.Select(x => new Option<X>(x)),
                Option<X>.None,
                (xo, yo) =>
                    xo.HasValue && yo.HasValue
                        ? new Option<X>(append(xo.Value, yo.Value))
                        : Option<X>.None
            );
    }
}


public sealed class Cell
    : Tuple<int, int>
{
    public int Value => Item1;
    public int Index => Item2;

    public Cell(int value, int index)
        : base(value, index)
    {
    }

    public static readonly Cell Max = new Cell(int.MaxValue, int.MaxValue);
}

public static class SegmentTreeExtension
{
    public static ISlicableSegmentTree<Cell> ToSegmentTree(this IEnumerable<Cell> items)
    {
        return
            SlicableSegmentTree.Create(
                items,
                Cell.Max,
                (x, y) => Comparer<Cell>.Default.Compare(x, y) > 0 ? y : x
            );
    }
}

public sealed class Range
    : IComparable<Range>
{
    readonly ArraySegment<Cell> segment;

    public int Offset => segment.Offset;

    readonly ISlicableSegmentTree<Cell> evenSide;
    readonly ISlicableSegmentTree<Cell> oddSide;

    public int CompareTo(Range other)
    {
        return Comparer<Cell>.Default.Compare(evenSide.Query(), other.evenSide.Query());
    }

    void AddSlice(int index, int count, BinaryHeap<Range> heap)
    {
        if (evenSide.Query(index, count) == Cell.Max) return;

        if (index % 2 == 0)
        {
            var s = new ArraySegment<Cell>(segment.Array, segment.Offset + index, count);
            var es = evenSide.Slice(index, count);
            var os = oddSide.Slice(index, count);
            heap.Enqueue(new Range(s, es, os));
        }
        else
        {
            var s = new ArraySegment<Cell>(segment.Array, segment.Offset + index, count);
            var es = oddSide.Slice(index, count);
            var os = evenSide.Slice(index, count);
            heap.Enqueue(new Range(s, es, os));
        }
    }

    public void Pop(List<int> values, BinaryHeap<Range> slices)
    {
        var evenCell = evenSide.Query();
        var ei = evenCell.Index - Offset;

        var oddCell = oddSide.Query(ei + 1, oddSide.Count - (ei + 1));
        var oi = oddCell.Index - Offset;

        evenSide[ei] = Cell.Max;
        oddSide[oi] = Cell.Max;

        values.Add(evenCell.Value);
        values.Add(oddCell.Value);

        AddSlice(0, ei, slices);
        AddSlice(ei + 1, oi - (ei + 1), slices);
        AddSlice(oi + 1, segment.Count - (oi + 1), slices);
    }

    public Range(Cell[] cells)
    {
        segment = new ArraySegment<Cell>(cells);
        evenSide = segment.Select((x, i) => i % 2 == 0 ? x : Cell.Max).ToSegmentTree();
        oddSide = segment.Select((x, i) => i % 2 == 1 ? x : Cell.Max).ToSegmentTree();
    }

    public Range(ArraySegment<Cell> segment, ISlicableSegmentTree<Cell> evenSide, ISlicableSegmentTree<Cell> oddSide)
    {
        this.segment = segment;
        this.evenSide = evenSide;
        this.oddSide = oddSide;
    }
}

public class Program
{
    #region Standard I/O
    readonly TextReader input;
    readonly TextWriter output;

    X[] ReadLine<X>(Func<string, X> func)
    {
        return input.ReadLine().Split(' ').Select(func).ToArray();
    }

    void WriteLineOne(object obj)
    {
        output.WriteLine("{0}", obj);
    }

    void WriteLineMany(params object[] objects)
    {
        output.WriteLine(string.Join(" ", objects.Select(obj => obj.ToString())));
    }
    #endregion

    int n;
    int[] ps;

    public void Run()
    {
        {
            n = ReadLine(int.Parse)[0];
            ps = ReadLine(int.Parse);
        }

        var qs = new List<int>(n);

        var allCells = ps.Select((p, i) => new Cell(p, i)).ToArray();

        var heap = BinaryHeap.Create<Range>();
        heap.Enqueue(new Range(allCells));

        while (heap.Count > 0)
        {
            var range = heap.Dequeue();
            range.Pop(qs, heap);
        }

        WriteLineOne(string.Join(" ", qs));
    }

    public Program(TextReader input, TextWriter output)
    {
        this.input = input;
        this.output = output;
    }

    public static void Main(string[] args)
    {
        new Program(Console.In, Console.Out).Run();
    }
}
