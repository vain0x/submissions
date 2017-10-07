using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

public static class TemplateExtension
{
    public static X[] MakeArray<X>(this int count, Func<int, X> func)
    {
        var xs = new X[count];
        for (var i = 0; i < count; i++)
        {
            xs[i] = func(i);
        }
        return xs;
    }

    public static int[] Range(this int count, int start = 0)
    {
        return count.MakeArray(i => i + start);
    }

    public static string Intercalate<X>(this IEnumerable<X> @this, string separator)
    {
        return string.Join(separator, @this);
    }

    public sealed class ValueIndexPair<T>
        : Tuple<T, int>
    {
        public T Value { get { return Item1; } }
        public int Index { get { return Item2; } }

        public ValueIndexPair(T value, int index)
            : base(value, index)
        {
        }
    }

    public static IEnumerable<ValueIndexPair<X>> Indexed<X>(this IEnumerable<X> @this)
    {
        var i = 0;
        foreach (var x in @this)
        {
            yield return new ValueIndexPair<X>(x, i);
            i++;
        }
    }
}

public sealed class Scanner
{
    readonly TextReader reader;
    readonly StringBuilder sb = new StringBuilder();

    /// <summary>
    /// Reads next word separated by spaces.
    /// </summary>
    public string Word()
    {
        sb.Clear();

        while (true)
        {
            var r = reader.Read();

            if (r == '\r')
            {
                if (reader.Peek() == '\n') reader.Read();
                break;
            }
            else if (r == -1 || r == ' ' || r == '\n')
            {
                break;
            }
            else
            {
                sb.Append((char)r);
            }
        }

        return sb.ToString();
    }

    /// <summary>
    /// Reads next word as <see cref="int"/>.
    /// </summary>
    public int N()
    {
        return int.Parse(Word());
    }

    /// <summary>
    /// Reads next word as <see cref="long"/>.
    /// </summary>
    public long L()
    {
        return long.Parse(Word());
    }

    /// <summary>
    /// Reads next word as <see cref="double"/>.
    /// </summary>
    public double F()
    {
        return double.Parse(Word());
    }

    public int[] Ns(int count)
    {
        return count.MakeArray(_ => N());
    }

    public long[] Ls(int count)
    {
        return count.MakeArray(_ => L());
    }

    public double[] Fs(int count)
    {
        return count.MakeArray(_ => F());
    }

    /// <summary>
    /// Reads next line and splits it by spaces.
    /// </summary>
    public X[] Words<X>(Func<string, X> func)
    {
        return reader.ReadLine().Split(' ').Select(func).ToArray();
    }

    public Scanner(TextReader reader)
    {
        this.reader = reader;
    }
}

public partial class Program
{
    readonly TextReader input;
    readonly TextWriter output;
    readonly Scanner scanner;

    void WriteLine(int value)
    {
        output.WriteLine(value);
    }

    void WriteLine(long value)
    {
        output.WriteLine(value);
    }

    void WriteLine(double value)
    {
        output.WriteLine(value);
    }

    void WriteLine(char value)
    {
        output.WriteLine(value);
    }

    void WriteLine(string value)
    {
        output.WriteLine(value);
    }

    public Program(TextReader input, TextWriter output)
    {
        this.input = input;
        this.output = output;
        scanner = new Scanner(input);
    }

    public static void Main(string[] args)
    {
#if DEBUG
        using (var writer = new VainZero.IO.DebugTextWriter(Console.Out))
#else
        var writer = Console.Out;
#endif
        {
            new Program(Console.In, writer).EntryPoint();
        }
    }
}

// 参考: https://gist.github.com/yutopio/5643839
public class Tree<T> : ICollection<T>, IList<T> where T : IComparable<T>
{
    public class TreeNode : ICollection<T>, IList<T>
    {
        public TreeNode(T value, Tree<T> tree)
        {
            this.Value = value;
            this.Level = 1;
            this.Count = 1;
            this.Tree = tree;
        }

        public override string ToString()
        {
            return string.Format("[Count = {0}, Value = {1}]", Count, Value);
        }

        public Tree<T> Tree { get; private set; }
        public T Value { get; private set; }
        public TreeNode Parent { get; private set; }
        public TreeNode LeftHand { get; private set; }
        public TreeNode RightHand { get; private set; }
        int Level { get; set; }
        public int Count { get; private set; }

        public void Add(T item)
        {
            var compare = item.CompareTo(this.Value);
            if (compare < 0)
                if (this.LeftHand == null)
                    ((this.LeftHand = new Tree<T>.TreeNode(item, this.Tree)).Parent = this).Reconstruct(true);
                else this.LeftHand.Add(item);
            else
                if (this.RightHand == null)
                ((this.RightHand = new Tree<T>.TreeNode(item, this.Tree)).Parent = this).Reconstruct(true);
            else this.RightHand.Add(item);
        }

        public void Clear()
        {
            if (this.LeftHand != null) this.LeftHand.Clear();
            if (this.RightHand != null) this.RightHand.Clear();
            this.LeftHand = this.RightHand = null;
        }

        public bool Contains(T item)
        {
            var compare = item.CompareTo(this.Value);
            if (compare < 0)
                return this.LeftHand == null ? false : this.LeftHand.Contains(item);
            else if (compare == 0)
                return true;
            else
                return this.RightHand == null ? false : this.RightHand.Contains(item);
        }

        public void CopyTo(T[] array, int arrayIndex)
        {
            if (this.LeftHand != null)
            {
                this.LeftHand.CopyTo(array, arrayIndex);
                arrayIndex += this.LeftHand.Count;
            }
            array[arrayIndex++] = this.Value;
            if (this.RightHand != null)
                this.RightHand.CopyTo(array, arrayIndex);
        }

        public bool IsReadOnly { get { return false; } }

        public bool Remove(T item)
        {
            var compare = item.CompareTo(this.Value);
            if (compare == 0)
            {
                if (this.LeftHand == null && this.RightHand == null)
                    if (this.Parent != null)
                    {
                        if (this.Parent.LeftHand == this) this.Parent.LeftHand = null;
                        else this.Parent.RightHand = null;
                        this.Parent.Reconstruct(true);
                    }
                    else this.Tree.RootNode = null;
                else if (this.LeftHand == null || this.RightHand == null)
                {
                    var child = this.LeftHand == null ? this.RightHand : this.LeftHand;
                    if (this.Parent != null)
                    {
                        if (this.Parent.LeftHand == this) this.Parent.LeftHand = child;
                        else this.Parent.RightHand = child;
                        (child.Parent = this.Parent).Reconstruct(true);
                    }
                    else (this.Tree.RootNode = child).Parent = null;
                }
                else
                {
                    var replace = this.LeftHand;
                    while (replace.RightHand != null) replace = replace.RightHand;
                    var temp = this.Value;
                    this.Value = replace.Value;
                    replace.Value = temp;
                    return replace.Remove(replace.Value);
                }
                this.Parent = this.LeftHand = this.RightHand = null;
                return true;
            }
            else if (compare < 0)
                return this.LeftHand == null ? false : this.LeftHand.Remove(item);
            else
                return this.RightHand == null ? false : this.RightHand.Remove(item);
        }

        public IEnumerator<T> GetEnumerator()
        {
            if (this.LeftHand != null)
                foreach (var item in this.LeftHand)
                    yield return item;
            yield return this.Value;
            if (this.RightHand != null)
                foreach (var item in this.RightHand)
                    yield return item;
        }

        IEnumerator IEnumerable.GetEnumerator() { return GetEnumerator(); }

        void Reconstruct(bool recursive)
        {
            this.Count = 1;

            int leftLevel = 0, rightLevel = 0;
            if (this.LeftHand != null)
            {
                leftLevel = this.LeftHand.Level;
                this.Count += this.LeftHand.Count;
            }
            if (this.RightHand != null)
            {
                rightLevel = this.RightHand.Level;
                this.Count += this.RightHand.Count;
            }

            if (leftLevel - rightLevel > 1)
            {
                var leftLeft = this.LeftHand.LeftHand == null ? 0 : this.LeftHand.LeftHand.Level;
                var leftRight = this.LeftHand.RightHand == null ? 0 : this.LeftHand.RightHand.Level;
                if (leftLeft >= leftRight)
                {
                    this.LeftHand.Elevate();
                    this.Reconstruct(true);
                }
                else
                {
                    var pivot = this.LeftHand.RightHand;
                    pivot.Elevate(); pivot.Elevate();
                    pivot.LeftHand.Reconstruct(false);
                    pivot.RightHand.Reconstruct(true);
                }
            }
            else if (rightLevel - leftLevel > 1)
            {
                var rightRight = this.RightHand.RightHand == null ? 0 : this.RightHand.RightHand.Level;
                var rightLeft = this.RightHand.LeftHand == null ? 0 : this.RightHand.LeftHand.Level;
                if (rightRight >= rightLeft)
                {
                    this.RightHand.Elevate();
                    this.Reconstruct(true);
                }
                else
                {
                    var pivot = this.RightHand.LeftHand;
                    pivot.Elevate(); pivot.Elevate();
                    pivot.LeftHand.Reconstruct(false);
                    pivot.RightHand.Reconstruct(true);
                }
            }
            else
            {
                this.Level = Math.Max(leftLevel, rightLevel) + 1;
                if (this.Parent != null && recursive)
                    this.Parent.Reconstruct(true);
            }
        }

        void Elevate()
        {
            var root = this.Parent;
            var parent = root.Parent;
            if ((this.Parent = parent) == null) this.Tree.RootNode = this;
            else
            {
                if (parent.LeftHand == root) parent.LeftHand = this;
                else parent.RightHand = this;
            }

            if (root.LeftHand == this)
            {
                root.LeftHand = this.RightHand;
                if (this.RightHand != null) this.RightHand.Parent = root;
                this.RightHand = root;
                root.Parent = this;
            }
            else
            {
                root.RightHand = this.LeftHand;
                if (this.LeftHand != null) this.LeftHand.Parent = root;
                this.LeftHand = root;
                root.Parent = this;
            }
        }

        public int IndexOf(T item)
        {
            var compare = item.CompareTo(this.Value);
            if (compare == 0)
                if (this.LeftHand == null) return 0;
                else
                {
                    var temp = this.LeftHand.IndexOf(item);
                    return temp == -1 ? this.LeftHand.Count : temp;
                }
            else if (compare < 0)
                if (this.LeftHand == null) return -1;
                else return this.LeftHand.IndexOf(item);
            else
                if (this.RightHand == null) return -1;
            else return this.RightHand.IndexOf(item);
        }

        public void Insert(int index, T item) { throw new InvalidOperationException(); }

        public void RemoveAt(int index)
        {
            if (this.LeftHand != null)
                if (index < this.LeftHand.Count)
                {
                    this.LeftHand.RemoveAt(index);
                    return;
                }
                else index -= this.LeftHand.Count;
            if (index-- == 0)
            {
                this.Remove(this.Value);
                return;
            }
            if (this.RightHand != null)
                if (index < this.RightHand.Count)
                {
                    this.RightHand.RemoveAt(index);
                    return;
                }
            throw new ArgumentOutOfRangeException("index");
        }

        public T this[int index]
        {
            get
            {
                if (this.LeftHand != null)
                    if (index < this.LeftHand.Count) return this.LeftHand[index];
                    else index -= this.LeftHand.Count;
                if (index-- == 0) return this.Value;
                if (this.RightHand != null)
                    if (index < this.RightHand.Count) return this.RightHand[index];
                throw new ArgumentOutOfRangeException("index");
            }
            set { throw new InvalidOperationException(); }
        }
    }

    public TreeNode RootNode { get; private set; }

    public void Add(T item)
    {
        if (this.RootNode == null) this.RootNode = new Tree<T>.TreeNode(item, this);
        else this.RootNode.Add(item);
    }

    public void Clear()
    {
        if (this.RootNode == null) return;
        this.RootNode.Clear();
        this.RootNode = null;
    }

    public bool Contains(T item) { return this.RootNode == null ? false : this.RootNode.Contains(item); }

    public void CopyTo(T[] array, int arrayIndex)
    {
        if (array == null) throw new ArgumentNullException("array");
        if (arrayIndex < 0) throw new ArgumentOutOfRangeException("arrayIndex");
        if ((array.Length <= arrayIndex) || (this.RootNode != null && array.Length < arrayIndex + this.RootNode.Count))
            throw new ArgumentException();

        if (this.RootNode != null)
            this.RootNode.CopyTo(array, arrayIndex);
    }

    public int Count { get { return this.RootNode == null ? 0 : this.RootNode.Count; } }

    public bool IsReadOnly { get { return false; } }

    public bool Remove(T item) { return this.RootNode == null ? false : this.RootNode.Remove(item); }

    public IEnumerator<T> GetEnumerator()
    {
        if (this.RootNode != null)
            foreach (var item in this.RootNode)
                yield return item;
        else
            yield break;
    }

    IEnumerator IEnumerable.GetEnumerator() { return GetEnumerator(); }

    public int IndexOf(T item) { return this.RootNode != null ? this.RootNode.IndexOf(item) : -1; }

    public void Insert(int index, T item) { throw new InvalidOperationException(); }

    public void RemoveAt(int index) { if (this.RootNode != null) this.RootNode.RemoveAt(index); }

    public T this[int index]
    {
        get
        {
            if (this.RootNode != null) return this.RootNode[index];
            else throw new ArgumentOutOfRangeException("index");
        }
        set { throw new InvalidOperationException(); }
    }

    public T RemoveMax()
    {
        var index = Count - 1;
        var item = this[index];
        RemoveAt(index);
        return item;
    }

    bool TryLowerBoundCore(TreeNode node, T source, out T value)
    {
        if (node != null)
        {
            var ord = source.CompareTo(node.Value);

            if (ord < 0)
            {
                return TryLowerBoundCore(node.LeftHand, source, out value);
            }

            if (TryLowerBoundCore(node.RightHand, source, out value))
            {
                return true;
            }

            value = node.Value;
            return true;
        }

        value = default(T);
        return false;
    }

    public bool TryLowerBound(T source, out T value)
    {
        if (RootNode == null)
        {
            value = default(T);
            return false;
        }

        return TryLowerBoundCore(RootNode, source, out value);
    }
}

public sealed partial class Program
{
    int n;
    long c;
    long[] ls;

    int Solve()
    {
        var t = new Tree<long>();

        foreach (var l in ls)
        {
            t.Add(l);
        }

        var k = 0;
        while (t.Count > 0)
        {
            var first = t.RemoveMax();

            long second;
            if (t.TryLowerBound(c - (first + 1), out second))
            {
                t.Remove(second);
            }

            k++;
        }

        return k;
    }

    void Read()
    {
        var a = scanner;
        n = a.N();
        c = a.L();
        ls = n.MakeArray(i => a.L());
    }

    public void EntryPoint()
    {
        Read();
        WriteLine(Solve());
    }
}
