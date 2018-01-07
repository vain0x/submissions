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
    private readonly TextReader _reader;
    private readonly StringBuilder _sb = new StringBuilder();

    /// <summary>
    /// Reads next word separated by spaces.
    /// </summary>
    public string Word()
    {
        _sb.Clear();

        while (true)
        {
            var r = _reader.Read();

            if (r == ' ' || r == '\r' || r == '\n')
            {
                if (r == '\r' && _reader.Peek() == '\n')
                {
                    _reader.Read();
                }

                // Ignore leading spaces.
                if (_sb.Length == 0) continue;

                break;
            }
            else if (r == -1)
            {
                break;
            }
            else
            {
                _sb.Append((char)r);
            }
        }

        return _sb.ToString();
    }

    /// <summary>
    /// Reads next word as <see cref="int"/>.
    /// </summary>
    public int N()
    {
        return int.Parse(Word().Trim());
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

    /// <summary>
    /// Reads next line and splits it by spaces.
    /// </summary>
    public X[] Words<X>(Func<string, X> func)
    {
        return _reader.ReadLine().Split(' ').Select(func).ToArray();
    }

    public Scanner(TextReader reader)
    {
        _reader = reader;
    }
}

public partial class Program
{
    private readonly TextReader _input;
    private readonly TextWriter _output;
    private readonly Scanner _scanner;

    private void WriteLine(int value)
    {
        _output.WriteLine(value);
    }

    private void WriteLine(long value)
    {
        _output.WriteLine(value);
    }

    private void WriteLine(double value)
    {
        _output.WriteLine(value);
    }

    private void WriteLine(char value)
    {
        _output.WriteLine(value);
    }

    private void WriteLine(string value)
    {
        _output.WriteLine(value);
    }

    public Program(TextReader input, TextWriter output)
    {
        _input = input;
        _output = output;
        _scanner = new Scanner(input);
    }

    public static void Main(string[] args)
    {
        new Program(Console.In, Console.Out).EntryPoint();
    }
}

/// <summary>
/// Provides binary search implementations.
/// </summary>
public static class BinarySearch
{
    /// <summary>
    /// Performs Meguru-style binary search.
    /// <seealso cref="https://twitter.com/meguru_comp/status/697008509376835584"/>
    /// </summary>
    public static int Meguru(int ok, int ng, Func<int, bool> isOk)
    {
        while (Math.Abs(ok - ng) > 1)
        {
            var mid = (ok + ng) / 2;
            if (isOk(mid))
            {
                ok = mid;
            }
            else
            {
                ng = mid;
            }
        }
        return ok;
    }

    /// <summary>
    /// Performs Meguru-style binary search.
    /// </summary>
    public static long Meguru(long ok, long ng, Func<long, bool> isOk)
    {
        while (Math.Abs(ok - ng) > 1)
        {
            var mid = (ok + ng) / 2;
            if (isOk(mid))
            {
                ok = mid;
            }
            else
            {
                ng = mid;
            }
        }
        return ok;
    }

    /// <summary>
    /// Performs binary search
    /// to find the lower bound index of the specified value in the range.
    /// </summary>
    public static int LowerBound<X>(this IReadOnlyList<X> @this, X value, int lb, int ub, IComparer<X> comparer)
    {
        if (lb > ub) throw new ArgumentOutOfRangeException(nameof(ub));

        while (lb != ub)
        {
            var m = lb + (ub - lb) / 2;
            if (comparer.Compare(@this[m], value) < 0)
            {
                lb = m + 1;
            }
            else
            {
                ub = m;
            }
        }

        return lb;
    }

    /// <summary>
    /// Performs binary search
    /// to find the lower bound index of the specified value.
    /// </summary>
    public static int LowerBound<X>(this IReadOnlyList<X> @this, X value, IComparer<X> comparer)
    {
        return LowerBound(@this, value, 0, @this.Count, comparer);
    }

    /// <summary>
    /// Performs binary search
    /// to find the lower bound index of the specified value in the range.
    /// </summary>
    public static int LowerBound<X>(this IReadOnlyList<X> @this, X value, int lb, int ub)
    {
        return LowerBound(@this, value, lb, ub, Comparer<X>.Default);
    }

    /// <summary>
    /// Performs binary search
    /// to find the lower bound index of the specified value.
    /// </summary>
    public static int LowerBound<X>(this IReadOnlyList<X> @this, X value)
    {
        return LowerBound(@this, value, 0, @this.Count, Comparer<X>.Default);
    }

    /// <summary>
    /// Performs binary search
    /// to find the upper bound index of the specified value in the range.
    /// </summary>
    public static int UpperBound<X>(this IReadOnlyList<X> @this, X value, int lb, int ub, IComparer<X> comparer)
    {
        if (lb > ub) throw new ArgumentOutOfRangeException(nameof(ub));

        while (lb != ub)
        {
            var m = lb + (ub - lb) / 2;
            if (comparer.Compare(@this[m], value) <= 0)
            {
                lb = m + 1;
            }
            else
            {
                ub = m;
            }
        }

        return lb;
    }

    /// <summary>
    /// Performs binary search
    /// to find the upper bound index of the specified value.
    /// </summary>
    public static int UpperBound<X>(this IReadOnlyList<X> @this, X value, IComparer<X> comparer)
    {
        return UpperBound(@this, value, 0, @this.Count, comparer);
    }

    /// <summary>
    /// Performs binary search
    /// to find the upper bound index of the specified value in the range.
    /// </summary>
    public static int UpperBound<X>(this IReadOnlyList<X> @this, X value, int lb, int ub)
    {
        return UpperBound(@this, value, lb, ub, Comparer<X>.Default);
    }

    /// <summary>
    /// Performs binary search
    /// to find the upper bound index of the specified value.
    /// </summary>
    public static int UpperBound<X>(this IReadOnlyList<X> @this, X value)
    {
        return UpperBound(@this, value, 0, @this.Count, Comparer<X>.Default);
    }
}

public sealed partial class Program
{
    int N;
    long H;
    long[] a;
    long[] b;

    private long Solve()
    {
        var maxA = a.Max();
        var cs = b.Where(x => x > maxA).ToArray();
        Array.Sort(cs);
        Array.Reverse(cs);

        var d = 0L;
        for (var i = 0; i < cs.Length; i++)
        {
            d += cs[i];
            if (d >= H) return i + 1;
        }

        var h = H - d;
        var ak = (h + maxA - 1) / maxA;
        var bk = cs.Length;
        return ak + bk;
    }

    public void EntryPoint()
    {
        var I = _scanner;
        N = I.N();
        H = I.L();
        a = new long[N];
        b = new long[N];

        for (var i = 0; i < N; i++)
        {
            a[i] = I.L();
            b[i] = I.L();
        }

        WriteLine(Solve());
    }
}
