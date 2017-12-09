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

            if (r == '\r')
            {
                if (_reader.Peek() == '\n') _reader.Read();
                break;
            }
            else if (r == -1 || r == ' ' || r == '\n')
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

public static class GcdExtension
{
    public static long Gcd(this long x, long y)
    {
        return y == 0 ? x : y.Gcd(x % y);
    }

    public static long GcdExtended(this long x, long y, out long s, out long t)
    {
        if (y == 0)
        {
            s = 1;
            t = 0;
            return x;
        }
        else
        {
            var g = y.GcdExtended(x % y, out t, out s);
            t -= (x / y) * s;
            return g;
        }
    }

    public struct GcdExtendedResult
    {
        public readonly long G, X, Y;

        public GcdExtendedResult(long g, long x, long y)
        {
            G = g;
            X = x;
            Y = y;
        }
    }

    public static GcdExtendedResult GcdExtended(this long x, long y)
    {
        long s, t;
        var g = x.GcdExtended(y, out s, out t);
        return new GcdExtendedResult(g, s, t);
    }
}

public sealed class StructuralArrayEqualityComparer<T>
    : IEqualityComparer<T[]>
{
    private IEqualityComparer<T> _comparer;

    public bool Equals(T[] x, T[] y)
    {
        if (x == null)
        {
            return y == null;
        }

        if (x.Length != y.Length)
        {
            return false;
        }

        for (var i = 0; i < x.Length; i++)
        {
            if (!_comparer.Equals(x[i], y[i]))
            {
                return false;
            }
        }

        return true;
    }

    public int GetHashCode(T[] obj)
    {
        if (obj == null) return 0;

        var h = 17;
        foreach (var x in obj)
        {
            h = unchecked(h + _comparer.GetHashCode(x) * 23);
        }

        return h;
    }

    public StructuralArrayEqualityComparer(IEqualityComparer<T> equalityComparer)
    {
        _comparer = equalityComparer;
    }

    public static IEqualityComparer<T[]> Default { get; } =
        new StructuralArrayEqualityComparer<T>(EqualityComparer<T>.Default);
}

public sealed partial class Program
{
    long l, r;
    List<long>[] disjoints;

    Dictionary<long[], long> memo =
        new Dictionary<long[], long>(1, StructuralArrayEqualityComparer<long>.Default);

    private long SolveRec(long[] xs, bool cache)
    {
        if (xs.Length == 0) return 1;
        if (xs.Length == 1) return 2;

        if (xs.Length == 2)
        {
            return (xs[0].Gcd(xs[1]) == 1) ? 4 : 3;
        }

        long cached;
        if (!memo.TryGetValue(xs, out cached))
        {
            {
                var x = xs[0];
                var ys = new long[xs.Length - 1];
                Array.Copy(xs, 1, ys, 0, ys.Length);

                var zc = SolveRec(ys.Where(y => x.Gcd(y) == 1).ToArray(), true);
                var yc = SolveRec(ys, false);
                cached = yc + zc;
            }

            if (cache)
            {
                memo.Add(xs, cached);
            }
        }

        return cached;
    }

    private long Solve()
    {
        var n = (int)(r - l);

        disjoints = n.MakeArray(i => new List<long>());
        for (var i = 0; i < n; i++)
        {
            for (var j = i + 1; j < n; j++)
            {
                if ((l + i).Gcd(l + j) == 0) continue;
                disjoints[i].Add(l + j);
            }
        }

        var xs = n.Range().OrderByDescending(i => disjoints[i].Count).Select(i => l + i).ToArray();
        return SolveRec(xs, false);
    }

    private void Read()
    {
        var a = _scanner;
        l = a.L();
        r = a.L() + 1;
    }

    public void EntryPoint()
    {
        Read();
        WriteLine(Solve());
    }
}
