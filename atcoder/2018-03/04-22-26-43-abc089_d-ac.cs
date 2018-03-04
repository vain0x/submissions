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

public sealed partial class Program
{
    int H, W, D;
    int HW;
    int[][] A;
    int Q;
    int[] L, R;

    Tuple<int, int>[][] Points;
    long[][][] Skip;

    long Distance(Tuple<int, int> p, Tuple<int, int> q)
    {
        return Math.Abs(p.Item1 - q.Item1) + Math.Abs(p.Item2 - q.Item2);
    }

    int Length(int c)
    {
        return (HW - c + D - 1) / D;
    }

    int Height(int c, int s)
    {
        var h = 0;
        while (c + (s + (1 << h)) * D < HW)
        {
            h++;
        }
        return h;
    }

    void Build()
    {
        HW = H * W;
        Points = D.MakeArray(c => new Tuple<int, int>[Length(c)]);
        for (var y = 0; y < H; y++)
        {
            for (var x = 0; x < W; x++)
            {
                var u = A[y][x];
                var c = u % D;
                var i = (u - c) / D;
                Points[c][i] = Tuple.Create(y, x);
            }
        }

        var baseCosts = D.MakeArray(c => new long[Length(c)]);
        var skip = D.MakeArray(c => Length(c).MakeArray(s => new long[Height(c, s)]));
        for (var c = 0; c < D; c++)
        {
            var ps = Points[c];
            var length = Length(c);
            var cost = 0L;
            var p = ps[0];
            for (var i = 0; i < length; i++)
            {
                baseCosts[c][i] = cost;
                for (var h = 0; 1 << h <= i; h++)
                {
                    var u = i - (1 << h);
                    skip[c][u][h] = cost - baseCosts[c][u];
                }

                if (i + 1 < length)
                {
                    cost += Distance(ps[i], ps[i + 1]);
                }
            }
        }

        Skip = skip;//.Select(xs => xs.ToArray()).ToArray();
    }

    long Cost(int c, int v)
    {
        if (v == c) return 0;
        // Debug.Assert(v % D == c);

        var len = Length(c);
        var k = (v - c) / D;

        var i = 0;
        var cost = 0L;
        while (i < k)
        {
            var h = 0;
            while (i + (1 << (h + 1)) <= k)
            {
                h++;
            }
            cost += Skip[c][i][h];
            i += 1 << h;
        }
        return cost;
    }

    private long Solve(int l, int r)
    {
        var c = l % D;
        // Debug.Assert(r % D == c);

        var lc = Cost(c, l);
        var rc = Cost(c, r);
        // Console.WriteLine($"{rc} - {lc}");
        return rc - lc;
    }

    public void EntryPoint()
    {
        var I = _scanner;
        H = I.N();
        W = I.N();
        D = I.N();
        A = H.MakeArray(y => W.MakeArray(x => I.N() - 1));
        Q = I.N();
        L = new int[Q];
        R = new int[Q];
        for (var i = 0; i < Q; i++)
        {
            L[i] = I.N() - 1;
            R[i] = I.N() - 1;
        }

        Build();

        for (var i = 0; i < Q; i++)
        {
            WriteLine(Solve(L[i], R[i]));
        }
    }
}
