using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;



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

public sealed class Point
    : Tuple<int, int>
{
    public int X
    {
        get
        {
            return Item1;
        }
    }

    public int Y
    {
        get
        {
            return Item2;
        }
    }

    public Point(int x, int y)
        : base(x, y)
    {
    }

    public double Md(Point r)
    {
        return Math.Abs(X - r.X) + Math.Abs(Y - r.Y);
    }
}

public sealed partial class Program
{
    Point s, t;
    Point[] fountains;

    // 噴水の円周長の 1/4
    static readonly double Arc = 2 * Math.PI / 4 * 10;

    static readonly Point O = new Point(0, 0);

    void Read()
    {
        var a = scanner;
        s = new Point(a.N(), a.N());
        t = new Point(a.N(), a.N());
        var n = a.N();
        fountains = n.MakeArray(_ => new Point(a.N(), a.N()));
    }

    /// <summary>
    /// s を原点に、t を (+, +) に移すような座標変換を施す。
    /// </summary>
    void Transform()
    {
        var reverseX = s.X > t.X;
        var reverseY = s.Y > t.Y;

        var transform = new Func<Point, Point>(p =>
            new Point(
                reverseX ? s.X - p.X : p.X - s.X,
                reverseY ? s.Y - p.Y : p.Y - s.Y
            ));

        t = transform(t);

        fountains =
            fountains
            .Select(transform)
            .Where(p =>
                // 始点～終点の矩形に収まらない噴水は、そこに停止する機会がないので、除外しておく。
                0 <= p.X && p.X <= t.X
                && 0 <= p.Y && p.Y <= t.Y
            )
            .ToArray();

        s = O;
    }

    /// <summary>
    /// 最長増加部分列の長さを計算する。
    /// </summary>
    static int LisLength(IReadOnlyList<int> ys)
    {
        var dp = (ys.Count + 1).MakeArray(_ => int.MaxValue);
        var l = 0;

        foreach (var y in ys)
        {
            var lb = dp.LowerBound(y);
            dp[lb] = y;
            l = Math.Max(l, lb + 1);
        }

        return l;
    }

    double Solve()
    {
        Read();
        Transform();

        // 噴水を X 座標について整列してから Y 座標の最長増加部分列を求める。
        // X, Y ともに増加するような部分列だけが考慮される。
        Array.Sort(fountains);
        var k = LisLength(fountains.Select(f => f.Y).ToArray());

        if (k == Math.Min(t.X, t.Y) + 1)
        {
            // k - 1 個の噴水を曲折し、1 個の噴水を横断する。
            return (t.X + t.Y) * 100 + (k - 1) * (Arc - 20) + (2 * Arc - 20);
        }
        else
        {
            // k 個の噴水を曲折する。横断することはない。
            return (t.X + t.Y) * 100 + k * (Arc - 20);
        }
    }

    public void EntryPoint()
    {
        WriteLine(Solve());
    }
}

#region Custom Library
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

    public static void ForEach<X>(this IEnumerable<X> @this, Action<X, int> action)
    {
        var list = @this as IReadOnlyList<X>;
        if (list != null)
        {
            var count = list.Count;
            for (var i = 0; i < count; i++)
            {
                action(list[i], i);
            }
        }
        else
        {
            var i = 0;
            foreach (var x in @this)
            {
                action(x, i);
                i++;
            }
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
#endregion
