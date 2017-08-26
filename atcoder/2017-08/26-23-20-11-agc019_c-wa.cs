using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

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

public enum Direction
{
    Left,
    Up,
    Right,
    Down,
}

public sealed partial class Program
{
    int n;
    Point s, t;
    Point[] fountains;

    Dictionary<Point, double> memo = new Dictionary<Point, double>();

    static readonly double Arc = Math.PI / 2 * 10;

    static readonly Point o = new Point(0, 0);

    double SolveCore(Point p)
    {
        if (p.X == t.X || p.Y == t.Y)
        {
            // 進行方向に噴水があれば距離が少し遠くなる。
            var exists =
                p.X == t.X
                    ? fountains.Any(f => f.X == p.X && p.Y <= f.Y && f.Y <= t.Y)
                    : fountains.Any(f => f.Y == p.Y && p.X <= f.X && f.X <= t.X);

            return p.Md(t) * 100 - 10 + (exists ? Arc * 2 - 20 : 0);
        }

        var rest = default(double);
        if (!memo.TryGetValue(p, out rest))
        {
            var fs =
                fountains
                .Where(f => f != p && p.X <= f.X && f.X <= t.X && p.Y <= f.Y && f.Y <= t.Y)
                .ToArray();

            if (fs.Length > 0)
            {
                // x, y 方向それぞれで p に一番近い噴水
                var xf = default(Point);
                var yf = default(Point);

                for (var i = 0; i < fs.Length; i++)
                {
                    if (i == 0)
                    {
                        xf = fs[i];
                        yf = fs[i];
                    }
                    else
                    {
                        if (xf.X > fs[i].X)
                        {
                            xf = fs[i];
                        }

                        if (yf.Y > fs[i].Y)
                        {
                            yf = fs[i];
                        }
                    }
                }

                var xr = SolveCore(xf);
                var xd = xf.Md(p) * 100 + Arc - 20;

                var yr = SolveCore(yf);
                var yd = yf.Md(p) * 100 + Arc - 20;

                rest = Math.Min(xd + xr, yd + yr);
            }
            else
            {
                // p から t までの間に噴水がなければ直接 t に行く。
                rest = p.Md(t) * 100 - 10;
            }

            memo.Add(p, rest);
        }

        return rest;
    }

    double Solve()
    {
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
                .Where(f =>
                    0 <= f.X && f.X <= t.X
                    && 0 <= f.Y && f.Y <= t.Y
                )
                .ToArray();
        }

        // 始点にいるときだけ噴水の縁にいないので、距離が10m長くなる。
        return SolveCore(o) + 10;
    }

    void Read()
    {
        var a = scanner;
        s = new Point(a.N(), a.N());
        t = new Point(a.N(), a.N());
        n = a.N();
        fountains = n.MakeArray(_ => new Point(a.N(), a.N()));
    }

    public void EntryPoint()
    {
        Read();
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
