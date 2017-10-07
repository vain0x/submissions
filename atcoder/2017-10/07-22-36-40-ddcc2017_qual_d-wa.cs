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

public struct Point
    : IEquatable<Point>
{
    public readonly int X, Y;

    public override string ToString()
    {
        return string.Format("X={0}, Y={1}", X, Y);
    }

    public Point(int x, int y)
    {
        X = x;
        Y = y;
    }

    long ToInt64()
    {
        return ((long)X << 32) | (long)Y;
    }

    public bool Equals(Point other)
    {
        return X == other.X && Y == other.Y;
    }

    public override bool Equals(object obj)
    {
        return obj is Point && Equals((Point)obj);
    }

    public override int GetHashCode()
    {
        return EqualityComparer<long>.Default.GetHashCode(ToInt64());
    }
}

public sealed partial class Program
{
    int h, w;
    long scoreY, scoreX;
    bool[][] board;

    HashSet<Point> unlinked = new HashSet<Point>();
    HashSet<Point> yLinked = new HashSet<Point>();
    HashSet<Point> xLinked = new HashSet<Point>();
    HashSet<Point> yUnlinked = new HashSet<Point>();
    HashSet<Point> xUnlinked = new HashSet<Point>();
    HashSet<Point> doubleLinked = new HashSet<Point>();

    Point DualY(Point p)
    {
        return new Point(p.X, h - p.Y - 1);
    }

    Point DualX(Point p)
    {
        return new Point(w - p.X - 1, p.Y);
    }

    bool IsSymmetricY()
    {
        return yUnlinked.Count == 0;
    }

    bool IsSymmetricX()
    {
        return xUnlinked.Count == 0;
    }

    int stoneCount;

    HashSet<Point>[] sets;

    void Register(Point p)
    {
        if (!Exists(p)) return;

        var yd = DualY(p);
        var yl = board[yd.Y][yd.X];
        var xd = DualX(p);
        var xl = board[xd.Y][xd.X];

        if (yl)
        {
            yLinked.Add(p);
        }
        else
        {
            yUnlinked.Add(p);
        }

        if (xl)
        {
            xLinked.Add(p);
        }
        else
        {
            xUnlinked.Add(p);
        }

        if (yl && xl)
        {
            doubleLinked.Add(p);
        }
        else if (!yl && !xl)
        {
            unlinked.Add(p);
        }
    }

    void Unregister(Point p)
    {
        foreach (var set in sets)
        {
            set.Remove(p);
        }
    }

    bool Exists(Point p)
    {
        return board[p.Y][p.X];
    }

    void Remove(Point p)
    {
        stoneCount--;
        board[p.Y][p.X] = false;

        var dy = DualY(p);
        var dx = DualX(p);
        var q = DualX(dy);

        var points = new[] { p, dy, dx, q };
        foreach (var point in points)
        {
            Unregister(point);

            if (!point.Equals(p))
            {
                Register(point);
            }
        }
    }

    void Swap<X>(ref X first, ref X second)
    {
        var t = first;
        first = second;
        second = t;
    }

    long Solve()
    {
        if (scoreY < scoreX)
        {
            // 転置
            Swap(ref h, ref w);
            Swap(ref scoreY, ref scoreX);
            board = h.MakeArray(y => w.MakeArray(x => board[x][y]));
        }

        sets =
            new[]
            {
                unlinked,
                yUnlinked,
                xUnlinked,
                yLinked,
                xLinked,
                doubleLinked,
            };

        for (var y = 0; y < h; y++)
        {
            for (var x = 0; x < w; x++)
            {
                if (!board[y][x]) continue;

                stoneCount++;

                var p = new Point(x, y);
                Register(p);
            }
        }

        var score = 0L;

        while (stoneCount > 0)
        {
            // 上下ともにリンクしていない石があれば、それを除去する。
            // 上下左右どちらかにリンクしていない石があれば、どれかを除去する。
            // すべての石が上下左右の両方にリンクしているなら、どれかを除去する。
            // 左右対称、上下対称かどうかを判定する。

            var p = sets.Where(s => s.Count > 0).First().First();
            Remove(p);

            if (IsSymmetricY())
            {
                score += scoreY;
            }

            if (IsSymmetricX())
            {
                score += scoreX;
            }
        }

        return score;
    }

    void Read()
    {
        var a = scanner;
        h = a.N();
        w = a.N();
        scoreY = a.L();
        scoreX = a.L();
        board = h.MakeArray(y => input.ReadLine().Select(c => c == 'S').ToArray());
    }

    public void EntryPoint()
    {
        Read();
        WriteLine(Solve());
    }
}
