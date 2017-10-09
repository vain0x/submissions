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
        return string.Format("[X={0}, Y={1}]", X, Y);
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

public sealed class Simulator
{
    int h, w;
    long scoreY, scoreX;
    bool[][] board;

    HashSet<Point> unlinked = new HashSet<Point>();
    HashSet<Point> yUnlinked = new HashSet<Point>();
    HashSet<Point> xUnlinked = new HashSet<Point>();
    HashSet<Point> doubleLinked = new HashSet<Point>();
    HashSet<Point>[] sets;

    int stoneCount;
    long score = 0L;

    static void Swap<X>(ref X first, ref X second)
    {
        var t = first;
        first = second;
        second = t;
    }

    bool Exists(Point p)
    {
        return board[p.Y][p.X];
    }

    // p の y 方向の反射
    Point Fy(Point p)
    {
        return new Point(p.X, h - p.Y - 1);
    }

    Point Fx(Point p)
    {
        return new Point(w - p.X - 1, p.Y);
    }

    bool IsSymmetricY()
    {
        return unlinked.Count == 0 && yUnlinked.Count == 0;
    }

    bool IsSymmetricX()
    {
        return unlinked.Count == 0 && xUnlinked.Count == 0;
    }

    void Register(Point p)
    {
        if (!Exists(p)) return;

        var yl = Exists(Fy(p));
        var xl = Exists(Fx(p));

        if (!yl && !xl)
        {
            unlinked.Add(p);
        }

        if (!yl && xl)
        {
            yUnlinked.Add(p);
        }

        if (yl && !xl)
        {
            xUnlinked.Add(p);
        }

        if (yl && xl)
        {
            doubleLinked.Add(p);
        }
    }

    void Unregister(Point p)
    {
        foreach (var set in sets)
        {
            set.Remove(p);
        }
    }

    void Remove(Point p)
    {
        if (!Exists(p)) return;

        stoneCount--;
        board[p.Y][p.X] = false;

        var dy = Fy(p);
        var dx = Fx(p);
        var q = Fx(dy);

        var points = new[] { p, dy, dx, q };
        foreach (var point in points)
        {
            Unregister(point);
            Register(point);
        }

        // 石を取り除いたのでスコアを加算する。
        if (IsSymmetricY())
        {
            score += scoreY;
        }

        if (IsSymmetricX())
        {
            score += scoreX;
        }
    }

    void Prepare()
    {
        sets =
            new[]
            {
                unlinked,
                yUnlinked,
                xUnlinked,
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
    }

    // シミュレーションにより最大スコアを計算する。
    // 1. 上下左右どちらにもリンクしていない石があるかぎり、幸福度は得られないので、まずそれらを取り除く。
    // 2. 上下対称か左右対称になるように石を取り除く。preferX が true なら上下対象、false なら左右対称を目指すが、この戦略によって過程3で得られる幸福度が変わってくる。(解説によれば最適な preferY を計算できるが、ここではシミュレーション解法の実装を目指す。)
    // 3. 上下左右ともに対称になるように石を取り除く。
    // 4. 上下左右対称になった後は、ある点 p から始めて p, dx(p), dy(p), dx(dy(p)) の順で取るのを繰り返す。
    public long Solve(bool preferY)
    {
        Prepare();

        var ss =
            new[]
            {
                unlinked,
                preferY ? yUnlinked : xUnlinked,
                preferY ? xUnlinked : yUnlinked,
                doubleLinked,
            };

        while (stoneCount > 0 && !(IsSymmetricY() && IsSymmetricX()))
        {
            var p = ss.Where(s => s.Count > 0).First().First();
            Remove(p);
        }

        while (stoneCount > 0)
        {
            var p = doubleLinked.First();
            Remove(p);
            Remove(Fy(p));
            Remove(Fx(p));
            Remove(Fy(Fx(p)));
        }

        return score;
    }

    public Simulator(int h, int w, long scoreY, long scoreX, bool[][] board)
    {
        this.h = h;
        this.w = w;
        this.scoreY = scoreY;
        this.scoreX = scoreX;
        this.board = board;
    }
}

public sealed partial class Program
{
    int h, w;
    long scoreY, scoreX;
    bool[][] board;

    long Simulate(bool preferY)
    {
        if (scoreY < scoreX)
        {
            var transposed = w.MakeArray(x => h.MakeArray(y => board[y][x]));
            return new Simulator(w, h, scoreX, scoreY, transposed).Solve(preferY);
        }
        else
        {
            var clone = h.MakeArray(y => w.MakeArray(x => board[y][x]));
            return new Simulator(h, w, scoreY, scoreX, clone).Solve(preferY);
        }
    }

    long Solve()
    {
        return Math.Max(Simulate(true), Simulate(false));
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

/*

4 4
5 4
.S..
S..S
....
SS.S

*/
