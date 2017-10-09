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

public sealed partial class Program
{
    int h, w;
    long scoreY, scoreX;
    bool[][] board;

    bool Exists(Point p)
    {
        return board[p.Y][p.X];
    }

    int E(Point p)
    {
        return Exists(p) ? 1 : 0;
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

    long Solve()
    {
        // p, fyp, fxp, fzp の4点が揃っているときに獲得できる幸福度。
        var scoreQ = scoreX + scoreY + Math.Max(scoreY, scoreX);

        var sn = 0;
        var yn = 0;
        var xn = 0;
        var qn = 0;

        for (var y = 0; y < h / 2; y++)
        {
            for (var x = 0; x < w / 2; x++)
            {
                var p = new Point(x, y);
                var fyp = Fy(p);
                var fxp = Fx(p);
                var fzp = Fy(fxp);

                // 石を数える。
                sn += E(p) + E(fyp) + E(fxp) + E(fzp);

                if (Exists(p) && Exists(fyp) && Exists(fxp) && Exists(fzp))
                {
                    qn++;
                }
                else
                {
                    // x方向のリンクは無視して、y方向にリンクしているペアの個数を数える。
                    if (Exists(p) && Exists(fyp)) yn++;
                    if (Exists(fxp) && Exists(fzp)) yn++;

                    // 同様
                    if (Exists(p) && Exists(fxp)) xn++;
                    if (Exists(fyp) && Exists(fzp)) xn++;
                }
            }
        }

        var s1 = 0L;
        // まずy方向にリンクしていない石を取り除くが、
        // 始めからそういう石がない場合は加点されない。
        if (qn * 4 + yn * 2 != sn) s1 += scoreY;
        s1 += yn * scoreY;
        if (qn * 4 != sn) s1 += scoreX;
        s1 += qn * scoreQ;

        // 最初にx方向にリンクしていない石を取り除く場合も考える。
        var s2 = 0L;
        if (qn * 4 + xn * 2 != sn) s2 += scoreX;
        s2 += xn * scoreX;
        if (qn * 4 != sn) s2 += scoreY;
        s2 += qn * scoreQ;

        return Math.Max(s1, s2);
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
2 5
....
.SS.
..S.
....

12


2 2
4 7
.S
S.

11


4 8
9 13
.SS.....
.SS.....
.SS.....
.SS.....

49


4 4
5 4
S..S
....
....
S..S

14

4 4
5 4
S..S
.S..
....
S..S

23


4 4
5 4
S..S
.SS.
....
S.SS

27

4 4
5 4
S..S
.SS.
....
S..S

23


4 4
5 4
.S..
S..S
....
SS.S

17

*/
