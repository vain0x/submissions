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

public sealed partial class Program
{
    int n, k;
    Tuple<long, long>[] ps;
    long[] ys;

    long[] F(Func<Tuple<long, long>, long> selector)
    {
        return
            ps
            .Select(selector)
            .OrderBy(x => x)
            .ToArray();
    }

    long Solve()
    {
        // X座標について昇順にしておく。
        Array.Sort(ps, (l, r) => Comparer<long>.Default.Compare(l.Item1, r.Item1));

        ys = F(p => p.Item2);

        var min = long.MaxValue;

        for (var yl = 0; yl < ys.Length; yl++)
        {
            for (var yr = yl + 1; yr < ys.Length; yr++)
            {
                // 以下、y 座標が ys[yl] 以上 ys[yr] 以下の点のみ考える。

                // acc[i] = (x 座標が xs[0] 以上 xs[i - 1] 以下の点の個数)
                var acc = (n + 1).MakeArray(i => 0);
                {
                    var count = 0;
                    for (var i = 0; i < n; i++)
                    {
                        var p = ps[i];
                        var x = p.Item1;
                        var y = p.Item2;

                        if (ys[yl] <= y && y <= ys[yr])
                        {
                            count++;
                        }

                        acc[i + 1] = count;
                    }
                }

                for (var xl = 0; xl < n; xl++)
                {
                    for (var xr = xl + 1; xr < n; xr++)
                    {
                        var count = acc[xr + 1] - acc[xl];
                        if (count >= k)
                        {
                            var sizeX = ps[xr].Item1 - ps[xl].Item1;
                            var sizeY = ys[yr] - ys[yl];
                            min = Math.Min(min, sizeX * sizeY);
                            break;
                        }
                    }
                }
            }
        }

        return min;
    }

    void Read()
    {
        var a = scanner;

        n = a.N();
        k = a.N();
        ps = n.MakeArray(i => Tuple.Create(a.L(), a.L()));
    }

    public void EntryPoint()
    {
        Read();
        WriteLine(Solve());
    }
}
