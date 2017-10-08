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
    int n, m;
    List<int>[] es;

    bool[] color;
    bool[] done;

    // 2部グラフだったら color に色を付けつつ true を返す。
    bool Dfs(int u, bool c)
    {
        if (done[u])
        {
            return color[u] == c;
        }

        done[u] = true;
        color[u] = c;

        foreach (var v in es[u])
        {
            if (!Dfs(v, !c)) return false;
        }

        return true;
    }

    long Solve()
    {
        done = n.MakeArray(u => false);
        color = n.MakeArray(u => default(bool));

        var isBipartite = Dfs(0, false);

        // 単純グラフ上に存在できる辺の個数。
        var k = n * (n - 1) / 2;

        if (isBipartite)
        {
            var p = color.Count(c => c);
            var q = n - p;

            // 同じ色の点の間に存在できる辺の個数。
            var l = (p * (p - 1) / 2) + (q * (q - 1) / 2);

            // l から、同じ色の点の間にすでに存在する辺の個数を引く。
            for (var u = 0; u < n; u++)
            {
                foreach (var v in es[u])
                {
                    if (u >= v) continue;

                    if (color[u] == color[v])
                    {
                        l--;
                    }
                }
            }

            throw new Exception();
            //return k - (m + l);
        }
        else
        {
            return k - m;
        }
    }

    void Read()
    {
        var a = scanner;

        n = a.N();
        m = a.N();
        es = n.MakeArray(u => new List<int>());

        for (var i = 0; i < m; i++)
        {
            var u = a.N() - 1;
            var v = a.N() - 1;
            es[u].Add(v);
            es[v].Add(u);
        }
    }

    public void EntryPoint()
    {
        Read();
        WriteLine(Solve());
    }
}
