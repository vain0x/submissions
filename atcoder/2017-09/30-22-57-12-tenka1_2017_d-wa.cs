// 記念提出

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
    public struct Item
    {
        public readonly ulong X;
        public readonly long V;

        public Item(ulong x, long v)
        {
            X = x;
            V = v;
        }
    }

    int n;
    ulong k;
    Item[] items;

    long Dfs(int i, int bj, bool tight, long value)
    {
        if (i == n || bj == 31)
        {
            return value;
        }

        var bi = 31 - bj - 1;
        var b = 1UL << bi;
        var kb = k & b;
        var km = k & ~(b - 1);

        var x = items[i].X;
        var xb = x & b;

        var v = items[i].V;

        var max = value;

        // 買う場合
        if (xb != 0)
        {
            if (tight)
            {
                if (xb > kb)
                {
                    var bi2 = bi;
                    var t2 = xb == kb;

                    while (bi2 >= 0)
                    {
                        bi2--;

                        var b2 = 1UL << bi2;
                        if ((km | x | b2) > k) continue;

                        var xb2 = x & b2;
                        var kb2 = k & b2;
                        t2 = t2 && xb2 == kb2;
                        break;
                    }

                    var bj2 = 31 - bi2 - 1;
                    max = Math.Max(max, value + Dfs(i + 1, bj2, t2, value + v));
                }
            }
            else
            {
                max = Math.Max(max, value + Dfs(i + 1, bj, false, value + v));
            }
        }

        // 買わない場合
        max = Math.Max(max, value + Dfs(i + 1, bj, tight && xb == kb, value));
        max = Math.Max(max, value + Dfs(i, bj + 1, tight && kb == 0, value));

        return max;
    }

    long Solve()
    {
        Array.Sort(items, (l, r) => Comparer<ulong>.Default.Compare(r.X, l.X));
        return Dfs(0, 0, tight: true, value: 0);
    }

    void Read()
    {
        var a = scanner;

        n = a.N();
        k = (ulong)a.N();
        items = n.MakeArray(i => new Item((ulong)a.N(), a.L()));
    }

    public void EntryPoint()
    {
        Read();
        WriteLine(Solve());
    }
}
