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

public static class PermutationAlgorithm
{
    static void InplaceSwap<X>(IList<X> list, int i, int j)
    {
        var t = list[i];
        list[i] = list[j];
        list[j] = t;
    }

    static void InplaceReverse<X>(IList<X> list, int first, int count)
    {
        for (var i = 0; i < count / 2; i++)
        {
            InplaceSwap(list, first + i, first + count - 1 - i);
        }
    }

    /// <summary>
    /// Rearranges items into the next lexicographically greater permutation.
    /// Returns <c>true</c> if rearranged.
    /// </summary>
    public static bool NextPermutation<X>(this IList<X> list, IComparer<X> comparer)
    {
        var count = list.Count;
        if (count <= 1) return false;

        var i = count - 1;
        while (true)
        {
            var j = i;

            i--;

            if (comparer.Compare(list[i], list[j]) < 0)
            {
                var k = count - 1;
                while (comparer.Compare(list[i], list[k]) >= 0)
                {
                    k--;
                }

                InplaceSwap(list, i, k);
                InplaceReverse(list, j, count - j);
                return true;
            }

            if (i == 0)
            {
                InplaceReverse(list, 0, count);
                return false;
            }
        }
    }

    /// <summary>
    /// Rearranges items into the next lexicographically greater permutation
    /// in the default order.
    /// Returns <c>true</c> if rearranged.
    /// </summary>
    public static bool NextPermutation<X>(this IList<X> list)
    {
        return NextPermutation(list, Comparer<X>.Default);
    }

    /// <summary>
    /// Generates all permutations.
    /// </summary>
    public static IEnumerable<IReadOnlyList<X>> Permutations<X>(this IEnumerable<X> xs, IComparer<X> comparer)
    {
        var array = xs.ToArray();
        Array.Sort(array, comparer);

        do
        {
            yield return array;
        }
        while (NextPermutation(array, comparer));
    }

    /// <summary>
    /// Generates all permutations.
    /// </summary>
    public static IEnumerable<IReadOnlyList<X>> Permutations<X>(this IEnumerable<X> xs)
    {
        return Permutations(xs, Comparer<X>.Default);
    }
}

public sealed partial class Program
{
    public void EntryPoint()
    {
        var a = scanner;
        var n = a.N();
        var m = a.N();
        var r = a.N();

        var rs = r.MakeArray(_ => a.N() - 1);
        var es = m.MakeArray(_ => Tuple.Create(a.N() - 1, a.N() - 1, a.L()));

        const long inf = 1L << 50;

        var dist = n.MakeArray(u => n.MakeArray(_ => inf));

        {
            for (var u = 0; u < n; u++)
            {
                dist[u][u] = 0;
            }

            foreach (var e in es)
            {
                dist[e.Item1][e.Item2] = e.Item3;
                dist[e.Item2][e.Item1] = e.Item3;
            }

            for (var k = 0; k < n; k++)
            {
                for (var u = 0; u < n; u++)
                {
                    for (var v = 0; v < n; v++)
                    {
                        dist[u][v] = Math.Min(dist[u][v], dist[u][k] + dist[k][v]);
                    }
                }
            }
        }

        var result =
            rs.Permutations()
            .Min(p =>
            {
                var d = 0L;
                var v = p[0];
                for (var i = 1; i < r; i++)
                {
                    d += dist[v][p[i]];
                    v = p[i];
                }
                return d;
            });
        WriteLine(result);
    }
}
