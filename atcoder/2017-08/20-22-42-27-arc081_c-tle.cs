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


public sealed partial class Program
{
    string source;

    List<int>[] indexesFromChar;
    int[] completeCounts;
    int completeCount;
    int n;

    void Prepare()
    {
        indexesFromChar = 26.MakeArray(_ => new List<int>());
        completeCounts = new int[source.Length];
        var completeRanges = new List<Tuple<int, int>>();

        {
            var cc = 0;
            var counts = new int[26];
            var k = 0;
            var beginIndex = 0;

            for (var i = 0; i < source.Length; i++)
            {
                indexesFromChar[source[i] - 'a'].Add(i);

                if (completeCounts[source[i] - 'a'] == cc)
                {
                    completeCounts[source[i] - 'a']++;
                    k++;

                    if (k == 26)
                    {
                        cc++;
                        k = 0;
                        completeRanges.Add(Tuple.Create(beginIndex, i + 1));
                        beginIndex = i + 1;
                    }
                }
            }



            completeCount = cc;
            n = cc + 1;
        }

        if (completeRanges.Count > 0)
        {
            var k = -1;
            var beginIndex = 0;
            var endIndex = 0;

            for (var i = 0; i < source.Length; i++)
            {
                if (i == endIndex && k + 1 < completeRanges.Count)
                {
                    k++;

                    beginIndex = completeRanges[k].Item1;
                    endIndex = completeRanges[k].Item2;
                }

                completeCounts[i] = k + 1;
            }
        }
    }

    bool SolveCore(char[] target, int ti, int si)
    {
        for (var c = 'a'; c <= 'z'; c++)
        {
            target[ti] = c;

            var indexes = indexesFromChar[c - 'a'];
            var lb = indexes.LowerBound(si);
            if (lb == indexes.Count)
            {
                // c がもう出現しない場合、
                // 残りの文字列がなんであれ、部分文字列になることはないので、a で埋めるのがベスト。
                // ありえる？

                for (var j = ti + 1; j < n; j++)
                {
                    target[ti] = 'a';
                }
                return true;
            }

            if (ti + 1 == n) continue;

            // 文字 c が次に出現する位置
            var ci = indexes[lb];

            // 残りの範囲に含まれる a..z の個数
            var cc = completeCount - completeCounts[ci];
            if (cc >= n - (ti + 1))
            {
                // 残りの文字列をどう埋めても部分文字列になる。
                continue;
            }

            if (SolveCore(target, ti + 1, ci + 1))
            {
                return true;
            }
        }

        return false;
    }

    string Solve()
    {
        Prepare();
        var target = new char[n];
        SolveCore(target, 0, -1);
        return new string(target);
    }

    public void EntryPoint()
    {
        source = input.ReadLine();
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

    /// <summary>
    /// Reads next word separated by spaces.
    /// </summary>
    public string Word()
    {
        var sb = default(StringBuilder);
        var firstChar = default(char);
        var count = 0;

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
                var c = (char)r;

                switch (count)
                {
                    case 0:
                        firstChar = c;
                        count = 1;
                        break;
                    case 1:
                        sb = new StringBuilder();
                        sb.Append(firstChar).Append(c);
                        count = 2;
                        break;
                    default:
                        sb.Append(c);
                        break;
                }
            }
        }

        switch (count)
        {
            case 0:
                return "";
            case 1:
                return firstChar.ToString();
            default:
                return sb.ToString();
        }
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
