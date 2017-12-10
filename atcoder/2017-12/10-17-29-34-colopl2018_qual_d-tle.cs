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
    private readonly TextReader _reader;
    private readonly StringBuilder _sb = new StringBuilder();

    /// <summary>
    /// Reads next word separated by spaces.
    /// </summary>
    public string Word()
    {
        _sb.Clear();

        while (true)
        {
            var r = _reader.Read();

            if (r == '\r')
            {
                if (_reader.Peek() == '\n') _reader.Read();
                break;
            }
            else if (r == -1 || r == ' ' || r == '\n')
            {
                break;
            }
            else
            {
                _sb.Append((char)r);
            }
        }

        return _sb.ToString();
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
        return _reader.ReadLine().Split(' ').Select(func).ToArray();
    }

    public Scanner(TextReader reader)
    {
        _reader = reader;
    }
}

public partial class Program
{
    private readonly TextReader _input;
    private readonly TextWriter _output;
    private readonly Scanner _scanner;

    private void WriteLine(int value)
    {
        _output.WriteLine(value);
    }

    private void WriteLine(long value)
    {
        _output.WriteLine(value);
    }

    private void WriteLine(double value)
    {
        _output.WriteLine(value);
    }

    private void WriteLine(char value)
    {
        _output.WriteLine(value);
    }

    private void WriteLine(string value)
    {
        _output.WriteLine(value);
    }

    public Program(TextReader input, TextWriter output)
    {
        _input = input;
        _output = output;
        _scanner = new Scanner(input);
    }

    public static void Main(string[] args)
    {
        new Program(Console.In, Console.Out).EntryPoint();
    }
}

public sealed partial class Program
{
    int N;
    long X;

    long[] T;

    private long[] Solve()
    {
        // 解説を読んで実装。

        // js[i] = j ⇔
        //  時刻 T[i] にスタミナ消費をした後、スタミナがカンストし始める直前の時刻が T[j] である。
        //  つまり j = max { j | T[j] - T[i] <= X }
        var js = N.MakeArray(i =>
            Enumerable.Range(i, N - i)
            .Last(j => T[j] - T[i] <= X));

        // dp[i][k] = x ⇔
        //   最後にスタミナを消費したのが時刻 T[i] であり、合計 k 回スタミナ消費を行った場合における、最大の知力が x である。
        // dp[_][1] = X (最初のスタミナ消費時は常にスタミナ X。)

        var dp = N.MakeArray(i => (N + 1).MakeArray(k =>
        {
            if (k == 0) return 0L;
            if (k == 1) return X;
            return long.MinValue;
        }));

        for (var k = 1; k < N; k++)
        {
            for (var i = k - 1; i < N; i++)
            {
                // 考慮に値する遷移先は2通り。
                // (1) カンストする時刻より前に消費するケース:
                // カンストする前に2回消費するのは単純に消費回数の無駄。
                // カンストする時刻より前の、最後の時刻に消費する。
                // (2) カンストする時刻、またはそれより後に消費するケース:
                // カンストした状態で時間を経過させるのは単純に時間の無駄。
                // カンストする時刻以降の、最初の時刻に消費する。

                for (var r = 0; r < 2; r++)
                {
                    var j = js[i] + r;
                    if (j >= N) continue;

                    var d = Math.Min(X, T[j] - T[i]);
                    dp[j][k + 1] = Math.Max(dp[j][k + 1], dp[i][k] + d);
                }
            }
        }

        return
            Enumerable.Range(1, N)
            .Select(k => Enumerable.Range(0, N).Max(i => dp[i][k]))
            .ToArray();
    }

    private void Read()
    {
        var a = _scanner;
        N = a.N();
        X = a.L();
        T = N.MakeArray(i => a.L());
    }

    public void EntryPoint()
    {
        Read();

        foreach (var y in Solve())
        {
            WriteLine(y);
        }
    }
}
