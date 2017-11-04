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
    // [0, k) の整数を m 個並べた数列の個数は k^m なので X = Σ_m=1..n k^m
    // 0-indexed なら求めるのは X/2 - 1 番目
    // 前から桁を確定していくとき、次の桁は なし, 0, 1, ..., k - 1 のどれか。
    // なしは常に1通りなので i=0 のときなし。
    // 0, ..., k - 1 はいずれも同じ個数なので、この個数を p とすると、i 番目 (i >= 1) を選ぶには
    // (i - 1) / p を配置すればよい。

    int K, N;

    private int[] Solve()
    {
        var seq = new List<int>();

        var X = 0L;
        {
            var s = 1L;
            for (var m = 1; m <= N; m++)
            {
                s *= K;
                X += s;
            }
        }

        // 数列辞典の最初に空列があると仮定して開始する。
        var j = (X / 2 - 1) + 1;
        var p = X + 1;

        while (j > 0)
        {
            // 「なし」の選択肢を削る。
            p--;
            j--;

            var q = p / K;
            var i = (int)(j / q);

            seq.Add(i);

            p /= K;
            j -= i * q;
        }

        return seq.ToArray();
    }

    private void Read()
    {
        var a = _scanner;
        K = a.N();
        N = a.N();
    }

    public void EntryPoint()
    {
        Read();
        WriteLine(Solve().Select(x => x + 1).Intercalate(" "));
    }
}
