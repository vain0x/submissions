using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Diagnostics;
using System.Globalization;
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

            if (r == ' ' || r == '\r' || r == '\n')
            {
                if (r == '\r' && _reader.Peek() == '\n')
                {
                    _reader.Read();
                }

                // Ignore leading spaces.
                if (_sb.Length == 0) continue;

                break;
            }
            else if (r == -1)
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
        return int.Parse(Word().Trim());
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
        _output.WriteLine(value.ToString(CultureInfo.InvariantCulture));
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
    void Solve(long[] A, List<int> output, out int l, out int r, out long sum)
    {
        var N = A.Length;
        if (N % 2 == 0)
        {
            N--;
        }
        Debug.Assert(N % 2 == 1);

        sum = 0;
        // A の偶数番目の要素で最初に正のものの番号
        l = 0;
        // A の偶数番目の要素で最後に正のものの番号
        r = N - 1;
        while (l < N && A[l] <= 0)
        {
            l += 2;
        }
        while (r >= 0 && A[r] <= 0)
        {
            r -= 2;
        }
        if (l > r)
        {
            return;
        }

        var L = l;
        var M = (r + 2 - l) / 2;
        var B = M.MakeArray(i => A[L + i * 2]);

        var gaps = new List<int>();
        {
            var gap = 0;
            sum += B[0];
            for (var i = 1; i < M; i++)
            {
                if (B[i] < 0)
                {
                    gap++;
                }
                else
                {
                    sum += B[i];
                    gaps.Add(gap);
                    gap = 0;
                }
            }
            Debug.Assert(gap == 0);
        }

        //Console.WriteLine($"l={l} r={r} M={M} B=({B.Intercalate(", ")}) gap=({gaps.Intercalate(", ")}) sum={sum}");

        {
            var len = A.Length;

            for (var i = 0; i < L; i++)
            {
                WriteLine(1);
                len--;
            }

            foreach (var g in gaps)
            {
                for (var i = 0; i < g; i++)
                {
                    output.Add(3);
                    len -= 2;
                }

                output.Add(2);
                len -= 2;
            }

            while (len > 1)
            {
                output.Add(2);
                len--;
            }
        }
    }

    public void EntryPoint()
    {
        var I = _scanner;
        var N = I.N();
        var A = N.MakeArray(_ => I.L());

        var A1 = A.ToArray();
        var A2 = new ArraySegment<long>(A, 1, A.Length - 1).ToArray();

        var output1 = new List<int>();
        var output2 = new List<int>();
        int l1, r1, l2, r2;
        long sum1, sum2 = 0;
        Solve(A1, output1, out l1, out r1, out sum1);
        Solve(A2, output2, out l2, out r2, out sum2);
        if (sum1 >= sum2)
        {
            WriteLine(sum1);
            WriteLine(output1.Count);
            foreach (var i in output1)
            {
                WriteLine(i);
            }
        }
        else
        {
            WriteLine(sum2);
            WriteLine(output2.Count);
            WriteLine(1);
            foreach (var i in output2)
            {
                WriteLine(i);
            }
        }
    }
}
