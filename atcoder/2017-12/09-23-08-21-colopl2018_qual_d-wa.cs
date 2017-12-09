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
    int n;
    long x;
    long[] ts;

    private IEnumerable<long> Solve()
    {
        var total = 0L;
        var peeks = n.MakeArray(i => default(long));
        {
            var t = 0L;
            var y = x;
            for (var i = 0; i < n; i++)
            {
                var peek = Math.Min(x, y + ts[i] - t);
                total += peek;
                peeks[i] = peek;
                t = ts[i];
                y = 0;
            }
        }

        var scores = (n + 1).MakeArray(i => 0L);
        scores[n] = total;

        var m = n;
        var points = n.MakeArray(i => new { t = ts[i], y = peeks[i] });

        for (var k = n - 1; k >= 1; k--)
        {
            int index = 0;

            {
                var minIndex = new int?();
                var r = 0;
                while (!minIndex.HasValue)
                {
                    long minY = long.MaxValue;
                    for (var i = 0; i < m - 1; i++)
                    {
                        if (r == 0 && points[i + 1].y == x) continue;

                        var y = points[i].y;
                        if (minY > y)
                        {
                            minIndex = i;
                            minY = y;
                        }
                    }
                    r++;
                }
                index = minIndex.Value;
            }

            // points を更新する。
            {
                var p = points[index];
                var q = points[index + 1];
                var y = Math.Min(x, p.y + q.y);
                points[index] = new { t = q.t, y };
                for (var i = index + 1; i < m - 1; i++)
                {
                    points[i] = points[i + 1];
                }

                m--;
            }

            scores[k] = points.Take(m).Sum(p => p.y);
        }

        return scores.Skip(1);
    }

    private void Read()
    {
        var a = _scanner;
        n = a.N();
        x = a.L();

        ts = n.MakeArray(i => a.L());
    }

    public void EntryPoint()
    {
        Read();
        foreach (var x in Solve())
        {
            WriteLine(x);
        }
    }
}
