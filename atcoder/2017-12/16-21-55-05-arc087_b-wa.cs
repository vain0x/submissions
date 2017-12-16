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
    int X, Y;
    int[] ops;

    private bool Solve()
    {
        var x = 0;
        var y = 0;

        var points = new List<Tuple<int, int>>();
        points.Add(Tuple.Create(x, y));
        
        for (var i = 0; i < ops.Length; i++)
        {
            var goY = i % 2 == 0;
            var d = ops[i];

            if (goY)
            {
                y += d;
            }
            else
            {
                x += d;
            }

            points.Add(Tuple.Create(x, y));
        }

        if (ops.Length == 0)
        {
            return X == 0 && Y == 0;
        }
        else if (ops.Length == 1)
        {
            return X == 0 && Math.Abs(Y) == ops[0];
        }
        else
        {
            // 第一象限に移動
            X = Math.Abs(X);
            Y = Math.Abs(Y);

            var p = points[points.Count - 3];
            var q = points[points.Count - 1];
            var dx = q.Item1 - p.Item1;
            var dy = q.Item2 - p.Item2;

            if (X == x && Y == y) return true;

            if (X == x && Y == y - 2 * dy) return true;

            if (X == x - 2 * dx && Y == y) return true;

            if (X == x - 2 * dx && Y == y - 2 * dy) return true;

            return false;
        }
    }

    private void Read()
    {
        var a = _scanner;
        var s = _input.ReadLine();
        X = a.N();
        Y = a.N();
        
        var offset = s.TakeWhile(c => c == 'F').Count();
        X -= offset;
        ops = s.Substring(offset).Split('T').Select(fs => fs.Length)
            .Skip(1) // s.Substring は T から始まるので最初に不要な 0 が生じる
            .ToArray();
    }

    public void EntryPoint()
    {
        Read();
        WriteLine(Solve() ? "Yes" : "No");
    }
}
