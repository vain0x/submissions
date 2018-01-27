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
    static int[] Dx = { 1, 0, -1, 0 };
    static int[] Dy = { 0, 1, 0, -1 };

    int r, c;
    bool[][] C;
    int[][] D;
    int k;

    private void Refresh()
    {
        for (var y = 0; y < r; y++)
        {
            for (var x = 0; x < c; x++)
            {
                if (!C[1 + y][1 + x])
                {
                    D[1 + y][1 + x] = 0;
                    continue;
                }

                var d = 0;
                for (var i = 0; i < 4; i++)
                {
                    if (!C[1 + y + Dy[i]][1 + x + Dx[i]]) continue;
                    d++;
                }

                D[1 + y][1 + x] = d;
            }
        }
    }

    void Fill(int y, int x)
    {
        C[y + 1][x + 1] = false;
        for (var i = 0; i < 4; i++)
        {
            C[1 + y + Dy[i]][1 + x + Dx[i]] = false;
        }
        k++;
    }

    private long Solve()
    {
        k = 0;
        D = (r + 2).MakeArray(y => new int[c + 2]);

        while (true)
        {
            Refresh();

            {
                for (var y = 0; y < r; y++)
                {
                    for (var x = 0; x < c; x++)
                    {
                        if (C[1 + y][1 + x] && D[1 + y][1 + x] <= 1)
                        {
                            Fill(y, x);
                        }
                    }
                }
            }

            Refresh();

            Tuple<int, int, int, int, int> minT = null;
            Tuple<int, int> minP = null;
            var comp = Comparer<Tuple<int, int, int, int, int>>.Default;

            for (var y = 0; y < r; y++)
            {
                for (var x = 0; x < c; x++)
                {
                    if (!C[1 + y][1 + x]) continue;

                    var ds =
                        Enumerable
                        .Range(0, 4)
                        .Select(i => -D[1 + y + Dy[i]][1 + x + Dx[i]])
                        .OrderByDescending(d => d)
                        .ToArray();
                    var t = Tuple.Create(D[1 + y][1 + x], ds[0], ds[1], ds[2], ds[3]);

                    if (minT == null || comp.Compare(minT, t) > 0)
                    {
                        minT = t;
                        minP = Tuple.Create(y, x);
                    }
                }
            }

            if (minP != null)
            {
                Fill(minP.Item1, minP.Item2);
                continue;
            }
            else
            {
                return k;
            }
        }
    }

    public void EntryPoint()
    {
        var I = _scanner;
        r = I.N();
        c = I.N();
        var board = r.MakeArray(i => _input.ReadLine());

        C = (r + 2).MakeArray(y => new bool[c + 2]);

        for (var y = 0; y < r; y++)
        {
            for (var x = 0; x < c; x++)
            {
                C[y + 1][x + 1] = board[y][x] == '.';
            }
        }


        WriteLine(Solve());
    }
}
