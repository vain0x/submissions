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

public struct Var
{
    public int V;
    public long D;
}

public sealed partial class Program
{
    int N, M;
    int[] L;
    int[] R;
    long[] D;

    Var[] m;

    Var Solve(int ni, out bool free)
    {
        if (m[ni].V == ni)
        {
            free = true;
            return m[ni];
        }

        var d = m[ni].D;
        var w = Solve(m[ni].V, out free);
        free = false;
        return m[ni] = new Var { V = w.V, D = w.D + d };
    }

    void Set(int li, int ri, long d)
    {
        if (li > ri)
        {
            Set(ri, li, -d);
            return;
        }

        var free = false;
        var lv = Solve(li, out free);
        if (free)
        {
            m[li] = new Var()
            {
                V = ri,
                D = d,
            };
        }
        else
        {
            var llv = m[lv.V];
            // llv: free

            m[lv.V] = new Var()
            {
                V = ri,
                D = -lv.D + d,
            };
        }
    }

    public void EntryPoint()
    {
        var I = _scanner;
        N = I.N();
        M = I.N();
        // L = new int[M];
        // R = new int[M];
        // D = new long[M];

        var ok = true;
        m = new Var[N];

        for (var ni = 0; ni < N; ni++)
        {
            m[ni] = new Var()
            {
                V = ni,
                D = 0,
            };
        }

        for (var mi = 0; mi < M; mi++)
        {
            var l = I.N() - 1;
            var r = I.N() - 1;
            var d = I.L();

            // x_l = x_r - d
            Set(l, r, -d);
        }

        for (var ni = 0; ni < N; ni++)
        {
            var free = false;
            var v = Solve(ni, out free);
            if (v.V == ni && v.D != 0)
            {
                ok = false;
                break;
            }
        }

        WriteLine(ok ? "Yes" : "No");
    }
}
