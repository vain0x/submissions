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
    int n, c;
    Tuple<int, int, int>[] programs;

    private long Solve()
    {
        // x = c のとき各チャンネル全体をそれぞれの録画機で録画できる。ゆえに min x <= c。
        //

        var ss =
            programs
            .Select((p, i) => Tuple.Create(p.Item1, i))
            .OrderBy(p => p)
            .ToArray();
        var ts =
            programs
            .Select((p, i) => Tuple.Create(p.Item2, i))
            .OrderBy(p => p)
            .ToArray();

        var timePoints =
            programs
            .SelectMany(p => new[] { p.Item1, p.Item2 })
            .Concat(new[] { int.MaxValue })
            .Distinct()
            .OrderBy(t => t)
            .ToArray();

        var next =
            Enumerable.Range(0, timePoints.Length - 1)
            .ToDictionary(i => timePoints[i], i => timePoints[i + 1]);

        var programIdFromChannelIdAndStartTime =
            programs
            .Indexed()
            .ToDictionary(indexed => Tuple.Create(indexed.Value.Item3, indexed.Value.Item1), indexed => indexed.Index);

        var solve = new Func<int, bool>(x =>
        {
            if (x == c) return true;
            var si = 0;
            var ti = 0;

            var done =
                n.MakeArray(i => false);

            var rs = c.MakeArray(i => -1);
            var available = x;
            var resumableAt = timePoints.ToDictionary(t => t, t => 0);

            while (true)
            {
                bool starts;
                int programId;
                int t;

                if (si < n && (ti == n || ss[si].Item1 <= ts[ti].Item1))
                {
                    var p = ss[si];
                    si++;
                    starts = true;
                    t = p.Item1;
                    programId = p.Item2;
                }
                else if (ti < n)
                {
                    Debug.Assert(si == n || ss[si].Item1 > ts[ti].Item1);
                    var p = ts[ti];
                    ti++;
                    starts = false;
                    t = p.Item1;
                    programId = p.Item2;
                }
                else
                {
                    Debug.Assert(si == n && ti == n);
                    return true;
                }

                // Resume.
                {
                    available += resumableAt[t];
                    resumableAt[t] = 0;
                }

                if (starts)
                {
                    if (done[programId]) continue;
                    if (available == 0) return false;
                    available--;

                    var pi = programId;
                    Tuple<int, int, int> p;

                    // 同チャンネルで直後に番組が始まるならぶっ続けで録画する。
                    while (true)
                    {
                        done[pi] = true;
                        p = programs[pi];
                        if (!programIdFromChannelIdAndStartTime.TryGetValue(Tuple.Create(p.Item3, p.Item2), out pi))
                        {
                            break;
                        }
                    }

                    // 録画の終了時刻の次の時刻に利用可能状態に戻す。
                    resumableAt[next[p.Item2]]++;
                }
                else
                {
                    //
                }
            }
        });

        return Enumerable.Range(1, c).First(i => solve(i));
    }

    private void Read()
    {
        var a = _scanner;
        n = a.N();
        c = a.N();
        programs = n.MakeArray(i => Tuple.Create(a.N(), a.N(), a.N() - 1));
    }

    public void EntryPoint()
    {
        Read();
        WriteLine(Solve());
    }
}
