using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

public class BruteForceSolver
{
    readonly int n, k;
    readonly long[] xs;

    IEnumerable<long> Loop(int j, int i)
    {
        if (i == xs.Length)
        {
            yield return 0L;
        }
        else
        {
            foreach (var sum in Loop(j, i + 1))
            {
                yield return sum;

                if (i != j)
                {
                    yield return sum + xs[i];
                }
            }
        }
    }

    public int Solve()
    {
        var count = 0;

        for (var j = 0; j < xs.Length; j++)
        {
            var ok = true;

            foreach (var sum in Loop(j, 0))
            {
                if (sum + xs[j] < k) continue;
                if (sum < k)
                {
                    ok = false;
                    break;
                }
            }

            if (ok) count++;
        }

        return count;
    }

    public BruteForceSolver(int n, int k, long[] xs)
    {
        this.n = n;
        this.k = k;
        this.xs = xs.ToArray();
    }
}

public class FastSolver
{
    readonly int n, k;
    readonly int[] xs;

    public static int MeguruBinarySearch(int ok, int ng, Func<int, bool> isOk)
    {
        while (Math.Abs(ok - ng) > 1)
        {
            var m = (ok + ng) / 2;
            if (isOk(m))
            {
                ok = m;
            }
            else
            {
                ng = m;
            }
        }
        return ok;
    }

    bool IsNecessary(int j)
    {
        var dp = new bool[k + 1];
        dp[0] = true;

        for (var i = 0; i < xs.Length; i++)
        {
            if (i == j) continue;

            for (var s = k; s >= 0; s--)
            {
                if (s >= xs[i])
                {
                    dp[s] = dp[s] || dp[s - xs[i]];
                }
            }
        }

        var min = int.MaxValue; 

        for (var s = k - xs[j]; s <= k; s++)
        {
            if (dp[s])
            {
                min = s;
                break;
            }
        }

        return min < k;
    }

    public int Solve()
    {
        return MeguruBinarySearch(n, -1, IsNecessary);
    }

    public FastSolver(int n, int k, long[] xs)
    {
        this.n = n;
        this.k = k;
        this.xs = xs.Select(x => (int)Math.Min(x, k)).OrderBy(x => x).ToArray();
    }
}

public class RandomChecker
{
    public void Run()
    {
        var random = new Random();

        for (var n = 1; n < 20; n++)
        {
            var xs = new long[n];

            for (var k = 1; k < n + 2; k++)
            {
                for (var i = 0; i < xs.Length; i++)
                {
                    xs[i] = random.Next(1, n + 1);
                }

                var bruteForceSolver = new BruteForceSolver(n, k, xs);
                var fastSolver = new FastSolver(n, k, xs);

                var expected = bruteForceSolver.Solve();
                var actual = fastSolver.Solve();
                if (actual != expected)
                {
                    Console.WriteLine("{0} {1}", n, k);
                    Console.WriteLine(string.Join(" ", xs));
                    Console.WriteLine("Expected {0}, actual {1}", expected, actual);
                    bruteForceSolver.Solve();
                    fastSolver.Solve();
                }
            }
        }
    }
}

public class Program
{
    #region Standard I/O
    readonly TextReader input;
    readonly TextWriter output;

    X[] ReadLine<X>(Func<string, X> func)
    {
        return input.ReadLine().Split(' ').Select(func).ToArray();
    }

    void WriteLineOne(object obj)
    {
        output.WriteLine("{0}", obj);
    }

    void WriteLineMany(params object[] objects)
    {
        output.WriteLine(string.Join(" ", objects.Select(obj => obj.ToString())));
    }
    #endregion

    int n, k;
    long[] xs;

    void Read()
    {
        var line = ReadLine(int.Parse);
        n = line[0];
        k = line[1];
        xs = ReadLine(long.Parse);
    }

    int Solve()
    {
        return new FastSolver(n, k, xs).Solve();
    }

    public void Run()
    {
        //new RandomChecker().Run();

        Read();
        WriteLineOne(Solve());
    }

    public Program(TextReader input, TextWriter output)
    {
        this.input = input;
        this.output = output;
    }

    public static void Main(string[] args)
    {
        new Program(Console.In, Console.Out).Run();
    }
}
