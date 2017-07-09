using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

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

    long n, a, b, c, d;

    void Read()
    {
        var line = ReadLine(long.Parse);
        n = line[0];
        a = line[1];
        b = line[2];
        c = line[3];
        d = line[4];
    }

    bool IsOk(int m)
    {
        // a + Σ(c + p_i) - Σ(c + q_i) = b となる (p_i), (q_i) が存在すればよい。
        // ただし 0 <= p_i, q_i <= d - c = r。
        // (p_i) の長さを m とする。(q_i) の長さは l。
        var r = d - c;
        var l = n - 1 - m;

        // a + (m - l) * c + Σp - Σq = b
        // Σp - Σq = b - a + (l - m) * c = f
        // Σp は [0, m * r] 上を、Σq は [0, l * r] 上を自由に動く。
        // Σp - Σq は [-l * r, m * r] 上を自由に動く。
        // f ∈ [-l * r, m * r] であればよい。
        var f = b - a + (l - m) * c;
        return -l * r <= f && f <= m * r;
    }

    bool Solve()
    {
        for (var m = 0; m <= n - 1; m++)
        {
            if (IsOk(m)) return true;
        }

        return false;
    }

    public void Run()
    {
        Read();
        WriteLineOne(Solve() ? "YES" : "NO");
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
