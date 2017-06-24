using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

public class FactorialFunction
{
    readonly long[] factorials;

    public long Factorial(int n)
    {
        return factorials[n];
    }

    public FactorialFunction(int maxValue, long mod)
    {
        factorials = new long[maxValue + 1];
        factorials[0] = 1;

        for (var i = 1; i <= maxValue; i++)
        {
            factorials[i] = (factorials[i - 1] * i) % mod;
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

    const int Mod = 1000000007;

    int n, m;

    void Read()
    {
        var l = ReadLine(int.Parse);
        n = l[0];
        m = l[1];
    }

    long Solve()
    {

        var d = Math.Abs(n - m);
        if (d >= 2) return 0;

        var factorialFunc = new FactorialFunction(Math.Max(n, m), Mod);
        var factorial = new Func<int, long>(factorialFunc.Factorial);

        if (d == 1)
        {
            return factorial(n) * factorial(m) % Mod;
        }

        return ((2 * factorial(n) % Mod) * factorial(m)) % Mod;
    }

    public void Run()
    {
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
