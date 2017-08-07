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

    int n;
    int[] xs;

    public void Run()
    {
        {
            n = ReadLine(int.Parse)[0];
            xs = ReadLine(int.Parse);
        }

        var count1 = 0;
        var count2 = 0;
        var count4 = 0;

        foreach (var x in xs)
        {
            if (x % 4 == 0)
            {
                count4++;
            }
            else if (x % 2 == 0)
            {
                count2++;
            }
            else
            {
                count1++;
            }
        }

        if (count2 > 0)
        {
            count1++;
        }

        WriteLineOne(count1 <= count4 + 1 ? "Yes" : "No");
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
