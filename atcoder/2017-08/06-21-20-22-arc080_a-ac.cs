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

    int count1;
    int count2;
    int count4;

    void Read()
    {
        n = ReadLine(int.Parse)[0];

        foreach (var x in ReadLine(long.Parse))
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
    }

    int PopLowest()
    {
        if (count1 > 0)
        {
            count1--;
            return 1;
        }
        else if (count2 > 0)
        {
            count2--;
            return 2;
        }
        else if (count4 > 0)
        {
            count4--;
            return 4;
        }

        throw new InvalidOperationException();
    }

    bool TryPop4()
    {
        if (count4 > 0)
        {
            count4--;
            return true;
        }

        return false;
    }

    bool TryPop2Or4(out int value)
    {
        if (count4 > 0)
        {
            count4--;
            value = 4;
            return true;
        }
        else if (count2 > 0)
        {
            count2--;
            value = 2;
            return true;
        }

        value = default(int);
        return false;
    }

    bool Verify(int[] xs)
    {
        for (var i = 0; i + 1 < xs.Length; i++)
        {
            if (xs[i] * xs[i + 1] % 4 != 0) return false;
        }

        return true;
    }

    bool Solve()
    {
        var result = new int[n];

        result[0] = PopLowest();
        result[n - 1] = PopLowest();

        for (var r = 0; r < n - 2; r++)
        {
            if (r % 2 == 0)
            {
                var i = 1 + r / 2;
                switch (result[i - 1])
                {
                    case 1:
                        {
                            if (!TryPop4()) return false;
                            result[i] = 4;
                            break;
                        }
                    case 2:
                        {
                            if (!TryPop2Or4(out result[i])) return false;
                            break;
                        }
                    case 4:
                        {
                            result[i] = PopLowest();
                            break;
                        }
                    default:
                        throw new InvalidOperationException();
                }
            }
            else
            {
                var i = n - 2 - r / 2;
                switch (result[i + 1])
                {
                    case 1:
                        {
                            if (!TryPop4()) return false;
                            result[i] = 4;
                            break;
                        }
                    case 2:
                        {
                            if (!TryPop2Or4(out result[i])) return false;
                            break;
                        }
                    case 4:
                        {
                            result[i] = PopLowest();
                            break;
                        }
                    default:
                        throw new InvalidOperationException();
                }
            }
        }

        return Verify(result);
    }

    public void Run()
    {
        Read();
        WriteLineOne(Solve() ? "Yes" : "No");
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
