using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

public sealed class Program
{
    public static void Main(string[] args)
    {
        string line = Console.ReadLine();
        string[] words = line.Split(new[] { ' ' });
        int n = int.Parse(words[0]);
        int m = int.Parse(words[1]);

        // 1回の提出で消費する時間
        int t = m * 1900 + (n - m) * 100;

        // 正解する確率
        double p = 1.0 / Math.Pow(2, m);

        // 不正解になる確率
        double q = 1 - p; 

        // 入力例1の解説から、期待値 x が次の無限級数で求まることが分かる。
        // x
        // = 1 * p * t + 2 * q * p * t + 3 * q^2 * p * t + ...
        // = p * t * (1 + 2 * q + 3 * q^2 + ...)

        // 無限級数を100万項まで展開する。
        double y = 0;
        double r = 1; // = q^i
        for (int i = 1; i < 1000000; i++)
        {
            y += i * r;
            r *= q;
        }

        // 四捨五入して整数にする。
        double x = p * t * Math.Round(y);

        Console.WriteLine("{0}", x);
        return;
    }
}
