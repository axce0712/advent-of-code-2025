var inputLines = File.ReadLines("inputs/day03.txt");
Console.WriteLine($"Part One: {Solve(inputLines, 2)}");
Console.WriteLine($"Part Two: {Solve(inputLines, 12)}");

static ReadOnlySpan<int> FindHighest(ReadOnlySpan<int> bank, int batteryCount)
{
    ArgumentOutOfRangeException.ThrowIfNegativeOrZero(batteryCount);
    if (bank.Length < batteryCount)
    {
        throw new ArgumentException("Not enough batteries", nameof(bank));
    }

    var acc = bank;
    var current = acc[1..];
    while (current.Length >= batteryCount)
    {
        if (current[0] > acc[0])
        {
            acc = current;
        }

        current = current[1..];
    }

    return acc;
}


static long IntPow10(int n)
{
    long x = 1;
    while (n-- > 0)
    {
        x *= 10;
    }
    
    return x;
}

static long LargestPossibleJoltage(ReadOnlySpan<int> bank, int batteryCount)
{
    ArgumentOutOfRangeException.ThrowIfNegativeOrZero(batteryCount);
    long acc = 0;
    var count = batteryCount;
    var current = bank;
    while (count > 0)
    {
        var highest = FindHighest(current, count);
        acc += highest[0] * IntPow10(count - 1);
        current = highest[1..];
        count--;
    }

    return acc;
}

static long Solve(IEnumerable<string> inputLines, int batteryCount)
{
    var banks = inputLines.Select(line => line.Select(c => c - '0').ToArray());
    long total = 0;
    foreach (var bank in banks)
    {
        total += LargestPossibleJoltage(bank, batteryCount);
    }

    return total;
}