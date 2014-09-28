package projectEuler;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.TreeSet;

public class UsefulMethods {

    private UsefulMethods() {
    }

    ;

    public static boolean isPrime(int num) {
        boolean prime = true;
        int limit = (int) Math.sqrt(num);
        if (num == 1 || num == 2) {
            return true;
        } else if (num % 2 == 0) {
            return false;
        }
        for (int i = 3; i <= limit; i += 2) {
            if (num % i == 0) {
                prime = false;
                break;
            }
        }
        return prime;
    }

    public static int nthPrime(int n) {
        int i = 2;
        int prime = 1;
        if (n == 1) {
            return 1;
        } else if (n == 2) {
            return 2;
        } else while (i < n) {
            prime += 2;
            if (isPrime(prime)) {
                i++;
            }
        }
        return prime;
    }

    public static boolean isTriangle(long num) {
        int n = (int) Math.sqrt(2 * num);
        return num == n * (n + 1) / 2;
    }

    public static boolean isSquare(long num) {
        return (int) Math.sqrt(num) * (int) Math.sqrt(num) == num;
    }

    public static boolean isPentagonal(long num) {
        long n = (long) (Math.sqrt(2 * num / 3.0) + 1);
        return num == (n * (3 * n - 1)) / 2;
    }

    public static boolean isHexagonal(long num) {
        int n = (int) (Math.sqrt(num / 2) + 1);
        return num == n * (2 * n - 1);
    }

    public static boolean isHeptagonal(long num) {
        int n = (int) (Math.sqrt(2 * num / 5) + 1);
        return num == n * (5 * n - 3) / 2;
    }

    public static boolean isOctagonal(long num) {
        int n = (int) (Math.sqrt(num / 3) + 1);
        return num == n * (3 * n - 2);
    }

    public static int gcd(int p, int q) {
        if (q == 0) {
            return p;
        }
        int r = p % q;
        return gcd(q, r);
    }

    public static int totient(int number) {
        int counts = 1;
        for (int i = 2; i < number; i++) {
            if (gcd(number, i) == 1) {
                counts++;
            }
        }
        return counts;
    }

    public static boolean isPermutation(int one, int two) {
        char[] first = String.valueOf(one).toCharArray();
        char[] second = String.valueOf(two).toCharArray();
        if (first.length != second.length) {
            return false;
        }
        Arrays.sort(first);
        Arrays.sort(second);
        return Arrays.equals(first,second);

    }


    public static int getNextPrime(int prime) {
        if (prime == 1) {
            return 2;
        } else if (prime == 2) {
            return 3;
        } else do {
            prime += 2;
        } while (!isPrime(prime));
        return prime;
    }

    public static TreeSet<Integer> getPrimeComponents(int num) {
        return getPrimeComponents(num, new TreeSet<Integer>());
    }

    private static TreeSet<Integer> getPrimeComponents(int num, TreeSet<Integer> components) {
        int divisor = getNextDivisor(num);
        if (divisor != 1) {
            components.add(divisor);
            return getPrimeComponents(num / divisor, components);
        } else {
            return components;
        }
    }

    private static int getNextDivisor(int num) {
        int prime = 2;
        while (prime <= Math.sqrt(num)) {
            if (num % prime == 0) {
                return prime;
            } else {
                prime = getNextPrime(prime);
            }
        }
        return num;
    }

    public static int getNextComposite(int num) {
        do {
            num += 1;
        } while (isPrime(num));

        return num;
    }

    public static int[] toDigits(long num) {
        String number = String.valueOf(num);
        int[] digits = new int[number.length()];
        for (int i = 0; i < digits.length; i++) {
            digits[i] = number.charAt(i) - 48;
        }
        return digits;
    }

    public static long factorial(int num) {
        if (num == 0) {
            return 1;
        } else {
            long fact = 1;
            for (int i = 1; i <= num; i++) {
                fact *= i;
            }
            return fact;
        }
    }

    public static boolean isPolindromic(BigInteger num) {
        StringBuilder str = new StringBuilder(String.valueOf(num));
        StringBuilder str2 = new StringBuilder(str);
        str2.reverse();
        return str.toString().equals(str2.toString());
    }

    public static long reverse(long num) {
        StringBuilder str = new StringBuilder(String.valueOf(num));
        long result = Long.parseLong(String.valueOf(str.reverse()));
        return result;
    }

    public static BigInteger reverse(BigInteger num) {
        StringBuilder str = new StringBuilder(String.valueOf(num));
        BigInteger result = new BigInteger(String.valueOf(str.reverse()));
        return result;
    }

    public static int sumDigits(BigInteger num) {
        int sum = 0;
        while (num.compareTo(BigInteger.ZERO) == 1) {
            sum += num.remainder(BigInteger.TEN).intValue();
            num = num.divide(BigInteger.TEN);
        }
        return sum;
    }

    public static int sumDigits(long num) {
        int sum = 0;
        long number = num;
        while (number >= 1) {
            sum += number % 10;
            number = number / 10;
        }
        return sum;
    }

    public static BigInteger power(int base, int power) {
        BigInteger value = BigInteger.valueOf(base);
        BigInteger baseB = BigInteger.valueOf(base);
        for (int i = 0; i < power; i++) {
            value = value.multiply(baseB);
        }
        return value;
    }


}
