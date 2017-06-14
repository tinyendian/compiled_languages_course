# Fortran variables

We will now proceed with defining variables in Fortran. Variables are used in the same way as in Python or R, but there is a fundamental difference: all variables need to have a defined type, such as integer, real, or character. While this removes some of the ease and flexibility of using these variables, it allows the compiler to help us by detecting erroneous use of variables, which can prevent nasty bugs in complex programs.

## Character strings

Let us start a new program which will compute the famous Mandelbrot set (https://en.wikipedia.org/wiki/Mandelbrot_set):
```{bash}
$ nano mandelbrot.f90
```
```{fortran}
PROGRAM mandelbrot
  IMPLICIT NONE
  CHARACTER(len=100) :: greeting
  
  greeting = 'This program computes the famous Mandelbrot set very quickly!'
  PRINT *, greeting

END PROGRAM
```
Compile and run the program as before (we will omit these steps from now on):
```{bash}
$ gfortran -o mandelbrot.x mandelbrot.f90
$ ./mandelbrot.x
```
~~~{output}
 This program computes the famous Mandelbrot set very quickly!
~~~

There are two important points here: we have to tell the compiler that `greeting` is a string variable, and we have to give the variable sufficient length to hold our string.

Fortran also supports "implicit typing" - this means that variables that start with a certain character automatically assume integer or floating point types. A variable called "i" would assume integer type, while a variable called "pi" would assume floating point type. Specifying `IMPLICIT NONE` turns implicit typing off, forcing the programmer to explicitly declare each variable type - highly recommended!

> Challenge
>
> 1. What happens if you set define `greeting` as `CHARACTER(len=10)`? Does the compiler warn you if `len` is smaller than the character string that is assigned to the variable?
>
> 2. What is the outcome of this code fragment:
>
> ```{fortran}
> greeting = 'This program computes the famous Mandelbrot set very quickly!'
> greeting = 'Mandelbrot!'
> PRINT *, greeting
> ```
>
> a. 'Mandelbrot!m computes the famous Mandelbrot set very quickly!'
>
> b. 'Mandelbrot!'
>
> c. 'This program computes the famous Mandelbrot set very quickly! Mandelbrot!'
>
> 3. What happens if you initialise `greeting` with an integer number, e.g., `greeting = -1`?

## Integer and floating point variables

Integers and floating point variables are easily defined in Fortran:
```{fortran}
PROGRAM mandelbrot
  IMPLICIT NONE
  CHARACTER(len=100) :: greeting
  INTEGER :: version
  REAL :: pi
  
  greeting = 'This program computes the famous Mandelbrot set very quickly!'
  PRINT *, greeting

  version = 1
  PRINT *, 'This is version', version

  pi = 4.0 * atan(1.0)
  PRINT *, 'pi =', pi

END PROGRAM
```
~~~{output}
 This program computes the famous Mandelbrot set very quickly!
 This is version           1
 pi =   3.14159274
~~~
Note that variable definitions **always** have to appear at the top of the program! It is not possible to mix variable definition and program statements.

> Challenge
>
> What happens if you redefine `greeting` as follows:
>
> ```{fortran}
> CHARACTER(len=100) :: greeting
> real :: greeting`
> ```

## Variable kind

An important topic is variable kind, which sets the number range of integer variables and the number range and precision of floating point variables. The correct way to achieve this is to get the compiler to work out the required variable kind for us:
```{fortran}
  ! Get kind for integer range -10^9 < n < 10^9, on most platforms 4 (32bit)
  INTEGER, PARAMETER :: int_kind = SELECTED_INT_KIND(9)
  INTEGER(kind=int_kind) :: long_int_number
  ! Get kind for precision of 15 digits and decimal exponent range of +-307,
  ! on most platforms 8 (64bit)
  INTEGER, PARAMETER :: real_kind = SELECTED_REAL_KIND(15,307)
  REAL(kind=real_kind) :: double_prec_number
```
You need to take care of this yourself - if range or precision are too small, your program will very likely produce erroneous results, and there will be no warning. If they are too large, you loose performance. Although using `SELECTED_INT_KIND` and `SELECTED_REAL_KIND` might seem cumbersome, it will ensure that the program will work correctly on all platforms.

## Type conversion

ADD TEXT: HOW TO CONVERT BETWEEN DIFFERENT VARIABLE TYPES

## Output formatting

The output that our program produced so far is not very pretty - we need to use format specifiers to get nicer output format:
```{fortran}
PROGRAM mandelbrot
  IMPLICIT NONE
  CHARACTER(len=100) :: greeting
  INTEGER :: version
  REAL :: pi
  
  greeting = 'This program computes the famous Mandelbrot set very quickly!'
  PRINT '(A)', greeting

  version = 1
  PRINT '(A, X, I1)', 'This is version', version

  pi = 4.0 * atan(1.0)
  PRINT '(A, 3X, F4.2)', 'pi =', pi

END PROGRAM
```
~~~{output}
This program computes the famous Mandelbrot set very quickly!
This is version 1
pi =    3.14
~~~
The format string contains so-called "data edit descriptors":

* `A` denotes a character string
* `X` denotes whitespace, `3X` repeats it 3 times
* `I1` denotes an integer with 1 digit
* `F4.2` denotes a floating point number with total length 4 and 2 decimal digits
