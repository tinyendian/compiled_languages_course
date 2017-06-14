# Compiled languages - Fortran

Scientific computing uses a large variety of programming languages, such as Python, R, Matlab, C, C++, and Fortran. They fall into two main categories: Python, R, and Matlab are essentially interpreted languages, while C, C++, and Fortran are compiled languages. Note that this distinction is somewhat blurry, as interpreted languages often also provide a way to compile source code, but we will stick with these two categories two distinguish the way in which these languages are typically used.

So what is the difference between an interpreted and a compiled language? Loosely speaking, interpreted languages work out each instruction in the source code at runtime, each time the program is run. Compiled languages translate the entire program into so-called machine code once when the program is built, and only the machine code is used afterwards. This implies that source codes are not needed to run the program, but the program can no longer be changed after compilation, too. Programs like the operating system, Excel, or the Python interpreter are examples for software that has been built using a compiler.

For many scientific applications, interpreted languages are a great choice - they enable very fast development, they provide very powerful commands for reading, writing, and visualising data, and much more, and they are often somewhat easier to debug. Compiled languages on the other hand can offer best possible performance, enable very large and complex applications (with a million lines of source code or more), and can be distributed without having to reveal the source code (only relevant for closed-source applications).

## Fortran

Fortran ("Formula tranlator") is the oldest compiled language. It is arguably easier to learn than C and C++, which are also frequently used in scientific computing, and makes it easier to write fast code by sacrificing some of the power of C/C++. It is most useful for scientific applications with high numerical intensity, such as weather forecasting and climate prediction, computational chemistry, or astrophysical simulations. It is also often used to speed up compute-intensive parts of R code or Python code. The Fortran standard has been improved in many iterations, and modern Fortran enables much more elegant code than the older Fortran standards, including object-oriented programming.

Let's write a first program. We will create a new source file "hello_world.f90", where the suffix indicates that we will be using syntax that follows the Fortran 90 standard:
```{bash}
$ nano hello_world.f90
```
We will insert the following source code:
```{fortran}
PROGRAM hello_world
  PRINT *, 'Hello World!'
END PROGRAM hello_world
```
> Note the usage of capitals - Fortran is case-insensitive, but using capitals for language keywords can make the code easier to read. Feel free to use lowercase or CamelCase.

Now we need to compile the code. We will use the `gfortran` compiler, which is part of the GCC compiler suite and readily available on most platforms. Note that it sometimes isn't installed by default, though.

```{bash}
$ gfortran -o hello_world.x hello_world.f90
```
This command will get `gfortran` to build a new executable program, and the flag `-o` sets its name to `hello_world.x`. Let's try it:
```{bash}
$ ./hello_world.x
```
~~~{output}
Hello World!
~~~
So far so simple. The difference with a Python or R program is that we now have two files, the source code and the executable. Let's have a look at the executable:
```{bash}
$ less hello_world.x
```
The program `hello_world.x` is in binary format - that means that it cannot be read like a text file, it contains our program in an encoded form. To understand this a bit better, we will now find out what the compiler really does. It works in 3 steps (in principle - reality is a bit more complicated, as always...), and these are essentially the same for all compiled languages:

1. Translate the source code into machine language - this is the main job of a compiler
2. Create a so-called object-code file, typically with suffix `.o`
3. Link the program with additional code that makes it executable and implements some functions that we use, such as `PRINT`

> Note that even though the principles are always the same, the details of the following commands will differ between platforms, operating systems, compiler versions etc. It is normally never necessary (and also not recommended!) to run these commands manually, they are explained here only to explain the process.

Let's do only the first step and create a machine language file:
```{bash}
$ gfortran -S hello_world.f90
```
# AIX: gfortran -maix64 -S hello_world.f90
The flag `-S` asks the compiler to stop after the first step and dump the machine language output in a text file. Let's look at the file:
```{bash}
$ less hello_world.s
```
You will see that our very short program was turned into a large number of lines with somewhat cryptic commands, such as `lea` or `mov`. This is machine language - each command represents a very basic instruction that tells the processor what it should do next, and a bit-coded version of these instructions are executed at a very high rate (roughly 3 billion per second for a 3 GHz processor). The commands are called "mnemonics", to make them easier to read than bitcode.

We will now perform the next step and create the object file. We will need to run an additional program, the `as` assembler (the compiler will normally do this for us):
```{bash}
$ as -o hello_world.o hello_world.s
```
# AIX: as -u -o hello_world.o hello_world.s
# Flag -u accepts undefined symbols as external symbols
The object file contains machine language in bit-encoded format, so it is a binary file. It only contains our own program and nothing else. Although the program is now in an "executable" format, it is still missing essential parts - an implementation of the `PRINT` command that we used, and additional code that the operating system needs to start our program and clean up when it finishes. The linker `ld` will perform these tasks for us, it will gather up all required pieces of code and put them together in a way that the different pieces can be found (it computes memory addresses):

```{bash}
$ ld -o hello_world.x hello_world.o -dynamic-linker /lib64/ld-linux-x86-64.so.2 /usr/lib64/crt1.o /usr/lib64/crti.o /usr/lib/gcc/x86_64-redhat-linux/4.4.7/crtbegin.o /usr/lib/gcc/x86_64-redhat-linux/4.4.7/crtend.o /usr/lib64/crtn.o /usr/lib/gcc/x86_64-redhat-linux/4.4.7/libgfortranbegin.a /usr/lib/gcc/x86_64-redhat-linux/4.4.7/libgfortran.so /usr/lib64/libc.so
```
# AIX: ld -o hello_world.x hello_world.o /lib/crt0_64.o /opt/freeware/lib/gcc/powerpc-ibm-aix6.1.0.0/4.8.2/ppc64/libgfortran.a -blibpath:/opt/freeware/lib/gcc/powerpc-ibm-aix6.1.0.0/4.8.2/ppc64 /usr/lib/libc.a
# Flag -blibpath is needed to find shared libraries for dynamic linking
Very complicated... Luckily, we will never have to do this by hand, as the compiler will handle all of this for us. The additional files that appear are additional object code (suffix `.o`), and so-called libraries (suffices `.a` and `.so`). We will talk more about libraries and what they are used for later on. Let's try the executable again:
```{bash}
$ ./hello_world.x
```
~~~{output}
Hello World!
~~~
... and it still works!

> ## Challenge
> What happens if you remove the file "/usr/lib64/libc.so" from the linker command line? Can you imagine what these "symbols" are?
>
> Why is it necessary for a linker to compute memory addresses?