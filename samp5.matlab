% This is a sample Matlab Notebook.  $Id$
% It has some tex commands.
	
\documentclass{notebook}
\begin{document}

Type C-c C-f to run convert this to {\tt samp5.tex}, and then run latex on it. 


Here is a matrix.
>>   a = [1 2 3; 2 1 5; -4 -1 3]  
a =
     1     2     3
     2     1     5
    -4    -1     3
 It's inverse is defined here:
>>  inv(a)  
ans =
   -0.2105    0.2368   -0.1842
    0.6842   -0.3947   -0.0263
   -0.0526    0.1842    0.0789

Next, we will try some plotting.  Here is $\sin(t)$ for $0 < t < pi$.
>>  t = [0:.01:pi];
plot(t, sin(t))
This is an example of a function in matlab.
>>  function y = junk(x)
y = 3 * x .^ 2 - x ;(function junk.m saved)
$x=1^4$
>>  plot(t, junk(t)) 
>>  eig(a) 
ans =
  -1.3352          
   3.1676 + 4.2925i
   3.1676 - 4.2925i

Here is some concatinated:
>> a = 1  
a =
     1

>> b = 2  
b =
     2


Here is some short
>  ehll = 123.2314234  
(no output yet)

 or
$> z_2 = 4.55^3;   $ or 
$>  z_4 + z_2  
ans =
   97.3364
 $

Here is:
>  [4  3]  
ans =
     4     3

Or >  a = [4  3]  
ans =
     4     3


Within math delimeters: $ >  z = 4+3  
z =
     7
$
and without math delimeters >  z = 4+3  
z =
     7
(end)


With dollar signs $> [4 3 sin(3)]  
ans =
    4.0000    3.0000    0.1411
$



\end{document}
