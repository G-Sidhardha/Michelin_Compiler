## Improvement 1:
Multithreading is a highly compatible feature with our language which includes
various processes that can happen parallelly thereby increasing the efficiency of the
code/process.
We plan to explicitly program the thread support to help the user to parallelize the
various processes while ensuring that they can be implemented easily.
This process of parallelization is highly dependent on the users and we plan to make
the library thread-safe.
For example, multiple stoves can run at the same time, accessing the ingredients
and other resources parallelly, multiple chefs using the oven or refrigerator simultaneously,
etc.
Possible Ideas for implementing thread support:
1. Creating a library from scratch that can interact with the OS.
2. Trying to involve the C or C++ library of threads into our language.
Due to the time constraints present on the project and due to the extra
knowledge required which involves the communication between the
language/compiler with the operating system to implement parallel programming
(multithreading), we have decided to not add this feature to our language as it may
end up taking too much time away from the designing process for other important
features.

<br /><br />
## Improvement 2:
We want to introduce a built-in function called as calibrate, this feature's main use would be to
change the various parameters of the in-built functions, for example , temp required for boil, 
speed required for mixing, since these values can change depending upon the type of container.
For example, In the case that we replaced a small pan with a sligtly larger pan within the hardware, 
now upon running the calibrate function with a small amount of input such as the dimensions of the pan,
this allows the inbuilt methods to be changed to fit the new pan.

<br /><br />
## Improvement 3:

Currently we handle the 3 main "datatypes" (Ingredients, Vessels, and Tools) of our languages in the form of classes and their objects. The classes in themselves are somewhat like super classes and have many kinds and layers of subclasses to them. 
While the number of vareities of Vessels and Tools are usually limited enough, the variation among the types of vegetables could be pretty high and so maintaining classes for each of those could be quite inefficient at times. 
While the machine possesses vessels and tools of its own, the amount of various ingredients naturally keep on changing. Thus instead of incorporating some space for those in the machine itself, we thought of connecting it to a small food storage centre (say some sort of a vending machine) which would contain ingredients and we could retrieve the required ingredients for a particular dish using some sort of database query, as they are in general, known to be highly efficient!
However, we didn't have enough knowledge and the understanding as to how we could integrate a database into our language and so refrained from trying to go too deep in that direction, though we feel that it should be possible to do so.

<br /><br />
## Improvement 4:

Right now we haven't implemented any explicit support for debugging, since we didn't have a very good idea of how we might want to start out on it. There is one aspect of debugging though that we will describe below as a possible improvement to our language. 

<br /><br />
## Improvement 5:

We thought of introducing the concept of a "dry run", before running any code in the machine (robotic chef). This in essence would go through the source code and look for inconsistencies in some of the potentially insecure aspects of the code. 
For instance there could be a code which can be correctly lexed as well as parsed, but has a statement which tries to fill in more milk into a bowl than it can hold! This is probably something we could check for at the semantic level, but actually, there are examples which even a normal semantic check might not be able to predict or warn of.

For instance, all the measurements, timings, etc. from the previous code could be correctly coded in, but the user might not have taken care of the fact that if at the temperature as coded in by the user, the milk in the bowl exceeds the maximum safe threshold (volume) limit of the bowl. 
Were the user not to be warned of this, he/she might run the code and find a nasty spill later!
This is the reason we wanted to implement this kind of a check. It's kind of a more sophisticated semantic analysis. 
