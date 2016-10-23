Slides: 
https://docs.google.com/presentation/d/17p_wZjFICOFEzSX6-hSsV492Nhjl5Sh3uiK7P6tOMjQ/edit#slide=id.p





#Discussion:
##Marco
both C++ and Csharp keep both implementations, and you can use a
syntax similar to your
`c.A.m() (I think it is c.A::m() )`.
In java also both implementations are kept, but you need to use super
(and thus this as receiver)

##Marco
```
Base {m(){0}}
A extends Base{m(){1}}
B extends Base{m(){2}}
C extends A,B
fun(Base b){ b.m() }
//This call is ambiguos, but only a full program analysis can see that is ambiguos.
```
Reply[by Bruno]: 
I believe that the right thing todo for diamonds is to follow the traits approach, and force the user to chose *one* implementation. 

##Bruno
Yanlin, I think the point you want to make is that with your proposal you can override both A.m() and B.m() in C with *two different* implementations. This is not possible in existing languages: if you do override you can only have one implementation of m in C.

##Marco
one possibility is to say that
-every method is born is exactly one point and overridden here and there...
-when definining a method you use two different syntax:
one to define it as "new" and one to "override" a method from a base class.
-Conceptually this is a sort of overloading, in the "bytecode" every
method would have the name of
the original class encoded

This have some more similarities with Csharp

##Bruno
I think an example would be something like this:

```java
interface Deck {
   draw();  // draws (i.e. picks) a card from the Deck
}

interface Drawable {
   draw();  // draws something on the screen
}

interface DrawableDeck extends Deck, Drawable {
   ...
} 
```

Here there are two “draw” methods (independently developed) 
with very different semantic meanings. However, merging the two 
methods is clearly the wrong thing todo in this case. 

I think this problem is a kind of dual to the diamond problem: 
the diamond problem is about what happens when you inherit 
semantically related methods. However, what if you have a situation, 
where you happen to have semantically unrelated methods.


---
#Literature Review
##

###Smalltalk
- single inheritance
- subclass/superclass
- super

```
class Person 
	instance variables: name 
	method: display 
		name display

class Graduate 
	superclass: Person 
	instance variables: degree 	
	method: display 
		super display. degree display
```
`C = delta(P) |+| P` 

###Beta
- single inheritance.
- superpattern/subpattern 
- inner

```
Person: class 
	(# name : string; 
	  display: virtual proc 
		  (# do name.display; inner #);
#);

Graduate: class Person 
(# degree: string; 
  display: extended proc 
    (# do degree.display; inner #);
#);
```

In single inheritance, conclusion:

- Method conflict may occur but will be treated as overriding. 
- Different methods with the same signature should appear in the first place!


###CLOS (Common-Lisp Object System)
- multiple inheritance
- linearization: attribute values appearing earlier in the list replace (and may refer to) those appearing later. 
- Disadvantage of linearization: violating encapsulation - the relationships between primitive components may be changed. Also, linearization order is fixed, cannot call arbitrary super methods.
- already have 'mixins'
- deltas cannot be defined independently, have to specify the parent class.

```
(defclass Person() (name))
(defmethod display ((self Person))
	(display (slot-value self 'name)))
(defclass Graduate (Person) (degree))
(defmethod display ((self Graduate))
	(call-next-method)
	(display (slot-value self 'degree)))

(defclass Doctor (Person) ())
(defmethod display ((self Doctor))
	(display "Dr. ")
	(call-next-method))

(defclass Research-Doctor (Doctor Graduate))
```

```
(defclass C1() (c1))
(defmethod display ((self C1))
	(display (slot-value self 'c1)))
(defclass C2 (C1) (c2))
(defmethod display ((self C2))
	(call-next-method)
	(display (slot-value self 'c2)))

(defclass C3 (C1) ())
(defmethod display ((self C3))
	(display "HI. ")
	(call-next-method))

(defclass C4 (C3 C2))
```



### Mixins Models
Previous work (not a full list):

- early attempt: Moon's Flavors (OO programming with Flavors)
- Bracha and Cook (mixin-based inheritance)
- Mens and van Limberghen (Encapsulation and composition as orthogonal operators on mixins: A solution to multiple inheritance problems.)
- Flatt, Krishnamurthi and Felleisen (Classes and Mixins)
- Ancona, Lagorio and Zucca (Jam: java with mixins)

Pros (compared with CLOS):   

- deltas can be independently defined.
- does not break encapsulation.

Cons (in general):

- total ordering. 
- dispersal of glue code 
- fragile hierarchies

Mixins use the ordinary single inheritance operator to extend various base classes with the same set of features. However, although this inheritance operator is well-suited
for deriving new classes from existing ones, it is not appropriate for composing reusable building blocks. Specifically, inheritance requires that mixins be composed linearly; this severely restricts one’s ability to specify the “glue code” that is necessary to adapt the mixins so that they fit together.

Mixin composition is linear: all the mixins used by a class must be inherited one at a time. Mixins appearing later in the order override all the identically named features of earlier mixins. When we wish to resolve conflicts by selecting features from different mixins, we may find that a suitable total order does not exist.

```scala
/* example: 
F3 use F2-m1, F2-m2; F4 use F1-m1, F1-m2
But there's no suitable ordering to have the `F1-m1, F2-m2` combination.
*/
mixin F1 { 
	def m1() {println("F1-m1()")} 
	def m2() {println("F1-m2()")}
}
mixin F2 { 
  def m1() {println("F2-m1()")} 
  def m2() {println("F2-m2()")} 
}
mixin F3 extends F1 * F2
mixin F4 extends F2 * F1
```

### Scala Mixin

- trait C extends traitA with traitB, right traitB is prefered.
- Trait classes do not have any independent existence.
- mixins (trait) have no constructor parameters.


```scala
trait E1 { def m() {println("E1")} }
trait E2 { def m() {println("E2")} }
trait E3 extends E1 with E2 
/*error:trait E3 inherits conflicting members: method m in trait E1 
of type ()Unit and method m in trait E2 of type ()Unit 
(Note: this can be resolved by declaring an override in trait E3.)*/
```

```scala
trait A { def m() { println("A") } }
trait B extends A { override def m() { println("B"); super.m }}
trait C extends A { override def m() { println("C"); super.m }}
trait D extends B with C
/* this is ok! result is : "CBA" 
linearization is done, no duplication of A
can we say encapsulation is broke since now C does not directly extends A?
*/
```

```scala
class Base { def m() { println("Base")}}
trait A extends Base { override def m() { println("A"); super.m } }
trait B extends A { override def m() { println("B"); super.m }}
trait C extends A { override def m() { println("C"); super.m }}
trait D extends B with C { override def m() { println("D"); super.m }}
/* prints "D C B A Base" */
```

### Java 8
```java
interface M1 { default void m() {System.out.println("M1");} }
interface M2 { default void m() {System.out.println("M2");} }
interface M3 extends M1, M2 { }
/* Duplicate default methods named m with the parameters () and () 
are inherited from the types M2 and M1 */

interface M3 extends M1, M2 { default void m() {...} }
```

### Traits
Traits: a new OO model to fighting against various problems in multiple inheritance (especially mixins).

Previous work:

- Schärli, Nathanael, et al. "Traits: Composable units of behaviour." ECOOP'03
- Ducasse, Stéphane, et al. "Traits: A mechanism for fine-grained reuse." TOPLAS'06
- Reppy, John, and Aaron Turon. "Metaprogramming with traits." ECOOP'07
- ...

In case of a naming collision, when more than one trait to be used by a class has a method with the same name, the programmer must explicitly disambiguate which one of those methods will be used in the class; thus manually solving the diamond problem of multiple inheritance.

3 ways to resolve conflict:

- overriding
- rename
- exclusion

We use the syntax from the Traits paper (Traits: Composable Units of Behaviour).

- Explicit overriding, similar to Java 8.

- rename

```haskell
Trait named: #T1 uses: {}
	m
		"T1" print

Trait named: #T2 uses: {}
	m
		"T2" print

-- If T3 extends T1 and T2 with renaming, then both methods are kept.
-- However, because of the name changing, it breaks subtyping relation!
Object subclass: #T3 
	uses: {	T1 @ {#T1m -> #m}.
			T2 @ {#T2m -> #m} }
```

- exclusion

```haskell
Trait named: #T1 uses: {}
	m
		"T1" print

Trait named: #T2 uses: {}
	m
		"T2" print

-- T3 uses T2.m, T1.m is excluded.
Object subclass: #T3 
	uses: { T1 - {#m} . T2 }
```

For unintentional naming conflicts problem, as said in the Traits paper,
> At present, traits offer no real solution to this problem. Aliases alleviate the problem only to a small extent. A complete solution requires both good refactoring tools and explicit namespaces.

### C++

C++ virtual inheritance:

- Multiple conflicting methods can be preserved.
- These methods can be accessed as `ParentName::methoeName` style, even in further subclasses.
- However, the semantics is fragile to method extension.

```c++
class A { public: void m() {cout << "MA" << endl;}};
class B { public: void m() {cout << "MB" << endl;}};
class C : public A, public B { void m() {cout << "MC" << endl;}};
void func(A* a) { a->m(); }
int main() {
	C* c = new C();
	c->B::m();
	func(c); 
	return 0; //Running result: MB MA
}
```

```c++
class A { public: virtual void m() {cout << "MA" << endl;}};
class B { public: virtual void m() {cout << "MB" << endl;}};
class C : public A, public B { public: virtual void m() {cout << "MC" << endl;}};
void func(A* a) { a->m(); }
int main() {
	C* c = new C();
	c->B::m();
	func(c); 
	return 0; //Running result: MB MC
}
```


### CZ

#Formalization
##FJ:
- missing multiple inheritance, interface defaults.
- Featherweight Java. Igarashi.
- Featherweight Defenders. Brian Goetz and Robert Field.

##traits:
- missing conflicts resolution algorithms.
- A typed calculus of traits. Fisher and Reppy
- A foundation for trait-based metaprogramming. Reppy and Turon.

##c++:
- missing ‘improves’;
- we don’t need class multiple inheritance, only traits-like is needed.
- https://books.google.com.hk/books?id=zwxQ1dd1Nu8C&pg=PA160&lpg=PA160&dq=c%2B%2B+multiple+inheritance+calculus+model&source=bl&ots=oCdXifC2Tr&sig=OIbQqTGcoXEW3zr63NXjOA1MjWA&hl=en&sa=X&ved=0ahUKEwiIsK-L84jPAhUBN5QKHWwDDwcQ6AEIYDAJ#v=onepage&q=c%2B%2B%20multiple%20inheritance%20calculus%20model&f=false
- An operational semantics and type safety proof for multiple inheritance in C++

##FTJ:
- FeatherTrait Java. Liquori and Spiwack
- Extending FeatherTrait Java with Interfaces.

##There are also formal models for Mixins:
- A Core Calculus of Classes and Mixins. Bono, Patel and Shmatikov.
- Jam: Java with Mixins extension. (This paper has formal definitions for the model). Ancona, Lagorio, Zucca.

##Others
- Papers discussing design space, principle, problems of multiple inheritance.
- Other models/languages on multiple inheritance.
- Featherweight Defenders

## `mbody`
`interface I extends I_bar {M_bar}`

`mbody(m, I)` algorithm: 
 
* If m is defined in I directly, then return `I.m()`  
* Else, let `I'_bar = mdefined(fathers(I))`, all accestors of `I` that has directly defined `m()`.
* `I''_bar = needed(I'_bar)`, keep only interfaces that are needed, which are not superinterface of others.
* If `I''_bar` is unique, then return this unique one. Else if any two I1,I2 in `I''_bar` share a parent in `I'_bar`, then diamond conflict is detected, report error. Else return multiple m()s. 

##Question
- How to forbid super-call outside class def. (Separate two sets of expressions seems not to be a good solution. Just mention this in the text as a documentation?)

```c++
mbody(m, C, A)
// Q1: which method m() will be returned?
// Q2: Is it reasonable to allow both "update C" (which overrides A.m and B.m) and "update A" coexist?

interface C extends A, B {
	m() { ... }
	m() update A { ... }
}
```

```c++
A {m()}
B {m()}
D extends A, B { m() }
C extends D { ?? } 
//inside C, is it OK to override A or B?

//I think we can restrict to original.
```

```c++
A {m()}
B extends A { m() override A }
C extends B { ?? }
//inside C, is "m() override B" correct? is "m() override A" correct?
```

```c++
 
```
#References
- Mixin-based Inheritance. Ecoop'90.
-  Matthew Flatt, Shriram Krishnamurthi, and Matthias Felleisen. Classes and mixins. POPL'98
- Jam - a smooth extension of java with mixins. ECOOP'00.
-  Encapsulation and composition as orthogonal operators
on mixins: A solution to multiple inheritance problems
- Object-oriented programming with flavors
- CZ
- Traits: composable units of behaviour.
- Java language specification.
- C++ virtual inheritance. http://www.cprogramming.com/tutorial/virtual_inheritance.html
