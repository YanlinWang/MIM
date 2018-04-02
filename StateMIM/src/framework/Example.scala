package framework

object Example {
  import AST._
  /*
   * interface A {
   * 	static A of();
   * }
   * A.of()
   */
  val intfA : TypeDef = TypeDef("A", List(), List(), Some(Constructor("A", "of", List())))
  val interfaces : List[TypeDef] = List(intfA)
  val expr : Expr = InvkStatic("A", "of", List())
  val program0: Program = Program(interfaces, expr)
   /*
   * interface A {
   * 	static A of();
   * }
   * let A x = A.of(); x
   */
  val expr1 : Expr = LetExpr("A", "x", expr, Var("x"))
  val program1: Program = Program(interfaces, expr1)
  /*
   * interface Int {}
   * interface I3 extends Int {
   * 	static I3 of();
   * }
   * interface A {
   * 	Int x() update A;
   * 	static A of(Int A.x);
   * }
   * A.of(I3.of()).x()
   */
  val intfInt: TypeDef = TypeDef("Int", List(), List(), None)
  val intfI3 : TypeDef = TypeDef("I3", List("Int"), List(), Some(Constructor("I3","of", List())))
  val A2Constr: Constructor = Constructor("A","of", List(Field("Int","A","x")))
  val methX: MethDef = MethDef("Int","x",List(), "A", None)
  val intfA2 : TypeDef = TypeDef("A", List(), List(methX), Some(A2Constr))
  val expr2: Expr = Invk(InvkStatic("A", "of", List(InvkStatic("I3","of",List()))), "x", List())
  val program2: Program = Program(List(intfInt, intfI3, intfA2), expr2)
   
  /*
   * interface Int {}
   * interface I2 extends Int {
   * 	static I2 of();
   * }
   * interface I3 extends Int {
   * 	static I3 of();
   * }
   * interface A {
   * 	Int x() update A;
   * 	static A of(Int A.x);
   * }
   * A.of(I3.of()).SET_x(I2.of()).x()
   */
  val intfI2 : TypeDef = TypeDef("I2", List("Int"), List(), Some(Constructor("I2","of", List())))
  val expr3: Expr = Invk(InvkSetter(InvkStatic("A", "of", List(InvkStatic("I3","of",List()))), "x", InvkStatic("I2", "of", List())),
      "x", List())
  val program3: Program = Program(List(intfInt, intfI3, intfI2, intfA2), expr3)
  
  /*
   * interface Int {}
   * interface I2 extends Int {
   * 	static I2 of();
   * }
   * interface I3 extends Int {
   * 	static I3 of();
   * }
   * interface A {
   * 	Int x() update A;
   *  Int m() update A { return this.x(); }
   * 	static A of(Int A.x);
   * }
   * A.of(I3.of()).SET_x(I2.of()).m()
   */  
  val methM: MethDef = MethDef("Int", "m", List(), "A", Some(Invk(Var("this"), "x", List())))
  val intfA3 : TypeDef = TypeDef("A", List(), List(methX, methM), Some(A2Constr))
  val expr4 : Expr = Invk(InvkSetter(InvkStatic("A", "of", List(InvkStatic("I3","of",List()))), "x", InvkStatic("I2", "of", List())),
      "m", List())
  val program4: Program = Program(List(intfInt, intfI3, intfI2, intfA3), expr4) // (Int, I2)
  
  /* //AnnoExpr
   * interface Int {}
   * interface I2 extends Int {
   * 	static I2 of();
   * }
   * interface I3 extends Int {
   * 	static I3 of();
   * }
   * interface A5 {
   * 	Int x() update A5;
   * 	static A5 of(Int A5.x);
   * }
   * interface B5 {
   * 	Int x() update B5;
   * 	static B5 of(Int B5.x);
   * }
   * interface C extends A5, B5 {
   * 	static C of(Int A5.x, Int B5.x);
   * }
   * (A5)((A5)C.of(I2.of(), I2.of()).SET_x(I3.of())).x()
   */  
  val A5Constr: Constructor = Constructor("A5","of", List(Field("Int","A5","x")))
  val methAX5: MethDef = MethDef("Int","x",List(), "A5", None)
  val intfA5: TypeDef = TypeDef("A5", List(), List(methAX5), Some(A5Constr))
  
  val B5Constr: Constructor = Constructor("B5","of", List(Field("Int","B5","x")))
  val methBX5: MethDef = MethDef("Int","x",List(), "B5", None)
  val intfB5: TypeDef = TypeDef("B5", List(), List(methBX5), Some(B5Constr))
  
  val CConstr: Constructor = Constructor("C", "of", List(Field("Int", "A5", "x"), Field("Int", "B5", "x")))
  val intfC: TypeDef = TypeDef("C", List("A5","B5"), List(), Some(CConstr)) 
  
  val anno: Expr = AnnoExpr("A5", InvkStatic("C", "of", List(InvkStatic("I2","of",List()), InvkStatic("I2","of",List()))))
  val expr5: Expr = Invk(AnnoExpr("A5", InvkSetter(anno, "x", InvkStatic("I3", "of", List()))), "x", List())
  val program5: Program = Program(List(intfInt, intfI2, intfI3, intfA5, intfB5, intfC), expr5)
}