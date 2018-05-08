package casestudy.sql;

import lombok.Obj;

//BEGIN_FLUENT_DATABASE
@Obj interface Database {
    String select(); Database select(String select);
    String from();   Database from(String from);
    String where();  Database where(String where);
    static Database of() {return of("", "", "");} }
//END_FLUENT_DATABASE

//BEGIN_FLUENT_DATABASE_EXT
@Obj interface ExtendedDatabase extends Database {
	String orderBy();
	ExtendedDatabase orderBy(String orderBy);
	static ExtendedDatabase of() {
		return of("", "", "","");} }
//END_FLUENT_DATABASE_EXT

public interface DatabaseTest {
    static Database runTest0() {
//BEGIN_NORMAL_QUERY
Database query1 = Database.of();
query1.select("a, b");
query1.from("Table");
query1.where("c > 10");
//END_NORMAL_QUERY
        return query1;
    }
    static Database runTest() {
//BEGIN_FLUENT_QUERY1
Database query1 = Database.of().select("a, b").from("Table").where("c > 10");
//END_FLUENT_QUERY1
        return query1;
    }
    static ExtendedDatabase runTest2() {
//BEGIN_FLUENT_QUERY2
ExtendedDatabase query2 = ExtendedDatabase.of().select("a, b").from("Table").where("c > 10").orderBy("b");
//END_FLUENT_QUERY2
        return query2;
    }
    public static void main(String[] args) {
        runTest();
        runTest2();
    }
}
