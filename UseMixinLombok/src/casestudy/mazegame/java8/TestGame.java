package casestudy.mazegame.java8;
import static java.lang.System.out;

/* Defines a base-door with no particular features */
interface TDoor {
    public boolean getLocked();
    public int getDoorMaxCoins();

    default boolean isLocked() {
        return getLocked();
    }
    default int open() {
        if (!isLocked()) {
            out.println("The door has been opened!");
            double rnd = Math.random();
            int cns = (int) (rnd * getDoorMaxCoins()) + 1;
            out.println("You got " + cns + " coins.");
            return cns;
        }
        else {
            out.println("This door is locked.");
            return -1;
        }
    }
    default int knock() {
        out.print("Door says: ");
        out.print("How you dare, ");
        out.println("I am the one who knocks!");
        int c = (Math.random() < 0.8) ? 0 : 1;
        if (c > 0)
            out.println("Ow! You got a free coin!");
        return c;
    }
}

/* Provides a counter that after a limit releases coins */
interface TCounter {
    public int getCounter();
    public void setCounter(int c);
    public int getLimit();
    public int getCounterMaxCoins();
    default void incrementCounter() {
        setCounter(getCounter() + 1);
    }
    default void decrementCounter() {
        setCounter(getCounter() - 1);
    }
    default boolean hasReachedLimit() {
        return getCounter() >= getLimit();
    }
    default int releaseCoins() {
        double rnd = Math.random();
        int cns = (int) (rnd * getCounterMaxCoins()) + 1;
        out.println("You got " + cns + " coins.");
        return cns;
    }
}

/* Provides a chest that contains coins */
interface TChest {
    public int getChestMaxCoins();
    default int open() {
        out.print("The chest is now opened!");
        double rnd = Math.random();
        int c = (int) (rnd * getChestMaxCoins());
        out.print("You got " + c);
        out.println(" coins from the chest.");
        return c;
    }
}

/* Provides an enchantment that can give or take coins */ 
interface TEnchantment {
    public int getEnchantMaxCoins(); 
    /** An enchantment can give coins
(max getEnchantMaxCoins()) or
remove coins (max -getEnchantMaxCoins()) **/ 
    default int applyEnchantment() {
        out.println("\nThis is an enchantment!");
        out.print("\"If the luck is up, ");
        out.println("of coins you’ll have a cup,");
        out.print("but if no luck you got, ");
        out.println("you are gonna lose a lot.\"");
        int max = getEnchantMaxCoins();
        double rnd = Math.random();
        int cns = -max + (int)(rnd*((max*2)+1)); 
        if(cns >= 0) {
            out.print("Ohoh! You got "+cns);
            out.println(" coins!"); 
        }
        else {
            out.print("You lost "+Math.abs(cns)); 
            out.println(" coins!");
        }
        return cns; 
    }
}

/* Puts together a door and an enchantment */
interface TEnchantedDoor extends TDoor, TEnchantment {
    /** When you open an enchanted door,
    you break the enchantment and so
    you apply it. **/
    default int open() {
        int coins = TDoor.super.open();
        if (coins > 0) //if the door is open
            coins += applyEnchantment();
        return coins;
    }
}

/* Puts together a door and a chest */ 
interface TChestedDoor extends TDoor, TChest {
    /** When you open a chested door,
you also get the prize from the chest.
This overrides the TDoor’s open(). **/
    default int open() {
        int coins = TDoor.super.open(); 
        if(coins > 0) //if the door is open
            coins += openChest();
        return coins;
    }
    /* Alias for open() from TChest */
    default int openChest() {
        return TChest.super.open();
    }
}

/* Puts together a door and a counter */
interface TKnockDoor extends TDoor, TCounter {
    /** Every know makes the counter increment.
     * If the limit is reached, more coins are released. **/
    default int knock() {
        int coins = TDoor.super.knock(); 
        incrementCounter(); 
        if(hasReachedLimit()) {
            out.print("Ohh! A special drop for you!");
            coins += releaseCoins();
        } else {
            //Let’s give a suggestion to the player 
            out.print("Don’t challenge me... ");
            int c = getLimit();
            String sug = "never knock a door ";
            sug = sug + "more then " + c + " times.";
            out.println(sug);
        }
        return coins;
    }
}

class ChestedDoor implements TChestedDoor {
    private boolean locked;
    public boolean getLocked()
    {
        return this.locked;
    }
    public int getDoorMaxCoins()
    {
        return 120;
    }
    public int getChestMaxCoins()
    {
        return 250;
    }
    /* Constructor */
    public ChestedDoor(boolean l)
    {
        setLocked(l);
    }
    public void setLocked(boolean l)
    {
        this.locked = l;
    }
}

class EnchantedDoor implements TEnchantedDoor {
    /* Fields for the door */
    private boolean locked;
    /* Glue Code for TDoor */
    public boolean getLocked() {
        return locked;
    }
    /* Glue code for coin management */
    public int getDoorMaxCoins() {
        return 120;
    }
    public int getEnchantMaxCoins() {
        return 150;
    }
    /* Constructor */
    public EnchantedDoor(boolean l) {
        setLocked(l);
    }
    /* Other helpful methods */
    public void setLocked(boolean l) {
        this.locked = l;
    }
}

class KnockDoor implements TKnockDoor { 
    /* Fields for the door */
    private boolean locked;
    /* Fields for the counter */
    private int counter;
    private int limit;
    /* Glue Code for TDoor */
    public boolean getLocked()
    {
        return this.locked;
    }
    /* Glue code for TCounter */
    public int getCounter()
    {
        return this.counter;
    }
    public void setCounter(int c)
    {
        this.counter = c;
    }
    public int getLimit()
    {
        return this.limit;
    }
    /* Glue code for coin management */
    public int getDoorMaxCoins()
    {
        return 120;
    }
    public int getCounterMaxCoins()
    {
        return 500;
    } 
    /* Constructor */
    public KnockDoor(boolean l, int li) {
        setCounter(0);
        setLocked(l);
        setLimit(li);
    }
    /* Other helpful methods */
    private void setLocked(boolean l)
    {
        this.locked = l;
    }
    private void setLimit(int l)
    {
        this.limit = l;
    }
}

class Player {
    private final String nickname;
    private int bag = 150; //contains the coins 
    // Constructor
    public Player(String n) {
        nickname = n;
    }
    // Setters and getters 
    public int getCoins() {
        return bag;
    }
    public String getNickname() {
        return this.nickname;
    }
    // Helpful methods
    public void addInBag(int amount) {
        this.bag += amount;
    }
    public void removeFromBag(int amount) {
        this.bag -= amount;
    }
    public String toString() {
        String s = "I’m " + getNickname();
        s += " and i’ve got " + getCoins();
        s += " coins in my bag.";
        return s;
    }
}

class DoorsRoom {
    private TDoor leftDoor;
    private TDoor rightDoor;
    private TDoor frontDoor; 
    /* Constructor */
    public DoorsRoom(TDoor l, TDoor r,
            TDoor f) {
        leftDoor = l;
        rightDoor = r;
        frontDoor = f;
    }
    /* Getters */
    public TDoor getLeftDoor()
    {
        return leftDoor;
    }
    public TDoor getRightDoor()
    {
        return rightDoor;
    }
    public TDoor getFrontDoor()
    {
        return frontDoor;
    }
}

class Game {
    private Player player;
    private DoorsRoom doorsRoom;
    private final String version = "0.0"; 
    /* Constructor */
    public Game(Player p, DoorsRoom dr) {
        player = p;
        doorsRoom = dr;
    }
    /* Setters and getters */
    public Player getPlayer()
    {
        return player;
    }
    public DoorsRoom getDoorsRoom()
    {
        return doorsRoom;
    }
}

public interface TestGame {
    public static void main(String[] args) {
        Player player = new Player("Grace");
        TDoor l = new EnchantedDoor(false);
        TDoor r = new ChestedDoor(false);
        TDoor f = new KnockDoor(false, 50);
        DoorsRoom doorsRoom = new DoorsRoom(l, r, f);
        Game game = new Game(player, doorsRoom);
        game.getDoorsRoom().getFrontDoor().open();
    }
}