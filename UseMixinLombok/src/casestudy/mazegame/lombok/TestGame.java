package casestudy.mazegame.lombok;

import static java.lang.System.out;
import lombok.Obj;

/* Defines a base-door with no particular features */
@Obj interface TDoor {
    public boolean locked();
    public int doorMaxCoins();

    default boolean isLocked() {
        return locked();
    }
    default int open() {
        if (!isLocked()) {
            out.println("The door has been opened!");
            double rnd = Math.random();
            int cns = (int) (rnd * doorMaxCoins()) + 1;
            out.println("You got " + cns + " coins.");
            return cns;
        } else {
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
@Obj interface TCounter {
    public int counter();
    public void counter(int c);
    public int limit();
    public int counterMaxCoins();
    default void incrementCounter() {
        counter(counter() + 1);
    }
    default void decrementCounter() {
        counter(counter() - 1);
    }
    default boolean hasReachedLimit() {
        return counter() >= limit();
    }
    default int releaseCoins() {
        double rnd = Math.random();
        int cns = (int) (rnd * counterMaxCoins()) + 1;
        out.println("You got " + cns + " coins.");
        return cns;
    }
}

/* Provides a chest that contains coins */
interface TChest {
    public int chestMaxCoins();
    default int open() {
        out.print("The chest is now opened!");
        double rnd = Math.random();
        int c = (int) (rnd * chestMaxCoins());
        out.print("You got " + c);
        out.println(" coins from the chest.");
        return c;
    }
}

/* Provides an enchantment that can give or take coins */
interface TEnchantment {
    public int enchantMaxCoins();
    /** An enchantment can give coins
    (max getEnchantMaxCoins()) or
    remove coins (max -getEnchantMaxCoins()) **/
    default int applyEnchantment() {
        out.println("\nThis is an enchantment!");
        out.print("\"If the luck is up, ");
        out.println("of coins you will have a cup,");
        out.print("but if no luck you got, ");
        out.println("you are gonna lose a lot.\"");
        int max = enchantMaxCoins();
        double rnd = Math.random();
        int cns = -max + (int) (rnd * ((max * 2) + 1));
        if (cns >= 0) {
            out.print("Ohoh! You got " + cns);
            out.println(" coins!");
        }
        else {
            out.print("You lost " + Math.abs(cns));
            out.println(" coins!");
        }
        return cns;
    }
}

/* Puts together a door and an enchantment */
@Obj interface TEnchantedDoor extends TDoor, TEnchantment {
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
@Obj interface TChestedDoor extends TDoor, TChest {
    /** When you open a chested door,
    you also get the prize from the chest.
    This overrides the TDoor’s open(). **/
    default int open() {
        int coins = TDoor.super.open();
        if (coins > 0) //if the door is open
            coins += openChest();
        return coins;
    }
    /* Alias for open() from TChest */
    default int openChest() {
        return TChest.super.open();
    }
}

/* Puts together a door and a counter */
@Obj interface TKnockDoor extends TDoor, TCounter {
    /** Every know makes the counter increment.
     * If the limit is reached, more coins are released. **/
    default int knock() {
        int coins = TDoor.super.knock();
        incrementCounter();
        if (hasReachedLimit()) {
            out.print("Ohh! A special drop for you!");
            coins += releaseCoins();
        } else {
            //Let’s give a suggestion to the player 
            out.print("Don’t challenge me... ");
            int c = limit();
            String sug = "never knock a door ";
            sug = sug + "more then " + c + " times.";
            out.println(sug);
        }
        return coins;
    }
}

@Obj interface Player {
    int coins();
    String nickname();
    void coins(int Coins);
    default void addInBad(int amount) {
        coins(coins() + amount);
    }
    default void removeFromBag(int amount) {
        coins(coins() - amount);
    }
    default String toS() {
        String s = "I am " + nickname();
        s += " and i got " + coins();
        s += " coins in my bag.";
        return s;
    }
    //model mutable field with initialization.
    static Player of(String nickname) {
        return of(150, nickname); //TODO: discuss fields' order
    }
}

@Obj interface DoorsRoom {
    TDoor leftDoor();
    TDoor rightDoor();
    TDoor frontDoor();
}

@Obj interface Game {
    Player player();
    DoorsRoom doorsRoom();
    // model immutable field with default method
    default String version() {
        return "0.0";
    }
}

public interface TestGame {
    public static void main(String[] args) {
        Player player = Player.of("Grace");
        TDoor l = TDoor.of(200, false);
        TDoor r = TEnchantedDoor.of(10, 100, false);
        TDoor f = TKnockDoor.of(200, 0, 100, 200, true);
        DoorsRoom doorsRoom = DoorsRoom.of(l, r, f);
        Game game = Game.of(doorsRoom, player);
        game.doorsRoom().frontDoor().open();
    }
}