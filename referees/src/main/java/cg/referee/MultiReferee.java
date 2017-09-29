package cg.referee;

import com.sun.media.sound.InvalidFormatException;

import java.io.InputStream;
import java.io.PrintStream;
import java.util.Properties;

public abstract class MultiReferee {
    public MultiReferee(InputStream is, PrintStream out, PrintStream err) {
    }

    protected abstract void initReferee(int playerCount, Properties prop) throws InvalidFormatException;

    protected abstract Properties getConfiguration();

    protected abstract String[] getInitInputForPlayer(int playerIdx);

    protected abstract int getExpectedOutputLineCountForPlayer(int playerIdx);

    protected abstract void handlePlayerOutput(int frame, int round, int playerIdx, String[] outputs)
            throws WinException, LostException, InvalidInputException ;

    protected abstract void prepare(int round);

    protected abstract String[] getInputForPlayer(int round, int playerIdx);

    protected abstract void updateGame(int round) throws GameOverException;

    protected abstract String[] getInitDataForView();

    protected abstract void populateMessages(Properties p);


    protected abstract String[] getFrameDataForView(int round, int frame, boolean keyFrame);

    protected abstract String getGameName();

    protected abstract String getHeadlineAtGameStartForConsole();

    protected abstract int getMinimumPlayerCount();

    protected abstract boolean showTooltips();

    protected abstract String[] getPlayerActions(int playerIdx, int round);

    protected abstract boolean isPlayerDead(int playerIdx);

    protected abstract String getDeathReason(int playerIdx);

    protected abstract int getScore(int playerIdx);

    protected abstract String[] getGameSummary(int round);

    protected abstract void setPlayerTimeout(int frame, int round, int playerIdx);

    protected int getMaxRoundCount(int playerCount) {
        return 200;
    }

    protected abstract int getMillisTimeForRound();

    protected boolean isTurnBasedGame() {
        return true;
    }

    protected boolean gameIsOver() { return false; }


        public class InvalidFormatException extends Exception {
    }

    public class InvalidInputException extends Exception {

        public InvalidInputException(String msg, String line) {
        }
    }

    public class WinException extends Exception {
    }

    public class LostException extends Exception {
        public LostException(String msg, int src) {
        }

        public LostException(String badCoords, int x, int y) {
        }

        public LostException(String pushInvalid, String dir1, String dir2) {
        }

        public LostException(String invalidCommand, String command) {
        }
    }

    public class GameOverException extends Exception {
        public GameOverException(String msg) {
        }
    }
}
