import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Scanner;

class GameState {
	/**
	 * The visitor's score at the time of the state
	 */
	public int visitorScore;

	/**
	 * The home team's score at the time of the state
	 */
	public int homeScore;

	/**
	 * The number of innings completed at the time of the state
	 */
	public int innings;

	/**
	 * Indicator of the winning team. True if the home team won, false if the
	 * visitors won
	 */
	public boolean homeWin;
	
	/**
	 * The first line that should be inserted in a CSV list of game states, showing the column titles
	 */
	public static final String CSV_HEADERS = "visitorScore,homeScore,innings,homeWin";

	/**
	 * Reads a line of comma-separated values.
	 * 
	 * @param line
	 *            the string to be read.
	 * @return the array of values in order.
	 */
	public static String[] readCSVLine(String line) {
		Scanner lineScanner = new Scanner(line);
		lineScanner.useDelimiter(",");
		ArrayList<String> output = new ArrayList<String>();
		while (lineScanner.hasNext()) {
			output.add(lineScanner.next());
		}
		lineScanner.close();
		return output.toArray(new String[] {});
	}

	/**
	 * @param visitorScore
	 * @param homeScore
	 * @param innings
	 * @param homeWin
	 */
	public GameState(int visitorScore, int homeScore, int innings, boolean homeWin) {
		this.visitorScore = visitorScore;
		this.homeScore = homeScore;
		this.innings = innings;
		this.homeWin = homeWin;
	}
	
	/**
	 * Construct a list of game states from a CSV line score.
	 * 
	 * @param gameLog a CSV line in the format of visLine,homeLine,visScore,homeScore
	 * @return a list of game states from the given game
	 */
	public static ArrayList<GameState> lineToStates(String line) {
		String[] fields = readCSVLine(line);
		ArrayList<GameState> output = new ArrayList<GameState>();

		// Variables to keep track of score by inning
		int visitorScore = 0, homeScore = 0, innings = 0;

		boolean homeWin = Integer.parseInt(fields[3]) > Integer.parseInt(fields[2]);

		// The line scores will shrink from the front as innings are read and processed
		String visitorLine = fields[0],
				homeLine = fields[1];
		
		// Start of game, 0 - 0 thru 0 innings
		output.add(new GameState(visitorScore, homeScore, innings, homeWin));
		
		while (visitorLine.length() > 0) {
			// Advance an inning

			if (visitorLine.charAt(0) == '(') {
				// 10+ run inning
				visitorScore += Integer.parseInt(visitorLine.substring(1, visitorLine.indexOf(')')));
				visitorLine = visitorLine.substring(visitorLine.indexOf(')') + 1);
			} else {
				visitorScore += Integer.parseInt(visitorLine.substring(0, 1));
				visitorLine = visitorLine.substring(1);
			}

			if (homeLine.charAt(0) == '(') {
				// 10+ run inning
				homeScore += Integer.parseInt(homeLine.substring(1, homeLine.indexOf(')')));
				homeLine = homeLine.substring(homeLine.indexOf(')') + 1);
			} else {
				homeScore += Integer.parseInt(homeLine.substring(0, 1));
				homeLine = homeLine.substring(1);
			}

			innings++;

			output.add(new GameState(visitorScore, homeScore, innings, homeWin));
		}
		
		// Strike last inning
		output.remove(output.size() - 1);

		return output;
	}

	/**
	 * Construct a list of game states from a CSV game log for a single game.
	 * 
	 * Qualifications for a game to be included (field numbers are 1-based):
	 * 
	 * - Fields 10 & 11, the final score must be uneven; game must not end in a tie
	 * 
	 * - Field 12, length of game must be at least 51 outs; game must not be
	 * rain-shortened
	 * 
	 * - Field 15, forfeit must be ""; game must not be forfeited
	 * 
	 * - Field 161, acquisition information must be "Y"; game information must be
	 * complete
	 * 
	 * For games that do not qualify, an empty list is returned.
	 * 
	 * @param gameLog
	 *            a CSV game log for a game, under Retrosheet specifications
	 * @return a list of game states from the given game
	 */
	public static ArrayList<GameState> logToStates(String gameLog) {
		String[] fields = readCSVLine(gameLog);
		ArrayList<GameState> output = new ArrayList<GameState>();
		if (
				Integer.parseInt(fields[9]) == Integer.parseInt(fields[10]) ||
				Integer.parseInt(fields[11]) < 51 ||
				!(fields[14].equals("\"\"")) ||
				!(fields[160].equals("\"Y\""))
			)
			// Game disqualified, return empty list
			return output;

		// Variables to keep track of score by inning
		int visitorScore = 0, homeScore = 0, innings = 0;

		boolean homeWin = Integer.parseInt(fields[10]) > Integer.parseInt(fields[9]);

		int totalInnings = Integer.parseInt(fields[11]) / 6;
		// Above calculation rounds down. Add one if last inning is less than 6 outs
		if (Integer.parseInt(fields[11]) % 6 != 0)
			totalInnings++;

		// The line scores will shrink from the front as innings are read and processed
		// Strike the first and last characters, which are quotes
		String visitorLine = fields[19].substring(1, fields[19].length() - 1),
				homeLine = fields[20].substring(1, fields[20].length() - 1);
		// Start of game, 0 - 0 thru 0 innings
		output.add(new GameState(visitorScore, homeScore, innings, homeWin));
		while (innings < totalInnings - 1) { // Last inning not included
			// Advance an inning

			if (visitorLine.charAt(0) == '(') {
				// 10+ run inning
				visitorScore += Integer.parseInt(visitorLine.substring(1, visitorLine.indexOf(')')));
				visitorLine = visitorLine.substring(visitorLine.indexOf(')') + 1);
			} else {
				visitorScore += Integer.parseInt(visitorLine.substring(0, 1));
				visitorLine = visitorLine.substring(1);
			}

			if (homeLine.charAt(0) == '(') {
				// 10+ run inning
				homeScore += Integer.parseInt(homeLine.substring(1, homeLine.indexOf(')')));
				homeLine = homeLine.substring(homeLine.indexOf(')') + 1);
			} else {
				homeScore += Integer.parseInt(homeLine.substring(0, 1));
				homeLine = homeLine.substring(1);
			}

			innings++;

			output.add(new GameState(visitorScore, homeScore, innings, homeWin));
		}

		return output;
	}

	public String toString() {
		String output = visitorScore + " - " + homeScore + " through " + innings + " inning(s), ";
		if (homeWin)
			output += "home win";
		else
			output += "visitor win";
		return output;
	}

	/**
	 * Builds an CSV representation of the game state.
	 * 
	 * @return visitorScore,homeScore,innings, homeWin
	 */
	public String toCSV() {
		return visitorScore + "," + homeScore + "," + innings + "," + homeWin;
	}
}

public class RetrosheetToGameState {
	
	public static GameState[] lineFilesToStates() {
		// Directory where the list and game log files are stored
		final String PATH_PREFIX = "***";

		// File name for a list of file names; each file should be a list of line scores
		final String LIST_OF_FILES = "***";

		// Reads list of files
		System.out.println("Reading list of files...");
		ArrayList<String> listOfFiles = new ArrayList<String>();

		try {
			Scanner listScanner;
			listScanner = new Scanner(new File(PATH_PREFIX + LIST_OF_FILES));
			while (listScanner.hasNextLine()) {
				listOfFiles.add(listScanner.nextLine());
			}
			listScanner.close();
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		}

		System.out.println("File list read, " + listOfFiles.size() + " files in list");

		// Reads files
		ArrayList<GameState> stateList = new ArrayList<GameState>();
		for (String fileName : listOfFiles) {
			// Reads a file
			System.out.println("Reading " + fileName + "...");

			try {
				Scanner fileScanner = new Scanner(new File(PATH_PREFIX + fileName));
				int count = 0;
				while (fileScanner.hasNextLine()) {
					stateList.addAll(GameState.lineToStates(fileScanner.nextLine()));
					count++;
					System.out.println(count);
				}
				fileScanner.close();
			} catch (FileNotFoundException e) {
				e.printStackTrace();
			}
		}
		System.out.println("All files read, " + stateList.size() + " game states in total");
		return stateList.toArray(new GameState[] {});
	}

	public static GameState[] filesToStates() {
		// Directory where the list and game log files are stored
		final String PATH_PREFIX = "***";

		// File name for a list of file names; each file should be a list of game logs
		final String LIST_OF_FILES = "***";

		// Reads list of files
		System.out.println("Reading list of files...");
		ArrayList<String> listOfFiles = new ArrayList<String>();

		try {
			Scanner listScanner;
			listScanner = new Scanner(new File(PATH_PREFIX + LIST_OF_FILES));
			while (listScanner.hasNextLine()) {
				listOfFiles.add(listScanner.nextLine());
			}
			listScanner.close();
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		}

		System.out.println("File list read, " + listOfFiles.size() + " files in list");

		// Reads files
		ArrayList<GameState> stateList = new ArrayList<GameState>();
		for (String fileName : listOfFiles) {
			// Reads a file
			System.out.println("Reading " + fileName + "...");

			try {
				Scanner fileScanner = new Scanner(new File(PATH_PREFIX + fileName));
				while (fileScanner.hasNextLine()) {
					//System.out.println(fileScanner.nextLine());
					stateList.addAll(GameState.logToStates(fileScanner.nextLine()));
				}
				fileScanner.close();
			} catch (FileNotFoundException e) {
				e.printStackTrace();
			}
		}
		System.out.println("All files read, " + stateList.size() + " game states in total");
		return stateList.toArray(new GameState[] {});
	}
	
	public static String generateCSVStates(GameState[] gameStates) {
		StringBuilder stringBuilder = new StringBuilder();
		stringBuilder.append(GameState.CSV_HEADERS);
		
		for(GameState gameState : gameStates) {
			stringBuilder.append("\n");
			stringBuilder.append(gameState.toCSV());
		}
		
		return stringBuilder.toString();
	}
	
	public static void writeToFile(String content) {
		// Path and filename of the output file
		final String OUTPUT_PATH = "***";
	    FileWriter fileWriter;
		try {
			fileWriter = new FileWriter(OUTPUT_PATH);
			fileWriter.write(content);
		    fileWriter.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
		
		System.out.println("Finished writing to file");
	}

	public static void main(String[] args) {
		writeToFile(generateCSVStates(filesToStates()));
		writeToFile(generateCSVStates(lineFilesToStates()));
	}
}
