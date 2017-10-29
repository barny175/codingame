package bustspeeding;

import java.util.*;

import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.toMap;

public class Solution {
    private static class Reading {
        String license;
        long timestamp;
        int cameraDist;
        Reading(String license, long timestamp, int cameraDist) {
            this.license = license;
            this.timestamp = timestamp;
            this.cameraDist = cameraDist;
        }
    }

    public static void main(String args[]) {
//        Scanner in = new Scanner(System.in);
        Scanner in = new Scanner("100\n" +
                "6\n" +
                "SKRD94 75 1447407932\n" +
                "SKRD94 175 1447411532\n" +
                "SKRD94 275 1447415132\n" +
                "ZBZJ42 75 1447418732\n" +
                "ZBZJ42 175 1447422333\n" +
                "ZBZJ42 275 1447425932");
        int L = in.nextInt();
        int N = in.nextInt();
        if (in.hasNextLine()) {
            in.nextLine();
        }

        Map<String, List<Reading>> readings = new HashMap<>();
        List<Reading> speeding = new ArrayList<>();
        for (int i = 0; i < N; i++) {
            String[] R = in.nextLine().split(" ");
            Reading reading = new Reading(R[0], Long.valueOf(R[2]), Integer.valueOf(R[1]));
            List<Reading> readingsList = readings.get(reading.license);
            if (readingsList != null) {
                Reading lastReading = readingsList.get(readingsList.size() - 1);
                int speed = (int) speed(reading.cameraDist - lastReading.cameraDist, lastReading.timestamp, reading.timestamp);
                System.out.println(speed);
                if (speed > L)
                    speeding.add(reading);
            } else {
                readingsList = new ArrayList<>();
                readings.put(reading.license, readingsList);
            }
            readingsList.add(reading);
        }

        if (speeding.isEmpty())
            System.out.println("OK");
        else
            speeding.forEach(r -> {
                System.out.println(r.license + " " + r.cameraDist);
            });
    }

    private static long speed(int dist, long timestamp1, long timestamp2) {
        return dist * 3600L / (timestamp2 - timestamp1);
    }
}
