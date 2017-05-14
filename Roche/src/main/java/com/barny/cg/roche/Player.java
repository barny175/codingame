package com.barny.cg.roche;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Scanner;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;

/**
 * Bring data on patient samples from the diagnosis machine to the laboratory
 * with enough molecules to produce medicine!
 *
 */
class Player {

	String target;
	int eta;
	int score;
	Map<Character, Integer> storage = new HashMap<>();
	int expertiseA;
	int expertiseB;
	int expertiseC;
	int expertiseD;
	int expertiseE;

	public static void main(String args[]) {
		Scanner in = new Scanner(System.in);
		readProjects(in);

		Player me = new Player();
		Player him = new Player();

		while (true) {
			readPlayer(in, me);
			System.err.println(me);
			readPlayer(in, him);

			availableMolecules(in);

			int sampleCount = in.nextInt();
			List<Sample> samples = readSamples(sampleCount, in);

			List<Sample> mySamples = samples.stream()
					.filter(s -> s.carriedBy == 0)
					.collect(toList());

			System.err.println(listToString(mySamples));

			if (mySamples.isEmpty()) {
				if (me.target.equals("DIAGNOSIS") && !samples.isEmpty()) {
					connect(samples.get(0).sampleId);
				} else {
					goTo("DIAGNOSIS");
				}
			} else {
				List<Character> missingMolecules = me.missingMolecules(mySamples);
				System.err.println("Missing: " + listToString(missingMolecules));
				if (missingMolecules.isEmpty()) {
					goTo("DIAGNOSIS");
				} else {
					if (!me.target.equals("MOLECULES")) {
						goTo("MOLECULES");
					} else {
						connect(missingMolecules.get(0));
					}
				}
			}
		}
	}

	private static List<Sample> readSamples(int sampleCount, Scanner in) {
		List<Sample> samples = new ArrayList<>();
		for (int i = 0; i < sampleCount; i++) {
			samples.add(Sample.read(in));
		}
		samples.sort((s1, s2) -> Integer.compare(s1.totalCost(), s2.totalCost()));
		return samples;
	}

	private static void goTo(String target) {
		System.out.println("GOTO " + target);
	}

	List<Character> missingMolecules(List<Sample> samples) {
		return samples.stream()
				.flatMap(s -> missingMolecules(s).keySet().stream())
				.collect(toList());
	}

	Map<Character, Integer> missingMolecules(Sample sample) {
		return sample.cost.entrySet().stream()
				.filter(e -> !storage.containsKey(e.getKey()) || e.getValue() > storage.get(e.getKey()))
				.collect(() -> new HashMap<Character, Integer>(), 
						(m, e) ->  {
							if (storage.containsKey(e.getKey()))
								m.put(e.getKey(), e.getValue() - storage.get(e.getKey()));
							else
								m.put(e.getKey(), e.getValue());
						}, 
						(m1, m2) -> { m1.putAll(m2); });
	}

	static class Sample {

		int sampleId;
		int carriedBy;
		int rank;
		String expertiseGain;
		int health;
		Map<Character, Integer> cost = new HashMap<>();

		int totalCost() {
			return cost.values().stream().mapToInt(i -> i).sum();
		}
		static Sample read(Scanner in) {
			Sample sample = new Sample();
			sample.sampleId = in.nextInt();
			sample.carriedBy = in.nextInt();
			sample.rank = in.nextInt();
			sample.expertiseGain = in.next();
			sample.health = in.nextInt();
			sample.cost.put('A', in.nextInt());
			sample.cost.put('B', in.nextInt());
			sample.cost.put('C', in.nextInt());
			sample.cost.put('D', in.nextInt());
			sample.cost.put('E', in.nextInt());
			return sample;
		}

		@Override
		public String toString() {
			return "Sample{" + "sampleId=" + sampleId + ", carriedBy=" + carriedBy + ", rank=" + rank + ", expertiseGain=" + expertiseGain + ", health=" + health + ", costA=" + cost.get('A') + ", costB=" + cost.get('B') + ", costC=" + cost.get('C') + ", costD=" + cost.get('D') + ", costE=" + cost.get('E') + '}';
		}

	}

	@Override
	public String toString() {
		return "Player{" + "target=" + target + ", eta=" + eta + ", score=" + score + ", A=" + storage.get('A') + ", B=" + storage.get('B') + ", C=" + storage.get('C') + ", D=" + storage.get('D') + ", E=" + storage.get('E') + ", expertiseA=" + expertiseA + ", expertiseB=" + expertiseB + ", expertiseC=" + expertiseC + ", expertiseD=" + expertiseD + ", expertiseE=" + expertiseE + '}';
	}

	static String listToString(List<?> l) {
		return l.stream().map(o -> o.toString()).collect(joining("\n"));
	}

	private static void availableMolecules(Scanner in) {
		int availableA = in.nextInt();
		int availableB = in.nextInt();
		int availableC = in.nextInt();
		int availableD = in.nextInt();
		int availableE = in.nextInt();
	}

	private static void readProjects(Scanner in) {
		int projectCount = in.nextInt();
		for (int i = 0; i < projectCount; i++) {
			int a = in.nextInt();
			int b = in.nextInt();
			int c = in.nextInt();
			int d = in.nextInt();
			int e = in.nextInt();
		}
	}

	private static void readPlayer(Scanner in, Player p) {
		p.target = in.next();
		p.eta = in.nextInt();
		p.score = in.nextInt();
		p.storage.put('A', in.nextInt());
		p.storage.put('B', in.nextInt());
		p.storage.put('C', in.nextInt());
		p.storage.put('D', in.nextInt());
		p.storage.put('E', in.nextInt());
		p.expertiseA = in.nextInt();
		p.expertiseB = in.nextInt();
		p.expertiseC = in.nextInt();
		p.expertiseD = in.nextInt();
		p.expertiseE = in.nextInt();
	}

	private static void connect(Object c) {
		System.out.println("CONNECT " + c);
	}
}
