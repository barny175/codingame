package com.barny.cg.roche;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Scanner;
import java.util.stream.Collectors;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;
import java.util.stream.Stream;

class Player {

	private static int getRank(int expertise) {
		if (expertise < 5) {
			return 1;
		} else if (expertise < 10) {
			return 2;
		}
		return 3;
	}

	Module target;
	int eta;
	int score;
	Map<Character, Integer> storage = new HashMap<>();
	Map<Character, Integer> expertise = new HashMap<>();

	public static void main(String args[]) {
		Scanner in = new Scanner(System.in);
		readProjects(in);

		Player me = new Player();
		Player him = new Player();

		while (true) {
			readPlayer(in, me);
			System.err.println(me);
			readPlayer(in, him);
			Map<Character, Integer> availableMolecules = Player.availableMolecules(in);

			int sampleCount = in.nextInt();
			List<Sample> samples = readSamples(sampleCount, in);
//            System.err.println(listToString(samples));
			Command cmd = getCommand(samples, me, availableMolecules);
			cmd.act();
		}
	}

	private static Command getCommand(List<Sample> samples, Player me, Map<Character, Integer> availableMolecules) {
		List<Sample> mySamples = samples.stream()
				.filter(s -> s.carriedBy == 0)
				.collect(toList());
		System.err.println(listToString(mySamples));
		if (me.target == Module.SAMPLES) {
			if (mySamples.size() < 3) {
				int rank = getRank(me.getExpertise());
				System.err.println("Get sample ranked " + rank);
				return new Connect(rank);
			}
		}
		if (mySamples.isEmpty() && me.target != Module.SAMPLES) {
			System.err.println("No samples.");
			return new Go(Module.SAMPLES);
		}
		List<Sample> undiagnosed = getUndiagnosedSamples(mySamples);
		if (!undiagnosed.isEmpty()) {
			System.err.println("Some samples are not diagnosed yet.");
			if (me.target == Module.DIAGNOSIS) {
				System.err.println("Diagnosed sample " + undiagnosed.get(0).sampleId);
				return new Connect(undiagnosed.get(0).sampleId);
			} else {
				return new Go(Module.DIAGNOSIS);
			}
		}
		List<Character> missingMolecules = me.missingMolecules(mySamples);
		System.err.println("Missing: " + listToString(missingMolecules, ", "));
		if (missingMolecules.isEmpty() || me.molecules() >= 10) {
			System.err.println("My molecules: " + me.molecules());
			if (me.target != Module.LABORATORY) {
				return new Go(Module.LABORATORY);
			}
		} else {
			Optional<Sample> notCompleteSample = mySamples.stream().filter(s -> !me.missingMolecules(s).isEmpty()).findFirst();
			if (notCompleteSample.isPresent()) {
				List<Character> molecules = me.missingMolecules(notCompleteSample.get());
				if (me.target == Module.MOLECULES) {
					System.err.println("Missing molecules for sample " + notCompleteSample.get().toString() + ": " + listToString(molecules, ", "));
					System.err.println("Available: " + mapToString(availableMolecules, ", "));
					final List<Character> availMols = availableMolecules(molecules, availableMolecules);
					System.err.println("Intersection: " + listToString(availMols, ", "));
					return new GetMolecule(availMols.get(0));
				} else {
					return new Go(Module.MOLECULES);
				}
			}
		}
		if (me.target == Module.LABORATORY) {
			Optional<Sample> completedSample = me.getCompletedSample(mySamples);
			if (completedSample.isPresent()) {
				System.err.println("Molecules ready for sample: " + completedSample.get());
				return new Connect(completedSample.get().sampleId);
			}
		} else {
			return new Go(Module.LABORATORY);
		}
		return WAIT;
	}

	static List<Character> availableMolecules(List<Character> needed, Map<Character, Integer> available) {
		Map<Character, Long> neededMap = needed.stream().collect(Collectors.groupingBy(c -> c, Collectors.counting()));
		return neededMap.entrySet().stream()
				.flatMap(e -> {
					if (available.containsKey(e.getKey()))
						return Stream.generate(() -> e.getKey()).limit(available.get(e.getKey()));
					else
						return Stream.empty();
				})
				.collect(toList());
	}

	private Optional<Sample> getCompletedSample(List<Sample> samples) {
		return samples.stream().filter(s -> this.missingMolecules(s).isEmpty()).findAny();
	}

	private static List<Sample> getUndiagnosedSamples(List<Sample> samples) {
		return samples.stream().filter(s -> !s.isDiagnosed()).collect(toList());
	}

	private static List<Sample> readSamples(int sampleCount, Scanner in) {
		List<Sample> samples = new ArrayList<>();
		for (int i = 0; i < sampleCount; i++) {
			samples.add(Sample.read(in));
		}
		samples.sort((s1, s2) -> Integer.compare(s1.totalCost(), s2.totalCost()));
		return samples;
	}

	List<Character> missingMolecules(List<Sample> samples) {
		final HashMap<Character, Integer> totalCost = samples.stream().map(s -> s.cost).collect(HashMap::new, (m, cost) -> {
			cost.forEach((c, i) -> {
				m.merge(c, i, Integer::sum);
			});
		}, (m1, m2) -> m1.putAll(m2));
		return missingMolecules(totalCost);
	}

	List<Character> missingMolecules(Sample sample) {
		return missingMolecules(sample.cost);
	}

	List<Character> missingMolecules(Map<Character, Integer> cost) {
		return cost.entrySet().stream()
				.filter(e -> !storage.containsKey(e.getKey()) || e.getValue() > storage.get(e.getKey()))
				.flatMap(e -> {
					long take = storage.containsKey(e.getKey()) ? (e.getValue() - storage.get(e.getKey())) : e.getValue();
					return Stream.generate(() -> e.getKey()).limit(take);
				})
				.collect(toList());
	}

	private int molecules() {
		return storage.entrySet().stream()
				.map(e -> e.getValue())
				.mapToInt(i -> i)
				.sum();
	}

	static class Sample {

		int sampleId;
		int carriedBy;
		int rank;
		String Gain;
		int health;
		Map<Character, Integer> cost = new HashMap<>();

		int totalCost() {
			return cost.values().stream().mapToInt(i -> i).sum();
		}

		boolean isDiagnosed() {
			return !cost.values().contains(-1);
		}

		static Sample read(Scanner in) {
			Sample sample = new Sample();
			sample.sampleId = in.nextInt();
			sample.carriedBy = in.nextInt();
			sample.rank = in.nextInt();
			sample.Gain = in.next();
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
			return "Sample{" + "sampleId=" + sampleId + ", carriedBy=" + carriedBy + ", rank=" + rank + ", Gain=" + Gain + ", health=" + health + ", costA=" + cost.get('A') + ", costB=" + cost.get('B') + ", costC=" + cost.get('C') + ", costD=" + cost.get('D') + ", costE=" + cost.get('E') + '}';
		}

	}

	@Override
	public String toString() {
		return "Player{" + "target=" + target + ", eta=" + eta + ", score=" + score + ", A=" + storage.get('A') + ", B=" + storage.get('B') + ", C=" + storage.get('C') + ", D=" + storage.get('D') + ", E=" + storage.get('E') + ", expA=" + expertise.get('A') + ", expB=" + expertise.get('B') + ", expC=" + expertise.get('C') + ", expD=" + expertise.get('D') + ", expE=" + expertise.get('E') + '}';
	}

	static String listToString(List<?> l, String delim) {
		return l.stream().map(o -> o.toString()).collect(joining(delim));
	}

	static String listToString(List<?> l) {
		return listToString(l, "\n");
	}

	private static Map<Character, Integer> availableMolecules(Scanner in) {
		Map<Character, Integer> m = new HashMap<>(5);
		m.put('A', in.nextInt());
		m.put('B', in.nextInt());
		m.put('C', in.nextInt());
		m.put('D', in.nextInt());
		m.put('E', in.nextInt());
		return m;
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
		p.target = Module.valueOf(in.next());
		p.eta = in.nextInt();
		p.score = in.nextInt();
		p.storage.put('A', in.nextInt());
		p.storage.put('B', in.nextInt());
		p.storage.put('C', in.nextInt());
		p.storage.put('D', in.nextInt());
		p.storage.put('E', in.nextInt());
		p.expertise.put('A', in.nextInt());
		p.expertise.put('B', in.nextInt());
		p.expertise.put('C', in.nextInt());
		p.expertise.put('D', in.nextInt());
		p.expertise.put('E', in.nextInt());
	}

	int getExpertise() {
		return expertise.values().stream().mapToInt(i -> i).sum();
	}

	private static void connect(Object c) {
		System.out.println("CONNECT " + c);
	}

	enum Module {

		START_POS,
		MOLECULES,
		LABORATORY,
		DIAGNOSIS,
		SAMPLES;

		void go() {
			System.out.println("GOTO " + this.name());
		}
	}

	private static abstract class Command {

		void act() {
			System.out.println(command());
		}

		protected abstract String command();
	}

	private static class Go extends Command {

		Module module;

		public Go(Module module) {
			this.module = module;
		}

		@Override
		protected String command() {
			return "GOTO " + this.module.name();
		}
	}

	private static class Connect extends Command {

		String where;

		public Connect(String where) {
			this.where = where;
		}

		private Connect(int rank) {
			this.where = Integer.toString(rank);
		}

		@Override
		protected String command() {
			return "CONNECT " + where;
		}
	}
	
	static class GetMolecule extends Command {
		char molecule;

		public GetMolecule(char molecule) {
			this.molecule = molecule;
		}
		
		@Override
		protected String command() {
			return "CONNECT " + molecule;
		}
		
	}

	private static Command WAIT = new Command() {

		@Override
		protected String command() {
			return "WAIT";
		}
	};

	private static String mapToString(Map<Character, Integer> availableMolecules, String delim) {
		return availableMolecules.entrySet().stream()
				.map(e -> e.getKey() + "=" + e.getValue())
				.collect(joining(delim));
	}
}
