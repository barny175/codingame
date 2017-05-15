package com.barny.cg.roche;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Scanner;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;
import java.util.stream.Stream;

/**
 * Bring data on patient samples from the diagnosis machine to the laboratory
 * with enough molecules to produce medicine!
 *
 */
class Player {

	Module target;
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
//            System.err.println(listToString(samples));
            
			List<Sample> mySamples = samples.stream()
					.filter(s -> s.carriedBy == 0)
					.collect(toList());
            System.err.println(listToString(mySamples));

			if (me.target == Module.SAMPLES) {
				if (mySamples.size() < 3) {
				    int rank = 1;
					System.err.println("Get sample ranked " + rank);
					connect(rank);
					continue;
				}
			}
			
			if (mySamples.isEmpty() && me.target != Module.SAMPLES) {
				System.err.println("No samples.");
				Module.SAMPLES.go();
				continue;
			}
			
			List<Sample> undiagnosed = getUndiagnosedSamples(mySamples);
			if (!undiagnosed.isEmpty()) {
				System.err.println("Some samples are not diagnosed yet.");
				if (me.target == Module.DIAGNOSIS) {
					System.err.println("Diagnosed sample " + undiagnosed.get(0).sampleId);
					connect(undiagnosed.get(0).sampleId);
				} else {
					Module.DIAGNOSIS.go();
				}
				continue;
			}
			
			List<Character> missingMolecules = me.missingMolecules(mySamples);
			System.err.println("Missing: " + listToString(missingMolecules, ", "));
			if (missingMolecules.isEmpty() || me.molecules() >= 10) {
				if (me.target != Module.LABORATORY) {
					Module.LABORATORY.go();
					continue;
				}
			} else {
				Optional<Sample> notCompleteSample = mySamples.stream().filter(s -> !me.missingMolecules(s).isEmpty()).findFirst();
				if (notCompleteSample.isPresent()) {
					List<Character> molecules = me.missingMolecules(notCompleteSample.get());
					if (me.target == Module.MOLECULES) {
						System.err.println("Missing molecules for sample " + notCompleteSample.get().toString() + ": " + listToString(molecules, ", "));
						connect(molecules.get(0));
					} else {
						Module.MOLECULES.go();
					}
					continue;
				}
			}
			
			if (me.target == Module.LABORATORY) {
				Optional<Sample> completedSample = me.getCompletedSample(mySamples);
				if (completedSample.isPresent()) {
				    System.err.println("Molecules ready for sample: " + completedSample.get());
					connect(completedSample.get().sampleId);
					continue;
				}
			} else {
				Module.LABORATORY.go();
				continue;
			}
			System.out.println("WAIT");
		}
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
		String expertiseGain;
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

	static String listToString(List<?> l, String delim) {
		return l.stream().map(o -> o.toString()).collect(joining(delim));
	}
	
	static String listToString(List<?> l) {
		return listToString(l, "\n");
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
		p.target = Module.valueOf(in.next());
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
}
