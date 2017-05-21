package com.barny.cg.roche;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Scanner;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;
import java.util.stream.Stream;

class Player {

	private static final int MAX_SAMPLES = 3;
	private static final boolean GREEDY = false;
	private static final int RANK_TWO_THRESHOLD = 2;
	private static final int RANK_THREE_THRESHOLD = 5;

	private int getRank() {
		if (this.getExpertise() > RANK_THREE_THRESHOLD) {
			return 3;
		}

		if (this.getExpertise() > RANK_TWO_THRESHOLD) {
			return 2;
		}

		return 1;
	}

	Module target;
	int eta;
	int score;
	MoleculeMap storage = new MoleculeMap();
	MoleculeMap expertise = new MoleculeMap();
	List<Sample> mySamples;

	public static void main(String args[]) {
		Scanner in = new Scanner(System.in);
		readProjects(in);

		Player me = new Player();
		Player him = new Player();

		while (true) {
			readPlayer(in, me);
			System.err.println(me);
			readPlayer(in, him);
			MoleculeMap availableMolecules = Player.availableMolecules(in);
			System.err.println("Available molecules: " + MoleculeMap.mapToString(availableMolecules, ", "));

			int sampleCount = in.nextInt();
			List<Sample> samples = readSamples(sampleCount, in);
//            System.err.println(listToString(samples));

			List<Sample> mySamples = samples.stream()
					.filter(s -> s.carriedBy == 0)
					.collect(toList());
			System.err.println(listToString(mySamples));
			me.mySamples = mySamples;

			Command cmd = me.getCommand(availableMolecules);
			cmd.act();
		}
	}

	Command getCommand(MoleculeMap availableMolecules) {
		Optional<Command> produce = this.produceMedicine(mySamples);
		if (!mySamples.isEmpty() && this.target == Module.LABORATORY && produce.isPresent()) {
			return produce.get();
		}

		Optional<Command> getSampleCmd = this.sampleAcquisition(mySamples, availableMolecules);
		if (getSampleCmd.isPresent()) {
			return getSampleCmd.get();
		}

		Optional<Command> diagnoseCmd = this.diagnose(mySamples, availableMolecules);
		if (diagnoseCmd.isPresent()) {
			return diagnoseCmd.get();
		}

		Optional<Command> getMolecules = this.getMissingMolecules(mySamples, availableMolecules);
		return getMolecules.orElseGet(()
				-> this.produceMedicine(mySamples)
				.orElseGet(() -> this.putSampleToCloud(mySamples, availableMolecules)
						.orElse(WAIT))
		);
	}

	Optional<Command> getMissingMolecules(List<Sample> mySamples, MoleculeMap availableMolecules) {
		if (this.molecules() >= 10) {
			return Optional.empty();
		}

		List<Character> missingMolecules = this.missingMolecules(mySamples);
		System.err.println("Missing: " + listToString(missingMolecules, ", "));
		if (missingMolecules.isEmpty()) {
			if (GREEDY) {
				List<Character> mm = availableMolecules.entrySet().stream()
						.filter(e -> e.getValue() > 0)
						.map(e -> e.getKey())
						.sorted((m1, m2) -> Integer.compare(this.getMolecules(m1), this.getMolecules(m2)))
						.collect(toList());
				System.err.println("Get as much mollecules as possible. Best to get: " + listToString(mm, ", "));
				if (!mm.isEmpty() && this.target == Module.MOLECULES) {
					return Optional.of(new GetMolecule(mm.get(0)));
				}
			}
		}

		if (!missingMolecules.isEmpty()) {
			Optional<Character> molToGet = this.moleculeToGet(mySamples, availableMolecules);
			if (this.molecules() < 10 && molToGet.isPresent()) {
				return Optional.of(new GetMolecule(molToGet.get()));
			}
		}

		return Optional.empty();
	}

	Optional<Command> putSampleToCloud(List<Sample> mySamples, MoleculeMap availableMolecules) {

		List<Character> missingMolecules = this.missingMolecules(mySamples);

		if (!missingMolecules.isEmpty()) {
			System.err.println("Cannot obtain any molecules.");
			List<Sample> sorted = mySamples.stream().sorted((s1, s2) -> {
				final List<Character> mm1 = this.missingMolecules(s1);
				final List<Character> mm2 = this.missingMolecules(s2);
				return (-1) * Integer.compare(mm1.size(), mm2.size());
			}).collect(toList());
			if (!sorted.isEmpty()) {
				return Optional.of(new PutSampleToCloud(sorted.get(0)));
			}
		}
		return Optional.empty();
	}

	Optional<Command> produceMedicine(List<Sample> mySamples) {
		List<Sample> completedSamples = this.completedSamples(mySamples);
		if (!completedSamples.isEmpty()) {
			System.err.println("Molecules ready for samples: \n" + listToString(completedSamples));
			return Optional.of(new PutSample(completedSamples.get(0)));
		}
		return Optional.empty();
	}

	Optional<Command> diagnose(List<Sample> mySamples, MoleculeMap availableMolecules) {
		List<Sample> undiagnosed = getUndiagnosedSamples(mySamples);
		if (!undiagnosed.isEmpty()) {
			System.err.println("Some samples are not diagnosed yet.");
			System.err.println("Diagnosed sample " + undiagnosed.get(0).sampleId);
			return Optional.of(new DiagnoseSample(undiagnosed.get(0).sampleId));
		}

		return Optional.empty();
	}

	Optional<Command> sampleAcquisition(List<Sample> mySamples, MoleculeMap availableMolecules) {
		if (mySamples.size() < MAX_SAMPLES) {
			int rank = getRank();
			System.err.println("Get sample ranked " + rank);
			return Optional.of(new GetSample(rank));
		}

		return Optional.empty();
	}

	Optional<Character> moleculeToGet(List<Sample> samples, MoleculeMap availableMolecules) {
		List<Sample> incompleteSamples = incompleteSamples(samples);
		final Optional<Character> molToGet = incompleteSamples.stream()
				.map(s -> moleculesToGet(missingMolecules(s), availableMolecules))
				.sorted((mols1, mols2) -> Integer.compare(mols1.size(), mols2.size()))
				.flatMap(mols -> mols.stream())
				.findFirst();
		if (molToGet.isPresent()) {
			return molToGet;
		}

		MoleculeMap moleculeMap = new MoleculeMap();
		moleculeMap.mergeMaps(samples.stream().map(s -> s.cost).collect(toList()));
		final List<Character> missing = moleculesToGet(
				missingMolecules(moleculeMap, this::getMolecules),
				availableMolecules);
		return Optional.ofNullable(missing.isEmpty() ? null : missing.get(0));
	}

	static List<Character> moleculesToGet(List<Character> needed, MoleculeMap available) {
		Map<Character, Long> neededMap = needed.stream().collect(Collectors.groupingBy(c -> c, Collectors.counting()));
		return neededMap.entrySet().stream()
				.flatMap(e -> {
					if (available.containsKey(e.getKey())) {
						return Stream.generate(() -> e.getKey()).limit(Long.min(e.getValue(), available.get(e.getKey())));
					} else {
						return Stream.empty();
					}
				})
				.collect(toList());
	}

	private List<Sample> incompleteSamples(List<Sample> samples) {
		List<Sample> completedSamples = completedSamples(samples);
		List<Sample> incomplete = new ArrayList<>(samples);
		incomplete.removeAll(completedSamples);
		return incomplete;
	}
	private List<Sample> completedSamples(List<Sample> samples) {
		MoleculeMap moleculeMap = new MoleculeMap();
		moleculeMap.putAll(storage);
		moleculeMap.merge(expertise);
		return this.getCompletedSamples(samples, moleculeMap);
	}

	private List<Sample> getCompletedSamples(List<Sample> samples, MoleculeMap moleculeMap) {
		if (samples.isEmpty()) {
			return Collections.emptyList();
		}

		final ArrayList<Sample> list = new ArrayList<>();
		final Sample sample = samples.get(0);
		if (missingMolecules(sample.cost, moleculeMap).isEmpty()) {
			list.add(sample);
			moleculeMap.removeMolecules(sample.cost);
			list.addAll(getCompletedSamples(samples.subList(1, samples.size()), moleculeMap));
		}
		return list;
	}

	private static List<Sample> getUndiagnosedSamples(List<Sample> samples) {
		return filterSamples(samples, s -> !s.isDiagnosed());
	}

	private static List<Sample> filterSamples(List<Sample> samples, Predicate<Sample> p) {
		return samples.stream().filter(s -> p.test(s)).collect(toList());
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
		final MoleculeMap totalCost = samples.stream().map(s -> s.cost).collect(MoleculeMap::new, (m, cost) -> {
			cost.forEach((c, i) -> {
				m.merge(c, i, Integer::sum);
			});
		}, (m1, m2) -> m1.putAll(m2));
		return missingMolecules(totalCost, this::getMolecules);
	}

	List<Character> missingMolecules(Sample sample) {
		return missingMolecules(sample.cost, this::getMolecules);
	}

	public interface Molecules {

		int getMolecules(Character c);
	}

	static List<Character> missingMolecules(MoleculeMap cost, Molecules molecules) {
		return cost.entrySet().stream()
				.filter(e -> e.getValue() > molecules.getMolecules(e.getKey()))
				.flatMap(e -> {
					long take = e.getValue() - molecules.getMolecules(e.getKey());
					return Stream.generate(() -> e.getKey()).limit(take > 0 ? take : 0);
				})
				.collect(toList());
	}

	private int molecules() {
		return storage.entrySet().stream()
				.map(e -> e.getValue())
				.mapToInt(i -> i)
				.sum();
	}

	public static class Sample {

		int sampleId;
		int carriedBy;
		int rank;
		String Gain;
		int health;
		MoleculeMap cost = new MoleculeMap();

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
		return "Player{" + "target=" + target + ", eta=" + eta + ", score=" + score + ", expertise=" + this.getExpertise() + ", A=" + this.getMolecules('A') + ", B=" + this.getMolecules('B') + ", C=" + this.getMolecules('C') + ", D=" + this.getMolecules('D') + ", E=" + this.getMolecules('E') + ", mollecules=" + this.molecules() + '}';
	}

	static String listToString(List<?> l, String delim) {
		return l.stream().map(o -> o.toString()).collect(joining(delim));
	}

	static String listToString(List<?> l) {
		return listToString(l, "\n");
	}

	private static MoleculeMap availableMolecules(Scanner in) {
		MoleculeMap m = new MoleculeMap();
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

	int getMolecules(Character c) {
		return this.storage.getOrDefault(c, 0) + this.expertise.getOrDefault(c, 0);
	}

	enum Module {

		START_POS,
		MOLECULES,
		LABORATORY,
		DIAGNOSIS,
		SAMPLES;
	}

	abstract class Command {

		void act() {
			if (target != necessaryModule()) {
				System.err.println("Go to module " + necessaryModule());
				System.out.println(goTo());
			} else {
				System.out.println(command());
			}
		}

		protected String goTo() {
			return "GOTO " + necessaryModule();
		}

		protected abstract String command();

		protected abstract Module necessaryModule();
	}

	abstract class ConnectCommand extends Command {

		String connStr;
		Module module;

		public ConnectCommand(String connStr, Module m) {
			this.connStr = connStr;
			this.module = m;
		}

		public ConnectCommand(int id, Module module) {
			this.connStr = Integer.toString(id);
			this.module = module;
		}

		public ConnectCommand(char c, Module module) {
			this.connStr = Character.toString(c);
			this.module = module;
		}

		@Override
		protected String command() {
			return "CONNECT " + connStr;
		}

		@Override
		protected Module necessaryModule() {
			return module;
		}
	}

	class GetSample extends ConnectCommand {

		private GetSample(int rank) {
			super(Integer.toString(rank), Module.SAMPLES);
		}
	}

	class PutSample extends ConnectCommand {

		public PutSample(Sample sample) {
			super(Integer.toString(sample.sampleId), Module.LABORATORY);
		}
	}

	class PutSampleToCloud extends ConnectCommand {

		public PutSampleToCloud(Sample sample) {
			super(sample.sampleId, Module.DIAGNOSIS);
		}
	}

	class GetMolecule extends ConnectCommand {

		public GetMolecule(char molecule) {
			super(molecule, Module.MOLECULES);
		}
	}

	class DiagnoseSample extends ConnectCommand {

		public DiagnoseSample(int sampleId) {
			super(sampleId, Module.DIAGNOSIS);
		}
	}

	Command WAIT = new Command() {
		@Override
		protected String command() {
			return "WAIT";
		}

		@Override
		protected Module necessaryModule() {
			return target;
		}
	};

	static class MoleculeMap extends HashMap<Character, Integer> implements Molecules {

		public void merge(MoleculeMap map) {
			map.forEach((c, i) -> {
				merge(c, i, (i1, i2) -> i1 + i2);
			});
		}

		public void mergeMaps(List<MoleculeMap> maps) {
			for (MoleculeMap map : maps) {
				merge(map);
			}
		}

		@Override
		public int getMolecules(Character c) {
			return this.get(c);
		}

		public void removeMolecules(MoleculeMap map) {
			map.forEach((c, i) -> {
				merge(c, i, (i1, i2) -> i1 - i2);
			});
		}

		public static String mapToString(MoleculeMap map, String delim) {
			return map.entrySet().stream()
					.map(e -> e.getKey() + "=" + e.getValue())
					.collect(joining(delim));
		}
	}
}
