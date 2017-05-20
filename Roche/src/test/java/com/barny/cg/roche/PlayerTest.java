/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.barny.cg.roche;

import com.barny.cg.roche.Player.Command;
import com.barny.cg.roche.Player.GetMolecule;
import com.barny.cg.roche.Player.Module;
import com.barny.cg.roche.Player.PutSample;
import com.barny.cg.roche.Player.PutSampleToCloud;
import com.barny.cg.roche.Player.Sample;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import static java.util.stream.Collectors.toList;
import java.util.stream.Stream;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class PlayerTest {
	
	@Test
	public void testMissingMolecules_List() {
		List<Player.Sample> samples = new ArrayList<>();
		Sample sample = new Sample();
		sample.cost.put('A', 2);
		sample.cost.put('C', 1);
		samples.add(sample);
		
		Sample sample2 = new Sample();
		sample2.cost.put('A', 1);
		sample2.cost.put('D', 2);
		sample2.cost.put('B', 1);
		sample2.cost.put('E', 0);
		samples.add(sample2);
		
		Player player = new Player();
		player.storage.put('A', 1);
		player.storage.put('B', 2);
		
		List<Character> result = player.missingMolecules(samples);
		assertEquals(2, countMolecules(result, 'A'));
		assertEquals(0, countMolecules(result, 'B'));
		assertEquals(1, countMolecules(result, 'C'));
		assertEquals(2, countMolecules(result, 'D'));
		assertEquals(0, countMolecules(result, 'E'));
	}

	@Test
	public void testMoleculesToGet() {
		List<Character> molecules = Stream.of('A', 'A', 'C', 'E', 'E', 'E', 'E').collect(toList());
		
		Map<Character, Integer> available = moleculeMap(1, 4, 1, 0, 2);
		List<Character> result = Player.moleculesToGet(molecules, available);
		assertEquals(1, countMolecules(result, 'A'));
		assertEquals(0, countMolecules(result, 'B'));
		assertEquals(1, countMolecules(result, 'C'));
		assertEquals(2, countMolecules(result, 'E'));
	}

	private static long countMolecules(List<Character> result, Character molecule) {
		return result.stream().filter(c -> c == molecule).count();
	}
	
	/**
	 * Test of missingMolecules method, of class Player.
	 */
	@Test
	public void testMissingMolecules_PlayerSample() {
		Sample sample = new Sample();
		sample.cost.put('A', 2);
		sample.cost.put('C', 1);
		
		Player player = new Player();
		player.storage.put('A', 1);
		player.storage.put('B', 2);
		
		List<Character> result = player.missingMolecules(sample);
		assertEquals(2, result.size());
		assertTrue(result.contains('A'));
		assertTrue(result.contains('C'));
	}
	
	@Test
	public void testAvailableMolecules2() {
		List<Character> molecules = listOf('B', 'B', 'C');
		
		Map<Character, Integer> availableMolecules = moleculeMap(5, 3, 6, 6, 6);
		
		final List<Character> availMols = Player.moleculesToGet(molecules, availableMolecules);
		assertEquals(0, countMolecules(availMols, 'A'));
		assertEquals(1, countMolecules(availMols, 'C'));
		assertEquals(2, countMolecules(availMols, 'B'));
	}

	@Test
	public void testExpertise() {
		Sample sample = new Sample();
		sample.cost.put('A', 2);
		sample.cost.put('B', 4);
		sample.cost.put('C', 1);
		sample.cost.put('E', 1);
		
		Player player = new Player();
		player.storage.put('A', 1);
		player.storage.put('B', 2);
		player.expertise.put('C', 1);
		player.expertise.put('D', 2);
		player.expertise.put('E', 2);
		
		List<Character> missingMolecules = player.missingMolecules(sample);
		assertEquals(1, countMolecules(missingMolecules, 'A'));
		assertEquals(2, countMolecules(missingMolecules, 'B'));
		assertEquals(0, countMolecules(missingMolecules, 'C'));
		assertEquals(0, countMolecules(missingMolecules, 'D'));
		assertEquals(0, countMolecules(missingMolecules, 'E'));
	}
	
	@Test
	public void testMoleculeToGet() {
		Player player = new Player();
		player.storage.put('A', 0);
		player.storage.put('B', 0);
		player.storage.put('C', 0);
		player.storage.put('D', 0);
		player.storage.put('E', 0);
		
		Sample sample = new Sample();
		sample.cost.put('A', 2);
		sample.cost.put('B', 2);
		sample.cost.put('C', 1);
		sample.cost.put('E', 1);
		
		Sample sample2 = new Sample();
		sample2.cost.put('A', 2);
		sample2.cost.put('B', 4);
		sample2.cost.put('C', 1);
		sample2.cost.put('E', 1);
		
		List<Sample> samples = new ArrayList<>();
		samples.add(sample);
		samples.add(sample2);
		
		Map<Character, Integer> availableMolecules = moleculeMap(2, 2, 1, 0, 1);
		
		List<Character> mols = new ArrayList<>();
		for (int i = 0; i < 6; i++) {
			Optional<Character> moleculeToGet = player.moleculeToGet(samples, availableMolecules);
			assertTrue(moleculeToGet.isPresent());
			player.storage.merge(moleculeToGet.get(), 1, (old, val) -> old + 1);
			mols.add(moleculeToGet.get());
			availableMolecules.merge(moleculeToGet.get(), 0, (old, val) -> old - 1);
		}
		assertEquals(2, countMolecules(mols, 'A'));
		assertEquals(2, countMolecules(mols, 'B'));
		assertEquals(1, countMolecules(mols, 'C'));
		assertEquals(0, countMolecules(mols, 'D'));
		assertEquals(1, countMolecules(mols, 'E'));
		assertFalse(player.moleculeToGet(samples, availableMolecules).isPresent());
	}

	private Map<Character, Integer> moleculeMap(int a, int b, int c, int d, int e) {
		Map<Character, Integer> available = new HashMap<>();
		available.put('A', a);
		available.put('C', c);
		available.put('B', b);
		available.put('D', d);
		available.put('E', e);
		return available;
	}
	
	@Test
	public void without_samples_command_is_to_go_to_get_some() {
		Player p = new Player();
		p.target = Module.SAMPLES;
		Player.Command command = p.getCommand(listOf(), moleculeMap(1, 1, 1, 1, 1));
		assertTrue(command instanceof Player.GetSample);
	}
	
	@Test
	public void get_molecules_when_possible_and_necessary() {
		Player p = new Player();
		p.target = Module.MOLECULES;
		p.storage = moleculeMap(2, 1, 0, 2, 2);
		
		Map<Character, Integer> availableMolecules = moleculeMap(3, 0, 3, 3, 2);
		final Sample sample1 = new SampleBuilder()
				.id(2)
				.cost('A', 2)
				.cost('B', 0)
				.cost('C', 0)
				.cost('D', 1)
				.cost('E', 0)
				.build();
		final Sample sample2 = new SampleBuilder()
				.id(4)
				.cost('A', 1)
				.cost('B', 1)
				.cost('C', 0)
				.cost('D', 1)
				.cost('E', 1)
				.build();		
		final Sample sample3 = new SampleBuilder()
				.id(6)
				.cost('A', 1)
				.cost('B', 1)
				.cost('C', 0)
				.cost('D', 1)
				.cost('E', 2)
				.build();	
		
		Command cmd = p.getCommand(listOf(sample1, sample2, sample3), availableMolecules);
		assertTrue(cmd instanceof GetMolecule);
		
		List<Character> playerStillNeeds = listOf('A', 'A', 'B', 'D', 'E');
		char molecule = ((GetMolecule)cmd).connStr.charAt(0);
		assertTrue(playerStillNeeds.contains(molecule));
	}
	
	@Test
	public void produce_medicine_when_possible_and_no_molecules_available() {
		Player p = new Player();
		p.target = Module.MOLECULES;
		p.storage = moleculeMap(2, 1, 0, 2, 2);
		
		Map<Character, Integer> availableMolecules = new HashMap<>();
		final Sample sample1 = new SampleBuilder()
				.id(2)
				.cost('A', 2)
				.cost('B', 0)
				.cost('C', 0)
				.cost('D', 2)
				.cost('E', 0)
				.build();
		final Sample sample2 = new SampleBuilder()
				.id(4)
				.cost('A', 1)
				.cost('B', 1)
				.cost('C', 0)
				.cost('D', 1)
				.cost('E', 1)
				.build();		
		final Sample sample3 = new SampleBuilder()
				.id(6)
				.cost('A', 1)
				.cost('B', 1)
				.cost('C', 0)
				.cost('D', 1)
				.cost('E', 2)
				.build();	
		
		Command cmd = p.getCommand(listOf(sample1, sample2, sample3), availableMolecules);
		assertTrue(cmd instanceof PutSample);
		final String sampleId = ((PutSample)cmd).connStr;
		assertTrue(sampleId.equals("2") || sampleId.equals("4") || sampleId.equals("6"));
	}
	
	@Test
	public void return_some_samples_to_cloud_if_molecules_not_available() {
		Player p = new Player();
		p.target = Module.DIAGNOSIS;
		p.storage = moleculeMap(1, 2, 0, 0, 0);
		
		final Sample sample1 = new SampleBuilder()
				.id(1)
				.cost('A', 1)
				.cost('B', 2)
				.cost('E', 1)
				.build();
		final Sample sample2 = new SampleBuilder()
				.id(2)
				.cost('A', 3)
				.build();
		final Sample sample3 = new SampleBuilder()
				.id(3)
				.cost('A', 1)
				.cost('B', 1)
				.cost('C', 1)
				.cost('D', 1)
				.cost('E', 1)
				.build();		
		
		Player.Command command = p.getCommand(listOf(sample1, sample2, sample3), moleculeMap(0, 0, 0, 0, 0));
		assertTrue(command instanceof PutSampleToCloud);
		assertEquals(Integer.toString(sample3.sampleId), ((PutSampleToCloud)command).connStr);
	}
	
	static <T> List<T> listOf(T... ts) {
		List<T> l = new ArrayList<>();
		for (T t : ts) {
			l.add(t);
		}
		return l;
	}
	
	static class SampleBuilder {
		private Map<Character, Integer> costMap = new HashMap<>();
		private int id;
		public SampleBuilder id(int id) {
			this.id = id;
			return this;
		}
		
		public SampleBuilder cost(Character mol, int required) {
			costMap.put(mol, required);
			return this;
		}
		
		public Sample build() {
			Sample sample = new Sample();
			sample.sampleId = this.id;
			sample.cost.putAll(costMap);
			return sample;
		}
	}
}
