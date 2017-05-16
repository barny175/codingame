/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.barny.cg.roche;

import com.barny.cg.roche.Player.Sample;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import static java.util.stream.Collectors.toList;
import java.util.stream.Stream;
import org.junit.After;
import org.junit.AfterClass;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 *
 * @author martin
 */
public class PlayerTest {
	
	public PlayerTest() {
	}
	
	@BeforeClass
	public static void setUpClass() {
	}
	
	@AfterClass
	public static void tearDownClass() {
	}
	
	@Before
	public void setUp() {
	}
	
	@After
	public void tearDown() {
	}

	/**
	 * Test of missingMolecules method, of class Player.
	 */
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
		
		Map<Character, Integer> available = new HashMap<>();
		available.put('A', 1);
		available.put('C', 1);
		available.put('B', 4);
		available.put('E', 2);
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
		List<Character> molecules = new ArrayList<>();
		molecules.add('B');
		molecules.add('B');
		molecules.add('C');
		
		Map<Character, Integer> availableMolecules = new HashMap<>();
		availableMolecules.put('A' ,5);
		availableMolecules.put('B' ,3);
		availableMolecules.put('C' ,6);
		availableMolecules.put('D' ,6);
		availableMolecules.put('E' ,6);
		
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
}
