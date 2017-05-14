/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.barny.cg.roche;

import com.barny.cg.roche.Player.Sample;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import org.junit.After;
import org.junit.AfterClass;
import static org.junit.Assert.assertEquals;
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
		assertEquals(2, result.stream().filter(c -> c == 'A').count());
		assertEquals(0, result.stream().filter(c -> c == 'B').count());
		assertEquals(1, result.stream().filter(c -> c == 'C').count());
		assertEquals(2, result.stream().filter(c -> c == 'D').count());
		assertEquals(0, result.stream().filter(c -> c == 'E').count());
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
		
		Map<Character, Integer> result = player.missingMolecules(sample);
		assertEquals(1, (int) result.get('A'));
		assertEquals(1, (int) result.get('C'));
	}
}