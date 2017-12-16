package lastcrusade;

import org.assertj.core.util.Lists;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

import static org.assertj.core.api.Assertions.*;

public class PlayerTest {
    @Test
    public void dirRotate() {
        assertThat(Player.Dir.TOP.right()).isEqualByComparingTo(Player.Dir.RIGHT);
        assertThat(Player.Dir.RIGHT.right()).isEqualByComparingTo(Player.Dir.BOTTOM);
        assertThat(Player.Dir.BOTTOM.right()).isEqualByComparingTo(Player.Dir.LEFT);
        assertThat(Player.Dir.LEFT.right()).isEqualByComparingTo(Player.Dir.TOP);
    }

    @Test
    public void findPath_one_level() {
        ArrayList<List<Integer>> tunnel = new ArrayList<>();
        tunnel.add(Lists.newArrayList(0, 3, 0));
        Player player = new Player(3, 1, tunnel, 1);
        List<Player.Path> paths = player.findPaths(1, 0, Player.Dir.TOP);
        assertThat(paths).hasSize(1);
        assertThat(paths.get(0).positions).hasSize(1);
        assertThat(paths.get(0).positions).extracting(p -> p.x, p-> p.y)
                .contains(tuple(1, 0));
    }

    @Test
    public void findPath_two_level_no_rotation() {
        ArrayList<List<Integer>> tunnel = new ArrayList<>();
        tunnel.add(Lists.newArrayList(0, 1, 0));
        tunnel.add(Lists.newArrayList(0, 7, 0));
        Player player = new Player(3, 2, tunnel, 1);
        List<Player.Path> paths = player.findPaths(1, 0, Player.Dir.TOP);
        assertThat(paths).hasSize(1);
        assertThat(paths.get(0).positions).hasSize(2);
        assertThat(paths.get(0).positions).extracting(p -> p.x, p-> p.y)
                .contains(
                        tuple(1, 0),
                        tuple(1, 1));
        assertThat(paths.get(0).getCommands()).isEmpty();
    }


    @Test
    public void findPath_two_level_rotation_right() {
        ArrayList<List<Integer>> tunnel = new ArrayList<>();
        tunnel.add(Lists.newArrayList(0, 1, 0));
        tunnel.add(Lists.newArrayList(0, 10, 13));
        Player player = new Player(3, 2, tunnel, 2);
        List<Player.Path> paths = player.findPaths(1, 0, Player.Dir.TOP);
        assertThat(paths).hasSize(1);
        assertThat(paths.get(0).positions).hasSize(3);
        assertThat(paths.get(0).positions).extracting(p -> p.x, p-> p.y)
                .contains(
                        tuple(1, 0),
                        tuple(1, 1),
                        tuple(2, 1));
        assertThat(paths.get(0).getCommands())
                .extracting(Player.Command::toString)
                .contains("1 1 RIGHT");
    }

    @Test
    public void findPath_four_level() {
        ArrayList<List<Integer>> tunnel = new ArrayList<>();
        tunnel.add(Lists.newArrayList(4, -3));
        tunnel.add(Lists.newArrayList(11, -10));
        tunnel.add(Lists.newArrayList(11, 5));
        tunnel.add(Lists.newArrayList(2, 3));
        Player player = new Player(2, 4, tunnel, 1);
        List<Player.Path> paths = player.findPaths(1, 0, Player.Dir.TOP);
        assertThat(paths).hasSize(1);
        assertThat(paths.get(0).positions).hasSize(6);
        assertThat(paths.get(0).positions).extracting(p -> p.x, p-> p.y)
                .contains(
                        tuple(1, 0),
                        tuple(1, 1),
                        tuple(0, 1),
                        tuple(0, 2),
                        tuple(1, 2),
                        tuple(1, 3));
        assertThat(paths.get(0).getCommands())
                .extracting(cmd -> cmd.toString())
                .contains("0 1 RIGHT");
    }


    @Test
    public void findPath_two_level_nonrotating_room() {
        ArrayList<List<Integer>> tunnel = new ArrayList<>();
        tunnel.add(Lists.newArrayList(0, 1, 0));
        tunnel.add(Lists.newArrayList(0, -3, 0));
        Player player = new Player(3, 2, tunnel, 1);
        List<Player.Path> paths = player.findPaths(1, 0, Player.Dir.TOP);
        assertThat(paths).hasSize(1);
        assertThat(paths.get(0).positions).hasSize(2);
        assertThat(paths.get(0).positions).extracting(p -> p.x, p-> p.y)
                .contains(
                        tuple(1, 0),
                        tuple(1, 1));
        assertThat(paths.get(0).getCommands()).isEmpty();
    }

    @Test
    public void findPath_rotate_left() {
        ArrayList<List<Integer>> tunnel = new ArrayList<>();
        tunnel.add(Lists.newArrayList(0, 1, 0));
        tunnel.add(Lists.newArrayList(13, 11, 0));
        tunnel.add(Lists.newArrayList(-3, 0, 0));
        Player player = new Player(3, 3, tunnel, 0);
        List<Player.Path> paths = player.findPaths(1, 0, Player.Dir.TOP);
        assertThat(paths).hasSize(1);
        assertThat(paths.get(0).positions).hasSize(4);
        assertThat(paths.get(0).positions)
                .extracting(p -> p.x, p-> p.y)
                .contains(
                        tuple(1, 0),
                        tuple(1, 1),
                        tuple(0, 1),
                        tuple(0, 2));
        assertThat(paths.get(0).getCommands())
                .extracting(Player.Command::toString)
                .contains("1 1 LEFT", "0 1 LEFT");
    }
    @Test
    public void findPath_many_rotations() {
        ArrayList<List<Integer>> tunnel = new ArrayList<>();
        tunnel.add(Lists.newArrayList(0, -3, 0, 0, 0, 0));
        tunnel.add(Lists.newArrayList(0, 11, 2, 3, 4, 0));
        tunnel.add(Lists.newArrayList(0, 0, 0, 0, 2, 0));
        tunnel.add(Lists.newArrayList(-1, 2, 3, 2, 5, 0));
        Player player = new Player(6, 4, tunnel, 0);
        List<Player.Path> paths = player.findPaths(1, 0, Player.Dir.TOP);
        assertThat(paths).isNotEmpty();
        assertThat(paths.get(0).positions)
                .extracting(p -> p.x, p-> p.y)
                .contains(
                        tuple(1, 0),
                        tuple(1, 1),
                        tuple(2, 1),
                        tuple(3, 1),
                        tuple(4, 1),
                        tuple(4, 2),
                        tuple(4, 3),
                        tuple(3, 3),
                        tuple(2, 3),
                        tuple(1, 3),
                        tuple(0, 3));
    }

    @Test
    public void findPath_two_rotations_of_same_room() {
        ArrayList<List<Integer>> tunnel = new ArrayList<>();
        tunnel.add(Lists.newArrayList(0, -3, 0, 0));
        tunnel.add(Lists.newArrayList(0, 3, 0, 0));
        tunnel.add(Lists.newArrayList(0, 13, 13, 0));
        tunnel.add(Lists.newArrayList(0, 0, -3, 0));
        Player player = new Player(4, 4, tunnel, 2);
        List<Player.Path> paths = player.findPaths(1, 0, Player.Dir.TOP);
        assertThat(paths).hasSize(1);
        assertThat(paths.get(0).positions)
                .extracting(p -> p.x, p-> p.y)
                .contains(
                        tuple(1, 0),
                        tuple(1, 1),
                        tuple(1, 2),
                        tuple(2, 2),
                        tuple(2, 3));
        assertThat(paths.get(0).getCommands())
                .extracting(Player.Command::toString)
                .contains("1 2 RIGHT", "1 2 RIGHT");
    }

    @Test
    public void findPath_more_paths() {
        ArrayList<List<Integer>> tunnel = new ArrayList<>();
        tunnel.add(Lists.newArrayList(0, -3, 0, 0));
        tunnel.add(Lists.newArrayList(12, 5, 13, 0));
        tunnel.add(Lists.newArrayList(11, 12, 9, 0));
        tunnel.add(Lists.newArrayList(0, 0, -3, 0));
        Player player = new Player(4, 4, tunnel, 2);
        List<Player.Path> paths = player.findPaths(1, 0, Player.Dir.TOP);
        assertThat(paths).hasSize(2);
    }
}