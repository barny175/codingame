package lastcrusade;

import org.assertj.core.util.Lists;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.assertj.core.api.Assertions.*;

public class PlayerTest {
    @Test
    public void dirRotate() {
        assertThat(Player.Dir.Top.right()).isEqualByComparingTo(Player.Dir.Right);
        assertThat(Player.Dir.Right.right()).isEqualByComparingTo(Player.Dir.Bottom);
        assertThat(Player.Dir.Bottom.right()).isEqualByComparingTo(Player.Dir.Left);
        assertThat(Player.Dir.Left.right()).isEqualByComparingTo(Player.Dir.Top);
    }

    @Test
    public void findPath_one_level() {
        ArrayList<List<Integer>> tunnel = new ArrayList<>();
        tunnel.add(Lists.newArrayList(0, 3, 0));
        Player player = new Player(3, 1, tunnel, 1);
        List<Player.Path> paths = player.findPaths(1, 0, Player.Dir.Top);
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
        List<Player.Path> paths = player.findPaths(1, 0, Player.Dir.Top);
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
        tunnel.add(Lists.newArrayList(0, 6, 0));
        Player player = new Player(3, 2, tunnel, 1);
        List<Player.Path> paths = player.findPaths(1, 0, Player.Dir.Top);
        assertThat(paths).hasSize(1);
        assertThat(paths.get(0).positions).hasSize(2);
        assertThat(paths.get(0).positions).extracting(p -> p.x, p-> p.y)
                .contains(
                        tuple(1, 0),
                        tuple(1, 1));
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
        List<Player.Path> paths = player.findPaths(1, 0, Player.Dir.Top);
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
}