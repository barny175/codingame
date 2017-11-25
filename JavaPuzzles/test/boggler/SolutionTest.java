package boggler;

import org.junit.Ignore;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import static org.assertj.core.api.Assertions.assertThat;

public class SolutionTest {
    private char[][] matrix = new char[][] {
            {'M', 'P', 'L', 'R'},
            {'D', 'S', 'D', 'A'},
            {'H', 'N', 'E', 'O'},
            {'S', 'H', 'T', 'Y'}};

    @Test
    public void neighbours() {
        Solution solution = new Solution(new char[4][4]);
        Set<Solution.Pos> neighbours = solution.neighbours(0, 0);
        assertThat(neighbours)
                .contains(
                        new Solution.Pos(1, 0),
                        new Solution.Pos(0, 1),
                        new Solution.Pos(1, 1));
    }

    @Test
    public void match() {
        Solution solution = new Solution(matrix);
        assertThat(solution.matches("AR")).isTrue();
        assertThat(solution.matches("ARDENT")).isTrue();
        assertThat(solution.matches("KOPR")).isFalse();
    }

    @Test
    public void nextMatches() {
        Solution solution = new Solution(matrix);
        Solution.Match match = new Solution.Match();
        match.used.add(new Solution.Pos(1, 3));
        List<Solution.Match> matches = new ArrayList<>();
        matches.add(match);
        List<Solution.Match> nextMatches = solution.nextMatches('R', matches);
        assertThat(nextMatches).hasSize(1);
        assertThat(nextMatches.get(0).used)
                .containsOnly(
                        new Solution.Pos(0, 3),
                        new Solution.Pos(1, 3));
    }

    @Test
    public void initialMatch() {
        List<Solution.Match> matches = new Solution(matrix).initialMatches("SH");
        assertThat(matches).extracting(m -> m.last())
                .containsOnly(new Solution.Pos(1, 1), new Solution.Pos(3, 0));

        List<Solution.Match> matches2 = new Solution(matrix).initialMatches("A");
        assertThat(matches2).extracting(m -> m.last())
                .containsOnly(new Solution.Pos(1, 3));
    }
}