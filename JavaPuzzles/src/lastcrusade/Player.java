package lastcrusade;

import java.util.*;
import java.util.function.Function;
import java.util.stream.IntStream;

import static java.util.stream.Collectors.reducing;
import static java.util.stream.Collectors.toList;

class Player {
    private int W;
    private int H;
    private List<List<Integer>> tunnel;
    private int exit;

    public Player() {
    }

    public Player(int w, int h, List<List<Integer>> tunnel, int exit) {
        W = w;
        H = h;
        this.tunnel = tunnel;
        this.exit = exit;
    }

    enum Dir {
        TOP, RIGHT, BOTTOM, LEFT;

        Dir right() {
            return Dir.values()[(this.ordinal() + 1) % Dir.values().length];
        }

        Dir exitToEntry() {
            return this.right().right();
        }
    }

    enum RoomType {
        Type0(d -> Dir.BOTTOM),
        Type1(d -> Dir.BOTTOM, Dir.TOP, Dir.LEFT, Dir.RIGHT),
        Type2(d -> d == Dir.LEFT ? Dir.RIGHT : Dir.LEFT, Dir.LEFT, Dir.RIGHT),
        Type3(d -> Dir.BOTTOM, Dir.TOP),
        Type4(d -> d == Dir.TOP ? Dir.LEFT : Dir.BOTTOM, Dir.TOP, Dir.RIGHT),
        Type5(d -> d == Dir.TOP ? Dir.RIGHT : Dir.BOTTOM, Dir.TOP, Dir.LEFT),
        Type6(d -> d == Dir.LEFT ? Dir.RIGHT : Dir.LEFT, Dir.LEFT, Dir.RIGHT),
        Type7(d -> Dir.BOTTOM, Dir.TOP, Dir.RIGHT),
        Type8(d -> Dir.BOTTOM, Dir.LEFT, Dir.RIGHT),
        Type9(d -> Dir.BOTTOM, Dir.TOP, Dir.LEFT),
        Type10(d -> Dir.LEFT, Dir.TOP),
        Type11(d -> Dir.RIGHT, Dir.TOP),
        Type12(d -> Dir.BOTTOM, Dir.RIGHT),
        Type13(d -> Dir.BOTTOM, Dir.LEFT);
        Function<Dir, Dir> exitFun;
        List<Dir> entries = new ArrayList<>();

        RoomType(Function<Dir, Dir> exitFun) {
            this.exitFun = exitFun;
        }

        RoomType(Function<Dir, Dir> exitFun, Dir... entries) {
            this.exitFun = exitFun;
            this.entries.addAll(Arrays.asList(entries));
        }
    }

    private static Map<Integer, Integer> roomTypeRotationMapping = new HashMap<>();

    static {
        roomTypeRotationMapping.put(0, 0);
        roomTypeRotationMapping.put(1, 1);
        roomTypeRotationMapping.put(2, 3);
        roomTypeRotationMapping.put(3, 2);
        roomTypeRotationMapping.put(5, 4);
        roomTypeRotationMapping.put(4, 5);
        roomTypeRotationMapping.put(6, 7);
        roomTypeRotationMapping.put(7, 8);
        roomTypeRotationMapping.put(8, 9);
        roomTypeRotationMapping.put(9, 6);
        roomTypeRotationMapping.put(10, 11);
        roomTypeRotationMapping.put(11, 12);
        roomTypeRotationMapping.put(12, 13);
        roomTypeRotationMapping.put(13, 10);
    }

    private static Player init(Scanner in) {
        Player player = new Player();
        player.W = in.nextInt(); // number of columns.
        player.H = in.nextInt(); // number of rows.
        if (in.hasNextLine()) {
            in.nextLine();
        }
        player.tunnel = new ArrayList<>();
        for (int i = 0; i < player.H; i++) {
            List<Integer> line = IntStream.range(0, player.W)
                    .map(j -> in.nextInt())
                    .peek(j -> System.err.print(j))
                    .mapToObj(Integer::valueOf)
                    .collect(toList());
            System.err.println();
            player.tunnel.add(line);
        }
        player.exit = in.nextInt(); // the coordinate along the X axis of the exit.
        System.err.println("Exit: " + player.exit);
        return player;
    }

    public static void main(String args[]) {
        Scanner in = new Scanner(System.in);

        Player player = Player.init(in);

        int XI = in.nextInt();
        int YI = in.nextInt();
        Dir POSI = Dir.valueOf(in.next());

        List<Path> paths = player.findPaths(XI, YI, POSI);
        List<Command> commands = paths.get(0).getCommands();
        int i = 0;
        // game loop
        while (true) {
            int R = in.nextInt(); // the number of rocks currently in the grid.
            for (int j = 0; j < R; j++) {
                int XR = in.nextInt();
                int YR = in.nextInt();
                String POSR = in.next();
            }

            if (i < commands.size())
                System.out.println(commands.get(i++));
            else
                System.out.println("WAIT");

            XI = in.nextInt();
            YI = in.nextInt();
            POSI = Dir.valueOf(in.next());

        }
    }

    Optional<RoomType> getRoom(Pos pos, Path path) {
        int x = pos.x;
        int y = pos.y;
        if (x < 0 || x >= W
                || y < 0 || y >= H)
            return Optional.empty();

        Optional<RoomType> rotatedRoom = getRotatedRoom(path.commands, pos);
        return rotatedRoom.isPresent()
                ? rotatedRoom
                : Optional.of(getRoomType(tunnel.get(y).get(x)));
    }

    Optional<RoomType> getRotatedRoom(List<Command> commands, Pos pos) {
        RoomType roomType = getRoomType(tunnel.get(pos.y).get(pos.x));
        return commands.stream()
                .filter(cmd -> cmd instanceof RotateCommand)
                .map(cmd -> (RotateCommand) cmd)
                .filter(cmd -> cmd.roomPos.equals(pos))
                .map(cmd -> cmd.rotationFun)
                .reduce((fun1, fun2) -> fun1.andThen(fun2))
                .map(fun -> fun.apply(roomType));
    }

    RoomType getRoomType(int rt) {
        if (rt < 0)
            rt = -rt;
        return RoomType.values()[rt < 0 ? -rt : rt];
    }

    Pos getNextRoom(Pos pos, Dir dir) {
        switch (dir) {
            case RIGHT:
                return new Pos(pos.x + 1, pos.y);
            case BOTTOM:
                return new Pos(pos.x, pos.y + 1);
            case LEFT:
                return new Pos(pos.x - 1, pos.y);
            default:
                return new Pos(pos.x, pos.y - 1);
        }
    }

    static class Pos {
        int x;
        int y;

        public Pos(int x, int y) {
            this.x = x;
            this.y = y;
        }

        @Override
        public String toString() {
            return "Pos{" + "x=" + x + ", y=" + y + '}';
        }
    }

    List<Path> findPaths(int enterX, int enterY, Dir dir) {

        Pos pos = new Pos(enterX, enterY);
        return findPaths(pos, dir, new Path());
    }

    static class Command { }
    static class WaitCommand extends Command {
        @Override
        public String toString() {
            return "WAIT";
        }
    }
    static class RotateCommand extends Command {
        private final String dir;
        private final Function<RoomType, RoomType> rotationFun;
        Pos roomPos;

        public RotateCommand(Pos roomPos, String dir, Function<RoomType, RoomType> rotationFun) {
            this.roomPos = roomPos;
            this.dir = dir;
            this.rotationFun = rotationFun;
        }

        @Override
        public String toString() {
            return roomPos.x + " " + roomPos.y + " " + dir;
        }
    }

    static class Path {
        int commandSlots = 1;
        final List<Pos> positions = new LinkedList<>();
        final List<Command> commands = new LinkedList<>();
        public Path() { }
        private Path(Path path) {
            this.commands.addAll(path.commands);
            this.positions.addAll(path.positions);
        }

        public Path addCommand(Command command) {
            Path newPath = new Path(this);
            newPath.commands.add(command);
            return newPath;
        }

        public Path add(Pos pos) {
            Path newPath = new Path(this);
            newPath.commandSlots++;
            newPath.positions.add(pos);
            return newPath;
        }

        public List<Command> getCommands() {
            return commands;
        }

        public List<Pos> getPositions() {
            return positions;
        }
    }

    List<Path> findPaths(Pos pos, Dir dir, Path path) {
        ArrayList<Path> paths = new ArrayList<>();
        System.err.println("entering room at " + pos + " from " + dir);
        Path newPath = path.add(pos);
        if (pos.y == H - 1 && pos.x == exit) {
            paths.add(newPath);
            return paths;
        }

        Optional<RoomType> room = getRoom(pos, path);
        Optional<Dir> exitDir = room
                .map(rt -> rt.exitFun.apply(dir));

        if (!exitDir.isPresent()) {
            System.err.println("no way to leave " + pos);
            return paths;
        }

        Pos nextRoomPos = getNextRoom(pos, exitDir.get());
        Dir newEntryDir = exitDir.get().exitToEntry();
        // no rotation
        Optional<RoomType> nextRoom = getRoom(nextRoomPos, path);

        if (!nextRoom.isPresent()) {
            System.err.println("nowhere to go");
        }

        nextRoom
                .filter(rt -> rt.entries.contains(newEntryDir))
                .map(rt -> findPaths(nextRoomPos, newEntryDir, newPath))
                .ifPresent(pths -> {
                    paths.addAll(pths);
                    System.err.println("no rotation at " + nextRoomPos);
                });

        // rotation right
        nextRoom.map(rt -> rotate(rt, newEntryDir, newPath, new RotateCommand(nextRoomPos,"RIGHT", this::rotateRight)))
                .ifPresent(pths -> paths.addAll(pths));

        // rotation left
        nextRoom.map(rt -> rotate(rt, newEntryDir, newPath, new RotateCommand(nextRoomPos,"LEFT", this::rotateLeft)))
                .ifPresent(pths -> paths.addAll(pths));

        // rotate twice
        if (path.commandSlots >= 2) {
            nextRoom.map(rt -> rotateRight(rt))
                .map(this::rotateRight)
                    .filter(rt -> nextRoom.filter(beforeRotation -> beforeRotation != rt).isPresent())
                .filter(rt -> rt.entries.contains(newEntryDir))
                .ifPresent(roomType -> {
                    System.err.println("issuing two rotations");
                    RotateCommand command = new RotateCommand(nextRoomPos, "RIGHT", this::rotateRight);
                    paths.addAll(findPaths(nextRoomPos, newEntryDir, newPath.addCommand(command).addCommand(command)));
                });
        }

        return paths;
    }
    List<Path> rotate(RoomType room, Dir entry, Path path, RotateCommand command) {
        if (path.commandSlots == 0) {
            return Collections.emptyList();
        }
        RoomType rotatedRoom = command.rotationFun.apply(room);
        if (rotatedRoom.entries.contains(entry)) {
            System.err.println("issuing command " + command);
            return findPaths(command.roomPos, entry, path.addCommand(command));
        }
        return Collections.emptyList();
    }

    RoomType rotateRight(RoomType room) {
        return getRoomType(roomTypeRotationMapping.get(room.ordinal()));
    }

    RoomType rotateLeft(RoomType room) {
        return rotateRight(rotateRight(rotateRight(room)));
    }
}