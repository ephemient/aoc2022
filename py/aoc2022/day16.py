"""
Day 16: Proboscidea Volcanium
"""

import heapq
import itertools
import re
from collections import defaultdict
from dataclasses import dataclass

PATTERN = re.compile(
    r"Valve (\w+) has flow rate=(\d+); "
    r"(?:tunnel leads to valve|tunnels lead to valves) (\w+(?:, \w+)*)"
)

SAMPLE_INPUT = [
    "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB",
    "Valve BB has flow rate=13; tunnels lead to valves CC, AA",
    "Valve CC has flow rate=2; tunnels lead to valves DD, BB",
    "Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE",
    "Valve EE has flow rate=3; tunnels lead to valves FF, DD",
    "Valve FF has flow rate=0; tunnels lead to valves EE, GG",
    "Valve GG has flow rate=0; tunnels lead to valves FF, HH",
    "Valve HH has flow rate=22; tunnel leads to valve GG",
    "Valve II has flow rate=0; tunnels lead to valves AA, JJ",
    "Valve JJ has flow rate=21; tunnel leads to valve II",
]


def _parse(lines):
    return {
        (match := re.match(PATTERN, line)).group(1): (
            int(match.group(2)),
            match.group(3).split(", "),
        )
        for line in lines
    }


def _distances(adj):
    keys, distances = set(), defaultdict(lambda: float("inf"))
    for src, dsts in adj:
        keys.add(src)
        distances[src, src] = 0
        for dst, weight in dsts:
            keys.add(dst)
            distances[dst, dst] = 0
            distances[src, dst] = weight
    for mid in keys:
        for src in keys:
            for dst in keys:
                distance = distances[src, mid] + distances[mid, dst]
                if distance < distances[src, dst]:
                    distances[src, dst] = distance
    return distances


@dataclass(order=True, frozen=True)
class _State:
    rooms: tuple[tuple[str, int]]
    valves: frozenset[str]
    flow: int
    total: int
    time: int


def _solve(lines, num_agents, total_time):
    # pylint: disable=too-many-branches,too-many-nested-blocks,too-many-locals
    graph = _parse(lines)
    distances = _distances(
        (src, ((dst, 1) for dst in dsts)) for src, (_, dsts) in graph.items()
    )
    seen, max_seen = set(), 0
    heap = [
        (
            0,
            _State(
                rooms=(("AA", 0),) * num_agents,
                valves=frozenset(src for src, (flow, _) in graph.items() if flow > 0),
                flow=0,
                total=0,
                time=total_time,
            ),
        )
    ]

    while heap:
        estimate, state = heapq.heappop(heap)
        estimate = -estimate
        if state in seen:
            continue
        seen.add(state)
        potential = estimate + sum(
            max(
                (
                    graph[valve][0] * (state.time - delta - 1)
                    for room, age in state.rooms
                    if (delta := distances[room, valve] - age) in range(state.time)
                ),
                default=0,
            )
            for valve in state.valves
        )
        if estimate > max_seen:
            max_seen = estimate
        if potential < max_seen:
            continue

        moves_by_time = defaultdict(lambda: defaultdict(list))
        for valve in state.valves:
            for i, (room, age) in enumerate(state.rooms):
                delta = distances[room, valve] - age
                if delta in range(state.time):
                    moves_by_time[delta][i].append(valve)
        if not moves_by_time:
            continue

        for delta, moves_by_agent in moves_by_time.items():
            for size in range(1, num_agents + 1):
                for combo in itertools.combinations(moves_by_agent.items(), size):
                    indices = [i for i, _ in combo]
                    for valves in itertools.product(*(valves for _, valves in combo)):
                        if len(set(valves)) != size:
                            continue
                        new_rooms = [
                            (room, age + delta + 1) for room, age in state.rooms
                        ]
                        for i, valve in zip(indices, valves):
                            new_rooms[i] = valve, 0
                        rate = sum(graph[valve][0] for valve in valves)
                        new_state = _State(
                            rooms=tuple(sorted(new_rooms)),
                            valves=state.valves - set(valve for valve in valves),
                            flow=state.flow + rate,
                            total=state.total + state.flow * (delta + 1),
                            time=state.time - delta - 1,
                        )
                        heapq.heappush(
                            heap, (-estimate - rate * new_state.time, new_state)
                        )

    return max_seen


def part1(lines):
    """
    >>> part1(SAMPLE_INPUT)
    1651
    """
    return _solve(lines, num_agents=1, total_time=30)


def part2(lines):
    """
    >>> part2(SAMPLE_INPUT)
    1707
    """
    return _solve(lines, num_agents=2, total_time=26)


parts = (part1, part2)
