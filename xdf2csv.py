import sys
from enum import StrEnum, auto, unique

import pyxdf
import matplotlib.pyplot as plt
import numpy as np

@unique
class TokExpMarker(StrEnum):
    JEBSEN_TAYLOR = auto()
    BOXBLOCK = auto()
    TASK_BLOCK_START_JEBSEN_TAYLOR = "task_block_start|JEBSEN_TAYLOR"
    TASK_BLOCK_END_JEBSEN_TAYLOR = "task_block_end|JEBSEN_TAYLOR"
    TASK_BLOCK_START_BOX_BLOCK = "task_block_start|BOX_BLOCK"
    TASK_BLOCK_END_BOX_BLOCK = "task_block_end|BOX_BLOCK"
    PRACTICE_SESSION_START = "practice_session_start"
    LABRECORDER_START = "labrecorder_start"
    PRACTICE_START = "practice_start"
    COUNTDOWN_START = "countdown_start"
    COUNTDOWN_3 = "countdown_3"
    COUNTDOWN_2 = "countdown_2"
    COUNTDOWN_1 = "countdown_1"
    PRACTICE_JEBSEN_TAYLOR_START = "practice_jebsen_taylor_start"
    PRACTICE_JEBSEN_TAYLOR_END = "practice_jebsen_taylor_end"
    TRIAL_JEBSEN_TAYLOR_START = "jebsen_taylor_start"
    TRIAL_JEBSEN_TAYLOR_END = "jebsen_taylor_end"
    PRACTICE_END = "practice_end"
    LABRECORDER_STOP = "labrecorder_stop"
    PRACTICE_SESSION_END = "practice_session_end"
    TRIAL_START = "trial_start"
    TASK_REPEAT = "task_repeat"
    TASK_REPEAT_REASON_PARTICIPANT = "task_repeat_reason|participant"
    TASK_ACCEPT = "task_accept"
    TASK_ACCEPT_1 = "task_accept|1"
    TASK_ACCEPT_2 = "task_accept|2"
    TASK_ACCEPT_3 = "task_accept|3"
    TASK_ACCEPT_4 = "task_accept|4"
    TASK_ACCEPT_5 = "task_accept|5"
    TASK_ACCEPT_6 = "task_accept|6"
    TRIAL_END = "trial_end"
    TASK_REPEAT_REASON_HARDWARE = "task_repeat_reason|hardware"
    PRACTICE_BOXBLOCK_START = "practice_boxblock_start"
    PRACTICE_BLOCK_MOVED = "practice_block_moved"
    PRACTICE_BOXBLOCK_END = "practice_boxblock_end"
    TRIAL_BOXBLOCK_START = "boxblock_start"
    TRIAL_BOXBLOCK_END = "boxblock_end"
    BLOCK_MOVED = "block_moved"
    BOXBLOCK_SKIPPED = "boxblock_skipped"
    SESSION_END = "session_end"
    SCORE_MANUALLY_CHANGED = auto()

class TimeSeriesParser:
    def __init__(self, time_series, do_unpack=True):
        self.time_series = time_series
        self.idx = 0
        if do_unpack:
            self.unpack()

    def unpack(self):
        arr = []
        for l in self.time_series:
            assert isinstance(l, list), f"Expected list, got {type(l)}"
            assert len(l) == 1, f"Expected list with 1 element, got {len(l)}"
            arr.append(l[0])
        self.time_series = arr

    def print(self):
        for idx,token in enumerate(self.time_series):
            print(f"{token} ({idx})")

    def string(self):
        s = ""
        for idx,token in enumerate(self.time_series):
            s += f"{token} ({idx})\n"
        return s

    def error(self, error_str=None):
        assert error_str != None
        assert False, f"{error_str} ({self.idx-1})\n" + self.string()

    def next(self):
        if len(self.time_series) > self.idx:
            ret = self.time_series[self.idx]
            self.idx += 1
            return ret
        return None

    def get_next_token(self, expected=None):
        token = self.next()
        if (isinstance(expected, TokExpMarker) and token != expected):
            self.error(f"expected {expected}, but got {token}")
        return token

    def peak_next_token(self, expected=None):
        token = None
        if len(self.time_series) > self.idx:
            token = self.time_series[self.idx]
        if isinstance(expected, TokExpMarker):
            return token == expected
        return token

def consum_practice(parser):
    token = parser.next()
    assert token == TokExpMarker.PRACTICE_SESSION_START, f"No {TokExpMarker.PRACTICE_SESSION_START} was found"
    while(token != None):
        token = parser.next()
        if (token == TokExpMarker.PRACTICE_SESSION_END):
            token = parser.next()
            return
    assert False, f"No {TokExpMarker.PRACTICE_SESSION_END} was found"

def consum_task(parser):
    task_type = None
    token = parser.get_next_token()
    if token == TokExpMarker.TASK_BLOCK_START_JEBSEN_TAYLOR:
        task_type = TokExpMarker.JEBSEN_TAYLOR
    elif token == TokExpMarker.TASK_BLOCK_START_BOX_BLOCK:
        task_type = TokExpMarker.BOXBLOCK
    else:

        parser.error(f"Expected either {TokExpMarker.TASK_BLOCK_START_JEBSEN_TAYLOR} or " \
                     f"{TokExpMarker.TASK_BLOCK_START_BOX_BLOCK} as task type, but got {task_type}")

    # Jump over practice
    consum_practice(parser)

    # A trail should be started
    token = parser.get_next_token(TokExpMarker.TRIAL_START)
    while (token != None):
        parser.get_next_token(TokExpMarker.COUNTDOWN_START)
        parser.get_next_token(TokExpMarker.COUNTDOWN_3)
        parser.get_next_token(TokExpMarker.COUNTDOWN_2)
        parser.get_next_token(TokExpMarker.COUNTDOWN_1)

        if task_type == TokExpMarker.JEBSEN_TAYLOR:
            parser.get_next_token(TokExpMarker.TRIAL_JEBSEN_TAYLOR_START)
            parser.get_next_token(TokExpMarker.TRIAL_JEBSEN_TAYLOR_END)
        else:
            parser.get_next_token(TokExpMarker.TRIAL_BOXBLOCK_START)
            while parser.peak_next_token(TokExpMarker.BLOCK_MOVED):
                parser.get_next_token(TokExpMarker.BLOCK_MOVED)

            if parser.peak_next_token(TokExpMarker.BOXBLOCK_SKIPPED) or parser.peak_next_token(TokExpMarker.TRIAL_BOXBLOCK_END):
               parser.get_next_token()

        token = parser.get_next_token()
        if token == TokExpMarker.SCORE_MANUALLY_CHANGED:
            token = parser.get_next_token()

        if token == TokExpMarker.TASK_REPEAT:
            token = parser.get_next_token()
            if token not in (TokExpMarker.TASK_REPEAT_REASON_PARTICIPANT, TokExpMarker.TASK_REPEAT_REASON_HARDWARE):
                parser.error(f"expected {TokExpMarker.TASK_REPEAT_REASON_PARTICIPANT} or "\
                             f"{TokExpMarker.TASK_REPEAT_REASON_HARDWARE} but got {token}")
        elif token in (TokExpMarker.TASK_ACCEPT, TokExpMarker.TASK_ACCEPT_1, TokExpMarker.TASK_ACCEPT_2,
                       TokExpMarker.TASK_ACCEPT_3, TokExpMarker.TASK_ACCEPT_4,
                       TokExpMarker.TASK_ACCEPT_5, TokExpMarker.TASK_ACCEPT_6):
            parser.get_next_token(TokExpMarker.TRIAL_END)
            parser.get_next_token(TokExpMarker.LABRECORDER_STOP)
            if parser.peak_next_token(TokExpMarker.LABRECORDER_START):
                parser.get_next_token(TokExpMarker.LABRECORDER_START)
                parser.get_next_token(TokExpMarker.TRIAL_START)
            else:
                if task_type == TokExpMarker.JEBSEN_TAYLOR:
                    parser.get_next_token(TokExpMarker.TASK_BLOCK_END_JEBSEN_TAYLOR)
                else:
                    parser.get_next_token(TokExpMarker.TASK_BLOCK_END_BOX_BLOCK)

                return task_type
        else:
            parser.error(f"got a token that was not handled {token}")

    return None

def parse_xdf(file_path):
    print(f"Parsing: {file_path}")
    data, _ = pyxdf.load_xdf(file_path)
    all_tasks = []
    expmarker_count = 0

    for i,stream in enumerate(data):
        time_series = stream['time_series']
        name = stream['info']['name'][0]
        stream_type = stream['info']['type'][0]
        number_of_samples = len(time_series)

        # print(f"{i}: {name=}, {stream_type=}, {number_of_samples=}")


        if name == "ExpMarkers":
            parser = TimeSeriesParser(time_series)
            # parser.print()
            tasks = []
            tasks.append(consum_task(parser))
            print(f"success: finished parsing first task")
            tasks.append(consum_task(parser))
            print(f"success: finished parsing second task")
            parser.get_next_token(TokExpMarker.SESSION_END)
            all_tasks.append(tasks)
            break





        # continue

        # if name != "LatencyMarkers":
        #     continue

        # for y in time_series:
        #     assert len(y) == 1

        #     mark = y[0].split("|")
        #     if mark[0] =="condition_advance":
        #         continue
        #     elif mark[0] == "repetition_start":
        #         print(f"repetition start: {mark[1]}")
        #     elif mark[0] == "repetition_complete":
        #         print(f"repetition end: {mark[1]}")
        #     elif mark[0] == "latency_applied":
        #         print(f"latency: {mark[1]}")
        #     else:
                # print(mark)

    print(f"all_tasks: len={len(all_tasks)}, same={all(tasks == all_tasks[0] for tasks in all_tasks)}, {all_tasks}")


# print("----------------------------------------------------------------")
# parse_xdf('LatencyPerception/sub-001/sub-001_ses-_task-_run-001.xdf')
# print("\n----------------------------------------------------------------")
# parse_xdf('LatencyPerception/sub-002/sub-002_ses-_task-_run-001.xdf')
# print("\n----------------------------------------------------------------")
# parse_xdf('LatencyPerception/sub-003/sub-003_ses-_task-_run-001.xdf') # No practice_session_start nor task_block_start|JEBSEN_TAYLOR is present, in both ExpMarkers
# print("\n----------------------------------------------------------------")
# parse_xdf('LatencyPerception/sub-005/sub-005_ses-_task-_run-001.xdf') # No practice_session_start was found in second ExpMarker
# print("\n----------------------------------------------------------------")
# parse_xdf('LatencyPerception/sub-006/sub-006_ses-_task-_run-001.xdf')
print("\n----------------------------------------------------------------")
parse_xdf('LatencyPerception/sub-007/sub-007_ses-_task-_run-001.xdf')
print("\n----------------------------------------------------------------")
