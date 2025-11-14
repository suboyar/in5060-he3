import glob
import sys
from enum import StrEnum, auto, unique

import pyxdf
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

@unique
class TokExpMarker(StrEnum):
    JEBSEN_TAYLOR = "jebsen_taylor"
    BOXBLOCK = "boxblock"
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
    TASK_REPEAT_REASON_OTHER = "task_repeat_reason|other"
    TASK_ACCEPT = "task_accept"
    TASK_ACCEPT_0 = "task_accept|0"
    TASK_ACCEPT_1 = "task_accept|1"
    TASK_ACCEPT_2 = "task_accept|2"
    TASK_ACCEPT_3 = "task_accept|3"
    TASK_ACCEPT_4 = "task_accept|4"
    TASK_ACCEPT_5 = "task_accept|5"
    TASK_ACCEPT_6 = "task_accept|6"
    TASK_ACCEPT_7 = "task_accept|7"
    TASK_ACCEPT_8 = "task_accept|8"
    TASK_ACCEPT_9 = "task_accept|9"
    TASK_ACCEPT_10 = "task_accept|10"
    task_accept_11 = "task_accept|11"
    TRIAL_END = "trial_end"
    TASK_REPEAT_REASON_HARDWARE = "task_repeat_reason|hardware"
    PRACTICE_BOXBLOCK_START = "practice_boxblock_start"
    PRACTICE_BLOCK_MOVED = "practice_block_moved"
    PRACTICE_BOXBLOCK_END = "practice_boxblock_end"
    TRIAL_BOXBLOCK_START = "boxblock_start"
    TRIAL_BOXBLOCK_END = "boxblock_end"
    BLOCK_MOVED = "block_moved"
    BOXBLOCK_SKIPPED = "boxblock_skipped"
    PRACTICE_BOXBLOCK_SKIPPED = "practice_boxblock_skipped"
    SCORE_MANUALLY_CHANGED = "score_manually_changed"
    TASK_REPEAT_LIMIT_REACHED = "task_repeat_limit_reached"
    SESSION_END = "session_end"

    @classmethod
    def is_valid(cls, token):
        return token in cls._value2member_map_

    @classmethod
    def jebsen_taylor_type_tasks(cls, token):
        return token in [cls.TASK_BLOCK_START_JEBSEN_TAYLOR, cls.TASK_BLOCK_END_JEBSEN_TAYLOR,
                         cls.PRACTICE_JEBSEN_TAYLOR_START, cls.PRACTICE_JEBSEN_TAYLOR_END,
                         cls.TRIAL_JEBSEN_TAYLOR_START, cls.TRIAL_JEBSEN_TAYLOR_END]

    @classmethod
    def boxblock_type_tasks(cls, token):
        return token in [cls.PRACTICE_BOXBLOCK_START, cls.PRACTICE_BOXBLOCK_END,
                         cls.TRIAL_BOXBLOCK_START, cls.TRIAL_BOXBLOCK_END, cls.BOXBLOCK_SKIPPED]

@unique
class TokLatMarker(StrEnum):
    REPETITION_START = "repetition_start"
    LATENCY_APPLIED = "latency_applied"
    CONDITION_ADVANCE = "condition_advance"
    REPETITION_COMPLETE = "repetition_complete"
    LATENCY_SEQUENCE_COMPLETE = "latency_sequence_complete"
    LATENCY_CONTROLLER_SHUTDOWN = "latency_controller_shutdown"

    @classmethod
    def is_valid(cls, token):
        return token in cls._value2member_map_


class TimeSeriesParser:
    def __init__(self, time_series, split="", unpack=True):
        self.time_series = time_series
        self.idx = 0
        if unpack:
            self.time_series = [l[0] for l in self.time_series]
        if split != "":
            self.time_series = [l.split("|") for l in self.time_series]

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

def get_task_order(file_path):
    data, _ = pyxdf.load_xdf(file_path, select_streams=[{"name": "ExpMarkers"}])
    print(f"ExpMarkers count: {len(data)}")
    stream = data[0]

    time_series = stream['time_series']
    name = stream['info']['name'][0]
    stream_type = stream['info']['type'][0]
    number_of_samples = len(time_series)

    print(f"{name=}, {stream_type=}, {number_of_samples=}")
    parser = TimeSeriesParser(time_series)
    task_order = [None, None]

    first_jt_idx = first_bb_idx = None
    task_order = []
    tasks = []
    for token in parser.time_series[parser.idx:]:
        if not TokExpMarker.is_valid(token):
            parser.error(f"Unexpected {token}")

        is_jt = TokExpMarker.jebsen_taylor_type_tasks(token)
        is_bb = TokExpMarker.boxblock_type_tasks(token)

        if is_jt or is_bb:
            task_order.append(token)
            idx = len(task_order) - 1
            if first_jt_idx is None and is_jt:
                first_jt_idx = idx
            if first_bb_idx is None and is_bb:
                first_bb_idx = idx

        if first_jt_idx != None and first_bb_idx != None:
            break

    assert first_jt_idx != None and first_bb_idx != None, "Could not determen the order of the tasks"

    if first_jt_idx is not None and first_bb_idx is not None:
        if first_jt_idx < first_bb_idx:
            task_order = [TokExpMarker.JEBSEN_TAYLOR, TokExpMarker.BOXBLOCK]
        else:
            task_order = [TokExpMarker.BOXBLOCK, TokExpMarker.JEBSEN_TAYLOR]

    return task_order


def get_latency_order(file_path):
    data, _ = pyxdf.load_xdf(file_path, select_streams=[{"name": "LatencyMarkers"}])
    stream = data[0]

    time_series = stream['time_series']
    name = stream['info']['name'][0]
    stream_type = stream['info']['type'][0]
    number_of_samples = len(time_series)

    print(f"{name=}, {stream_type=}, {number_of_samples=}")
    parser = TimeSeriesParser(time_series, split="|")
    token = parser.next()
    num_conditions = 5
    num_reps = 2
    lats = [[None for _ in range(num_conditions)] for _ in range(num_reps)]
    last_applied_lat = "";
    rep = 0
    rep_started = True
    while token != None:
        if len(token) > 0 and token[0] != "" and not TokLatMarker.is_valid(token[0]):
            parser.error(f"Unexpected {token[0]}")

        elif token[0] == TokLatMarker.LATENCY_APPLIED and rep_started:
            last_applied_lat = token[1]

        elif token[0] == TokLatMarker.CONDITION_ADVANCE and rep_started:
            rep = int(token[1].split("_")[-1]) - 1 # has base 1
            lat = token[2]
            condition = int(token[3].split("_")[-1]) - 1 # has base 1
            lats[rep][condition] = lat
        token = parser.next()

    if lats[-1][-1] == None:
       lats[-1][-1] = last_applied_lat

    # Vanity check
    for rep in range(len(lats)):
        for cond in range(len(lats[rep])):
            if lats[rep][cond] == None:
                raise ValueError(f"Latency contains None value at rep={rep}, cond={cond}\n{lats}")

    return lats


exp = []
dirs = sorted(glob.glob("LatencyPerception/*/"))
for i,d in enumerate(dirs):
    xdf_files = glob.glob(f"{d}/*.xdf")
    print("----------------------------------------------------------------")

    subject_id = d.split('/')[-2]
    participant = int(subject_id.split('-')[-1])

    if len(xdf_files) > 1:
        print(f"TODO: Multiple XDF files for {d}")
        # Add 10 trials with NA values so R doesn't break
        for trial in range(1, 11):
            exp.append({
                "participant": participant,
                "task_type": None,
                "repetition": None,
                "condition": None,
                "latency": None,
                "trial_order": trial,
            })
        continue

    file_path = xdf_files[0]
    print(f"Parsing: {file_path}")
    task_order = get_task_order(file_path)
    print(f"task_order:  {task_order}")
    lats = get_latency_order(file_path)
    print(f"latency: {lats}")

    lat_task = {i+1: {task_order[0].value: lats[0], task_order[1].value: lats[1]}}

    # Correction for participant 002
    if participant == 2:
        lat_task[i+1][TokExpMarker.JEBSEN_TAYLOR.value][1] = lat_task[i+1][TokExpMarker.JEBSEN_TAYLOR.value][2]

    for rep_idx, rep_lats in enumerate(lats):
        task_type = task_order[rep_idx].value
        for cond_idx, latency in enumerate(rep_lats):
            row_data = {
                "participant": participant,
                "task_type": task_type,
                "repetition": rep_idx + 1,  # base 0
                "condition": cond_idx + 1,   # base 0
                "latency": latency,
                "trial_order": (rep_idx * len(rep_lats)) + cond_idx + 1, # R works in base 1
            }
            exp.append(row_data)

exp = pd.DataFrame(exp)
print(exp.head(20))
exp.to_csv("LatencyPerception/participant_experiment.csv", index=False)
