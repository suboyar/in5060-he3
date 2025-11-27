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
    SESSION_START = "session_start"
    LABRECORDER_START = "labrecorder_start"
    PRACTICE_START = "practice_start"
    IMPEDANCE_CHECK_DONE = "impedance_check_done"
    BLOCK_START_FIRST_SHORT_BLOCK = "block_start|first_short_block"
    CUE_ONSET = "cue_onset"
    CONDITION_START_REST = "condition_start|REST"
    CONDITION_END_REST = "condition_end|REST"
    CONDITION_START_HORZ = "condition_start|HORZ"
    CONDITION_END_HORZ = "condition_end|HORZ"
    CONDITION_START_BLINK = "condition_start|BLINK"
    CONDITION_END_BLINK = "condition_end|BLINK"
    CONDITION_START_VERT = "condition_start|VERT"
    CONDITION_END_VERT = "condition_end|VERT"
    BLINK_SHRINK_ONSET_1 = "blink_shrink_onset|1"
    BLINK_SHRINK_OFFSET_1 = "blink_shrink_offset|1"
    BLINK_SHRINK_ONSET_2 = "blink_shrink_onset|2"
    BLINK_SHRINK_OFFSET_2 = "blink_shrink_offset|2"
    BLINK_SHRINK_ONSET_3 = "blink_shrink_onset|3"
    BLINK_SHRINK_OFFSET_3 = "blink_shrink_offset|3"
    BLINK_SHRINK_ONSET_4 = "blink_shrink_onset|4"
    BLINK_SHRINK_OFFSET_4 = "blink_shrink_offset|4"
    BLINK_SHRINK_ONSET_5 = "blink_shrink_onset|5"
    BLINK_SHRINK_OFFSET_5 = "blink_shrink_offset|5"
    BLINK_SHRINK_ONSET_6 = "blink_shrink_onset|6"
    BLINK_SHRINK_OFFSET_6 = "blink_shrink_offset|6"
    BLINK_SHRINK_ONSET_7 = "blink_shrink_onset|7"
    BLINK_SHRINK_OFFSET_7 = "blink_shrink_offset|7"
    BLINK_SHRINK_ONSET_8 = "blink_shrink_onset|8"
    BLINK_SHRINK_OFFSET_8 = "blink_shrink_offset|8"
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
    task_accept_12 = "task_accept|12"
    TRIAL_END = "trial_end"
    BLOCK_END_FIRST_SHORT_BLOCK = "block_end|first_short_block"
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
    try:
        data, _ = pyxdf.load_xdf(file_path, select_streams=[{"name": "ExpMarkers"}])
    except ValueError:
        raise ValueError("Could not find ExpMarkers")

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
    task_order_markers = []
    for token in parser.time_series[parser.idx:]:
        if not TokExpMarker.is_valid(token):
            parser.error(f"Unexpected {token}")

        is_jt = TokExpMarker.jebsen_taylor_type_tasks(token)
        is_bb = TokExpMarker.boxblock_type_tasks(token)

        if is_jt or is_bb:
            task_order_markers.append(token)
            idx = len(task_order_markers) - 1
            if first_jt_idx is None and is_jt:
                first_jt_idx = idx
            if first_bb_idx is None and is_bb:
                first_bb_idx = idx

        if first_jt_idx != None and first_bb_idx != None:
            break

    if first_jt_idx is None and first_bb_idx is None:
        raise ValueError(f"No task markers found in the data\n" + parser.string())

    # assert first_jt_idx != None and first_bb_idx != None, \
    #     f"Could not determen the order of the tasks: {first_jt_idx=}, {first_bb_idx=}, {task_order=}\n" + parser.string()

    task_order = []
    if first_jt_idx is not None and first_bb_idx is not None:
        if first_jt_idx < first_bb_idx:
            task_order = [TokExpMarker.JEBSEN_TAYLOR, TokExpMarker.BOXBLOCK]
        else:
            task_order = [TokExpMarker.BOXBLOCK, TokExpMarker.JEBSEN_TAYLOR]
    elif first_jt_idx is not None:
        raise ValueError("Only Jebsen-Taylor task found")
    else:
        raise ValueError("Only Box Block task found")

    return task_order

def get_latency_order(file_path, num_reps=2):
    try:
        data, _ = pyxdf.load_xdf(file_path, select_streams=[{"name": "LatencyMarkers"}])
    except ValueError:
        raise ValueError("Could not find LatencyMarkers")

    stream = data[0]

    time_series = stream['time_series']
    name = stream['info']['name'][0]
    stream_type = stream['info']['type'][0]
    number_of_samples = len(time_series)

    print(f"{name=}, {stream_type=}, {number_of_samples=}")
    parser = TimeSeriesParser(time_series, split="|")
    token = parser.next()
    num_conditions = 5
    lats = [[None for _ in range(num_conditions)] for _ in range(num_reps)]
    last_applied_lat = "";
    rep = 0
    while token != None:
        if len(token) > 0 and token[0] != "" and not TokLatMarker.is_valid(token[0]):
            parser.error(f"Unexpected {token[0]}")

        elif token[0] == TokLatMarker.LATENCY_APPLIED:
            last_applied_lat = token[1]

        elif token[0] == TokLatMarker.CONDITION_ADVANCE:
            rep = int(token[1].split("_")[-1]) - 1 # has base 1
            lat = token[2]
            condition = int(token[3].split("_")[-1]) - 1 # has base 1

            if num_reps < 2: # Hack for participant 4
                rep = 0

            lats[rep][condition] = lat
        token = parser.next()

    if lats[-1][-1] == None:
       lats[-1][-1] = last_applied_lat

    # Vanity checks
    for rep in range(len(lats)):
        for cond in range(len(lats[rep])):
            if lats[rep][cond] == None:
                raise ValueError(f"Latency contains None value: {lats}")

    return lats

def get_xdf_info(file_path):
    data, _ = pyxdf.load_xdf(file_path)
    print(f"{len(data)=}")
    for stream in data:
        time_series = stream['time_series']
        name = stream['info']['name'][0]
        stream_type = stream['info']['type'][0]
        number_of_samples = len(time_series)
        print(f"{name=}, {stream_type=}, {number_of_samples=}")

def empty_entries(participant):
    exp = []
    for trial in range(1, 11):
        exp.append({
            "participant": participant,
            "task_type": None,
            "repetition": None,
            "condition": None,
            "latency": None,
            "trial_order": trial,
        })
    return exp


skipped_participant = {
    2: "In JTHFT, the 2nd latency condition was accidentally skipped and latency condition 3 was done twice for trial 2 and 3",
    7: "The rubber of the pink finger tip fell off. The wrist tracking was not working well, resulting in only using a lateral grip for JTHFT. In BBT, the wrist was still unresponsive, but the palm is facing down now, so test is possible. In trial 3, the number of blocks should be 3",
    8: "Missing rubber of the pink finger tip. Wrist in static position but facing down",
    9: "Wrist in static position but facing down. In the 2nd trial of JTHFT, the end time should be the acceptance time instead of the end time",
    20: "Recording was started after the 1st trial",
    32: "The experiment was aborted and not completed",
    35: "Wrist in static position with the hand facing down",
}
exp = []
dirs = sorted(glob.glob("LatencyPerception/*/"))
for d in dirs:
    xdf_files = glob.glob(f"{d}/*.xdf")
    print("----------------------------------------------------------------")

    subject_id = d.split('/')[-2]
    participant = int(subject_id.split('-')[-1])

    if participant in skipped_participant:
        print(f"Skipping participant", participant)
        print(skipped_participant[participant])
        exp += empty_entries(participant)
        continue

    file_path = xdf_files[0]
    task_order = []
    latency_order  = []

    # Hack for participant 4 with multiple xdf files
    if participant == 4:
        has_error = False
        xdf_files = list(filter(lambda f: "practice" not in f, xdf_files))
        print(f"Parsing: {xdf_files}")

        tasks = {TokExpMarker.JEBSEN_TAYLOR: 0.0, TokExpMarker.BOXBLOCK: 0.0}
        for f in xdf_files:
            try:
                data, _ = pyxdf.load_xdf(f, select_streams=[{"name": "ExpMarkers"}])
                print(f"ExpMarkers count: {len(data)}")
                stream = data[0]
                first_time_stamp = stream['time_stamps'][0]
                if "JTHFT" in f:
                    tasks[TokExpMarker.JEBSEN_TAYLOR] = first_time_stamp
                else:
                    tasks[TokExpMarker.BOXBLOCK] = first_time_stamp
            except ValueError as e:
                error_str = str(e)
                print(error_str)
                skipped_participant[participant] = error_str
                exp += empty_entries(participant)
                has_error = True
                break

        if has_error:
            continue

        task_order = sorted([TokExpMarker.JEBSEN_TAYLOR, TokExpMarker.BOXBLOCK],
                            key=lambda x: tasks[x])
        print(f"task_order:  {task_order}")

        for f in xdf_files:
            try:
                latency_order += get_latency_order(f, num_reps=1)
            except ValueError as e:
                error_str = str(e)
                print(error_str)
                skipped_participant[participant] = error_str
                exp += empty_entries(participant)
                has_error = True
                break

        if has_error:
            continue

        print(f"latency_order: {latency_order}")

    else:
        print(f"Parsing: {file_path}")
        try:
            task_order = get_task_order(file_path)
            print(f"task_order:  {task_order}")
            latency_order = get_latency_order(file_path)
            print(f"latency_order: {latency_order}")
        except ValueError as e:
            error_str = str(e)
            print(error_str)
            skipped_participant[participant] = error_str
            exp += empty_entries(participant)
            continue

    for rep_idx, rep_lats in enumerate(latency_order):
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
print(exp.head(100))
print("Skipped participants and the reason:")
for p in skipped_participant:
    print(f"\t{p}: {skipped_participant[p]}")
csv_file = "LatencyPerception/participant_experiment.csv"
print(f"Exporting to {csv_file}...")
exp.to_csv(csv_file, index=False)
