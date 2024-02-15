import numpy as np
import pandas as pd
pd.options.mode.chained_assignment = None 
import matplotlib.pyplot as plt
import seaborn as sns
import json
import re
import csv
import ast
import os
from pathlib import Path

"""
Build a dictionary that allows us to look up the stages from the gameId
key: gameId
  value:
	{ key: roundId
		value:{
			key: stageId 1
			value: timestamp for stage 1 start

			key: stageId 2
			value: timestamp for stage 2 start, 

			key: stageId 3
			value: timestamp for stage 3 start, 
		}
	}
"""
def build_game_id_dict(multi_task_data, multi_task_stages):
	game_id_lookup = {}

	for index, row in multi_task_data.iterrows():
		cur_gameid = row['gameId']
		if cur_gameid not in game_id_lookup.keys():
			# add it
			game_id_lookup.update({cur_gameid: {}})

		# get current task, stageId, and complexity
		roundId = row["_id"]
		stageId = row["stageIds"]

		# ensure current task is updated in dict:
		if roundId not in game_id_lookup[cur_gameid].keys():
			# add it
			game_id_lookup[cur_gameid].update({roundId: {}})

		# update the dictionary
		game_id_lookup[cur_gameid][roundId].update({stageId: multi_task_stages[multi_task_stages["_id"]==stageId]["startTimeAt"].unique()[0]})

	return game_id_lookup

"""
Parse information from being lumped together across stages into 'conversations' for each stage
"""
def process_data_into_conversations(muti_task_chat, game_id_lookup):
	current_script_directory = Path(__file__).resolve().parent
	output_path = current_script_directory / '../raw_data/multi_task_conversations.csv'
	with open(output_path, 'w', newline='') as csvfile:
		# start writing the header of the CSV
		fieldnames = ["stageId", "roundId", "gameId", "message", "speaker_nickname", "timestamp"]
		writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
		writer.writeheader()

		for index, row in muti_task_chat.iterrows():
			chat = "[" + row['data.A'] + "]" # convert messages to a list of dict objects
			chat_list = eval(chat)

			game_id = row["gameId"]
			round_id = row["_id"]

			# Get timing for all the stages
			# Sort the stage ID's by value (which is the timestamp)
			stage_dict = dict(sorted(game_id_lookup[game_id][round_id].items(), key=lambda item: item[1]))
			stageIds = list(stage_dict.keys())
			stage_timings = list(stage_dict.values())
			
			for chat in chat_list:
				text = chat['text']
				speaker = chat['player']['_id']
				timestamp = chat['timeStamp']
				stage_id = ""

				# Determine which stage a chat was spoken in
				if(len(stageIds) == 3):
					if(timestamp < stage_timings[1]):
						stage_id = stageIds[0]
					if(timestamp >= stage_timings[1] and timestamp < stage_timings[2]):
						stage_id = stageIds[1]
					if(timestamp >= stage_timings[2]):
						stage_id = stageIds[2]
				elif(timestamp == 2):
					if(timestamp < stage_timings[1]):
						stage_id = stageIds[0]
					if(timestamp >= stage_timings[1]):
						stage_id = stageIds[1]
				else:
					stage_id = stageIds[0]

				chat_obj = {'stageId':stage_id, 'roundId': round_id, 'gameId':game_id, 'message': text, 'speaker_nickname': speaker, 'timestamp': timestamp}

				# Write the row
				writer.writerow(chat_obj)

"""
Get DV's so that they can be merged back.
"""
def get_dvs(multi_task_data):
	multi_task_dv = multi_task_data[["_id", "stageIds", "task", "complexity", "playerCount", "score", "speed", "efficiency", "raw_duration_min", "default_duration_min"]]
	multi_task_dv = multi_task_dv.rename(columns = {"stageIds": "stageId", "_id": "roundId"})
	# account for cases where the scores was submitted multiple times per stage (take thte max)
	return(multi_task_dv.groupby('stageId').apply(lambda x: x.loc[x['score'].idxmax()]).reset_index(drop=True))

"""
Add the DV back to the data.
- When parsing out each conversation, we lost all the DV-related variables (score, efficiency, etc.)
- Therefore, we need to add them back.
"""
def add_dv_back_to_conversation(conversations_raw, multi_task_dv):
	conversations_with_dv = conversations_raw.merge(multi_task_dv, on=["roundId", "stageId"], how="left")
	return(conversations_with_dv)

"""
User composition information
- Add in the features of the members of the group.
"""
def compile_user_information(user_info):
	user_info.loc[:, 'country'] = (user_info['country'] == 'United States').astype(int)
	user_info['country'] = user_info['country'].astype(int)

	education_order = [
		'Less than a high school diploma',
		'High school diploma',
		'Some college or vocational training',
		'2-year college degree',
		'4-year college degree',
		'Post-college degree'
	]
	user_info['education_level'] = pd.Categorical(user_info['education_level'], categories=education_order, ordered=True)
	user_info['education_level'] = user_info['education_level'].cat.codes
	gender_order = [
		'Male',
		'Female',
		'Other'
	]
	user_info['gender'] = pd.Categorical(user_info['gender'], categories=gender_order, ordered=True)
	user_info['gender'] = user_info['gender'].cat.codes
	maritalstatus_order = [
		'Single Never Married',
		'Married or Domestic Partnership',
		'Divorced',
		'Widowed',
		'Separated'
	]
	user_info['marital_status'] = pd.Categorical(user_info['marital_status'], categories=maritalstatus_order, ordered=True)
	user_info['marital_status'] = user_info['marital_status'].cat.codes
	politicalparty_order = [
		'Democrat',
		'Independent',
		'Republican',
		'Other Party',
		'Neutral'
	]
	user_info['political_party'] = pd.Categorical(user_info['political_party'], categories=politicalparty_order, ordered=True)
	user_info['political_party'] = user_info['political_party'].cat.codes

	user_info.loc[:, 'race'] = (user_info['race'] == 'White').astype(int)
	user_info['race'] = user_info['race'].astype(int)

	user_features_numeric = ['birth_year', 'CRT', 'income_max', 'income_min', 'IRCS_GS', 'IRCS_GV', 'IRCS_IB', 'IRCS_IR', 'IRCS_IV', 'IRCS_RS', 'political_fiscal', 'political_social', 'RME', 'country', 'education_level', 'gender', 'marital_status', 'political_party', 'race']

	# Group by 'ID' and calculate mean and std for all numeric columns
	composition_by_stageId = user_info.groupby('stageId')[user_features_numeric].agg(["mean", "std"])

	# Flatten the multi-level column index
	composition_by_stageId.columns = ['_'.join(col).strip() for col in composition_by_stageId.columns.values]

	return(composition_by_stageId)

"""
	Save the final output(s)
	-  Standard Multi-Task Analysis (by StageId) multi_task_conversations_with_dv_and_composition.csv
	-  Round-based analysis (DV is last stage in group of 3): multi_task_conversations_with_dv_and_composition_dv_last_by_stage
	-  Round-based analysis (DV is the mean across all 3 stages): multi_task_conversations_with_dv_and_composition_dv_mean_by_stage

From previous preprocessing steps:
@param conversations_raw
@param multi_task_dv
@param composition_by_stageId
@param conversations_with_dv_and_composition

User-provided parameters:
@param output_path: the desired output path of the CSV.
@param conversation_id: one of {"stageId", "roundId"}, depending on the data grouping desired by the user.
@param dv_id: also one of {"stageId", "roundId"}. Controls what level we are examining for the dependent variable.
@param use_mean_for_roundId: use the mean across three stages when grouping by roundId. Defaults to false (and uses the performance for the FINAL task in a round.)
@param tiny: filter to just the first 3 games. (for testing!) Defaults to false.
"""
def save_final_result(conversations_raw, multi_task_dv, composition_by_stageId, conversations_with_dv_and_composition, output_path, conversation_id, dv_id, use_mean_for_roundId = False, tiny = False):
	assert conversation_id in {"stageId", "roundId"}, "The parameter `conversation_id must be one of `stageId` or `roundId`!"
	assert dv_id in {"stageId", "roundId"}, "The parameter `dv_id must be one of `stageId` or `roundId`!"

	# 2 ways of getting roundId-based dependent variables
	if(use_mean_for_roundId):
		# Gets the dependent variables averaged by round
		dv_mean =  multi_task_dv.groupby("roundId").agg({
			"score": np.mean,
			"speed": np.mean,
			"efficiency": np.mean,
			"raw_duration_min": np.mean,
			"default_duration_min": np.mean,
			"task": "last",
			"playerCount": "last"
		}).reset_index()
	if(not use_mean_for_roundId):
		last_roundId_dv = conversations_raw.sort_values(by=['timestamp', 'roundId']).merge(multi_task_dv, on = ['stageId', 'roundId'], how = 'left')[["roundId", "score", "speed", "efficiency", "raw_duration_min", "default_duration_min", "task", "complexity", "playerCount", "stageId"]].groupby('roundId').tail(1)
		assert len(last_roundId_dv["roundId"].value_counts()) == len(last_roundId_dv["stageId"].value_counts()), "There should be exactly 1 stageId per roundId."

	# Conversation ID = StageID and DV ID = StageId
	if conversation_id == "stageId" and dv_id == "stageId":
		output = conversations_with_dv_and_composition
	# Conversation ID = StageID and DV ID = RoundId
	elif conversation_id == "stageId" and dv_id == "roundId":
		if(use_mean_for_roundId):
			merged_df = conversations_raw.merge(dv_mean, on = "stageId", how = "right")
			assert merged_df["roundId_x"].equals(merged_df["roundId_y"]), "Both merged roundIds need to be the same."
			assert len(merged_df["roundId_y"].value_counts()) == len(merged_df["stageId"].value_counts()), "There should be exactly 1 stageId per roundId."
			merged_df = merged_df.drop("roundId_y", axis=1).rename(columns={"roundId_x": "roundId"})
			output = merged_df.merge(composition_by_stageId, on = "stageId", how = "left")
			output = output.sort_values("gameId")
		else:
			merged_df = conversations_raw.merge(last_roundId_dv, on = "stageId", how = "right")
			assert merged_df["roundId_x"].equals(merged_df["roundId_y"]), "Both merged roundIds need to be the same."
			assert len(merged_df["roundId_y"].value_counts()) == len(merged_df["stageId"].value_counts()), "There should be exactly 1 stageId per roundId."
			merged_df = merged_df.drop("roundId_y", axis=1).rename(columns={"roundId_x": "roundId"})
			output = merged_df.merge(composition_by_stageId, on = "stageId", how = "left")
			output = output.sort_values("gameId")

	# Conversation ID = RoundId and DV ID = RoundId
	elif conversation_id == "roundId" and dv_id == "roundId":
		if(use_mean_for_roundId):
			output = conversations_raw.merge(dv_mean, on = "roundId", how = "left").merge(composition_by_stageId, on = "stageId", how = "left")
		else:
			output = conversations_raw.merge(last_roundId_dv, on = "roundId", how = "left").merge(composition_by_stageId, on = "stageId", how = "left")
	else:
		print("Sorry, Conversation ID = roundId and DV ID = stageId is currently not supported.")

	if(tiny):
		output.groupby('gameId').head(3).to_csv(output_path, index=False)
	else:
		output.to_csv(output_path, index=False)
		
"""
Cleans the multi-task dataset according to the user's provided specifications.
---
User-provided parameters:

Input paths:
--- Note that these files can be generated from the script saved in `data_cleaning_dev/MultiTask_Input_Generator_for_Horserace.R`.
They have to be run in the "Analysis" folder of the `multi-task data`.
@param raw_round_data_path: The path to the round-based data.
@param raw_stage_data_path: The path to the stage-based data.
@param raw_user_data_path: The path to the user-based data (with information merged in from the full panel)
Output path:
@param output_path: the desired output path of the CSV.
Additional specifications:
@param conversation_id: one of {"stageId", "roundId"}, depending on the data grouping desired by the user.
@param dv_id: also one of {"stageId", "roundId"}. Controls what level we are examining for the dependent variable.
@param use_mean_for_roundId: use the mean across three stages when grouping by roundId. Defaults to false (and uses the performance for the FINAL task in a round.)
@param tiny: filter to just the first 3 games. (for testing!) Defaults to false.
"""
def clean_multi_task_data(raw_round_data_path, raw_stage_data_path, raw_user_data_path, output_path, conversation_id, dv_id = None, use_mean_for_roundId = False, tiny = False):
	
	if(dv_id is None):
		dv_id = conversation_id

	current_script_directory = Path(__file__).resolve().parent
	multi_task_data = pd.read_csv(current_script_directory.parent / raw_round_data_path)
	multi_task_stages = pd.read_csv(current_script_directory.parent / raw_stage_data_path, low_memory=False)
	user_info = pd.read_csv(current_script_directory.parent / raw_user_data_path).rename(columns = {"stageIds": "stageId"})

	# Add assertions for the expected / required values in each of the input datasets, to ensure the format is correct
	assert {"_id", "gameId", "data.A"}<=set(multi_task_data.columns), "Error: Colnames for multi-task round data are incorrect!"
	assert {"_id", "startTimeAt"}<=set(multi_task_stages.columns), "Error: Colnames for multi-task stage data are incorrect!"
	assert {'stageId', 'birth_year', 'CRT', 'income_max', 'income_min', 'IRCS_GS', 'IRCS_GV', 'IRCS_IB', 'IRCS_IR', 'IRCS_IV', 'IRCS_RS', 'political_fiscal', 'political_social', 'RME', 'country', 'education_level', 'gender', 'marital_status', 'political_party', 'race'}<=set(user_info.columns), "Error: Colnames for multi-task user data are incorrect!"

	muti_task_chat = multi_task_data[["_id", "gameId", "data.A"]].drop_duplicates()

	game_id_lookup = build_game_id_dict(multi_task_data, multi_task_stages)
	process_data_into_conversations(muti_task_chat, game_id_lookup)

	# This was the result of `process_data_into_conversations`; it's a temporary CSV that is not used.
	conversations_raw = pd.read_csv(current_script_directory.parent / 'raw_data/multi_task_conversations.csv').sort_values(['gameId', 'roundId', 'stageId','timestamp'])

	# Add DV's back
	multi_task_dv = get_dvs(multi_task_data)
	conversations_with_dv = add_dv_back_to_conversation(conversations_raw, multi_task_dv)

	# Add in user information
	composition_by_stageId = compile_user_information(user_info)

	# This dataframe aggregates all the composition-level features
	conversations_with_dv_and_composition = conversations_with_dv.merge(composition_by_stageId, on = "stageId", how = "left")

	# fill NA messages and speakers if necessary
	conversations_with_dv_and_composition["message"] = conversations_with_dv_and_composition["message"].fillna("NULL_MESSAGE")
	conversations_with_dv_and_composition["speaker_nickname"] = conversations_with_dv_and_composition["speaker_nickname"].fillna("NULL_SPEAKER")

	save_final_result(conversations_raw, multi_task_dv, composition_by_stageId, conversations_with_dv_and_composition, output_path, conversation_id, dv_id, use_mean_for_roundId, tiny)
