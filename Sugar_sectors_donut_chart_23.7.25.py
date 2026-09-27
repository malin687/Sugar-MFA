# -*- coding: utf-8 -*-
"""
Generate sector-level donut charts for the global sugar MFA.

This script creates the sugarcane and sugar beet sector-allocation
visualizations used in the analysis accompanying:

Nisnik et al. (2026)
"Global flows, losses, and circularity of sugar from cultivation to end users"
Resources, Conservation & Recycling.
https://doi.org/10.1016/j.resconrec.2026.109000

"""
import pandas as pd
import matplotlib.pyplot as plt

# === Load sugarcane Excel ===
file_path = "Donut_Chart_sectors_data- sugarcane - 23.7.25.xlsx"
df = pd.read_excel(file_path)

# === Setup orientation and sector order ===
df['side'] = df['Category'].apply(lambda x: 'right' if x == 'Loss' else 'left')
loss_order = ['Evaporation', 'Substance']
utilized_order = ['Non-food', 'Feed', 'Food industry', 'Energy', 'Agriculture']
combined_order = loss_order + utilized_order
df['Allocation'] = pd.Categorical(df['Allocation'], categories=combined_order, ordered=True)
df_sorted = df.sort_values(by=['side', 'Allocation'])

# === Manual crop order ===
manual_crop_order = [
    # Energy
    'Molasses', 'Sucrose', 'Bagasse',
    # Agriculture
    'Filter mud', 'Filter cake', 'Straw',
    # Food industry
    'Molasses', 'Sucrose',
    # Substance
    'Stalk','Molasses','Bagasse'
]
crop_order_dict = {name: i for i, name in enumerate(manual_crop_order)}
df_sorted['crop_order'] = df_sorted['part of the crop'].map(crop_order_dict).fillna(999).astype(int)

# === Donut layers ===
inner_sorted = df_sorted.groupby('Category')['mass_Gt'].sum().reset_index()
inner_labels = inner_sorted['Category']
inner_sizes = inner_sorted['mass_Gt']
inner_colors = ['#00b48a' if cat == 'Utilized' else '#333333' for cat in inner_labels]

middle_sorted = df_sorted[['part of the crop', 'Allocation', 'mass_Gt']].groupby(['Allocation', 'part of the crop'])['mass_Gt'].sum().reset_index()
middle_sorted['Allocation'] = pd.Categorical(middle_sorted['Allocation'], categories=combined_order, ordered=True)
middle_sorted['crop_order'] = middle_sorted['part of the crop'].map(crop_order_dict).fillna(999).astype(int)
middle_sorted = middle_sorted.sort_values(by=['Allocation', 'crop_order'])
middle_labels = middle_sorted['part of the crop']
middle_sizes = middle_sorted['mass_Gt']
middle_colors = ['#e0e0e0', '#cccccc', '#bdbdbd', '#9e9e9e', '#757575'] * 3
middle_colors = middle_colors[:len(middle_labels)]

outer_sorted = df_sorted.groupby(['Category', 'Allocation'])['mass_Gt'].sum().reset_index()
outer_labels = outer_sorted['Allocation']
outer_sizes = outer_sorted['mass_Gt']
outer_colors = ['#c8e6c9', '#a5d6a7', '#81c784', '#66bb6a', '#388e3c'] * 2
outer_colors = outer_colors[:len(outer_labels)]

# === Plot and save ===
fig, ax = plt.subplots(figsize=(8, 8))
start_angle = 270

ax.pie(outer_sizes, radius=1, labels=outer_labels, colors=outer_colors,
       wedgeprops=dict(width=0.3, edgecolor='white'), startangle=start_angle)
ax.pie(middle_sizes, radius=0.7, labels=middle_labels, colors=middle_colors,
       wedgeprops=dict(width=0.3, edgecolor='white'), startangle=start_angle)
ax.pie(inner_sizes, radius=0.4, labels=inner_labels, colors=inner_colors,
       wedgeprops=dict(width=0.3, edgecolor='white'), startangle=start_angle)

plt.title("Sugarcane Harvest – Manual Crop Order by Sector")
plt.savefig("sugarcane_donut_chart.pdf", format='pdf', bbox_inches='tight')
plt.show()




# === Load sugar beet Excel ===
file_path = "Donut_Chart_sectors_data- sugar_beet- 23.7.25.xlsx"
df = pd.read_excel(file_path)

# === Orientation and sector order ===
df['side'] = df['Category'].apply(lambda x: 'right' if x == 'Loss' else 'left')
loss_order = ['Evaporation', 'Water', 'Substance']
utilized_order = ['Non-food', 'Energy', 'Feed', 'Food industry', 'Agriculture']
combined_order = loss_order + utilized_order
df['Allocation'] = pd.Categorical(df['Allocation'], categories=combined_order, ordered=True)
df_sorted = df.sort_values(by=['side', 'Allocation'])

# === Manual crop order ===
manual_crop_order = [
    # Feed
    'Beet palp', 'Molasses',
    # Agriculture
    'Lime cake', 'Leaves',
    # Food industry
    'Molasses', 'Sucrose',
    # Substance
    'taproop','Sugar','Molasses'
]
crop_order_dict = {name: i for i, name in enumerate(manual_crop_order)}
df_sorted['crop_order'] = df_sorted['crop'].map(crop_order_dict).fillna(999).astype(int)

# === Donut layers ===
inner_sorted = df_sorted.groupby('Category')['mass_Gt'].sum().reset_index()
inner_labels = inner_sorted['Category']
inner_sizes = inner_sorted['mass_Gt']
inner_colors = ['#e91e63' if cat == 'Utilized' else '#4d4d4d' for cat in inner_sorted['Category']]

middle_sorted = df_sorted[['crop', 'Allocation', 'mass_Gt']].groupby(['Allocation', 'crop'])['mass_Gt'].sum().reset_index()
middle_sorted['Allocation'] = pd.Categorical(middle_sorted['Allocation'], categories=combined_order, ordered=True)
middle_sorted['crop_order'] = middle_sorted['crop'].map(crop_order_dict).fillna(999).astype(int)
middle_sorted = middle_sorted.sort_values(by=['Allocation', 'crop_order'])
middle_labels = middle_sorted['crop']
middle_sizes = middle_sorted['mass_Gt']
middle_colors = ['#f3e5f5', '#e1bee7', '#ce93d8', '#ba68c8', '#ab47bc'] * 3
middle_colors = middle_colors[:len(middle_labels)]

outer_sorted = df_sorted.groupby(['Category', 'Allocation'])['mass_Gt'].sum().reset_index()
outer_labels = outer_sorted['Allocation']
outer_sizes = outer_sorted['mass_Gt']
outer_colors = ['#f8bbd0', '#f48fb1', '#f06292', '#ec407a', '#e91e63'] * 2
outer_colors = outer_colors[:len(outer_labels)]

# === Plot and save ===
fig, ax = plt.subplots(figsize=(8, 8))
start_angle = 270

ax.pie(outer_sizes, radius=1, labels=outer_labels, colors=outer_colors,
       wedgeprops=dict(width=0.3, edgecolor='white'), startangle=start_angle)
ax.pie(middle_sizes, radius=0.7, labels=middle_labels, colors=middle_colors,
       wedgeprops=dict(width=0.3, edgecolor='white'), startangle=start_angle)
ax.pie(inner_sizes, radius=0.4, labels=inner_labels, colors=inner_colors,
       wedgeprops=dict(width=0.3, edgecolor='white'), startangle=start_angle)

plt.title("Sugar Beet Harvest – Manual Crop Order by Sector")
plt.savefig("sugar_beet_donut_chart.pdf", format='pdf', bbox_inches='tight')
plt.show()
