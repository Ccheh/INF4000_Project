INF4000 Project: Exploring Musical Feature Trends and Popularity Levels
Description
This repository contains the coursework project for the INF4000 Data Visualisation module. The project focuses on analyzing musical features to understand their influence on song popularity, using visualizations to provide insights into feature combinations, genre-specific trends, and patterns of success.

Dataset
The dataset used for this project is sourced from the Spotify Tracks Dataset. This dataset includes a variety of audio features and metadata for Spotify tracks, enabling a detailed exploration of track characteristics and their relation to popularity.

Key Features
track_id: Unique Spotify ID for the track.
artists: Artists who performed the track.
album_name: Album name of the track.
track_name: Name of the track.
popularity: Value (0-100) indicating the track's popularity.
duration_ms: Length of the track in milliseconds.
explicit: Whether the track has explicit content.
danceability: Suitability of the track for dancing (0.0 to 1.0).
energy: Intensity and activity of the track (0.0 to 1.0).
key: Musical key of the track.
loudness: Overall loudness in decibels.
mode: Modality (1 for major, 0 for minor).
speechiness: Presence of spoken words in the track.
acousticness: Confidence measure of whether the track is acoustic.
instrumentalness: Likelihood of the track containing no vocals.
liveness: Presence of an audience in the recording.
valence: Positiveness conveyed by the track (0.0 to 1.0).
tempo: Estimated tempo of the track (BPM).
time_signature: Estimated meter (e.g., 3/4, 4/4).
track_genre: Genre classification of the track.
Features
Composite Visualization: Integration of multiple charts for exploring musical feature trends across popularity levels.
Data Cleaning: Handling missing values, removing duplicates, and standardizing variables.
Visual Analysis:
Feature combinations and their role in popular songs.
Genre-specific feature analysis.
Synergies among key musical features.
Feature balance across different popularity levels.
Key Findings
Feature Synergies:

Popular songs often combine high energy, danceability, and moderate valence.
Genre-specific traits, such as low loudness in classical music, play a role in defining popularity.
Genre-Specific Insights:

Pop songs exhibit high danceability and medium valence, aligning with their emotional and rhythmic appeal.
Classical tracks emphasize acousticness and soft dynamics.
Balance in Features:

Successful songs maintain a balance across features like valence and loudness.
Tracks in the top 10% of popularity exhibit consistent patterns in feature combinations.
Visualizations
The following visualizations were developed:

Feature Combination Patterns: Key combinations in the top 10% songs.
Genre Feature Patterns: Distribution of audio features by genre.
Feature Synergies: Pairings of musical features influencing popularity.
Feature Balance Radar Chart: Comparison of feature distributions across popularity tiers.
File Structure
INF4000.R: Main R script for data visualization.
dataset.csv: Dataset used for visualization.
README.md: Documentation for the project.
charts/: Contains images of visualizations generated for the project.


Installation
Clone the repository to your local machine:

bash
复制代码
git clone https://github.com/Ccheh/INF4000_Project.git
