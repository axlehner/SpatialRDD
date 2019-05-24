
######################
######################

OPTRDD NOTE:

We here distribute only the part of the replication files of Keele and Titiunik
required to run our experiments. For all other files, contact the original authors.

######################
######################

------------------------------------------------------------------------------
June 6, 2014

This folder contains the replication files for the paper "Geographic Boundaries as Regression Discontinuities" to appear in Political Analysis

-------------------------------------------------------------------------------

Authors contact information			


Luke Keele				Rocio Titiunik
Associate Professor			Assistant Professor
Political Science			Political Science
Penn State University			University of Michigan
ljk20@psu.edu				titiunik@umich.edu

--------------------------------------------------------------------------------

File list

README.txt							This file

./Analysis:							Directory containing all codes to perform estimation ==> NOTE: these analysis require function distance-functions.R described below
Balance								Directory containing all codes to perform balance tests
Local Poly							Directory containing all codes to perform local polynomial estimation 
Matching							Directory containing all codes to perform matching analysis

./Analysis/Balance:
Housing								Analysis for house-level outcomes
Voters								Analysis for voter-level outcomes

./Analysis/Balance/Housing:
01-balance-pre-matching.R					Performs balance before matching
03-balance-buffers.R						Performs balance in buffers
04-balance-chordmatch.R						Performs balance after matching on chordal distance
05-balance-chordmatch-buffers.R					Performs balance after matching on chordal distance within buffers
House-Summary.txt						Summary of results
Results								Directory were all results are stored

./Analysis/Balance/Housing/Results:
balance_postmatch_buffers.RData
house_balance_buffers.RData
house_balance_postmatch_fulldata.RData
prematch_balance_house.RData

./Analysis/Balance/Voters:
01-balance-pre-matching.R					Performs balance before matching
02-balance-buffers.R						Performs balance in buffers	
03-balance-chordmatch.R						Performs balance after matching on chordal distance
04-balance-chordmatch-buffers.R					Performs balance after matching on chordal distance within buffers
Balance - Tests - Summary.txt					Summary of results
Results	  	  						Directory were all results are stored

./Analysis/Balance/Voters/Results:
balance_postmatch_buffers.RData
balance_postmatch_fulldata.RData
prematch_balance_fulldata.RData
voters_balance_buffers.RData

./Analysis/Local Poly:
01-localpoly-estimation-housing-outcomes.R			Local polynomial estimation for housing outcomes
01-localpoly-estimation-non-housing-outcomes.R			Local polynomial estimation for non-housing outcomes
distance-functions.R						Functions to calculate different geographic distances

./Analysis/Matching:
Housing
Voters

./Analysis/Matching/Housing:
01-chordmatch.R							Performs matching on chordal distance
02-chordmatch-buffers.R						Performs matching on chordal distance within buffers 
chordmatch_bufferdata.RData					Results from matching on chordal distance within buffers 
chordmatch_fulldata.RData					Results from matching on chordal distance

./Analysis/Matching/Voters:
01-chordmatch.R							Performs matching on chordal distance
03-chordmatch-buffers.R						Performs matching on chordal distance within buffers 							 
Results								Directory were matching results are stored

./Analysis/Matching/Voters/Results:
chordmatch_bufferdata.RData
chordmatch_fulldata.RData

./Data:								Datasets used for analysis 
BorderSegmentPoints_Project.dbf					Points along the border for Philadelphia vs New York media market application
Housing								Directory containing housing data
Voters								Directory containing voter data

./Data/Housing:
House_Buffer_Final.dta						Housing data for buffers around the boundary for Philadelphia vs New York media market application
NJ_House_Final.dta						Complete housing data around the boundary for Philadelphia vs New York media market application

./Data/Voters:
Voters_Buffer_Final.dta						Voter data for buffers around the boundary for Philadelphia vs New York media market application
Voters_Final.dta						Complete voter data for Philadelphia vs New York media market application
