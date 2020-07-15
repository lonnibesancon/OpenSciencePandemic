'''
Python script to fetch altmetric data based on a specific dataset
Takes two (optional) arguments: 
	- The input file containing the datasets (with a field 'DOI', 'doi', or 'id' [for arxvi])
	- The output file (preferably a tsv file, the current delimiter being a tabulation)
Authors: Lonni Besançon
Date: 08/07/2020
License: CC-BY
'''

import requests
import sys
from CustomProgressBar import printProgressBar
import csv


def getDataBasedOnFile(fileInput = ''):
	global header 
	requests_start = ''

	with open(fileInput, newline='', encoding='ISO-8859-1') as csvfile:
		spamreader = csv.reader(csvfile, delimiter=',', quotechar='"')
		header = next(spamreader)

		#print(header)
		doi_index = 0
		is_arxiv = False # We need this boolean to later do some operations on the arxiv ID
		for x in header:
			x = x.strip('"')
			if(x == 'DOI' or x == 'doi'):
				requests_start = 'https://api.altmetric.com/v1/doi/'
				break ;
			elif (x == 'id'):
				is_arxiv = True
				requests_start = 'https://api.altmetric.com/v1/arxiv/'
				break ;
			else:
				doi_index +=1
	

	
		# small progress bar code
		rows = list(spamreader)
		nb_papers_to_process = len(rows)
		i = 0
		printProgressBar(i, nb_papers_to_process, prefix='Progress:', suffix='Complete', decimals=1, length=50)
		for row in rows:

			doi = row[doi_index]
			#In the case of arxiv we need to remove the version of the preprint to fetch details from altmetric
			if(is_arxiv):
				doi = doi.split('v', 1)[0]
				#print("doi for request = "+doi)

			request = requests_start+doi
			tmp = requests.get(str(request))
			#print(request)

			nb_shares = -1
			nb_tweets = -1
			nb_facebooks = -1
			nb_reddits = -1
			nb_blogs = -1
			nb_news = -1
			nb_wikipedia = -1
			nb_researchhighlights = -1
			nb_videos = -1
			nb_policies = -1 
			nb_pinterests = -1
			nb_weibos = -1
			nb_reviews = -1
			
			if(tmp.status_code == 404):
				
				title = "Not Found In altmetric"
				currentscore = -1
				url = ""

			else:
				results = tmp.json()
				doi = x
				history = results['history']
				currentscore = history['at']
				url = results['details_url']
				if 'cited_by_posts_count' in results:
					nb_shares = results['cited_by_posts_count']
				if 'cited_by_tweeters_count' in results:
					nb_tweets = results['cited_by_tweeters_count']
				if 'cited_by_fbwalls_count' in results:
					nb_facebooks = results['cited_by_fbwalls_count']
				if 'cited_by_rdts_count' in results:
					nb_reddits = results['cited_by_rdts_count']
				if 'cited_by_feeds_count' in results:
					nb_blogs = results['cited_by_feeds_count']
				if 'cited_by_msm_count' in results:
					nb_news = results['cited_by_msm_count']
				if 'cited_by_wikipedia_count' in results:
					nb_wikipedia = results['cited_by_wikipedia_count']
				if 'cited_by_rh_count' in results:
					nb_researchhighlights = results['cited_by_rh_count']
				if 'cited_by_videos_count' in results:
					nb_videos = results['cited_by_videos_count']
				if 'cited_by_policies_count' in results:
					nb_policies = results['cited_by_policies_count']
				if 'cited_by_pinners_count' in results:
					nb_pinterests = results['cited_by_pinners_count']
				if 'cited_by_weibo_count' in results:
					nb_weibos = results['cited_by_weibo_count']
				if 'cited_by_peer_review_sites_count' in results:
					nb_reviews = results['cited_by_peer_review_sites_count']
				
			i+=1

			#Remove all carriage returns from already existing strings for easy visualization purposes
			for j, s in enumerate(row):
				row[j] = s.replace('\r','')
				row[j] = row[j].replace('\n','')
			data.append(row+[currentscore,url,nb_shares,nb_tweets,nb_facebooks,nb_reddits,nb_blogs,nb_news,nb_wikipedia,nb_researchhighlights,nb_videos,nb_policies,nb_pinterests,nb_weibos,nb_reviews])
			printProgressBar(i, nb_papers_to_process, prefix='Progress:', suffix='Complete', decimals=1, length=50)
		

def writeData(fileOutput = ''):
	f=open(fileOutput, "w+")
	initial_headers = delimiter.join(header)
	f.write(initial_headers+delimiter+'Score'+delimiter+'PageURL'+delimiter+'TotalShares'+delimiter+'TweeterShares'+delimiter+'FacebookShares'+delimiter+'RedditShares'+delimiter+'BlogShares'+delimiter+'NewsShare'+delimiter+'WikipediaShares'+delimiter+'ResearchHighlightShares'+delimiter+'VideoShares'+delimiter+'PoliciesShare'+delimiter+'PinterestShares'+delimiter+'WeiboShares'+delimiter+'PeerReviewShares\n')

	for line in data:
		to_write = delimiter.join(str(elem) for elem in line)
		f.write(to_write+'\n')
		#f.write(str(x[0])+delimiter+str(x[1])+delimiter+str(x[2])+delimiter+str(x[3])+delimiter+str(x[4])+delimiter+str(x[5])+delimiter+str(x[6])+delimiter+str(x[7])+delimiter+str(x[8])+delimiter+str(x[9])+delimiter+str(x[10])+delimiter+str(x[11])+delimiter+str(x[12])+delimiter+str(x[13])+delimiter+str(x[14])+delimiter+str(x[15])+delimiter+str(x[16])+"\n")

	f.close()


def main():
	if(len(sys.argv) == 1):
		print("No input or output file specified.")
	elif(len(sys.argv) == 2):
		print("Input file used = "+sys.argv[1])
		print("No output file specified")
	else:
		print("Input file used = "+sys.argv[1])
		print("Output file used = "+sys.argv[2])
		getDataBasedOnFile(sys.argv[1]);
		writeData(sys.argv[2]);

delimiter = '\t'	#cannot use "," or ";" because papers use them in their titles.
header = ''
data = []
main()


