package moa.clusterers.outliers.AnyOut;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.Reader;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import mypackage.FlagRecord;
import mypackage.TPRecord;

import com.github.javacliparser.FileOption;
import com.yahoo.labs.samoa.instances.Instance;

import moa.clusterers.outliers.MyBaseOutlierDetector.Outlier;
import moa.streams.clustering.SimpleCSVStream;

import com.opencsv.CSVReader;

public class ERAIDSAnyOut {
	static Long tmStart;
	static FileWriter fw;
    static BufferedWriter bw;
    static PrintWriter writer;
    static List<FlagRecord> subFlagAlertList;
	// variables depend on each community
	static int nbrSessions = 3096; 

	static Reader reader;
	static List<String[]> rows;
	
	public static void tuneParameters(String csvFile, int trainSetSize, int oScoreAggr, int confAggr, int conf, double threshold, int windowSize)
	{
		//if (true) return;
		
        int numInstances = nbrSessions;
        SimpleCSVStream stream = new SimpleCSVStream();
        stream.csvFileOption = new FileOption("csvFile", 'f',
    			"CSV file to load.", csvFile, "csv", false);

        stream.prepareForUse();
       
        AnyOut myOutlierDetector= new AnyOut();
        myOutlierDetector.windowSizeOption.setValue(windowSize); //the only one to control till now
     
      	myOutlierDetector.getOptions().getOption("TrainingSetSize").setValueViaCLIString(trainSetSize +"");
        myOutlierDetector.getOptions().getOption("OScorek").setValueViaCLIString(oScoreAggr +"");
        myOutlierDetector.getOptions().getOption("Confidencek").setValueViaCLIString(confAggr +"");
        myOutlierDetector.getOptions().getOption("confidence").setValueViaCLIString(conf +"");
        myOutlierDetector.getOptions().getOption("Threshold").setValueViaCLIString(threshold +"");
        
        myOutlierDetector.setModelContext(stream.getHeader());
        myOutlierDetector.prepareForUse(); 
        
        tmStart = System.currentTimeMillis();
     
        int numberSamples = 0; //number of samples processed  
        //int w = myOutlierDetector.windowSizeOption.getValue();
        
        int TP,FP;
        int flagAlert;	//flagAlert is assigned 1 if an outlier remains an outlier for a temporalFactor 
        				//i.e. the outlier that survived for a temporal factor is reported
        int tempFactor, vouchFactor=2;	//-----------------------------------
        List<TPRecord> TPTemporalList = new ArrayList<TPRecord>();
        
        int foundFlag;
        int nbrWindows= (int)nbrSessions/windowSize;
        subFlagAlertList = new ArrayList<FlagRecord>();
        int windowIter=0; //maps to windowIter=1 on chart
        
        
        while (stream.hasMoreInstances() && (numberSamples < numInstances)) 
        {
            Instance newInst = stream.nextInstance().getData();
            myOutlierDetector.processNewInstanceImpl(newInst);            
            numberSamples++;
            if (numberSamples % 100 == 0) {
                //System.out.println("Processed " + numberSamples + " stream objects.");  
            }
            TP=0;FP=0;
            flagAlert=0;
            
            
            //if ((numberSamples % (windowSize / 2)) == 0) 
            if ((numberSamples % windowSize ) == 0) 
            {
                //myOutlierDetector.PrintOutliers();
   
            	subFlagAlertList.add(new FlagRecord(windowIter,0));
            			
            	//writer.println("Processed Samples:"+numberSamples);
            	Set<Outlier> outliersFoundCurrent = myOutlierDetector.GetOutliersFound();
            	//writer.print("Current Outliers: ");
                for (Outlier o : outliersFoundCurrent) 
                {
	            	 //writer.print("["+o.id+"] ");
	            	 
	            	 //if(o.id < lowerBound || o.id > upperBound)
                	 Integer rowIndex = (int) (long) o.id;
      
                	 if(rows.get(rowIndex).equals("Normal") == false)
	            	 {
	            		 TP++;
	            		 foundFlag=0;
	            		 for(int rowIter=0;rowIter<TPTemporalList.size();rowIter++)
	            		 {
	            			 if(TPTemporalList.get(rowIter).getOutlierId()==o.id)
	            			 {
	            				 foundFlag=1;
	            				 tempFactor = TPTemporalList.get(rowIter).getTemporalFactor();	
	            				 TPTemporalList.get(rowIter).setTemporalFactor(tempFactor+1);//increments the temporalFactor
	            			 }
	            		 }
	            		 if(foundFlag==0)
	            		 {
	            			 //writer.println((windowIter*window));	
	            			 //window starts at (windowIter*window) and ends at numberSamples
	            			 TPTemporalList.add(new TPRecord((int)(long) o.id, 1,windowIter));
	            		 }
	            		 //System.out.println(TPTemporalList.size());
	            		 
	            	 }
	            	 else
	            		 FP++;
	            }
                //writer.println();
	            //writer.println("TP="+TP+" FP="+FP);
	            
                //after having all true outliers in this windowIter
				for(int rowIter=0;rowIter<TPTemporalList.size();rowIter++)
				{
					if(TPTemporalList.get(rowIter).getTemporalFactor()==vouchFactor)
					{
						// this means TP survived as TP for a number of windows (vouchFactor)
					
						flagAlert  = 1;
						//writer.println("FlagAlert="+flagAlert+ " An outlier has been confirmed as a TP under a Vouch Factor="+vouchFactor);
						
						if(subFlagAlertList.get(windowIter).getFlag()==0)
						{
							subFlagAlertList.get(windowIter).setFlag(1);
							subFlagAlertList.get(windowIter).appendOutlierSet(outliersFoundCurrent);
						 	//writer.println("FlagAlert="+SubFlagAlertList[windowIter][1]);
						}
						TPTemporalList.remove(rowIter);
					
					}
					else if(windowIter-TPTemporalList.get(rowIter).getIteration()==vouchFactor)	
						 // this means TP turned into an inlier
					{
						TPTemporalList.remove(rowIter);
					}
					
				}
	            
	            windowIter++;
            }
        }   
        //System.out.println("Total time = " + (System.currentTimeMillis() - tmStart) + " ms");
	}
	
	@SuppressWarnings("resource")
	public static void main(String[] args) throws Exception
	{
		String consoleFile="C:/Users/ID916780/Documents/Eclipse/workspace/MOAOutlierDetect/src/experiments/ProductionLineWorker/results/AnyOut/myConsole.txt";
	    fw = new FileWriter(consoleFile, true);
	    bw = new BufferedWriter(fw);
	    
	    writer = new PrintWriter(bw);
	    writer.println("test");
		File filesPath = new File("C:/Users/ID916780/Documents/Eclipse/workspace/MOAOutlierDetect/src/experiments/ProductionLineWorker/subspaces");
		String[] fileNamesList = filesPath.list();
		
		int voteFactor=1; //vote factor controls whether a flag alarm should be generated as a total vote of the ensemble.
		
        //read from threat file
		
		try {
			reader = new FileReader("C:/Users/ID916780/Documents/Eclipse/workspace/MOAOutlierDetect/src/experiments/ProductionLineWorker/threatLabels/threatLabel.csv");
			rows = new CSVReader(reader).readAll();
			
		    //reader.close();
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			//e.printStackTrace();
			System.out.println("tuneParameters: File not found exception.");
		} catch (IOException e) {
			// TODO Auto-generated catch block
			//e.printStackTrace();
			System.out.println("tuneParameters: IO exception.");
		}

		List<String> threatLabels = new ArrayList<String>();
		for (String[] rowPtr : rows) 
		{
            String label = rowPtr[0];
            //System.out.println(label);
            
            threatLabels.add(label);

		}
		
	/*	for(int rowIter=0; rowIter < rows.size(); rowIter++)
		{
			threatLabels.add(rows.get(rowIter).toString());
		}*/
		
	    String detectedthreatFile="C:/Users/ID916780/Documents/Eclipse/workspace/MOAOutlierDetect/src/experiments/ProductionLineWorker/results/AnyOut/detectedthreats.txt";
		FileWriter detectedthreatfw = new FileWriter(detectedthreatFile, true);
	    BufferedWriter detectedthreatbw = new BufferedWriter(detectedthreatfw);
	    
	    PrintWriter detectedthreatwriter = new PrintWriter(detectedthreatbw);
	    
		
		try{
			
			int confAggr=2; //between 1 & 10
			int conf=4; //between 1 & 6
	        double[] thresholdArray={0.1,0.4,0.7}; //between 0 & 1
			//double[] thresholdArray={0.01}; //between 0 & 1
			
			int[] oScoreAggrArray = {2, 4, 6, 8}; //between 1 & 10
			//int[] oScoreAggrArray = {1};
			int[] windowArray = {50, 100, 150, 200};
		    int trainSetSize=500; //between 0 & 10000
			
			int nbrRows;
			List<FlagRecord> ensembleVoteList;
			
			for(int t=0;t<thresholdArray.length;t++)
				for(int osc=0;osc<oScoreAggrArray.length;osc++)
					for(int w=0;w<windowArray.length;w++)
					{
							writer.println("trainSetSize="+trainSetSize+", oScoreAggr="+oScoreAggrArray[osc]+", confAggr="+confAggr+", conf="+conf+", threshold="+thresholdArray[t]+", window="+windowArray[w]);
							int TPt=0;
							int FPAlarm=0;	
							//int FNt=0;	//malicious behaviours (outliers) predicted as normal
							
							Set<String> uniqueThreats = new HashSet<String>(threatLabels);
							uniqueThreats.remove("Normal");
							//System.out.println(uniqueThreats.size());
							
							/*for (String thr:uniqueThreats) 
							{
					            System.out.println(thr);
							}*/
							
							
							Set<Outlier> allEnsembleOutliers = new TreeSet<Outlier>();
							nbrRows = (int)nbrSessions/windowArray[w];
							ensembleVoteList = new ArrayList<FlagRecord>();
				
							for(int windowIter=0;windowIter<nbrRows;windowIter++)
							{
								ensembleVoteList.add(new FlagRecord(windowIter*windowArray[w],0));
							}
							
							for (String csvFileName : fileNamesList)
							{
								
								//System.out.println(csvFileName);
					            //writer.println(csvFileName);
					            
						       
								tuneParameters(filesPath +"/"+ csvFileName, trainSetSize, oScoreAggrArray[osc], confAggr, conf, thresholdArray[t], windowArray[w]);
							
								
								for(int windowIter=0;windowIter<nbrRows;windowIter++)
								{
									int preVote = ensembleVoteList.get(windowIter).getFlag();
									int subVote = subFlagAlertList.get(windowIter).getFlag();
									ensembleVoteList.get(windowIter).setFlag(preVote+subVote);
									
									Set<Outlier> subOutliers = subFlagAlertList.get(windowIter).getOutlierSet();
									ensembleVoteList.get(windowIter).appendOutlierSet(subOutliers);
									
									allEnsembleOutliers.addAll(subOutliers);
									
								}
							}
							//TPt if outliers detected regardless of windows cover all threat  
							//FPAlarm if instances (session slots) covered by this window are all normal
								
							//iterate through ensembleVoteList
							//if pred >= voteFactor
								//if true check if actual label not normal
									//if true check if threat is found in unique list
										//if not then TPt++ and and remove from unique list 
							
							detectedthreatwriter.println("trainSetSize="+trainSetSize+", oScoreAggr="+oScoreAggrArray[osc]+", confAggr="+confAggr+", conf="+conf+", threshold="+thresholdArray[t]+", window="+windowArray[w]);
							List<List<String>> detectedwindowThreats = new ArrayList<List<String>>();
							for(int windowIter=0;windowIter<nbrRows;windowIter++)
							{
								detectedwindowThreats.add(new ArrayList<String>());
							}
							
							for(int windowIter=0;windowIter<nbrRows;windowIter++)
							{
								int vote = ensembleVoteList.get(windowIter).getFlag(); // predicted TP votes
								//System.out.println(vote);
								
								int foundFlag;
								
							    if(vote >= voteFactor) // TP votes > voteFactor i.e. flag alarm can be TPt or FP
							    {
							    	foundFlag=0;
							        for(Outlier o:ensembleVoteList.get(windowIter).getOutlierSet())
							        {
							        	String outlierLabel = threatLabels.get((int)(long) o.id);
							        	//System.out.println(outlierLabel);
							        	if(outlierLabel.equals("Normal") == false)
							        	{

						        			if(detectedwindowThreats.get(windowIter).contains(outlierLabel)==false)
											{
												detectedwindowThreats.get(windowIter).add(outlierLabel);
												
											}
						        			
						        			
							        		foundFlag=1;
							        		if(uniqueThreats.contains(outlierLabel) == true)
							        		{
												TPt++;
												//System.out.println(uniqueThreats.size());
												uniqueThreats.remove(outlierLabel);
												//System.out.println(uniqueThreats.size());
											}
							        	}
							        		
							        }
							        if(foundFlag==0)//no TP found, all outliers are Normal
							        {
							        	FPAlarm++;
							        }
							    }
							    detectedthreatwriter.print(windowIter);
								for(String detectedpresentThreat:detectedwindowThreats.get(windowIter))
								{
									detectedthreatwriter.print(","+detectedpresentThreat);
								}
								detectedthreatwriter.println();
							}
							
							for(int windowIter=0;windowIter<nbrRows;windowIter++)
							{
								writer.println(ensembleVoteList.get(windowIter).getWIter()+","+ensembleVoteList.get(windowIter).getFlag());
							}
							writer.println();
							writer.println("TPt="+TPt+", FPAlarm="+FPAlarm);
							writer.println("--------------------------------------------------------------");
							
						}
				
		    		writer.close();
	        } catch (Exception e){
	           // do something
	        }
		
	}
	        
}


