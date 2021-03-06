package moa.clusterers.outliers.MCOD;

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

public class ERAIDSMCOD {
	static Long tmStart;
	static FileWriter fw;
    static BufferedWriter bw;
    static PrintWriter writer;
    static List<FlagRecord> subFlagAlertList;
	// variables depend on each community
	static int nbrSessions = 3096; 

	static Reader reader;
	static List<String[]> rows;
	
	public static void tuneParameters(String csvFile, int k,double radius,int windowSize)
	{
		//if (true) return;
		
        int numInstances = nbrSessions;
        SimpleCSVStream stream = new SimpleCSVStream();
        stream.csvFileOption = new FileOption("csvFile", 'f',
    			"CSV file to load.", csvFile, "csv", false);

        stream.prepareForUse();
       
        MCOD myOutlierDetector= new MCOD();
        myOutlierDetector.kOption.setValue(k);
        myOutlierDetector.radiusOption.setValue(radius);
        myOutlierDetector.windowSizeOption.setValue(windowSize);
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
						// this means TP survived as TP for a number of windows (actor)
					
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
		String consoleFile="C:/Users/ID916780/Documents/Eclipse/workspace/MOAOutlierDetect/src/experiments/ProductionLineWorker/results/MCOD/myConsole.txt";
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
		
		String threatFile="C:/Users/ID916780/Documents/Eclipse/workspace/MOAOutlierDetect/src/experiments/ProductionLineWorker/results/MCOD/threats.txt";
		FileWriter threatfw = new FileWriter(threatFile, true);
	    BufferedWriter threatbw = new BufferedWriter(threatfw);
	    
	    PrintWriter threatwriter = new PrintWriter(threatbw);
	    
	    String detectedthreatFile="C:/Users/ID916780/Documents/Eclipse/workspace/MOAOutlierDetect/src/experiments/ProductionLineWorker/results/MCOD/detectedthreats.txt";
		FileWriter detectedthreatfw = new FileWriter(detectedthreatFile, true);
	    BufferedWriter detectedthreatbw = new BufferedWriter(detectedthreatfw);
	    
	    PrintWriter detectedthreatwriter = new PrintWriter(detectedthreatbw);
		
		try{
			
	  	    int[] kArray = {50, 60, 70};
			double[] radiusArray = {0.3, 0.4, 0.5, 0.6, 0.7};
			int[] windowArray = {50, 100, 150, 200};
			int nbrRows;
			List<FlagRecord> ensembleVoteList;
			for(int k=0;k<kArray.length;k++)
				for(int r=0;r<radiusArray.length;r++)
					for(int w=0;w<windowArray.length;w++)
					{
						
						writer.println("k="+kArray[k]+", r="+radiusArray[r]+", w="+windowArray[w]);
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
						
						
						threatwriter.println("WindowSize="+windowArray[w]);
						List<List<String>> windowThreats = new ArrayList<List<String>>();
						for(int windowIter=0;windowIter<nbrRows;windowIter++)
						{
							windowThreats.add(new ArrayList<String>());
						}
						
						for(int windowIter=0;windowIter<nbrRows;windowIter++)
						{
							int windowLB=windowIter*windowArray[w];
							int windowUB=windowIter*windowArray[w]+windowArray[w];
							for(int wIndex=windowLB;wIndex<windowUB;wIndex++)
							{
								String wIndexThreat = threatLabels.get(wIndex);
								if(wIndexThreat.equals("Normal")==false)
								{
									if(windowThreats.get(windowIter).contains(wIndexThreat)==false)
									{
										windowThreats.get(windowIter).add(wIndexThreat);
										
									}
									
								}
							}
							
							threatwriter.print(windowIter);
							for(String presentThreat:windowThreats.get(windowIter))
							{
								threatwriter.print(","+presentThreat);
							}
							threatwriter.println();
						}
			
					
						for (String csvFileName : fileNamesList)
						{
							
							//System.out.println(csvFileName);
				            //writer.println(csvFileName);
				            
					       
							tuneParameters(filesPath +"/"+ csvFileName, kArray[k],radiusArray[r],windowArray[w]);
							
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
						
						detectedthreatwriter.println("k="+kArray[k]+", r="+radiusArray[r]+", w="+windowArray[w]);
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
		    		threatwriter.close();
		    		detectedthreatwriter.close();
	        } catch (Exception e){
	           // do something
	        }
		
	}
	        
}


