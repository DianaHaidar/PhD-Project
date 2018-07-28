package mypackage;


import java.util.Set;
import java.util.TreeSet;

import moa.clusterers.outliers.MyBaseOutlierDetector.Outlier;

public class FlagRecord
	{
		private int wIter; //window iteration an outlier generated an alert
		private int flag;   //0 or 1 for subFlagAlertList 
							//0 or more for ensembleVoteList
		private Set<Outlier> outlierSet;	
	public FlagRecord(int iter, int flagAlertVote)
	{
		wIter=iter;
		flag=flagAlertVote;
		outlierSet=new TreeSet<Outlier>();
	}
	public int getWIter()
	{
		return wIter;
	}
	public int getFlag()
	{
		return flag;
	}
	public void setFlag(int flagAlertVote)
	{
		flag=flagAlertVote;
	}
	public Set<Outlier> getOutlierSet()
	{
		return outlierSet;
	}
	public void appendOutlierSet(Set<Outlier> outliers) //ignores repetitions
	{
		outlierSet.addAll(outliers);
	}
}

