package mypackage;


public class TPRecord
	{
		private int outlierId;
		private int temporalFactor; //a temporalFactor is utilized to confirm that an outlier is true, 
								//thus minimizing the number of false alarms
	private int wIterDetect;	//window iteration an outlier was first detected on
	public TPRecord(int id,int tempFactor,int iter)
	{
		outlierId=id;
		temporalFactor=tempFactor;
		wIterDetect=iter;
	}
	public int getOutlierId()
	{
		return outlierId;
	}
	public int getTemporalFactor()
	{
		return temporalFactor;
	}
	public void setTemporalFactor(int tempFactor)
	{
		temporalFactor=tempFactor;
	}
	public int getIteration()
	{
		return wIterDetect;
	}
}

