package fr.diaspec.webcam.generated;


public abstract class AbstractRunner extends CommonRuncode {

    // Taxonomy components (actions, sources)
	// initialised immediately because final
    private static final AbstractCamera c  = new Camera();
    private static final AbstractIP     ip = new IP();
    private static final AbstractScreen s  = new Screen();
    // these need to be class fields so that the instances are long-lived.
    private AbstractMakeAd ad;
    private AbstractProcessPicture mp;
    private AbstractComposeDisplay as;
    private AbstractDisplay sc;
    
    @Override
    protected void init() {
    	Log.i("gc","starting init()");
    	
    	ad = getMakeAd();
    	ad.init(this);
    	ctxs.add(ad);
        
    	mp = getProcessPicture();
        mp.init(this);
        ctxs.add(mp);
        
        as = getComposeDisplay();
        as.init(this);
        ctxs.add(as);
        
        sc = getDisplay();
        sc.init(this); // give them a pointer to this instance of Runner (for querying resources)
        ctrls.add(sc);
           	
    	// taxonomy configuration
    	acts.add(s);
    	srcs.add(c);
    	srcs.add(ip);
    	        
        // subscription relations
        c.addSubscriber(mp);
        mp.addSubscriber(as);
        as.addSubscriber(sc);
  
        // resources have their init() called once the programmer 
        // uses the run() method, since they start the publication cycle.
    }
  
    // to-be-implemented components: (contexts, controllers)
    public abstract AbstractProcessPicture  getProcessPicture();
    public abstract AbstractMakeAd          getMakeAd();
    public abstract AbstractComposeDisplay  getComposeDisplay();
    public abstract AbstractDisplay         getDisplay();

   
    public AbstractScreen getScreen() {
        return s;
    }
   
    public AbstractIP getIP() {
    	return ip;
    }
    
    public AbstractCamera getCamera() {
    	return c;
    }
   
}
