package fr.diaspec.webcam.generated;


public abstract class AbstractRunner extends CommonRuncode {

    // Taxonomy components (actions, sources)
    private static final AbstractCamera c  = new Camera();
    private static final AbstractIP     ip = new IP();
    private static final AbstractScreen s  = new Screen();
  
    // to-be-implemented components: (contexts, controllers)
    public abstract AbstractProcessPicture  getProcessPicture();
    public abstract AbstractMakeAd          getMakeAd();
    public abstract AbstractComposeDisplay  getComposeDisplay();
    public abstract AbstractDisplay         getDisplay();

    // these need to be class fields so that the instances are long-lived.
    private AbstractMakeAd ad;
    private AbstractProcessPicture mp;
    private AbstractComposeDisplay as;
    private AbstractDisplay sc;
    
    @Override
    protected void init() {
    	Log.i("gc","starting init()");
    	// taxonomy configuration
    	acts.add(s);
    	srcs.add(c);
    	srcs.add(ip);
    	
    	// IP is an odd one out, it should be initialised first,
    	// so it has a chance to fetch stuff from the network before being polled.
    	// ugly hack. TODO consider just killing this. the user will be informed
    	// if they take a picture and internet wasn't ready yet.
    	//ip.init(this);
    	
        ad = getMakeAd();
        mp = getProcessPicture();
        as = getComposeDisplay();
        sc = getDisplay();
        
        sc.init(this); // give them a pointer to this instance of Runner (for querying resources)
        mp.init(this);
        ad.init(this);
        as.init(this);
        
        ctxs.add(mp);
        ctxs.add(ad);
        ctxs.add(as);
        ctrls.add(sc);
        
        c.addSubscriber(mp);
        mp.addSubscriber(as);
        as.addSubscriber(sc);
  
    }
   
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
