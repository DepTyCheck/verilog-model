-- Seed: 9594635396789106518,12011142928354116943

entity ymiwltwhpn is
  port (ref : inout severity_level; nedon : inout boolean_vector(1 to 4); sekdat : in boolean);
end ymiwltwhpn;

architecture vhikdlz of ymiwltwhpn is
  
begin
  -- Single-driven assignments
  nedon <= (FALSE, TRUE, FALSE, TRUE);
  ref <= ERROR;
end vhikdlz;

entity yucfb is
  port (qnltet : in time);
end yucfb;

architecture knbfi of yucfb is
  signal dhiel : boolean;
  signal ood : boolean_vector(1 to 4);
  signal buih : severity_level;
  signal mwpcq : boolean;
  signal julelpi : boolean_vector(1 to 4);
  signal kzfkv : severity_level;
  signal vpvizbxat : boolean_vector(1 to 4);
  signal qklz : severity_level;
  signal xologslamc : boolean;
  signal iiwwpcetf : boolean_vector(1 to 4);
  signal dtrryy : severity_level;
begin
  hruygeotey : entity work.ymiwltwhpn
    port map (ref => dtrryy, nedon => iiwwpcetf, sekdat => xologslamc);
  ciddtfy : entity work.ymiwltwhpn
    port map (ref => qklz, nedon => vpvizbxat, sekdat => xologslamc);
  bfiaqkjgyb : entity work.ymiwltwhpn
    port map (ref => kzfkv, nedon => julelpi, sekdat => mwpcq);
  rgokh : entity work.ymiwltwhpn
    port map (ref => buih, nedon => ood, sekdat => dhiel);
  
  -- Single-driven assignments
  xologslamc <= FALSE;
  dhiel <= TRUE;
  mwpcq <= FALSE;
end knbfi;



-- Seed after: 6501238944786112680,12011142928354116943
