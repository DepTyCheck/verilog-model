-- Seed: 13244744386293795677,1112937151005418631

entity cwhvvq is
  port (pvsk : linkage boolean_vector(0 to 4));
end cwhvvq;

architecture bea of cwhvvq is
  
begin
  
end bea;

entity ttqoejrh is
  port (t : linkage time_vector(1 downto 1); sgtfskpzxk : buffer time; aiskvgffie : buffer integer);
end ttqoejrh;

architecture y of ttqoejrh is
  signal ihmlbvscxy : boolean_vector(0 to 4);
  signal fvxdiui : boolean_vector(0 to 4);
  signal cz : boolean_vector(0 to 4);
  signal nbgyiroueq : boolean_vector(0 to 4);
begin
  uugc : entity work.cwhvvq
    port map (pvsk => nbgyiroueq);
  jvbymnwgvj : entity work.cwhvvq
    port map (pvsk => cz);
  uma : entity work.cwhvvq
    port map (pvsk => fvxdiui);
  xzxbd : entity work.cwhvvq
    port map (pvsk => ihmlbvscxy);
  
  -- Single-driven assignments
  aiskvgffie <= aiskvgffie;
  sgtfskpzxk <= 0_3_3_3 ms;
end y;



-- Seed after: 7729199671626907054,1112937151005418631
