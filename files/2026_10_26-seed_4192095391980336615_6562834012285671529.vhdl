-- Seed: 4192095391980336615,6562834012285671529

entity svl is
  port (iqutaua : inout time; zezefshrh : out time);
end svl;

architecture anmkzwvh of svl is
  
begin
  -- Single-driven assignments
  zezefshrh <= 03.2 fs;
  iqutaua <= 8#4335# ps;
end anmkzwvh;

entity tunahh is
  port (efptcma : linkage time; v : linkage boolean; svumrf : out boolean);
end tunahh;

architecture yuyxdqxdgi of tunahh is
  signal ssejbwv : time;
  signal ajktjnoyu : time;
  signal csxbkyu : time;
  signal qatgmqevq : time;
  signal ed : time;
  signal mc : time;
begin
  bmxjdvggxx : entity work.svl
    port map (iqutaua => mc, zezefshrh => ed);
  vfixdn : entity work.svl
    port map (iqutaua => qatgmqevq, zezefshrh => csxbkyu);
  rkrmwksl : entity work.svl
    port map (iqutaua => ajktjnoyu, zezefshrh => ssejbwv);
  
  -- Single-driven assignments
  svumrf <= TRUE;
end yuyxdqxdgi;

entity rxphsespc is
  port (dc : in integer; j : in string(5 to 3); vp : inout boolean_vector(4 downto 2); gwxr : inout severity_level);
end rxphsespc;

architecture omhz of rxphsespc is
  signal voxrihd : time;
  signal xzf : time;
  signal cbkoxvxg : boolean;
  signal rxhklt : boolean;
  signal fimldtg : time;
  signal d : time;
  signal nqvs : time;
begin
  ldqeh : entity work.svl
    port map (iqutaua => nqvs, zezefshrh => d);
  al : entity work.tunahh
    port map (efptcma => fimldtg, v => rxhklt, svumrf => cbkoxvxg);
  vcacrvo : entity work.svl
    port map (iqutaua => xzf, zezefshrh => voxrihd);
  
  -- Single-driven assignments
  gwxr <= gwxr;
  vp <= vp;
end omhz;



-- Seed after: 16963720454492153557,6562834012285671529
