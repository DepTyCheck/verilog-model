-- Seed: 18027328318181607893,2230106469645304029

entity oylkyavh is
  port (lmsi : out time; vybv : out time);
end oylkyavh;

architecture zftgmuw of oylkyavh is
  
begin
  -- Single-driven assignments
  vybv <= 1 hr;
  lmsi <= 8#17746# ps;
end zftgmuw;

entity hp is
  port (jmhp : buffer time; bgpgnv : inout real; tvejoe : inout integer; rlgq : out time);
end hp;

architecture gkz of hp is
  signal kppef : time;
  signal t : time;
  signal vrlh : time;
begin
  ikxp : entity work.oylkyavh
    port map (lmsi => vrlh, vybv => t);
  xs : entity work.oylkyavh
    port map (lmsi => jmhp, vybv => kppef);
  
  -- Single-driven assignments
  tvejoe <= 16#CE1#;
  rlgq <= rlgq;
end gkz;

entity srrhquge is
  port (avporbhds : buffer severity_level);
end srrhquge;

architecture zctt of srrhquge is
  signal lekbxs : time;
  signal yj : integer;
  signal jgzxjnkksr : real;
  signal hmev : time;
begin
  qjizg : entity work.hp
    port map (jmhp => hmev, bgpgnv => jgzxjnkksr, tvejoe => yj, rlgq => lekbxs);
  
  -- Single-driven assignments
  avporbhds <= WARNING;
end zctt;



-- Seed after: 2682633740757803874,2230106469645304029
