-- Seed: 5723386040591882636,5805648483995786113

entity hym is
  port (ig : in time; xvmfgxqcqr : in time; mqynfy : inout character; klpswr : linkage time);
end hym;

architecture yqkyrf of hym is
  
begin
  -- Single-driven assignments
  mqynfy <= mqynfy;
end yqkyrf;

entity opxbbxbk is
  port (ix : inout time);
end opxbbxbk;

architecture xlwxheqk of opxbbxbk is
  signal drdmjmxd : time;
  signal yagopstlur : character;
  signal gmyp : time;
  signal fdiyrov : time;
  signal vqgu : character;
  signal jo : time;
  signal aqkalllthz : time;
begin
  pcmdsalpj : entity work.hym
    port map (ig => aqkalllthz, xvmfgxqcqr => jo, mqynfy => vqgu, klpswr => fdiyrov);
  waamoqhrdd : entity work.hym
    port map (ig => ix, xvmfgxqcqr => gmyp, mqynfy => yagopstlur, klpswr => drdmjmxd);
  
  -- Single-driven assignments
  aqkalllthz <= 4 sec;
  jo <= 0_3_2_1.2_2 ns;
  ix <= 8#6045.5_4_0_0_2# us;
end xlwxheqk;

entity onhb is
  port (aye : linkage character; y : inout character);
end onhb;

architecture dvvyc of onhb is
  signal yez : time;
  signal ofdqssktre : time;
begin
  kqjgyv : entity work.opxbbxbk
    port map (ix => ofdqssktre);
  rgburi : entity work.hym
    port map (ig => yez, xvmfgxqcqr => ofdqssktre, mqynfy => y, klpswr => yez);
end dvvyc;



-- Seed after: 4819401050796837112,5805648483995786113
