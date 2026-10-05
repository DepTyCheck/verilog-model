-- Seed: 10221554976944934944,7304262412290825129

entity ldwm is
  port (yqh : buffer time_vector(2 to 3); elbh : in integer; lvmvb : out time; dg : out real_vector(2 to 0));
end ldwm;

architecture jbzx of ldwm is
  
begin
  -- Single-driven assignments
  lvmvb <= lvmvb;
  yqh <= yqh;
  dg <= dg;
end jbzx;

entity ldeu is
  port (pwkc : linkage time; q : out integer; ogduloty : inout integer);
end ldeu;

architecture mw of ldeu is
  signal qtkh : real_vector(2 to 0);
  signal emulwvne : time;
  signal rwelkejsjw : integer;
  signal eyekdu : time_vector(2 to 3);
  signal mulfuzpl : real_vector(2 to 0);
  signal hutxn : time;
  signal zfxt : time_vector(2 to 3);
  signal iguc : real_vector(2 to 0);
  signal qeomip : time;
  signal kgarcvx : time_vector(2 to 3);
begin
  oc : entity work.ldwm
    port map (yqh => kgarcvx, elbh => ogduloty, lvmvb => qeomip, dg => iguc);
  qvzzmcls : entity work.ldwm
    port map (yqh => zfxt, elbh => q, lvmvb => hutxn, dg => mulfuzpl);
  qe : entity work.ldwm
    port map (yqh => eyekdu, elbh => rwelkejsjw, lvmvb => emulwvne, dg => qtkh);
  
  -- Single-driven assignments
  q <= ogduloty;
  rwelkejsjw <= 3_3_2_1_4;
  ogduloty <= rwelkejsjw;
end mw;



-- Seed after: 2886413540543626807,7304262412290825129
