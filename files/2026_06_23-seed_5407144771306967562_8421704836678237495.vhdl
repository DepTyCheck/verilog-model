-- Seed: 5407144771306967562,8421704836678237495

entity rmslwepdo is
  port (hlioftox : in real; lzeaygz : inout time; maskmngzwb : in time; nrdxxnccw : inout real);
end rmslwepdo;

architecture vdtj of rmslwepdo is
  
begin
  -- Single-driven assignments
  nrdxxnccw <= 16#138.06A8#;
  lzeaygz <= 2_2.13 ps;
end vdtj;

entity o is
  port (zcfmqxrj : linkage integer; gytct : out time_vector(4 to 3));
end o;

architecture la of o is
  signal npmati : real;
  signal gcwgdzwgos : time;
  signal dzyxjwct : time;
  signal qk : real;
begin
  qtklxkolb : entity work.rmslwepdo
    port map (hlioftox => qk, lzeaygz => dzyxjwct, maskmngzwb => gcwgdzwgos, nrdxxnccw => npmati);
  
  -- Single-driven assignments
  gytct <= (others => 0 ns);
  gcwgdzwgos <= 2#0_0# us;
  qk <= 2#0_1_1_0_1.1_1_0_0_0#;
end la;



-- Seed after: 203186799375201106,8421704836678237495
