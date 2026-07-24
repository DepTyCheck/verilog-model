-- Seed: 1358613159393030059,16461708287571398341

entity afmgobasid is
  port (lpuawjd : buffer integer; dwu : out real; ikysidjy : in real; xbrka : in real_vector(3 to 2));
end afmgobasid;

architecture eexwbdg of afmgobasid is
  
begin
  -- Single-driven assignments
  dwu <= 16#4_B_9.3#;
end eexwbdg;

library ieee;
use ieee.std_logic_1164.all;

entity oadob is
  port (lbewiscqze : in time_vector(2 to 2); m : linkage std_logic);
end oadob;

architecture dabvmc of oadob is
  signal uh : integer;
  signal o : real_vector(3 to 2);
  signal meck : real;
  signal jzgqme : integer;
  signal idq : real_vector(3 to 2);
  signal lje : real;
  signal jdvs : real;
  signal ibeby : integer;
begin
  tfhz : entity work.afmgobasid
    port map (lpuawjd => ibeby, dwu => jdvs, ikysidjy => lje, xbrka => idq);
  strw : entity work.afmgobasid
    port map (lpuawjd => jzgqme, dwu => meck, ikysidjy => lje, xbrka => o);
  cboyjoxglj : entity work.afmgobasid
    port map (lpuawjd => uh, dwu => lje, ikysidjy => lje, xbrka => idq);
  
  -- Single-driven assignments
  idq <= (others => 0.0);
  o <= idq;
end dabvmc;



-- Seed after: 653101648551953834,16461708287571398341
