-- Seed: 17837057699199750767,5805648483995786113

entity vuojzpuzr is
  port (swedcu : out real);
end vuojzpuzr;

architecture lz of vuojzpuzr is
  
begin
  -- Single-driven assignments
  swedcu <= swedcu;
end lz;

entity vt is
  port (voxt : in time; nhxwkw : in integer);
end vt;

architecture ihlskbi of vt is
  signal upb : real;
begin
  zxolulcs : entity work.vuojzpuzr
    port map (swedcu => upb);
end ihlskbi;

library ieee;
use ieee.std_logic_1164.all;

entity zggltj is
  port (nbi : buffer time; kmhov : in std_logic_vector(0 to 1); yk : linkage integer);
end zggltj;

architecture jlyyvowdr of zggltj is
  signal lixbc : real;
  signal morsrrgtq : integer;
  signal kwyjttos : time;
begin
  jgn : entity work.vt
    port map (voxt => kwyjttos, nhxwkw => morsrrgtq);
  fnhdzrkzwb : entity work.vuojzpuzr
    port map (swedcu => lixbc);
end jlyyvowdr;



-- Seed after: 5937682837098966871,5805648483995786113
