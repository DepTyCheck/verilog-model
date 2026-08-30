-- Seed: 12635233736756050218,4080032123900078489

entity dnds is
  port (lgsephs : buffer integer; ihrmgebf : out time);
end dnds;

architecture rybvdl of dnds is
  
begin
  -- Single-driven assignments
  ihrmgebf <= ihrmgebf;
  lgsephs <= 223;
end rybvdl;

library ieee;
use ieee.std_logic_1164.all;

entity prutn is
  port (atxtfwuvru : buffer std_logic; ynjomy : in std_logic; fffpqtov : out time);
end prutn;

architecture sqkkiccn of prutn is
  signal vhypge : time;
  signal gnxat : integer;
  signal ldrzla : time;
  signal plij : integer;
  signal aqfuymby : time;
  signal ncwc : integer;
  signal f : integer;
begin
  rrg : entity work.dnds
    port map (lgsephs => f, ihrmgebf => fffpqtov);
  z : entity work.dnds
    port map (lgsephs => ncwc, ihrmgebf => aqfuymby);
  znh : entity work.dnds
    port map (lgsephs => plij, ihrmgebf => ldrzla);
  xsbc : entity work.dnds
    port map (lgsephs => gnxat, ihrmgebf => vhypge);
  
  -- Multi-driven assignments
  atxtfwuvru <= ynjomy;
end sqkkiccn;



-- Seed after: 12784103834822633740,4080032123900078489
