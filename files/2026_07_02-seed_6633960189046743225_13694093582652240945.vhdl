-- Seed: 6633960189046743225,13694093582652240945

entity ylaucp is
  port (nlfres : out time; ahx : in bit);
end ylaucp;

architecture m of ylaucp is
  
begin
  -- Single-driven assignments
  nlfres <= 3_2_3_0.3_0_0_2 ns;
end m;

library ieee;
use ieee.std_logic_1164.all;

entity sjkopkyeb is
  port (wwys : in std_logic; iacfpxjox : inout time_vector(1 downto 3); o : inout boolean; oablgx : buffer boolean);
end sjkopkyeb;

architecture igjgztjpqp of sjkopkyeb is
  signal pwvz : bit;
  signal nwdcqx : time;
  signal fkgtvw : bit;
  signal b : time;
  signal sjgqusum : bit;
  signal azclujxkzy : time;
begin
  ybjfald : entity work.ylaucp
    port map (nlfres => azclujxkzy, ahx => sjgqusum);
  yashmywnm : entity work.ylaucp
    port map (nlfres => b, ahx => fkgtvw);
  glhhwyq : entity work.ylaucp
    port map (nlfres => nwdcqx, ahx => pwvz);
  
  -- Single-driven assignments
  o <= FALSE;
  oablgx <= TRUE;
end igjgztjpqp;

entity cdocvhc is
  port (x : in time);
end cdocvhc;

architecture nbi of cdocvhc is
  signal clpy : bit;
  signal moqtzi : time;
  signal joocu : time;
  signal dx : time;
  signal klsuq : bit;
  signal fmrklsktaf : time;
begin
  fwxmmmi : entity work.ylaucp
    port map (nlfres => fmrklsktaf, ahx => klsuq);
  hbbfmoocfd : entity work.ylaucp
    port map (nlfres => dx, ahx => klsuq);
  ksiiks : entity work.ylaucp
    port map (nlfres => joocu, ahx => klsuq);
  jo : entity work.ylaucp
    port map (nlfres => moqtzi, ahx => clpy);
  
  -- Single-driven assignments
  klsuq <= '1';
  clpy <= '1';
end nbi;



-- Seed after: 300969775550741769,13694093582652240945
