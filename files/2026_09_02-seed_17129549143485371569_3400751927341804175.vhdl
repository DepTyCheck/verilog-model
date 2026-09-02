-- Seed: 17129549143485371569,3400751927341804175

entity afs is
  port (iwqnz : linkage integer);
end afs;

architecture eqcttqm of afs is
  
begin
  
end eqcttqm;

entity r is
  port (v : linkage integer);
end r;

architecture dsqygoyz of r is
  signal abeq : integer;
  signal npgz : integer;
  signal srpsesklvz : integer;
begin
  yjwzkkhaes : entity work.afs
    port map (iwqnz => srpsesklvz);
  zhhtza : entity work.afs
    port map (iwqnz => npgz);
  rqenvc : entity work.afs
    port map (iwqnz => abeq);
  hmiuwcq : entity work.afs
    port map (iwqnz => v);
end dsqygoyz;

entity u is
  port (smarrekw : in character);
end u;

architecture n of u is
  
begin
  
end n;

library ieee;
use ieee.std_logic_1164.all;

entity sizohgu is
  port (ahyxj : inout std_logic);
end sizohgu;

architecture ky of sizohgu is
  signal pmakbd : integer;
  signal jsqdf : integer;
begin
  oatrpgjnsk : entity work.r
    port map (v => jsqdf);
  utg : entity work.r
    port map (v => pmakbd);
  
  -- Multi-driven assignments
  ahyxj <= 'W';
  ahyxj <= 'X';
  ahyxj <= 'X';
end ky;



-- Seed after: 14450053205764699364,3400751927341804175
