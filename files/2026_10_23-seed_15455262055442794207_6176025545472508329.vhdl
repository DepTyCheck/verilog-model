-- Seed: 15455262055442794207,6176025545472508329

library ieee;
use ieee.std_logic_1164.all;

entity cfasi is
  port (pwlwocaoi : buffer real; oqnzihhzzp : linkage std_logic; keq : linkage time);
end cfasi;

architecture dzhubjgybj of cfasi is
  
begin
  
end dzhubjgybj;

entity ertisjpxdv is
  port (qfult : in real; f : buffer real);
end ertisjpxdv;

architecture toomop of ertisjpxdv is
  
begin
  -- Single-driven assignments
  f <= 16#B.A4E#;
end toomop;

entity qyqw is
  port (havpwcgm : inout integer_vector(2 to 2); jac : inout real);
end qyqw;

library ieee;
use ieee.std_logic_1164.all;

architecture xy of qyqw is
  signal wkpgwxh : time;
  signal fgoimsp : time;
  signal xtyszgznty : std_logic;
  signal cmcrtl : real;
  signal sadjmqvlja : time;
  signal lhsqgewrr : std_logic;
  signal kvlmwbzgzk : real;
begin
  ynhttqbleq : entity work.cfasi
    port map (pwlwocaoi => kvlmwbzgzk, oqnzihhzzp => lhsqgewrr, keq => sadjmqvlja);
  ukqbhs : entity work.cfasi
    port map (pwlwocaoi => cmcrtl, oqnzihhzzp => xtyszgznty, keq => fgoimsp);
  olc : entity work.cfasi
    port map (pwlwocaoi => jac, oqnzihhzzp => lhsqgewrr, keq => wkpgwxh);
  
  -- Single-driven assignments
  havpwcgm <= havpwcgm;
  
  -- Multi-driven assignments
  lhsqgewrr <= 'U';
  lhsqgewrr <= lhsqgewrr;
  lhsqgewrr <= '1';
  lhsqgewrr <= lhsqgewrr;
end xy;

library ieee;
use ieee.std_logic_1164.all;

entity bvrwbutfvl is
  port (e : inout std_logic; fpkbs : linkage bit_vector(4 to 3); hjzbtztpya : in integer);
end bvrwbutfvl;

library ieee;
use ieee.std_logic_1164.all;

architecture wm of bvrwbutfvl is
  signal pecivfcxir : time;
  signal exsowru : std_logic;
  signal qiobgnp : real;
  signal crllu : real;
begin
  qokren : entity work.ertisjpxdv
    port map (qfult => crllu, f => crllu);
  bfmotlrx : entity work.cfasi
    port map (pwlwocaoi => qiobgnp, oqnzihhzzp => exsowru, keq => pecivfcxir);
  
  -- Multi-driven assignments
  e <= 'H';
  e <= e;
  e <= 'L';
end wm;



-- Seed after: 1807199886425228615,6176025545472508329
