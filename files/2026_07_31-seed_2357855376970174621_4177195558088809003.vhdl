-- Seed: 2357855376970174621,4177195558088809003

library ieee;
use ieee.std_logic_1164.all;

entity kut is
  port (c : inout std_logic_vector(4 to 4); i : inout time);
end kut;

architecture psw of kut is
  
begin
  -- Single-driven assignments
  i <= i;
end psw;

entity ekyhh is
  port (eu : out integer; tcklnmlxs : out bit_vector(4 to 3));
end ekyhh;

library ieee;
use ieee.std_logic_1164.all;

architecture edywwu of ekyhh is
  signal vtavmroo : time;
  signal gsq : std_logic_vector(4 to 4);
begin
  kj : entity work.kut
    port map (c => gsq, i => vtavmroo);
  
  -- Multi-driven assignments
  gsq <= gsq;
  gsq <= (others => 'X');
end edywwu;

entity faxru is
  port (szgzc : in integer; h : linkage time; almktj : out character);
end faxru;

library ieee;
use ieee.std_logic_1164.all;

architecture x of faxru is
  signal tqd : time;
  signal lkxafokhc : std_logic_vector(4 to 4);
  signal ytlklexsqw : bit_vector(4 to 3);
  signal odv : integer;
begin
  d : entity work.ekyhh
    port map (eu => odv, tcklnmlxs => ytlklexsqw);
  podkfjupnt : entity work.kut
    port map (c => lkxafokhc, i => tqd);
end x;



-- Seed after: 14915499564074987358,4177195558088809003
