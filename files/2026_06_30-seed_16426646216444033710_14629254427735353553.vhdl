-- Seed: 16426646216444033710,14629254427735353553

library ieee;
use ieee.std_logic_1164.all;

entity flwn is
  port (tdhbvijhza : inout time; ewu : inout boolean; bqamekveq : out std_logic);
end flwn;

architecture oucqqdwjq of flwn is
  
begin
  -- Single-driven assignments
  ewu <= TRUE;
  tdhbvijhza <= 2_3.34112 ps;
end oucqqdwjq;

entity uk is
  port (chuzoac : buffer bit; iok : buffer bit; s : out time);
end uk;

library ieee;
use ieee.std_logic_1164.all;

architecture szpksdsxwt of uk is
  signal hidz : std_logic;
  signal mmhfnqtdwf : boolean;
  signal jlkx : time;
  signal rvnwoed : boolean;
  signal qumhqtsm : time;
  signal uqmcoox : std_logic;
  signal kno : boolean;
  signal bhrfky : time;
  signal ilvopo : std_logic;
  signal p : boolean;
  signal fzeczfb : time;
begin
  ntaitlbaso : entity work.flwn
    port map (tdhbvijhza => fzeczfb, ewu => p, bqamekveq => ilvopo);
  kxfqcqo : entity work.flwn
    port map (tdhbvijhza => bhrfky, ewu => kno, bqamekveq => uqmcoox);
  yxxijagg : entity work.flwn
    port map (tdhbvijhza => qumhqtsm, ewu => rvnwoed, bqamekveq => ilvopo);
  hsshxpssi : entity work.flwn
    port map (tdhbvijhza => jlkx, ewu => mmhfnqtdwf, bqamekveq => hidz);
  
  -- Multi-driven assignments
  ilvopo <= 'W';
  hidz <= 'H';
end szpksdsxwt;



-- Seed after: 16686456504748531460,14629254427735353553
