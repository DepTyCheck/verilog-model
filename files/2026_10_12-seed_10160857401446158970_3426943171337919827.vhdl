-- Seed: 10160857401446158970,3426943171337919827

entity edcvk is
  port (pbx : inout string(5 to 2));
end edcvk;

architecture pkm of edcvk is
  
begin
  
end pkm;

entity yejwcqiq is
  port (cz : buffer bit);
end yejwcqiq;

architecture nlx of yejwcqiq is
  signal lpbyiewtbr : string(5 to 2);
  signal kbsiyzkoa : string(5 to 2);
begin
  vwtcnut : entity work.edcvk
    port map (pbx => kbsiyzkoa);
  bxgfomqyun : entity work.edcvk
    port map (pbx => lpbyiewtbr);
  
  -- Single-driven assignments
  cz <= cz;
end nlx;

library ieee;
use ieee.std_logic_1164.all;

entity tbopsp is
  port (jf : inout real; kh : inout integer; hzkyvwdx : in integer; docqj : in std_logic_vector(3 to 0));
end tbopsp;

architecture uopabgyj of tbopsp is
  signal samjjk : bit;
  signal jzf : bit;
  signal r : string(5 to 2);
begin
  mio : entity work.edcvk
    port map (pbx => r);
  nxo : entity work.yejwcqiq
    port map (cz => jzf);
  sqwh : entity work.yejwcqiq
    port map (cz => samjjk);
  
  -- Single-driven assignments
  kh <= 31341;
  jf <= 16#5_3_C_3_D.4E#;
end uopabgyj;



-- Seed after: 14980722836723690500,3426943171337919827
