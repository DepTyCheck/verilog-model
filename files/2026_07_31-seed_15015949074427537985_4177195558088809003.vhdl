-- Seed: 15015949074427537985,4177195558088809003

library ieee;
use ieee.std_logic_1164.all;

entity dgzch is
  port (ulftdkqm : buffer std_logic);
end dgzch;

architecture i of dgzch is
  
begin
  -- Multi-driven assignments
  ulftdkqm <= '0';
  ulftdkqm <= ulftdkqm;
  ulftdkqm <= 'Z';
  ulftdkqm <= ulftdkqm;
end i;

entity q is
  port (khcvbiopnb : in severity_level; yijaujv : linkage integer; madjoukbq : linkage string(2 to 4));
end q;

library ieee;
use ieee.std_logic_1164.all;

architecture hz of q is
  signal yahynzyllu : std_logic;
begin
  limrcl : entity work.dgzch
    port map (ulftdkqm => yahynzyllu);
  
  -- Multi-driven assignments
  yahynzyllu <= yahynzyllu;
end hz;

library ieee;
use ieee.std_logic_1164.all;

entity mqyknp is
  port (gg : inout time; kofyyazem : inout std_logic; co : in real; hetifapju : linkage integer_vector(4 to 1));
end mqyknp;

architecture yvb of mqyknp is
  signal ugq : string(2 to 4);
  signal bhfmlq : integer;
  signal hlef : severity_level;
begin
  zubc : entity work.q
    port map (khcvbiopnb => hlef, yijaujv => bhfmlq, madjoukbq => ugq);
  hs : entity work.dgzch
    port map (ulftdkqm => kofyyazem);
  
  -- Single-driven assignments
  hlef <= WARNING;
  
  -- Multi-driven assignments
  kofyyazem <= kofyyazem;
  kofyyazem <= 'H';
end yvb;



-- Seed after: 18029708253940181495,4177195558088809003
