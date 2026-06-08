-- Seed: 970355606341666342,7142793346053417159

library ieee;
use ieee.std_logic_1164.all;

entity mccntvzrm is
  port (vtw : out std_logic_vector(2 to 0); eupdgcg : in real);
end mccntvzrm;



architecture lwrrsktni of mccntvzrm is
  
begin
  
end lwrrsktni;



entity aabxt is
  port (qgi : out time_vector(4 downto 1); yebipkm : in time_vector(3 to 0));
end aabxt;

library ieee;
use ieee.std_logic_1164.all;

architecture zwuts of aabxt is
  signal nco : real;
  signal itjnslbash : std_logic_vector(2 to 0);
begin
  jrni : entity work.mccntvzrm
    port map (vtw => itjnslbash, eupdgcg => nco);
end zwuts;

library ieee;
use ieee.std_logic_1164.all;

entity mu is
  port (afvfclcc : in std_logic; kkkjbcbufp : in std_logic_vector(0 downto 3));
end mu;

library ieee;
use ieee.std_logic_1164.all;

architecture km of mu is
  signal eqfngguj : time_vector(3 to 0);
  signal ihrqprkzk : time_vector(4 downto 1);
  signal aakvffffw : real;
  signal hey : std_logic_vector(2 to 0);
  signal f : real;
  signal ihmodfaf : std_logic_vector(2 to 0);
begin
  pf : entity work.mccntvzrm
    port map (vtw => ihmodfaf, eupdgcg => f);
  oanol : entity work.mccntvzrm
    port map (vtw => hey, eupdgcg => aakvffffw);
  xrr : entity work.mccntvzrm
    port map (vtw => hey, eupdgcg => f);
  qgmvmwuu : entity work.aabxt
    port map (qgi => ihrqprkzk, yebipkm => eqfngguj);
end km;



-- Seed after: 2989030758197152209,7142793346053417159
